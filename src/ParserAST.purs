module ParserAST where

import Prelude

import Data.Array ((:), snoc)
import Data.CatQueue (CatQueue)
import Data.CatQueue as Q
import Data.Either (Either(..))
import Data.Enum (class Enum, succ)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (List, cons, tail, head, length, nil)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import ParserTypes (AST(..), DerivationType(..), Token, TokenType(..), equals)

-- We let the LL1 parsing take care of correctness for us
-- LL1 parses depth first, left first, so we use a stack to build the AST
-- When a nonterminal is received from parsing, we put it on the stack.
-- When a token is received, we can begin building a portion of the AST,
-- which we can then throw into a queue.
-- If the stack is popped and the next thing on the top is a nonterminal,
-- then we can consider building an ast with that nonterminal and the ASTs in the queue

emptyBuildState :: BuildState
emptyBuildState =
    { stack: emptyStack
    , queue: emptyQueue
    } 


type BuildState = 
    { stack :: Stack
    , queue :: Queue
    }


extract :: BuildState -> Either String AST
extract state@{stack: s, queue: q} = 
    if sizeStack s /= 0
    then Left $ "AST Stack was not empty: " <> show s <> ", with queue: " <> show q
    else 
        if sizeQueue q > 1
        then Left $ "AST Queue did not contain single AST: " <> show q <> ", with stack " <> show s
        else case frontQueue q of 
            Nothing -> Left $ "AST Queue was empty, no AST found"
            Just (Fragment ast) -> Right ast



build :: Either Token DerivationType -> BuildState -> Either String BuildState
build next state@{stack: s, queue: q} = 
        let processInput :: BuildState
            processInput = case next of 
                Left token -> state  -- we don't do anything at this stage if we receive a token
                Right deriv -> state { stack = pushStack (Pending deriv) s}

            buildAst :: Either String BuildState
            buildAst = case next of 
                Right _ -> Right $ processInput -- if we didn't receive a token, we don't try to build
                Left token -> -- if we receive a token, we can try to see if building will work.
                        doBuildAst (Just token) processInput
        in  buildAst
        where
            doBuildAst :: Maybe Token -> BuildState -> Either String BuildState
            doBuildAst tok _state@{stack: _s, queue: _q} = -- we only build if we actually received a token
                case tok of 
                    Nothing -> Right $ _state
                    Just token -> case topStack _s of 
                        Nothing -> Left "Tried to build AST, but found nothing in stack to build with"
                        Just (Pending d) ->  -- if we find a derivation, we try to build an ast with what we have in the queue
                                            -- even if we can't, that's okay. we may just need to wait for another token to come in.
                            let afterPop = Right $ _state { stack = popStack _s }
                            in  case d of 
                                    -- These play no part in building the ast, even if the token matches,
                                    -- since they just help parsing, so 
                                    -- we do a sanity check that they match,
                                    -- and that's it. It's too soon at this point to try to collapse 
                                    -- anything, since we've only seen the header, and then try the next nonterminal on the 
                                    -- stack
                                    DNormalHeader -> matchAndContinue d token _state
                                    DErrorHeader -> matchAndContinue d token _state
                                    DDefaultHeader -> afterPop
                                    DTerminator -> afterPop
                                    DDefault -> afterPop
                                    DEof -> afterPop
                                    -- These allow us to try to match the top of the stack against the token
                                    DRegex -> tokenBuild d token _state
                                    DErrorMessage -> tokenBuild d token _state
                                    DName -> tokenBuild d token _state
                                    -- These require special action
                                    DFAIL -> Left "HANDLE"
                                    -- These require more combining of existing items in the queue.
                                    _ -> afterPop
            matchAndContinue :: DerivationType -> Token -> BuildState -> Either String BuildState
            matchAndContinue deriv tok _state@{stack: _s, queue: _q} = 
                    let afterPop = _state {stack = popStack _s}
                    in  if not $ tok.type `equals` deriv
                        then Left $ "Stack had " <> show deriv <> ", but token was " <> show tok.type
                        else -- we try to use the next thing on the stack to make an ast.
                            nonterminalBuild afterPop

            tokenBuild :: DerivationType -> Token -> BuildState -> Either String BuildState
            tokenBuild deriv tok _state@{stack: _s, queue: _q} = do 
                ast <- tryBuildAst deriv tok
                pure $ _state {stack = popStack _s, queue = addQueue (Fragment ast) _q}
                where
                    tryBuildAst :: DerivationType -> Token -> Either String AST
                    tryBuildAst d t = 
                        if not (t.type `equals` d)
                        then Left $ "Stack had " <> show d <> ", but token was " <> show t.type
                        else case makeAst t of 
                                Nothing -> Left $ "Unusable token when building AST: " <> show t
                                Just ast -> Right ast
                    makeAst :: Token -> Maybe AST
                    makeAst _tok = case _tok.type of
                        Regex -> Just $ NRegex _tok
                        Name -> Just $ NName _tok
                        ErrorMessage -> Just $ NErrorMessage _tok
                        _ -> Nothing


nonterminalBuild :: BuildState -> Either String BuildState
nonterminalBuild state = 
    let buildState :: Either String BuildState
        buildState = doBuild state
    in  buildState
    where
        doBuild :: BuildState -> Either String BuildState
        doBuild st@{stack: _s, queue: _q} = case topStack _s of 
            Nothing -> Right st
            -- We keep building until we find a nonterminal that cannot yet match
            Just (Pending nonterm) -> 
                let popped = popStack _s
                in  case nonterm of
                        DProgram -> case buildProgram _q of 
                            Just newQueue -> Right $ st { stack = popped, queue = newQueue }
                            Nothing -> Right $ st -- even if we can't convert the program at this time, that's okay.
                        DNormalTokens -> doBuild st { stack = popped, queue = buildNormalTokens _q }
                        DErrorTokens -> doBuild st { stack = popped, queue = buildErrorTokens _q}
                        _ -> Left $ "Tried to build a nonterminal AST using " <> show nonterm
        buildProgram :: Queue -> Maybe Queue
        buildProgram q = 
            let _prog :: Maybe (Tuple AST Queue)
                _prog = getProg q
            in  case _prog of 
                    Nothing -> Nothing
                    Just (Tuple prog newQueue) -> Just $ addQueue (Fragment $ prog) newQueue
            where
                getProg :: Queue -> Maybe (Tuple AST Queue)
                getProg _q = do 
                    (Fragment normal) <- frontQueue _q
                    let removedNormal = removeQueue _q
                        withNormal = NProgram normal
                    case frontQueue (removedNormal) of 
                        Just (Fragment err@(NErrorSpecs _)) -> do 
                            let removedError = removeQueue removedNormal
                                withError = withNormal $ Just err
                            case frontQueue(removedError) of 
                                Just (Fragment def@(NDefaultSpecs _)) -> do 
                                    let removedDefault = removeQueue removedError 
                                        withDone = withError $ Just def
                                    Just $ Tuple withDone removedDefault
                                _ -> do 
                                    let withDone = withError Nothing
                                    Just $ Tuple withDone removedError
                        Just (Fragment def@(NDefaultSpecs _)) -> do
                            let removedDefault = removeQueue removedNormal
                                withDone = withNormal Nothing $ Just def
                            Just $ Tuple withDone removedDefault
                        _ -> do 
                            let withDone = withNormal Nothing Nothing
                            Just $ Tuple withDone removedNormal
                
        buildNormalTokens :: Queue -> Queue
        buildNormalTokens q = -- while queue has one or more normal spec ASTs, we pop them
            let specs :: Tuple (Array AST) Queue
                specs = getNormalSpecs [] q

                newAst :: AST 
                newAst = NNormalSpecs $ fst specs

                newQueue :: Queue
                newQueue = snd specs
            in  addQueue (Fragment newAst) newQueue
            where 
                getNormalSpecs :: Array AST -> Queue -> Tuple (Array AST) Queue
                getNormalSpecs acc _q = 
                    case frontQueue _q of 
                        Just (Fragment ast@(NNormalSpec _ _)) -> getNormalSpecs (ast : acc) (removeQueue _q)
                        _ -> Tuple acc _q
        buildErrorTokens :: Queue -> Queue
        buildErrorTokens q = -- while queue has one or more normal spec ASTs, we pop them
            let specs :: Tuple (Array AST) Queue
                specs = getErrorSpecs [] q

                newAst :: AST 
                newAst = NErrorSpecs $ fst specs

                newQueue :: Queue
                newQueue = snd specs
            in  addQueue (Fragment newAst) newQueue
            where 
                getErrorSpecs :: Array AST -> Queue -> Tuple (Array AST) Queue
                getErrorSpecs acc _q = 
                    case frontQueue _q of 
                        Just (Fragment ast@(NErrorSpec _ _ _)) -> getErrorSpecs (ast : acc) (removeQueue _q)
                        _ -> Tuple acc _q

data StackElement
    = Pending DerivationType
derive instance genericStackElement :: Generic StackElement _
instance showStackElement :: Show StackElement where show = genericShow

type Stack = List StackElement

pushStack :: StackElement -> Stack -> Stack
pushStack = cons

popStack :: Stack -> Stack
popStack s = case tail s of 
    Nothing -> s
    Just rest -> rest

topStack :: Stack -> Maybe StackElement
topStack = head

sizeStack :: Stack -> Int
sizeStack = length

emptyStack :: Stack
emptyStack = nil

data QueueElement
    = Fragment AST
derive instance genericQueueElement :: Generic QueueElement _
instance showQueueElement :: Show QueueElement where show = genericShow  


type Queue = CatQueue QueueElement

addQueue :: QueueElement -> Queue -> Queue
addQueue = flip Q.snoc

removeQueue :: Queue -> Queue
removeQueue q = case Q.uncons q of 
    Nothing -> q
    Just (Tuple head newQueue) -> newQueue

frontQueue :: Queue -> Maybe QueueElement
frontQueue q = map fst $ Q.uncons q

sizeQueue :: Queue -> Int
sizeQueue = Q.length

emptyQueue :: Queue
emptyQueue = Q.empty



-- definition of data structures for tree building
-- we'll create a stack of the ast so far and lenses to which portion is currently under construction,
-- based on what the LL1 parsing passes.
-- When a derivative type is passed, the parser will evaluate it to determine whether it leads to an AST node being constructed.
-- If it doesn't, we'll just toss it.
-- If it does, we'll throw it on the stack along with the leftmost lens
-- As we receive tokens that match the top of the stack, obviously we'll update the top of the stack,
-- and then pop the stack to match the token into its parent node
-- If the parent node is full, then we can pop the parent node and match it into its parent.
-- If the parent node ISN'T full, then if we receive a request to merge something that is to the LEFT of the current 
-- lens, or if we receive a request to merge something that is NOT part of the node, then we try to merge the parent node to
-- its parent.
-- On the other hand, if a parent receives something that matches to the RIGHT of the leftmost lens, then we can move the leftmost lens
-- rightward, merge the input, and then move right again.
-- in that case, maybe DISCARD is a message to move to the right OR to pop the stack and try to merge.

type Program = 
    { normal :: Maybe NormalSpecs
    , error :: Maybe ErrorSpecs
    , default :: Maybe DefaultSpecs
    }

data ProgramLens 
    = NormalSpecs_ 
    | ErrorSpecs_ 
    | DefaultSpecs_ 
    | Done_
derive instance genProgramLens :: Generic ProgramLens _
instance eqProgramLens :: Eq ProgramLens where eq = genericEq
instance ordProgramLens :: Ord ProgramLens where compare = genericCompare
instance enumProgramLens :: Enum ProgramLens where 
    succ = genericSucc
    pred = genericPred

type NormalSpecs = 
    { specs :: Array NormalSpec
    }
data NormalSpecsLens 
    = ArrayNormalSpecs 
derive instance genNormalSpecsLens :: Generic NormalSpecsLens _
instance eqNormalSpecsLens :: Eq NormalSpecsLens where eq = genericEq
instance ordNormalSpecsLens :: Ord NormalSpecsLens where compare = genericCompare
instance enumNormalSpecsLens :: Enum NormalSpecsLens where 
    succ = genericSucc
    pred = genericPred

type NormalSpec = 
    { name :: Maybe Name
    , regex :: Maybe Regex
    }
data NormalSpecLens 
    = NormalName 
    | NormalRegex 
derive instance genNormalSpecLens :: Generic NormalSpecLens _
instance eqNormalSpecLens :: Eq NormalSpecLens where eq = genericEq
instance ordNormalSpecLens :: Ord NormalSpecLens where compare = genericCompare
instance enumNormalSpecLens :: Enum NormalSpecLens where 
    succ = genericSucc
    pred = genericPred

type ErrorSpecs = 
    { specs :: Array ErrorSpec
    }
data ErrorSpecsLens
    = ArrayErrorSpecs 
derive instance genErrorSpecsLens :: Generic ErrorSpecsLens _
instance eqErrorSpecsLens :: Eq ErrorSpecsLens where eq = genericEq
instance ordErrorSpecsLens :: Ord ErrorSpecsLens where compare = genericCompare
instance enumErrorSpecsLens :: Enum ErrorSpecsLens where 
    succ = genericSucc
    pred = genericPred

type ErrorSpec = 
    { name :: Maybe Name
    , error :: Maybe ErrorMessage
    , sync :: Maybe Regex
    }

data ErrorSpecLens
    = ErrorName 
    | ErrorEM 
    | ErrorSync 
derive instance genErrorSpecLens :: Generic ErrorSpecLens _
instance eqErrorSpecLens :: Eq ErrorSpecLens where eq = genericEq
instance ordErrorSpecLens :: Ord ErrorSpecLens where compare = genericCompare
instance enumErrorSpecLens :: Enum ErrorSpecLens where 
    succ = genericSucc
    pred = genericPred

type DefaultSpecs = 
    { specs :: Array DefaultSpec
    }
data DefaultSpecsLens 
    = ArrayDefaultSpecs 
derive instance genDefaultSpecsLens :: Generic DefaultSpecsLens _
instance eqDefaultSpecsLens :: Eq DefaultSpecsLens where eq = genericEq
instance ordDefaultSpecsLens :: Ord DefaultSpecsLens where compare = genericCompare
instance enumDefaultSpecsLens :: Enum DefaultSpecsLens where 
    succ = genericSucc
    pred = genericPred

type DefaultSpec =
    { error :: Maybe ErrorMessage
    , sync :: Maybe Regex
    }
data DefaultSpecLens 
    = DefaultMessage 
    | DefaultSync 
derive instance genDefaultSpecLens :: Generic DefaultSpecLens _
instance eqDefaultSpecLens :: Eq DefaultSpecLens where eq = genericEq
instance ordDefaultSpecLens :: Ord DefaultSpecLens where compare = genericCompare
instance enumDefaultSpecLens :: Enum DefaultSpecLens where 
    succ = genericSucc
    pred = genericPred

type Name =
    { token :: Maybe Token
    }
data NameLens
    = Name_ 
derive instance genNameLens :: Generic NameLens _
instance eqNameLens :: Eq NameLens where eq = genericEq
instance ordNameLens :: Ord NameLens where compare = genericCompare
instance enumNameLens :: Enum NameLens where 
    succ = genericSucc
    pred = genericPred

type Regex = 
    { token :: Maybe Token
    }
data RegexLens
    = Regex_ 
derive instance genRegexLens :: Generic RegexLens _
instance eqRegexLens :: Eq RegexLens where eq = genericEq
instance ordRegexLens :: Ord RegexLens where compare = genericCompare
instance enumRegexLens :: Enum RegexLens where 
    succ = genericSucc
    pred = genericPred

type ErrorMessage =
    { token :: Maybe Token
    }
data ErrorMessageLens
    = ErrorMessage_ 
derive instance genErrorMessageLens :: Generic ErrorMessageLens _
instance eqErrorMessageLens :: Eq ErrorMessageLens where eq = genericEq
instance ordErrorMessageLens :: Ord ErrorMessageLens where compare = genericCompare
instance enumErrorMessageLens :: Enum ErrorMessageLens where 
    succ = genericSucc
    pred = genericPred

class Successor a where
    successor :: a -> Maybe a

class Parent a where
    mergeToParent :: a -> a -> Either String a

data AstStackElement
    = P Program ProgramLens
    | NSS NormalSpecs NormalSpecsLens
    | NS NormalSpec NormalSpecLens
    | ESS ErrorSpecs ErrorSpecsLens
    | ES ErrorSpec ErrorSpecLens
    | DSS DefaultSpecs DefaultSpecsLens
    | DS DefaultSpec DefaultSpecLens
    | N Name NameLens
    | R Regex RegexLens
    | EM ErrorMessage ErrorMessageLens

instance succAstStackEl :: Successor AstStackElement where
    successor (P node lens) = (P node) <$> succ lens
    successor (NSS node lens) = (NSS node) <$> succ lens
    successor (NS node lens) = (NS node) <$> succ lens
    successor (ESS node lens) = (ESS node) <$> succ lens
    successor (ES node lens) = (ES node) <$> succ lens
    successor (DSS node lens) = (DSS node) <$> succ lens
    successor (DS node lens) = (DS node) <$> succ lens
    successor (N node lens) = (N node) <$> succ lens
    successor (R node lens) = (R node) <$> succ lens
    successor (EM node lens) = (EM node) <$> succ lens

instance parentAstStackElement :: Parent AstStackElement where
    mergeToParent (P node lens) p = Left $ "Program is must be top of tree, so there is no parent to merge into"
    mergeToParent (NSS node lens) p = 
        let nextLens = maybeSucc lens 
        in  case p of 
                (P parent NormalSpecs_) -> Right $ (P parent { normal = Just node } (maybeSucc NormalSpecs_))
                _ -> invalidParentError node
    mergeToParent (NS node lens) p = 
        case p of 
            (NSS parent ArrayNormalSpecs ) -> Right $ (NSS parent { specs = (snoc parent.specs node) } ArrayNormalSpecs)
            _ -> invalidParentError node
    mergeToParent (ESS node lens) p = case p of
            (P parent ErrorSpecs_) -> Right $ 
                (P parent { error = Just node } $ DefaultSpecs_ )
            _ -> invalidParentError node
    mergeToParent (ES node lens) p = case p of 
            (ESS parent ArrayErrorSpecs) -> Right $ 
                (ESS parent { specs = (snoc parent.specs node) } ArrayErrorSpecs)
            _ -> invalidParentError node
    mergeToParent (DSS node lens) p = case p of 
            (P parent DefaultSpecs_) -> Right $ 
                (P parent { default = Just node} Done_)
            _ -> invalidParentError node
    mergeToParent (DS node lens) p = case p of
            (DSS parent ArrayDefaultSpecs) -> Right $
                (DSS parent { specs = (snoc parent.specs node)} ArrayDefaultSpecs)
            _ -> invalidParentError node
    mergeToParent (N node lens) p = case p of 
            (NS parent NormalName) -> Right $ 
                (NS parent { name = Just node } NormalRegex )
            (ES parent ErrorName) -> Right $ 
                (ES parent { name = Just node } ErrorEM )
            _ -> invalidParentError node
    mergeToParent (R node lens) p = case p of
            (NS parent NormalRegex) -> Right
                (NS parent { name = Just node } NormalRegex)
            (ES parent ErrorSync) -> Right
                (ES parent { sync = Just node } ErrorSync)
            (DS parent DefaultSync) -> Right
                (DS parent { sync = Just node } DefaultSync)
            _ -> invalidParentError node
    mergeToParent (EM node lens) p = case p of
            (ES parent ErrorEM) -> Right $ 
                (ES parent { error = Just node } ErrorSync)
            (DS parent DefaultMessage) -> Right $ 
                (DS parent { error = Just node } DefaultSync)
            _ -> invalidParentError node

maybeSucc :: forall a. Enum a => a -> a
maybeSucc e = case succ e of 
    Nothing -> e
    Just s -> s

invalidParentError :: forall a b. Show b => b -> Either String a
invalidParentError sh = Left $ "Invalid parent for " <> show sh

type AstStack = List AstStackElement

pushAstStack :: AstStackElement -> AstStack -> AstStack
pushAstStack = cons

popAstStack :: AstStack -> AstStack
popAstStack s = case tail s of 
    Nothing -> s
    Just rest -> rest

topAstStack :: AstStack -> Maybe AstStackElement
topAstStack = head

replaceAstStack :: AstStack -> AstStackElement -> AstStack
replaceAstStack stack el = pushAstStack el stack

sizeAstStack :: AstStack -> Int
sizeAstStack = length

emptyAstStack :: AstStack
emptyAstStack = nil

type TreeBuildState = 
    { buildStack :: AstStack
    }

data BuildCommand 
    = Next
    | AddToken Token
    | AddDerivation DerivationType

buildTree :: TreeBuildState -> BuildCommand -> Either String TreeBuildState
buildTree state@{buildStack: stack} Next = -- we received a command to go to the next available opening
    let popped = popAstStack stack
    in
        case topAstStack stack of
            Nothing -> Left $ "Invalid equest to go to next when there was nothing in stack"
            Just element -> case successor element of 
                Just suc -> Right $ replaceState state suc
                Nothing -> case topAstStack popped of  -- if no successor, it is time to merge to parent.
                    Nothing -> emptyStackError
                    Just parent -> case mergeToParent element parent of
                        Right newTop -> Right $ 
                            { buildStack: replaceAstStack popped newTop
                            }
                        Left str -> Left str
    where 
        replaceState :: TreeBuildState -> AstStackElement -> TreeBuildState
        replaceState state_@{buildStack: stack_} el = state_ { buildStack = replaceAstStack stack_ el}

        emptyStackError :: forall a. Either String a
        emptyStackError = Left $ "Nothing on top of stack. Can't merge node with parent"

buildTree state@{buildStack: stack} (AddToken token) = Right state
buildTree state@{buildStack: stack} (AddDerivation deriv) = Right state

