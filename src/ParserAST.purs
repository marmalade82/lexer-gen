module ParserAST where

import Prelude

import Data.Array ((:))
import Data.CatQueue (CatQueue)
import Data.CatQueue as Q
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (List, cons, tail, head, length, nil)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Node.Stream (onFinish)
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
