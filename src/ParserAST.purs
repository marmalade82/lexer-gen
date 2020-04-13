module ParserAST where

import Prelude

import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Enum (class Enum, succ)
import Data.Foldable (foldl )
import Data.Traversable(sequence)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (List, cons, tail, head, length, nil)
import Data.Maybe (Maybe(..))
import ParserTypes (AST(..), DerivationType(..), Token, validForAst)

-- To extract, the function needs to do one final merge of the stack as far as possible.
extract :: TreeBuildState -> Either String AST
extract state@{buildStack: s}
    | sizeAstStack s < 0 = Left $ "AST Stack was empty: " <> show s
    | otherwise = 
        let extracting :: Either String AST
            extracting = do 
                merged <- mergeAll s
                toAst merged
        in  case extracting of 
                Right x -> Right x
                Left msg -> Left $ "Build stack was: " <> show s <> ", with extraction error: " <> msg

mergeAll :: AstStack -> Either String Program
mergeAll stack = 
    let popped = popAstStack stack
    in  case topAstStack stack of 
            Nothing -> Left "Stack was empty. Could not merge into a program"
            Just (P program _) -> Right program
            Just x -> case topAstStack popped of 
                Nothing -> Left $ "Could not merge in mergeAll"
                Just parent -> case mergeToParent x parent of
                    Right newTop -> mergeAll $ replaceAstStack popped newTop
                    Left y -> Left y

toAst :: Program -> Either String AST
toAst {normal: normalSec, error: errorSec, default: defaultSec} = do
    normalSpecs <- makeNormalSpecs normalSec
    errorSpecs <- makeErrorSpecs errorSec
    defaultSpecs <- makeDefaultSpecs defaultSec
    pure $ NProgram normalSpecs errorSpecs defaultSpecs

    where 
        makeNormalSpecs :: Maybe NormalSpecs -> Either String (Maybe AST)
        makeNormalSpecs n = case n of 
            Nothing -> Right $ Nothing
            Just {specs: s} -> do
                specs <- makeNormalSpecs_ s :: Either String (Array AST)
                pure $ Just (NNormalSpecs specs )

            where 
                makeNormalSpecs_ :: Array NormalSpec -> Either String (Array AST)
                makeNormalSpecs_ arr = 
                    let specs :: Array (Either String AST)
                        specs = map makeNormalSpec arr
                    in  sequence specs

                makeNormalSpec :: NormalSpec -> Either String AST
                makeNormalSpec {name: name, regex: regex} = do 
                    name_ <- makeName name
                    regex_ <- makeRegex regex
                    pure $ NNormalSpec name_ regex_

        makeErrorSpecs :: Maybe ErrorSpecs -> Either String (Maybe AST)
        makeErrorSpecs e = case e of 
            Nothing -> Right Nothing
            Just {specs: s} -> do 
                specs <- makeErrorSpecs_ s :: Either String (Array AST)
                pure $ Just (NErrorSpecs specs)
            where 
                makeErrorSpecs_ :: Array ErrorSpec -> Either String (Array AST)
                makeErrorSpecs_ arr = 
                    let specs :: Array (Either String AST)
                        specs = map makeErrorSpec arr
                    in  sequence specs
                makeErrorSpec :: ErrorSpec -> Either String AST
                makeErrorSpec {name: name, error: error, sync: sync} = do
                    name_ <- makeName name
                    error_ <- makeErrorMessage error
                    sync_ <- makeRegex sync
                    pure $ NErrorSpec name_ error_ (Just sync_)


        makeDefaultSpecs :: Maybe DefaultSpecs -> Either String (Maybe AST)
        makeDefaultSpecs d = case d of 
            Nothing -> Right Nothing
            Just {specs: s} -> do 
                specs <- makeDefaultSpecs_ s :: Either String (Array AST)
                pure $ Just (NDefaultSpecs specs)
            where
                makeDefaultSpecs_ :: Array DefaultSpec -> Either String (Array AST)
                makeDefaultSpecs_ arr = 
                    let specs :: Array (Either String AST)
                        specs = map makeDefaultSpec arr
                    in  sequence specs
                makeDefaultSpec :: DefaultSpec -> Either String AST
                makeDefaultSpec {error: error, sync: sync} = do 
                    error_ <- makeErrorMessage error
                    sync_ <- makeRegex sync
                    pure $ NDefaultError error_ (Just sync_)


        makeName :: Maybe Name -> Either String AST
        makeName n = case n of 
            Nothing -> Left $ "Required name was not found"
            Just {token: Nothing} -> Left $ "Required name was not found"
            Just {token: Just tok} -> Right $ NName tok

        makeRegex :: Maybe Regex -> Either String AST
        makeRegex r = case r of 
            Nothing -> Left $ "Required regex was not found"
            Just {token: Nothing} -> Left $ "Required regex was not found"
            Just {token: Just tok} -> Right $ NRegex tok

        makeErrorMessage :: Maybe ErrorMessage -> Either String AST
        makeErrorMessage e = case e of 
            Nothing -> Left $ "Required error message was not found"
            Just {token: Nothing} -> Left $ "Required error message was not found"
            Just {token: Just tok} -> Right $ NErrorMessage tok


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
instance showProgramLens :: Show ProgramLens where show = genericShow
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
instance showNormalSpecsLens :: Show NormalSpecsLens where show = genericShow
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
instance showNormalSpecLens :: Show NormalSpecLens where show = genericShow
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
instance showErrorSpecsLens :: Show ErrorSpecsLens where show = genericShow
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
instance showErrorSpecLens :: Show ErrorSpecLens where show = genericShow
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
instance showDefaultSpecsLens :: Show DefaultSpecsLens where show = genericShow
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
instance showDefaultSpecLens :: Show DefaultSpecLens where show = genericShow
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
instance showNameLens :: Show NameLens where show = genericShow
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
instance showRegexLens :: Show RegexLens where show = genericShow
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
instance showErrorMessageLens :: Show ErrorMessageLens where show = genericShow
instance eqErrorMessageLens :: Eq ErrorMessageLens where eq = genericEq
instance ordErrorMessageLens :: Ord ErrorMessageLens where compare = genericCompare
instance enumErrorMessageLens :: Enum ErrorMessageLens where 
    succ = genericSucc
    pred = genericPred

class Successor a where
    successor :: a -> Maybe a

class Parent a where
    mergeToParent :: a -> a -> Either String a

class Initializable a where
    init :: a

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
    | Placeholder (Array AstStackElement)
                    -- for holding a space for derivations that will NOT become a node part of the tree, but still need to 
                    -- exist to allow commands from the parser to work correctly.
derive instance genAstStackEl :: Generic AstStackElement _
instance showAstStackEl :: Show AstStackElement where 
    show (Placeholder arr) = "Placeholder " <> show arr
    show a = genericShow a

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
    successor (Placeholder arr) = Just $ Placeholder arr

instance parentAstStackElement :: Parent AstStackElement where
    -- We take care of placeholder patterns first.
    mergeToParent (Placeholder arr) parent =  -- for each stored child in the placeholder (which may include more placeholders, we merge into the parent)
        foldl acc (Right parent) arr
        where 
            acc :: Either String AstStackElement -> AstStackElement -> Either String AstStackElement
            acc p el = case p of 
                Right parent_ -> mergeToParent el parent_
                _ -> p
    mergeToParent child (Placeholder arr) = Right $ Placeholder (snoc arr child) -- we store the child in the placeholder
    mergeToParent prog@(P node lens) p = Left $ "Program " <> show prog <> " is supposed to be top. Could not merge into (" <> show p <> ")"
    mergeToParent (NSS node lens) p = 
        let nextLens = maybeSucc lens 
        in  case p of 
                (P parent NormalSpecs_) -> Right $ (P parent { normal = Just node } (maybeSucc NormalSpecs_))
                _ -> invalidParentError p node
    mergeToParent (NS node lens) p = 
        case p of 
            (NSS parent ArrayNormalSpecs ) -> Right $ (NSS parent { specs = (snoc parent.specs node) } (maybeSucc ArrayNormalSpecs))
            _ -> invalidParentError p node
    mergeToParent (ESS node lens) p = case p of
            (P parent ErrorSpecs_) -> Right $ 
                (P parent { error = Just node } $ (maybeSucc ErrorSpecs_) )
            _ -> invalidParentError p node
    mergeToParent (ES node lens) p = case p of 
            (ESS parent ArrayErrorSpecs) -> Right $ 
                (ESS parent { specs = (snoc parent.specs node) } (maybeSucc ArrayErrorSpecs))
            _ -> invalidParentError p node
    mergeToParent (DSS node lens) p = case p of 
            (P parent DefaultSpecs_) -> Right $ 
                (P parent { default = Just node} (maybeSucc DefaultSpecs_))
            _ -> invalidParentError p node
    mergeToParent (DS node lens) p = case p of
            (DSS parent ArrayDefaultSpecs) -> Right $
                (DSS parent { specs = (snoc parent.specs node)} (maybeSucc ArrayDefaultSpecs))
            _ -> invalidParentError p node
    mergeToParent (N node lens) p = case p of 
            (NS parent NormalName) -> Right $ 
                (NS parent { name = Just node } (maybeSucc NormalName) )
            (ES parent ErrorName) -> Right $ 
                (ES parent { name = Just node } (maybeSucc ErrorName) )
            _ -> invalidParentError p node
    mergeToParent (R node lens) p = case p of
            (NS parent NormalRegex) -> Right
                (NS parent { regex = Just node } (maybeSucc NormalRegex))
            (ES parent ErrorSync) -> Right
                (ES parent { sync = Just node } (maybeSucc ErrorSync))
            (DS parent DefaultSync) -> Right
                (DS parent { sync = Just node } (maybeSucc DefaultSync))
            _ -> invalidParentError p node
    mergeToParent (EM node lens) p = case p of
            (ES parent ErrorEM) -> Right $ 
                (ES parent { error = Just node } (maybeSucc ErrorEM))
            (DS parent DefaultMessage) -> Right $ 
                (DS parent { error = Just node } (maybeSucc DefaultMessage))
            _ -> invalidParentError p node

maybeSucc :: forall a. Enum a => a -> a
maybeSucc e = case succ e of 
    Nothing -> e
    Just s -> s

invalidParentError :: forall a b c . Show b => Show c => b -> c -> Either String a
invalidParentError parent sh = Left $ "Invalid parent (" <> show parent <> ") for " <> show sh

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
replaceAstStack stack el = pushAstStack el (popAstStack stack)

sizeAstStack :: AstStack -> Int
sizeAstStack = length

emptyAstStack :: AstStack
emptyAstStack = nil

emptyBuildState :: TreeBuildState
emptyBuildState = 
    { buildStack: emptyAstStack
    }

type TreeBuildState = 
    { buildStack :: AstStack
    }

data BuildCommand 
    = Next
    | Up
    | Match Token
    | AddDerivation DerivationType

replaceState :: TreeBuildState -> AstStackElement -> TreeBuildState
replaceState state_@{buildStack: stack_} el = state_ { buildStack = replaceAstStack stack_ el}

buildTree :: TreeBuildState -> BuildCommand -> Either String TreeBuildState
buildTree state@{buildStack: stack} Next = -- we received a command to go to the next available opening
    let popped = popAstStack stack
    in
        case topAstStack stack of
            Nothing -> Left $ "Invalid request to go to next when there was nothing in stack"
            Just element -> case successor element of 
                Just suc -> Right $ replaceState state suc
                Nothing -> Right state

buildTree state@{buildStack: stack} Up = -- command to merge node to parent
    let popped = popAstStack stack
    in  case topAstStack stack of 
            Nothing -> Left $ "Invalid request to go up when there was nothing in the stack"
            Just element -> case topAstStack popped of 
                Nothing -> Right state -- going up with one element is a no-op
                Just parent -> case mergeToParent element parent of 
                    Right newTop -> Right $ 
                            { buildStack: replaceAstStack popped newTop
                            }
                    Left str -> Left str

buildTree state@{buildStack: stack} (Match token) = -- we received a command to add a token
    case topAstStack stack of 
        Nothing -> Left $ "Could not add token (" <> show token <> "). Stack was empty"
        Just el -> case el of 
            (N name lens) -> Right $ replaceState state (N (name { token = Just token } ) lens)
            (R regex lens) -> Right $ replaceState state (R (regex { token = Just token }) lens)
            (EM em lens) -> Right $ replaceState state (EM (em { token = Just token } ) lens)
            _ -> if not validForAst token.type 
                 then Right state -- we discard if this is not a relevant token
                 else Left $ "Could not add token " <> show token <> ", current node (" <> show el <> ") does not accept tokens"
buildTree state@{buildStack: stack} (AddDerivation deriv) = -- we received a command to add a new node to the top of the stack
    case deriv of
        DProgram -> Right $ pushState state (P { normal: Nothing, error: Nothing, default: Nothing} NormalSpecs_) 
        DNormalSpecs -> Right $ pushState state (NSS { specs: [] } ArrayNormalSpecs)
        DNormalSpec -> Right $ pushState state (NS {name: Nothing, regex: Nothing} NormalName)
        DErrorSpecs -> Right $ pushState state (ESS { specs: [] } ArrayErrorSpecs)
        DErrorSpec -> Right $ pushState state (ES { name: Nothing, error: Nothing, sync: Nothing } ErrorName)
        DDefaultSpecs -> Right $ pushState state (DSS { specs: [] } ArrayDefaultSpecs)
        DDefaultError -> Right $ pushState state (DS { error: Nothing, sync: Nothing } DefaultMessage)
        DName -> Right $ pushState state (N { token: Nothing } Name_)
        DRegex -> Right $ pushState state (R { token: Nothing } Regex_)
        DErrorMessage -> Right $ pushState state (EM { token: Nothing } ErrorMessage_)
        _ -> if not validForAst deriv 
             then Right $ pushState state (Placeholder [])
             else Left $ "Tried to add derivation (" <> show deriv <> ") which was not valid for ast"
    where 
        pushState :: TreeBuildState -> AstStackElement -> TreeBuildState
        pushState state_@{ buildStack: stack_ } el = state_ { buildStack = pushAstStack el stack_ }


