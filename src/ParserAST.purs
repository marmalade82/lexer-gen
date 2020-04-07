module ParserAST where

import Prelude

import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Enum (class Enum, succ)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (List, cons, tail, head, length, nil)
import Data.Maybe (Maybe(..))
import ParserTypes (AST, DerivationType(..), Token)


extract :: TreeBuildState -> Either String AST
extract state@{buildStack: s}
    | sizeAstStack s < 0 = Left $ "AST Stack was empty: " <> show s
    | sizeAstStack s > 1 = Left $ "AST Stack was not fully merged: " <> show s
    | otherwise = Left "hi"

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
derive instance genAstStackEl :: Generic AstStackElement _
instance showAstStackEl :: Show AstStackElement where show = genericShow

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

emptyBuildState :: TreeBuildState
emptyBuildState = 
    { buildStack: emptyAstStack
    }

type TreeBuildState = 
    { buildStack :: AstStack
    }

data BuildCommand 
    = Next
    | AddToken Token
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
                Nothing -> case topAstStack popped of  -- if no successor, it is time to merge to parent.
                    Nothing -> emptyStackError
                    Just parent -> case mergeToParent element parent of
                        Right newTop -> Right $ 
                            { buildStack: replaceAstStack popped newTop
                            }
                        Left str -> Left str
    where 
        emptyStackError :: forall a. Either String a
        emptyStackError = Left $ "Nothing on top of stack. Can't merge node with parent"

buildTree state@{buildStack: stack} (AddToken token) = -- we received a command to add a token
    case topAstStack stack of 
        Nothing -> Left "Could not add token. Stack was empty"
        Just el -> case el of 
            (N name lens) -> Right $ replaceState state (N (name { token = Just token } ) lens)
            (R regex lens) -> Right $ replaceState state (R (regex { token = Just token }) lens)
            (EM em lens) -> Right $ replaceState state (EM (em { token = Just token } ) lens)
            _ -> Left $ "Could not add token " <> show token <> ", current node does not accept tokens"
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

        -- The following are not part of the AST at this time.
        DNormalTokens -> Right state
        DErrorTokens -> Right state
        DDefaultTokens -> Right state
        DNormalHeader -> Right state
        DErrorHeader -> Right state
        DDefaultHeader -> Right state
        DOptionalSync -> Right $ state
        DDefault -> Right $ state
        DTerminator -> Right $ state
        DEof -> Right $ state
        DFAIL -> Right $ state

    where 
        pushState :: TreeBuildState -> AstStackElement -> TreeBuildState
        pushState state_@{ buildStack: stack_ } el = state_ { buildStack = pushAstStack el stack_ }


