module ParserAST where

import Prelude

import Data.CatQueue (CatQueue)
import Data.CatQueue as Q
import Data.Either (Either(..))
import Data.List.Lazy (List, cons, tail, head, length, nil)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import ParserTypes (AST(..), DerivationType(..), Token, TokenType(..), equals)

-- We let the LL1 parsing take care of correctness for us
-- LL1 parses depth first, left first, so we use a stack to build the AST
-- When a nonterminal is received from parsing, we put it on the stack.
-- When a token is received, we can begin building a portion of the AST,
-- which we can then throw into a queue.
-- If the stack is popped and the next thing on the top is a nonterminal,
-- then we can consider building an ast with that nonterminal and the ASTs in the queue
type BuildState = 
    { stack :: Stack
    , queue :: Queue
    }

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
            makeAst :: Token -> Maybe AST
            makeAst tok = case tok.type of
                Regex -> Just $ NRegex tok
                Name -> Just $ NName tok
                ErrorMessage -> Just $ NErrorMessage tok
                _ -> Nothing
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
                                    -- These play no part in building the ast, since they just help parsing.
                                    DNormalHeader -> afterPop
                                    DErrorHeader -> afterPop
                                    DDefaultHeader -> afterPop
                                    DTerminator -> afterPop
                                    DDefault -> afterPop
                                    DEof -> afterPop
                                    -- These allow us to try to match the top of the stack against the token
                                    DRegex -> tryBuild d token _state
                                    DErrorMessage -> tryBuild d token _state
                                    DName -> tryBuild d token _state
                                    -- These require special action
                                    DFAIL -> Left "HANDLE"
                                    -- These require more combining of existing items in the queue.
                                    _ -> afterPop
            tryBuild :: DerivationType -> Token -> BuildState -> Either String BuildState
            tryBuild deriv tok _state@{stack: _s, queue: _q} = do 
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

data StackElement
    = Pending DerivationType

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
