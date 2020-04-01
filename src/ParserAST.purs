module ParserAST where

import Prelude

import Data.CatQueue as Q
import Data.CatQueue (CatQueue(..))
import Data.List.Lazy (List, cons, tail, head, length, nil)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import ParserTypes (AST, DerivationType, Token, TokenType(..))

-- We let the LL1 parsing take care of correctness for us
-- LL1 parses depth first, left first, so we use a stack to build the AST
-- When a nonterminal is received from parsing, we put it on the stack.
-- When a token is received, we can begin building a portion of the AST,
-- which we can then throw into a queue.
-- If the stack is popped and the next thing on the top is a nonterminal,
-- then we can consider building an ast with that nonterminal and the ASTs in the queue
build :: Token -> Stack -> Stack
build t stack = stack





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
