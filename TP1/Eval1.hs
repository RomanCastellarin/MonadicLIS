module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Integer
lookfor v s = case lookup v s of
                Just x -> x

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update v n s = case s of
                []      -> [(v,n)]
                (w,m):t -> if v == w then (v,n):t else (w,m):(update v n t)

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip state = state
evalComm (Let varA intA) state = update varA (evalIntExp intA state) state
evalComm (Seq comA comB) state = let state' = evalComm comA state in evalComm comB state'
evalComm (Cond bool comA comB) state = if (evalBoolExp bool state) then evalComm comA state else evalComm comB state
evalComm (Repeat comA bool) state = let state' = evalComm comA state in
                                    if (evalBoolExp bool state')
                                    then evalComm Skip state'
                                    else evalComm (Repeat comA bool) state'

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Integer
evalIntExp (Const intA)       _     = intA
evalIntExp (Var varA)         state = lookfor varA state
evalIntExp (UMinus intA)      state = negate (evalIntExp intA state)
evalIntExp (Plus intA intB)   state = (evalIntExp intA state) + (evalIntExp intB state)
evalIntExp (Minus intA intB)  state = (evalIntExp intA state) - (evalIntExp intB state)
evalIntExp (Times intA intB)  state = (evalIntExp intA state) * (evalIntExp intB state)
evalIntExp (Div intA intB)    state = (evalIntExp intA state) `div` (evalIntExp intB state)
evalIntExp (TerCond bool intA intB) state
    | evalBoolExp bool state  = evalIntExp intA state
    | otherwise               = evalIntExp intB state


-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue             _     = True
evalBoolExp BFalse            _     = False
evalBoolExp (Eq intA intB)    state = (evalIntExp intA state) == (evalIntExp intB state)
evalBoolExp (Lt intA intB)    state = (evalIntExp intA state) < (evalIntExp intB state)
evalBoolExp (Gt intA intB)    state = (evalIntExp intA state) > (evalIntExp intB state)
evalBoolExp (And boolA boolB) state = (evalBoolExp boolA state) && (evalBoolExp boolB state)
evalBoolExp (Or boolA boolB)  state = (evalBoolExp boolA state) || (evalBoolExp boolB state)
evalBoolExp (Not bool)        state = not (evalBoolExp bool state)
