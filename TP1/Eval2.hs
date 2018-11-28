module Eval2 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Errores
data Error = DivByZero | UndefVar Variable deriving Show

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Either Error Integer
lookfor v s = case lookup v s of
                Just x -> Right x
                _      -> Left $ UndefVar v

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update v n s = case s of
                []      -> [(v,n)]
                (w,m):t -> if v == w then (v,n):t else (w,m):(update v n t)

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> Either Error State
evalComm Skip state = Right state
evalComm (Let varA intA) state = 
            case (evalIntExp intA state) of
                Left error   -> Left error
                Right result -> Right $ update varA result state
evalComm (Seq comA comB) state =
            case (evalComm comA state) of
                Left error   -> Left error
                Right state' -> evalComm comB state'
evalComm (Cond bool comA comB) state =
            case (evalBoolExp bool state) of
                Left error   -> Left error
                Right result -> if result then evalComm comA state else evalComm comB state
evalComm (Repeat comA bool) state =
            case (evalComm comA state) of
                Left error -> Left error
                Right state' ->
                    case (evalBoolExp bool state') of
                        Left error   -> Left error
                        Right result -> if result
                                        then evalComm Skip state'
                                        else evalComm (Repeat comA bool) state'

-- Handlers especiales
evalBinaryOp :: Either Error a -> (a -> a -> b) -> Either Error a -> Either Error b
evalBinaryOp (Left error) _ _ = Left error
evalBinaryOp _ _ (Left error) = Left error
evalBinaryOp (Right intA) op (Right intB) = Right $ op intA intB

evalDivOp :: Either Error Integer -> Either Error Integer -> Either Error Integer
evalDivOp (Left error) _ = Left error
evalDivOp _ (Left error) = Left error
evalDivOp _ (Right 0) = Left DivByZero
evalDivOp (Right intA) (Right intB) = Right $ intA `div` intB

evalUnaryOp :: (a -> a) -> Either Error a -> Either Error a
evalUnaryOp _ (Left error) = Left error
evalUnaryOp op (Right intA) = Right $ op intA


-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const intA)       _     = Right intA
evalIntExp (Var varA)         state = lookfor varA state
evalIntExp (UMinus intA)      state = evalUnaryOp (negate) (evalIntExp intA state)
evalIntExp (Plus intA intB)   state = evalBinaryOp (evalIntExp intA state) (+) (evalIntExp intB state)
evalIntExp (Minus intA intB)  state = evalBinaryOp (evalIntExp intA state) (-) (evalIntExp intB state)
evalIntExp (Times intA intB)  state = evalBinaryOp (evalIntExp intA state) (*) (evalIntExp intB state)
evalIntExp (Div intA intB)    state = evalDivOp (evalIntExp intA state) (evalIntExp intB state)
evalIntExp (TerCond bool intA intB) state =
        case (evalBoolExp bool state) of 
            Left error   -> Left error
            Right result -> if result then evalIntExp intA state else evalIntExp intB state


-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp BTrue             _     = Right True
evalBoolExp BFalse            _     = Right False
evalBoolExp (Eq intA intB)    state = evalBinaryOp (evalIntExp intA state) (==) (evalIntExp intB state)
evalBoolExp (Lt intA intB)    state = evalBinaryOp (evalIntExp intA state) (<) (evalIntExp intB state)
evalBoolExp (Gt intA intB)    state = evalBinaryOp (evalIntExp intA state) (>) (evalIntExp intB state)
evalBoolExp (And boolA boolB) state = evalBinaryOp (evalBoolExp boolA state) (&&) (evalBoolExp boolB state)
evalBoolExp (Or boolA boolB)  state = evalBinaryOp (evalBoolExp boolA state) (||) (evalBoolExp boolB state)
evalBoolExp (Not bool)        state = evalUnaryOp (not) (evalBoolExp bool state)
