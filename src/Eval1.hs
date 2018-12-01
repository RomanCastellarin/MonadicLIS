module Eval1 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f = State (\s -> let (v, s') = runState m s in
                           runState (f v) s')

-- Para calmar al GHC
instance Functor State where
    fmap = liftM
 
instance Applicative State where
    pure   = return
    (<*>)  = ap      

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState State where
    lookfor v = State (\s -> (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = State (\s -> ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (evalComm p) initState)

-- Evalua un comando en un estado dado
evalComm :: MonadState m => Comm -> m ()
evalComm Skip = return ()
evalComm (Let varA intA) = evalIntExp intA >>= update varA
evalComm (Seq comA comB) = evalComm comA >>= \x -> evalComm comB
evalComm (Cond boolE comA comB) = do b <- evalBoolExp boolE
                                     if b 
                                     then evalComm comA 
                                     else evalComm comB
evalComm (While boolE comA) = do b <- evalBoolExp boolE
                                 if b
                                 then evalComm (Seq comA $ While boolE comA)
                                 else evalComm Skip


-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: MonadState m => IntExp -> m Int
evalIntExp (Const intA)      = return intA
evalIntExp (Var varA)        = lookfor varA
evalIntExp (UMinus intA)     = evalIntExp intA >>= return . negate                               
evalIntExp (Plus intA intB)  = do left <- evalIntExp intA 
                                  right <- evalIntExp intB
                                  return $ left + right
evalIntExp (Minus intA intB) = do left <- evalIntExp intA
                                  right <- evalIntExp intB
                                  return $ left - right
evalIntExp (Times intA intB) = do left <- evalIntExp intA
                                  right <- evalIntExp intB
                                  return $ left * right
evalIntExp (Div intA intB)   = do left <- evalIntExp intA
                                  right <- evalIntExp intB
                                  return $ left `div` right

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: MonadState m => BoolExp -> m Bool
evalBoolExp BTrue             = return True
evalBoolExp BFalse            = return False 
evalBoolExp (Eq intA intB)    = do left <- evalIntExp intA
                                   right <- evalIntExp intB
                                   return $ left == right
evalBoolExp (Lt intA intB)    = do left <- evalIntExp intA
                                   right <- evalIntExp intB
                                   return $ left < right
evalBoolExp (Gt intA intB)    = do left <- evalIntExp intA
                                   right <- evalIntExp intB
                                   return $ left > right
evalBoolExp (And boolA boolB) = do left <- evalBoolExp boolA
                                   right <- evalBoolExp boolB
                                   return $ left && right
evalBoolExp (Or boolA boolB)  = do left <- evalBoolExp boolA
                                   right <- evalBoolExp boolB
                                   return $ left || right
evalBoolExp (Not bool)        = evalBoolExp bool >>= return . not

