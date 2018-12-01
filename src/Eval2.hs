module Eval2 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype StateError a = StateError { runStateError :: Env -> Maybe (a, Env) }

instance Monad StateError where
    return x = StateError (\s -> Just (x, s))
    m >>= f = StateError (\s -> do (v, s') <- runStateError m s 
                                   runStateError (f v) s')


-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState StateError where
    lookfor v = StateError (\s -> lookfor' v s s)
                where lookfor' v [] s = Nothing 
                      lookfor' v ((u, j):ss) s |  v == u   = Just (j, s)
                                               | otherwise = lookfor' v ss s
                                             
    update v i = StateError (\s -> Just ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) |  v == u   = (v, i):ss
                       update' v i ((u, j):ss) | otherwise = (u, j):(update' v i ss)


-- Para calmar al GHC
instance Functor StateError where
    fmap = liftM
 
instance Applicative StateError where
    pure   = return
    (<*>)  = ap      


-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

instance MonadError StateError where
    throw = StateError (\s -> Nothing)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = maybe [] snd (runStateError (evalComm p) initState)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
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
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Int
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
                                  if right == 0
                                  then throw
                                  else return $ left `div` right

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
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

