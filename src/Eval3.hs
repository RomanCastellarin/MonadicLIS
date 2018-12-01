module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype StateErrorTick a = StateErrorTick { runStateErrorTick :: Env -> (Maybe (a, Env), Int) }

instance Monad StateErrorTick where
    return x = StateErrorTick (\s -> (Just (x, s), 0) )
    m >>= f  = StateErrorTick (\s -> let (v1, c1) = runStateErrorTick m s
                                     in case v1 of Nothing      -> (Nothing, c1)
                                                   Just (a, s') -> let (v2, c2) = runStateErrorTick (f a) s'
                                                                   in  (v2, c1 + c2))
-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState StateErrorTick where
    lookfor v = StateErrorTick (\s -> lookfor' v s s)
                where lookfor' v [] s = (Nothing, 0) 
                      lookfor' v ((u, j):ss) s |  v == u   = (Just (j, s), 0)
                                               | otherwise = lookfor' v ss s
                                             
    update v i = StateErrorTick (\s -> (Just ((), update' v i s), 0))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) |  v == u   = (v, i):ss
                       update' v i ((u, j):ss) | otherwise = (u, j):(update' v i ss)

class Monad m => MonadTick m where
    -- Incrementa en uno la cantidad de computaciones
    tick :: m ()

instance MonadTick StateErrorTick where
    tick = StateErrorTick( \s -> (Just ((), s), 1) )

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

instance MonadError StateErrorTick where
    throw = StateErrorTick (\s -> (Nothing,0))

-- Para calmar al GHC
instance Functor StateErrorTick where
    fmap = liftM
 
instance Applicative StateErrorTick where
    pure   = return
    (<*>)  = ap     

-- Evalua un programa en el estado nulo
eval :: Comm -> (Env, Int)
eval p = let (v, c) = runStateErrorTick (evalComm p) initState
             in maybe ([], c) (\p-> (snd p, c)) v

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
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
evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Int
evalIntExp (Const intA)      = return intA
evalIntExp (Var varA)        = lookfor varA
evalIntExp (UMinus intA)     = evalIntExp intA >>= return . negate                               
evalIntExp (Plus intA intB)  = do left <- evalIntExp intA 
                                  right <- evalIntExp intB
                                  tick
                                  return $ left + right
evalIntExp (Minus intA intB) = do left <- evalIntExp intA
                                  right <- evalIntExp intB
                                  tick
                                  return $ left - right
evalIntExp (Times intA intB) = do left <- evalIntExp intA
                                  right <- evalIntExp intB
                                  tick
                                  return $ left * right
evalIntExp (Div intA intB)   = do left <- evalIntExp intA
                                  right <- evalIntExp intB
                                  tick
                                  if right == 0
                                  then throw
                                  else return $ left `div` right

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
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
