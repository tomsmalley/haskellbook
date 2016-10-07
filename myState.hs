{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA2)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
--    fmap f (State g) = State $ \x -> (f . fst $ g x, snd $ g x)
--    fmap f (State g) = State $ liftA2 (,) (f . fst . g) (snd . g)
--    fmap f (State g) = State $ (,) <$> f . fst . g <*> snd . g
    fmap f (State g) = State $ \s -> let (a, s') = g s
                                      in (f a, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (\s -> (a, s))

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State fm) <*> (State gm) = State $ \s -> let (f, s') = fm s
                                                  (a, s'') = gm s'
                                               in (f a, s'')

instance Monad (State s) where
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State f) >>= g = State $ \s -> let (a, s') = f s
                                     in runState (g a) s'

get :: State s s
get = State $ \x -> (x, x)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec (State sa) = snd . sa

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
