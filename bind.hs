import Control.Monad (join, liftM)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = xs >>= (\x -> if even x then [x*x, x*x] else [])
