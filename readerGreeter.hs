import Control.Monad.Trans.Reader

greeter :: Reader Int String
greeter = do
    name <- ask
    return ("hello, " ++ show name ++ "!")

greeter' :: Reader String String
greeter' = ask >>= (\x -> return ("hello, " ++ x ++ "!"))

greeterF :: Reader String String
greeterF = (\x -> ("hello, " ++ x ++ "!")) <$> ask

greeterA :: Reader String String
greeterA = pure (\x -> ("hello, " ++ x ++ "!A")) <*> ask
