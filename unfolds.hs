myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                    Nothing -> []
                    Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))


data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
                  Nothing -> Leaf
                  Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
    where f :: Integer -> Maybe (Integer, Integer, Integer)
          f x = if x == n
                   then Nothing
                   else Just (x+1, x, x+1)
