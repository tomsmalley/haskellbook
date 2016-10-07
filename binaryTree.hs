data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' new Leaf = Node Leaf new Leaf
insert' new (Node left current right)
    | new == current = Node left current right
    | new < current  = Node (insert' new left) current right
    | new > current  = Node left current (insert' new right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay = if mapTree (+1) testTree' == mapExpected
          then print "yup okay!"
          else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = x : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = (inorder left) ++ [x] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = (postorder left) ++ (postorder right) ++ [x]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."
testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."
testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"
main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree' :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree' _ acc Leaf = acc
foldTree' f acc (Node left x right) = f x (foldTree' f acc left) (foldTree' f acc right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc bt = foldr f acc (inorder bt)
