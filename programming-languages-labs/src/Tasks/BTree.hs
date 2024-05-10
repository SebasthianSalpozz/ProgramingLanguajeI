data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show, Eq)
data Pair a = Pair a a deriving (Show)

instance Functor BinTree where
    fmap _ Empty = Empty
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative BinTree where
    pure::a -> BinTree a
    pure a = Node a Empty Empty
    (<*>) :: BinTree (a -> b) -> BinTree a -> BinTree b
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Node f lf rf) <*> (Node a la ra ) = Node (f a) (lf <*> la) (rf <*> ra)

incrementar :: Num a => BinTree a -> BinTree a
incrementar = fmap (+1)

counter :: BinTree String -> BinTree Int
counter = fmap length

treeInString :: Show a => BinTree a -> String
treeInString Empty = ""
treeInString (Node a l r) = show a ++ " " ++ treeInString l ++ " " ++ treeInString r

merge :: (a -> a -> Bool) -> BinTree a -> BinTree a -> BinTree a
merge _ Empty tree = tree
merge _ tree Empty = tree
merge comp (Node x left1 right1) (Node y left2 right2)
    | comp x y  = Node x left1 (merge comp right1 (Node y left2 right2))
    | otherwise = Node y left2 (merge comp (Node x left1 right1) right2)

mergeTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
mergeTrees tree1 tree2 = merge (<=) tree1 tree2

tree1 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
tree2 = Node 4 (Node 5 Empty Empty) (Node 6 Empty Empty)
tree3 = Node "hola" (Node "mundo" Empty Empty) (Node "hola" Empty Empty)

tree4 = Node "hola" Empty Empty

tree5 = Node "mundo" Empty Empty

tree6 = Node 5 (Node 1 Empty Empty) Empty

tree7 = Node 2 Empty Empty