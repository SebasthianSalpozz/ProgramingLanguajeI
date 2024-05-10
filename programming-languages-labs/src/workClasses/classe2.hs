-- -- classs functor f where 
-- -- fmap :: (a->b ) -> f a -> f b 

data MyList a = Empty | Cons a (MyList a) deriving (Show, Eq, Ord)

-- list1 = Cons 1 (Cons 2 (Cons 3 Empty))

-- list2 = Cons 4 (Cons 5 (Cons 6 Empty))

-- module Functor where

-- data MyList a = Empty | Cons a (MyList a)  deriving (Show, Eq, Ord)

instance Functor MyList where
    -- implementation of fmap function for MyList
     -- fmap :: (a -> b) -> MyList a -> MyList b
     fmap _ Empty = Empty
     fmap f (Cons x xs) = Cons (f x) (fmap f xs)


data TrafficLight = Red | Yellow | Green 

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Eq TrafficLight where
     -- (==) :: TrafficLight -> TrafficLight -> Bool
     Red == Red = True
     Yellow == Yellow = True
     Green == Green = True
     _ == _ = False
     -- <(/=)
     x /= y = not (x == y)


instance Functor CMaybe where
    fmap :: (a -> b) -> CMaybe a -> CMaybe b
    fmap _ CNothing = CNothing
    fmap f (CJust x y) = CJust (x+1) (f y)



append :: MyList a -> MyList a -> MyList a
append Empty xs = xs
append (Cons x xs) ys = Cons x (append xs ys)
instance Applicative MyList where
        -- pure :: a -> Cons a Empty
        pure a = Cons a Empty
        -- <*> :: MyList (a -> b) -> MyList a -> MyList b
        Empty <*> _ = Empty
        -- (Cons x y )<*> (Cons z w ) = Cons (x z) (fmap x w)
        (Cons x xs) <*> ys = append (fmap x ys) (xs <*> ys)


-- instance Alternative MyList where
--     -- empty :: MyList a
--     empty = Empty
--     -- (<|>) :: MyList a -> MyList a -> MyList a
--     Empty <|> xs = xs
--     xs <|> Empty = xs
--     (Cons x xs) <|> ys = Cons x (xs <|> ys)

-- example:: Maybe Int
-- example = Just 1 <|> Nothing