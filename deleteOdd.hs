{-# LANGUAGE FlexibleInstances #-}

-- deleteOdd :: [Int] -> [Int]
-- deleteOdd list = filter (\i -> mod i 2 == 0) list
-- foo = [1..10]
-- fooEven = deleteOdd foo

data LinkedListNode a = Nil
                      | LinkedListNode {next :: LinkedListNode a, value :: a}

instance Show (LinkedListNode String) where
    show Nil                    = ""
    show (LinkedListNode Nil v) = v
    show (LinkedListNode ptr v) = v ++ "\n" ++ show ptr

-- foo = Nil
-- bar = LinkedListNode ( LinkedListNode Nil 2 ) 1
-- baz = LinkedListNode { next =
--                            LinkedListNode { next =
--                                                 LinkedListNode { next = Nil, value = 3 },
--                                                                value = 2},
--                            value = 1
-- }

-- instance (Show a) => Show( LinkedListNode a ) where
--     show Nil                    = ""
--     show (LinkedListNode Nil v) = show v
--     show (LinkedListNode ptr v) = show v ++ "\n" ++ show ptr

getLinkedList :: [a] -> LinkedListNode a
getLinkedList []     = Nil
getLinkedList (x:xs) = LinkedListNode {next = getLinkedList xs, value = x}

-- foo = getLinkedList [1..10]

-- instance Foldable LinkedListNode where
--     foldr f z Nil                   = z
--     foldr f z (LinkedListNode xs x) = f x $ foldr f z xs

-- s = foldr (+) 0 foo



-- deleteOdd :: LinkedListNode Integer -> LinkedListNode Integer
-- deleteOdd lln = foldr oddX Nil lln

oddX :: Int -> LinkedListNode Int -> LinkedListNode Int
oddX x lln |
          mod x 2 == 0 = LinkedListNode { next = lln, value = x }
        | otherwise = lln

foldrlln :: (a -> b -> b) -> b -> LinkedListNode a -> b
foldrlln f z Nil                   = z
foldrlln f z (LinkedListNode xs x) = f x $ foldrlln f z xs

-- s = foldrlln (+) 0 foo

deleteOdd :: LinkedListNode Int -> LinkedListNode Int
deleteOdd lln = foldrlln oddX Nil lln

-- fooEven = deleteOdd foo
-- s' = foldrlln (+) 0 fooEven





