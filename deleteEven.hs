{-# LANGUAGE FlexibleInstances #-}

data LinkedListNode a = Nil
                      | LinkedListNode {next :: LinkedListNode a, value :: a}

instance Show (LinkedListNode String) where
    show Nil                    = ""
    show (LinkedListNode Nil v) = v
    show (LinkedListNode ptr v) = v ++ "\n" ++ show ptr

getLinkedList :: [a] -> LinkedListNode a
getLinkedList []     = Nil
getLinkedList (x:xs) = LinkedListNode {next = getLinkedList xs, value = x}

evenX :: Int -> LinkedListNode Int -> LinkedListNode Int
evenX x lln |
          mod x 2 == 1 = LinkedListNode { next = lln, value = x }
        | otherwise = lln

foldrlln :: (a -> b -> b) -> b -> LinkedListNode a -> b
foldrlln f z Nil                   = z
foldrlln f z (LinkedListNode xs x) = f x $ foldrlln f z xs

deleteEven :: LinkedListNode Int -> LinkedListNode Int
deleteEven lln = foldrlln evenX Nil lln


