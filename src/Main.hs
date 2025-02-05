module Main (main) where

-- (1) Find the last element of a list.
getLastElement :: [a] -> a
getLastElement = last

-- (2) Find the last-but-one (or second-last) element of a list.
-- init: Returns all elements of the list except of the last one.
getLastButOneElement :: [a] -> a
getLastButOneElement = last . init

-- (3) Find the K'th element of a list.
getNthElement :: [a] -> Int -> a
getNthElement list index = list !! (index - 1)
-- alternative version using a recursion
getNthElement' :: [a] -> Int -> a
getNthElement' (x:_) 1 = x
getNthElement' [] _ = error "Index out of bounds!"
getNthElement' (_:xs) index
    | index < 1 = error "Index out of bounds!"
    | otherwise = getNthElement' xs (index - 1)

-- (4) Find the number of elements in a list.
getListLength :: [a] -> Int
getListLength = length
-- alternatice using foldr
getListLength' :: [a] -> Int
getListLength' = foldr (\_ n -> n + 1) 0

main :: IO ()
main = do
    print $ getLastElement [1,2,3,4]
    print $ getLastElement ['x'..'z']

    print $ getLastButOneElement [1..10]

    print $ getNthElement [1..] 10
    print $ getNthElement' [1..] 10

    print $ getListLength [1..1000]
    print $ getListLength' [1..1000]

