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
-- alternaticve using foldr
getListLength' :: [a] -> Int
getListLength' = foldr (\_ n -> n + 1) 0

-- (5) Reverse a list.
getReversedList :: [a] -> [a]
getReversedList [] = []
getReversedList (x:xs) = getReversedList xs ++ [x]
-- alternative using foldr
getReversedList' :: [a] -> [a]
getReversedList' = foldl (flip (:)) []
-- which is essentially the same as
getReversedList'' :: [a] -> [a]
getReversedList'' = foldl (\acc x -> x : acc) []

-- (6) Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)
-- am alternative using reverse
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == (reverse xs)

-- (7) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
    deriving Show
-- some nested lists for debugging
nestedList1 = Elem 5
nestedList2 = (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []
-- an alternative using foldr
flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List xs) = foldr (++) [] $ map flatten' xs

-- (8) Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == head xs = compress xs
    | otherwise = x : compress xs
-- other more compact solution
compress' :: (Eq a) => [a] -> [a]
compress' (x:xs@(y:_))
    | x == y = compress' xs
    | otherwise = x : compress' xs
compress' xs = xs

-- (9) Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack = foldr func []
    where func x [] = [[x]]
          func x (y:xs) =
              if x == head y
                  then ((x:y):xs)
                  else ([x]:y:xs)
-- or using span
pack' :: (Eq a) => [a] -> [[a]]
pack' (x:xs) = let (first, rest) = span (== x) xs
                in (x:first) : pack' rest
pack' [] = []

-- (10) Run-length encoding of a list.
encode :: (Eq a) => [a] -> [(Int, a)]
encode list = map (\x -> (length x, head x)) (pack list)
-- without using a lambda
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' list = [(length x, head x) | x <- pack list]

main :: IO ()
main = do
    print $ getLastElement [1,2,3,4]
    print $ getLastElement ['x'..'z']

    print $ getLastButOneElement [1..10]

    print $ getNthElement [1..] 10
    print $ getNthElement' [1..] 10

    print $ getListLength [1..1000]
    print $ getListLength' [1..1000]

    print $ getReversedList "A man, a plan, a canal, panama!"
    print $ getReversedList' "A man, a plan, a canal, panama!"

    print $ isPalindrome [1,2,4,8,16,8,4,2,1]
    print $ isPalindrome "madamimadam"

    print $ flatten nestedList1
    print $ flatten' nestedList2

    print $ compress "aaaabccaadeeee"
    print $ compress' "aaaabccaadeeee"

    print $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    print $ pack' ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

    print $ encode "aaaabccaadeeee"
    print $ encode' "aaaabccaadeeee"
