fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

listReverse :: [a] -> [a]
listReverse [] = []
listReverse (x:xs) = listReverse(xs) ++ [x]

listAdd :: [Int] -> [Int] -> [Int]
listAdd list1 [] = list1
listAdd [] list2 = list2
listAdd (x:xs) (y:ys) = x + y : listAdd(xs)(ys)

inList :: (Eq a) => [a] -> a -> Bool
inList [] y = False
inList (x:xs) y = x == y || inList xs y

sumTailRec :: Num a => [a] -> a
sumTailRec xs = sumTailRecAux xs 0
    where
        sumTailRecAux :: Num a => [a] -> a -> a
        sumTailRecAux [] sum = sum
        sumTailRecAux (x:xs) sum = sumTailRecAux xs (sum + x)
