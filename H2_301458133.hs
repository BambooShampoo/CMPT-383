myFoldl :: Num a => (a -> b -> a) -> a -> [b] -> a
myFoldl f counter [] = counter
myFoldl f counter (x:xs) = myFoldl f (f counter x) xs

myFoldr :: Num a => (b -> a -> a) -> a -> [b] -> a
myFoldr f counter [] = counter
myFoldr f counter (x:xs)= f x (myFoldr f counter xs)

alternativeMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativeMap f g [] = []
alternativeMap f1 f2 (x:xs) = f1 x : alternativeMap f2 f1 xs

myLength :: [a] -> Int
myLength xs = foldr (\_ i -> 1 + i) 0 xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = foldl func [] xs
    where
        func i x = if p x then i ++ [x] else i

sumsqeven :: [Int] -> Int
<<<<<<< HEAD
sumsqeven = sum . filter (even) . map (^2)
=======
sumsqeven = sum . filter (even) . map (^2)
>>>>>>> 4acd2657ee5e94970fad36b661fd215728cf9065
