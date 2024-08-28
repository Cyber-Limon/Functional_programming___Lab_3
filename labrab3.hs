mapMy :: (a -> b) -> [a] -> [b]
mapMy function [] = []
mapMy function (x:xs) = function x : map function xs

filterMy :: (a -> Bool) -> [a] -> [a]
filterMy function [] = []
filterMy function (x:xs)
	| function x = x : filterMy function xs
	| otherwise = filterMy function xs

anyMy :: (a -> Bool) -> [a] -> Bool
anyMy function [] = False
anyMy function (x:xs)
	| function x = True
	| otherwise = anyMy function xs

allMy :: (a -> Bool) -> [a] -> Bool
allMy function [] = True
allMy function (x:xs) 
	| function x = allMy function xs
	| otherwise = False



deleteVowel :: String -> String
deleteVowel [] = []
deleteVowel (x:xs) 
	| elem x "AEIOUYaeiouy" = deleteVowel xs
	| otherwise = x : deleteVowel xs

deleteConsonants :: String -> String
deleteConsonants [] = []
deleteConsonants (x:xs) 
	| elem x "BCDFGHJKLMNPQRSTVWXYZbcdfghjklmnpqrstvwxyz" = deleteConsonants xs
	| otherwise = x : deleteConsonants xs

deleteNumbers :: String -> String
deleteNumbers [] = []
deleteNumbers (x:xs) 
	| elem x "0123456789" = deleteNumbers xs
	| otherwise = x : deleteNumbers xs

firstLetters :: [String] -> String
firstLetters xs = map head xs

lastElements :: [[a]] -> [a]
lastElements xs = map last xs

someElements :: [[a]] -> Int -> [a]
someElements xs n = map (!! n) xs

turnover :: [[a]] -> [[a]]
turnover xs = reverse (map (reverse) xs)

mapIfMy :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapIfMy cond f1 f2 xs = mapIfMy' cond f1 f2 xs []
mapIfMy' cond f1 f2 [] ys = ys
mapIfMy' cond f1 f2 (x:xs) ys
	| cond x == True = mapIfMy' cond f1 f2 xs (ys ++ [f1(x)])
	| otherwise = mapIfMy' cond f1 f2 xs (ys ++ [f2(x)])

composeAllMy :: [a -> a] -> (a -> a)
composeAllMy [x] = (x)
composeAllMy (x:xs) = (.) x (composeAllMy xs)
 
applyIterateMy :: [a -> a] -> a -> a
applyIterateMy xs n = applyIterateMy' (reverse xs) n
applyIterateMy' [] n = n
applyIterateMy' (x:xs) n = applyIterateMy' xs (x(n))

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition cond xs = partition' cond xs ([], [])
partition' cond [] (ys1, ys2) = (ys1, ys2)
partition' cond (x:xs) (ys1, ys2)
	| cond x == True = partition' cond xs (ys1 ++ [x], ys2)
	| otherwise = partition' cond xs (ys1, ys2 ++ [x])

findElement :: [a -> Bool] -> a -> Int
findElement (x:xs) y = findElement' (x:xs) y 0
findElement' [] y n = n
findElement' (x:xs) y n 
	| x y == True = findElement' xs y (n + 1)
	| otherwise = findElement' xs y n

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices cond xs = findIndices' cond xs [] 0
findIndices' cond [] ys n = ys
findIndices' cond (x:xs) ys n 
	| cond x == True = findIndices' cond xs (ys ++ [n]) (n + 1)
	| otherwise = findIndices' cond xs ys (n + 1)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy function (x:xs) = sortBy function (filter (\y -> (function y x) == LT) xs) ++ 
					     sortBy function (filter (\y -> (function y x) == EQ) xs) ++	
						 [x] ++ 
						 sortBy function (filter (\y -> (function y x) == GT) xs)
	
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on fb fa = \ x y -> fb (fa x) (fa y)

filterMapAndMy :: [a -> Bool] -> [a] -> [a]
filterMapAndMy [] ys = ys
filterMapAndMy (x:xs) ys = filterMapAndMy xs (filter x ys)

(\\) :: Eq a => [a] -> [a] -> [a] -- Доп. функция для filterMapOrMy
(\\) xs ys = (\\\) xs ys []
(\\\) [] ys zs = reverse zs
(\\\) (x:xs) ys zs 
	| elem x ys = (\\\) xs ys zs
	| otherwise = (\\\) xs ys (x : zs)

filterMapOrMy :: Eq a => [a -> Bool] -> [a] -> [a] -- Можно ли использовать "Eq a =>"?
filterMapOrMy xs ys = filterMapOrMy' xs ys ys
filterMapOrMy' [] ys zs = (\\) zs ys
filterMapOrMy' (x:xs) ys zs = filterMapOrMy' xs ((\\) ys (filter x ys)) zs

sumEqMy :: [[Int]] -> [Int]
sumEqMy xs = sumEqMy' xs [] [] 0 (length (maximum xs))
sumEqMy' (x:xs) xs' ys n i
	| length x < i = sumEqMy' xs (xs' ++ [x ++ replicate (i - length x) 0]) ys n i
	| otherwise = sumEqMy' xs (xs' ++ [x]) ys n i
sumEqMy' [] xs' ys n i 
	| i > n = sumEqMy' [] xs' (sum(map (!! n) xs') : ys) (n + 1) i
	| otherwise = reverse ys

mapAccumLMy :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumLMy function acc xs = mapAccumLMy' function acc xs []
mapAccumLMy' function acc [] ys = (acc, ys)
mapAccumLMy' function acc (x:xs) ys = mapAccumLMy' function (fst (function acc x)) xs (ys ++ [snd (function acc x)])

segregateFMy :: [a -> Bool] -> a -> ([a -> Bool], [a -> Bool]) -- Вывод?
segregateFMy xs n = segregateFMy' xs n ([], [])
segregateFMy' [] n (ys1, ys2) = (ys1, ys2) 
segregateFMy' (x:xs) n (ys1, ys2)
	| x n == True = segregateFMy' xs n (ys1 ++ [(x)], ys2)
	| otherwise = segregateFMy' xs n (ys1, ys2 ++ [(x)])



fstLessSnd :: [Integer] -> [(Integer, Integer)]
fstLessSnd xs = [(x, y) | x <- xs, y <- xs, x < y]

fstLessSndNoGenerator :: [Integer] -> [(Integer, Integer)]
fstLessSndNoGenerator xs = fstLessSndNoGenerator' xs xs xs []
fstLessSndNoGenerator' xs [] xs2 ys = ys 
fstLessSndNoGenerator' xs (x1:xs1) [] ys = fstLessSndNoGenerator' xs xs1 xs ys
fstLessSndNoGenerator' xs (x1:xs1) (x2:xs2) ys
	|x1 < x2 = fstLessSndNoGenerator' xs (x1:xs1) xs2 (ys ++ [(x1,x2)])
	|x1 >= x2 = fstLessSndNoGenerator' xs (x1:xs1) xs2 ys
