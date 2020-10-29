module HW1 (
    form,
    constGrid,
    flatten,
    access,
    slice,
    vcat,
    hcat,
    without,
    matches2d
) where

-- do not modify the module declaration above!
-- this will ensure that you cannot load (compile)
-- the module without implementing all of the functions.

-- If you have functions you do not want to implement,
-- leave them as undefined or make them have another
-- default value. If you fully remove any of their definitions,
-- that will be a compilation error during evaluation,
-- and you will be eligible for (yay!) a 5 point deduction
-- (that's bad for your grade). Runtime errors in your code 
-- (rather than compilation errors) are acceptable and will simply
-- result in you getting zero from the specific test case causing
-- an error.

-------------------------
-- Fellowship of the Grid (25, 5, 5, 5 points)
form :: [a] -> (Int, Int) -> [[a]]
form [] (h, w) = []
form x (h, w) = [take w x] ++ form (drop w x) (h, w)

constGrid :: a -> (Int, Int) -> [[a]]
constGrid x (h, w) = form (take (h*w) (repeat x)) (h,w)

flatten :: [[a]] -> [a]
flatten [] = []
flatten[[a]] = [a]
flatten (x:xs) = x ++ flatten xs

access :: [[a]] -> (Int, Int) -> a
access x (h, w) = ((x !! h) !! w)
----------------------------
-- The Two Signatures (10, 5, 5, 10 points)
slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice x (h1,w1) (h2,w2) = take1 xxs (h2,w2)
    where xxs = take (w1-h1) (drop h1 x)
          take1 :: [[a]] -> (Int, Int) -> [[a]]
          take1 [] (_,_) = []
          take1 x (a,b) = [take (b-a) (drop a (head x))] ++ (take1 (tail x) (a, b)) 

vcat :: [[a]] -> [[a]] -> [[a]]
vcat x y = x ++ y

hcat :: [[a]] -> [[a]] -> [[a]]
hcat x y = take2 x y
    where take2 :: [[a]] -> [[a]] -> [[a]]
          take2 [] [] = []
          take2 x y = [head x ++ head y] ++ take2 (tail x) (tail y)

without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
without x (h1,w1) (h2,w2) = take3 xxs (h2,w2)
    where xxs = (take h1 x) ++ (drop w1 x)
          take3 :: [[a]] -> (Int, Int) -> [[a]]
          take3 [] (_,_) = []
          take3 x (a,b) = [take a (head x) ++ drop b (head x)] ++ (take3 (tail x) (a,b))
----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)
recurList subNested lenSubRow (a,b) lmain lenMainRow
	| (lenSubRow+a) > lenMainRow = []
	| (map (drop a) (map (take (lenSubRow+a)) lmain) == subNested) = (b,a) : recurList subNested lenSubRow ((a+1),b) lmain lenMainRow
	| otherwise                   =  recurList subNested lenSubRow ((a+1),b) lmain lenMainRow

bigRecur pattern patternWidth patternDepth  (a,b) grid gridDepth gridWidth
	| (patternDepth + b) > gridDepth = []
	|  otherwise = let l2main = (take (patternDepth + b) grid)
						in (recurList pattern patternWidth (a,b) l2main gridWidth) ++ (bigRecur pattern patternWidth patternDepth (a,(b+1)) (tail grid) gridDepth gridWidth)



matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]
matches2d grid pattern = let
							 patternWidth = length (head pattern)
							 patternDepth = length pattern
							 gridDepth = length grid
							 gridWidth = length (head grid)
							 in bigRecur pattern patternWidth patternDepth  (0,0) grid gridDepth gridWidth
----------------------------
-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.
