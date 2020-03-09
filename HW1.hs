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

listify :: (a) ->[a]
listify (a) = [a]

form :: [a] -> (Int, Int) -> [[a]] 
form [] _ = []
form list  (vector, dimension)
 | vector > 0 = (take dimension list) : form (drop dimension list) (vector-1, dimension)
 | vector <= 0 = form [] (vector, dimension)
-- | row <= 0 = [list]


constGrid :: a -> (Int, Int) -> [[a]]
constGrid constant (vector, dimension) = replicate vector (replicate dimension constant)

flatten :: [[a]] -> [a]
flatten list = concat list

access :: [[a]] -> (Int, Int) -> a
access list (x,y)
 | (x < (length list) && y < (length (list !! x))) = (list !! x)!!y
 | otherwise = error "Out of index"

----------------------------
-- The Two Signatures (10, 5, 5, 10 points)
slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice [] (r1, r2) (c1, c2) = []
slice grid (r1, r2) (c1, c2)
 | (r1 >= 0 && r2 >= 0) = slice (drop r1(take  r2 grid)) (-6, -6) (c1, c2)
 | (r1 == -6) = drop c1 (take c2(head grid)) : (slice (tail grid) (-6, -6) (c1, c2))


vcat :: [[a]] -> [[a]] -> [[a]]
vcat [] [] = []
vcat g1 g2 = (g1 ++ g2)

hcat :: [[a]] -> [[a]] -> [[a]]
hcat [] [] = []
hcat g1 g2 = ((head g1) ++ (head g2)) : (vcat (tail g1) (tail g2))

without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
without [] (r1, r2) (c1, c2) = []
without grid (r1, r2) (c1, c2)
 | (r1 >= 0 && r2 >= 0) = without ((take r1 grid)++(drop r2 grid)) (-6, -6) (c1, c2)
 | (r1 == -6) = [(take c1(head grid))++(drop c2(head grid))] ++ (without (tail grid) (-6, -6) (c1, c2))

----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)
matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]
matches2d [[]] [[]] = [(0,0)]
matches2d grid pattern
 if (length grid) < (length pattern)
  then matches2d [[]] [[]]
----------------------------
-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.
