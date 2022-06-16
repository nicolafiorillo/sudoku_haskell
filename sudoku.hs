-- Compile for faster performances:
--    ghc -O2 sudoku.hs

import Data.List (transpose, (\\))

main :: IO ()
main = putStrLn (unlines (head (solve4 minimal)))

type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

type Choises = [Value]

blank :: Grid
blank = replicate 9 (replicate 9 '.')

easy :: Grid
easy =
  [ "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ]

gentle :: Grid
gentle =
  [ ".1.42...5",
    "..2.71.39",
    ".......4.",
    "2.71....6",
    "....4....",
    "6....74.3",
    ".7.......",
    "12.73.5..",
    "3...82.7."
  ]

--First diabolical example:
diabolical :: Grid
diabolical =
  [ ".9.7..86.",
    ".31..5.2.",
    "8.6......",
    "..7.5...6",
    "...3.7...",
    "5...1.7..",
    "......1.9",
    ".2.6..35.",
    ".54..8.7."
  ]

unsolvable :: Grid
unsolvable =
  [ "1..9.7..3",
    ".8.....7.",
    "..9...6..",
    "..72.94..",
    "41.....95",
    "..85.43..",
    "..3...7..",
    ".5.....4.",
    "2..8.6..9"
  ]

minimal :: Grid
minimal =
  [ ".98......",
    "....7....",
    "....15...",
    "1........",
    "...2....9",
    "...9.6.82",
    ".......3.",
    "5.1......",
    "...4...2."
  ]

boxsize :: Int
boxsize = 3

--
-- Rows and columns
--
rows :: Matrix a -> [Row a]
rows = id -- rows m = m

cols :: Matrix a -> [Row a]
cols = transpose

--
-- Boxes
--
boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
  where
    pack = split . map split
    split = chop boxsize
    unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

--
-- Grid validity
--
valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxes g)

-- no duplicated value in list
nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x : xs) = notElem x xs && nodups xs

--
-- Solver
--

solve :: Grid -> [Grid]
solve = filter valid . explode . choises -- solver g = filter valid (collapse (choises g))

solve2 :: Grid -> [Grid]
solve2 = filter valid . explode . prune . choises

solve3 :: Grid -> [Grid]
solve3 = filter valid . explode . fix prune . choises

solve4 :: Grid -> [Grid]
solve4 = search . prune . choises

choises :: Grid -> Matrix Choises
choises = map (map choise)
  where
    choise v =
      if v == '.'
        then ['1' .. '9']
        else [v]

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [y : ys | y <- xs, ys <- cartesianProduct xss]

-- Generate a list of matrix with the combination of possible values
explode :: Matrix [a] -> [Matrix a]
explode m = cartesianProduct (map cartesianProduct m) -- TO EXPLAIN

-- Prune the search space
prune :: Matrix Choises -> Matrix Choises
prune = pruneBy boxes . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map reduceChoises . f

-- List difference: subtrack "mask" from "elem"
diff :: Eq a => [a] -> [a] -> [a]
diff [x] _ = [x]
diff xs mask = xs \\ mask

reduceChoises :: Row Choises -> Row Choises
reduceChoises xs = map (`diff` singles) xs
  where
    singles = map (\[x] -> x) (filter (\x -> length x == 1) xs)

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  if x == x' then x else fix f x'
  where
    x' = f x

-- if choises has empty value somewhere
void :: Matrix Choises -> Bool
void = any (any null)

-- the matrix has rows, columns, and boxes are consistent
safe :: Matrix Choises -> Bool
safe m =
  all consistent (rows m)
    && all consistent (cols m)
    && all consistent (boxes m)

-- No duplicated single value
-- Hutton's implementation:
--   consistent = nodups . concat . filter single
consistent :: Row Choises -> Bool
consistent xs = nodups singles
  where
    singles = map (\[x] -> x) (filter (\x -> length x == 1) xs)

-- Check if Matrix is not a correct solution
blocked :: Matrix Choises -> Bool
blocked m = void m || not (safe m)

search :: Matrix Choises -> [Grid]
search m
  | blocked m = []
  | all (all single) m = explode m -- list of a single solution
  | otherwise = [g | m' <- expand m, g <- search (prune m')]

-- Expand only the first cell which hase more than one choise -- TO EXPLAIN
expand :: Matrix Choises -> [Matrix Choises]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = span (all single) m
    (row1, cs : row2) = span single row

-- Check if it is a single char.
single :: [a] -> Bool
single [_] = True
single _ = False
