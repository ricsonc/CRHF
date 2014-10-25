import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.Char (ord, chr)
import Data.Bits hiding (rotate)

md :: a -> ([Char] -> [b]) -> ([Char] -> [Char]) -> (a -> b -> a) -> (a -> [Char]) -> ([Char] -> [Char])
md iv blockf padf f finalf = (\msg -> finalf $ foldl f iv (blockf $ padf msg))

compose :: [a -> a] -> (a -> a)
compose fs = foldl (flip (.)) id fs

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose arr = (map head arr):(transpose (map tail arr))

replacen :: Int -> a -> [a] -> [a]
replacen n val (x:xs)
	| n == 0 = val:xs
	| otherwise = x:(replacen (n-1) val xs)

replace2n :: Int -> Int -> a -> [[a]] -> [[a]]
replace2n n1 n2 val arr = replacen n1 (replacen n2 val (arr !! n1)) arr

setval :: [[Bool]] -> Int -> Int -> Bool -> [[Bool]]
setval arr n1 n2 val = replace2n n1 n2 val arr

getringdict :: Int -> Int -> [(Int,(Int, Int))]
getringdict n s = zip [0..] (c1 ++ c2 ++ c3 ++ c4)
    where
	c1 = zip [n..s-n-2] (repeat n)
	c2 = zip (repeat (s-n-1)) [n..s-n-2]
	c3 = zip [s-n-1,s-n-2..n+1] (repeat (s-n-1))
	c4 = zip (repeat n) [s-n-1,s-n-2..n+1]

rraccum :: [(Int,(Int, Int))] -> Int -> ([[Bool]], [[Bool]]) -> (Int,(Int, Int)) -> ([[Bool]], [[Bool]])
rraccum rposs amount (oblock, nblock) entry = (oblock, replace2n n1 n2 obool nblock)
    where
        np = mod ((fst entry) + amount) (length rposs)
	(n1, n2) = fromJust $ lookup np rposs
	(o1, o2) = snd entry
	obool = (oblock !! o1) !! o2

rotring :: Int -> Int -> [[Bool]] -> [[Bool]]
rotring amount ring oblock = snd $ foldl (rraccum rposs dr) (oblock,oblock) rposs
    where
	rposs = getringdict ring $ length oblock
	dr
	    | mod amount 2 == 0 = amount
	    | mod amount 2 == 1 = (-amount)

rotate :: [[Bool]] -> Int -> [[Bool]]
rotate oblock amount = compose (map (rotring amount) [0..div (length oblock) 2]) $ oblock
	
lcycle :: (Int,[a]) -> [a]
lcycle (dif, lst) = l2++l1
    where (l1, l2) = splitAt (mod dif (length lst)) lst

slice :: [[Bool]] -> Int -> Int -> [[Bool]]
slice oblock amount dir
    | dir == 1 = transpose $ slice (transpose oblock) amount 0
    | dir == 0 = map lcycle $ zip (cycle [amount, -amount]) oblock

mix :: [[Bool]] -> Int -> [[Bool]]
mix arr amount = slice (slice arr amount 0) amount 1

displace :: [[Bool]] -> (Int, Int) -> Bool -> Bool -> Int -> [[Bool]]
displace arr (cx, cy) ori cv iters
    | iters == 0 = arr
    | otherwise = displace (replace2n mncx mncy cv arr) (mncx, mncy) (not ori) nv (iters-1)
    where
        ncx
            | cv && ori = cx + 1
            | (not cv) && ori = cx - 2
            | otherwise = cx
        ncy
            | cv && not(ori) = cy + 2
            | (not cv) && (not ori) = cy - 1
            | otherwise = cy
        mncx = mod ncx s
        mncy = mod ncy s
        nv = (arr !! mncx) !! mncy
        s = length arr

rmdaf :: Int -> Int -> Int -> [[Bool]] -> [Bool]-> [[Bool]]
rmdaf dt ds dd arr na = displace (mix (rotate narr dt) ds) (0,0) True nval dd
    where
        narr = foldl (\acarr (v, (p1, p2)) -> replace2n p1 p2 v acarr) arr (zip na $ zip [0..] [0..])
        nval = ((narr !! 0) !! 0)

sublist :: Int -> Int -> [a] -> [a] --inclusive both
sublist min max lst = fst $ splitAt (max-rmin) (snd $ splitAt rmin lst)
    where rmin = min - 1

subblock :: [[Bool]] -> Int -> Int -> Int -> Int -> [[Bool]] --inclusive both
subblock arr minx maxx miny maxy = sublist miny maxy (map (sublist minx maxx) arr)

eo :: [a] -> Int -> [a]
eo [] _ = []
eo lst s
    | s == 0 = x:(eo (drop 1 xs) 0)
    | s == 1 = eo (drop 1 lst) 0
    where (x:xs) = lst

checker :: [[Bool]] -> Int -> [[Bool]]
checker arr start = map (\(s, row) -> eo row s) (zip (cycle [0,1]) arr)

xorblock :: [[Bool]] -> [[Bool]] -> [[Bool]]
xorblock a b = map (\(l1, l2) -> map (\t -> (/=) (fst t) (snd t)) (zip l1 l2)) (zip a b)

mdpad :: Int -> Char -> Char -> [Char] -> [Char]
mdpad bsize nd yd istr= istr ++ [nd] ++ (take zp (repeat yd)) ++ (show len)
    where
        len = length istr
        zp = bsize - (mod (len + 1 + (length $ show len)) bsize)

blockf :: Int -> [Char] -> [[Bool]]
blockf s msg = map (concat.(map $ cbin.ord)) (chunksOf s msg)
    where
        cbin v = reverse $ snd $ foldl af (v,[]) [7,6..0]
        af = (\(t,a) x -> if (t >= 2^x) then (t-(2^x),True:a) else (t,False:a))

lehmer :: (Integral a) => a -> a -> a -> a
lehmer prev maxp mult = mod (mult * prev) maxp

plehmer :: (Integral a) => a -> a
plehmer seed = lehmer seed 2147483647 16807

iv :: Int -> Int -> [[Bool]]
iv size seed = chunksOf size (snd $ foldl acc (seed,[]) [1..size^2])
    where
        acc (s,a) i = let nr = plehmer s in (nr, (odd nr):a)

finalf :: Int -> Int -> Int -> Int -> Int -> [[Bool]] -> [Char]
finalf sbs dt ds dd k arr = tochar (xorblock sbb1 sbb2)
    where
        sbb1 = checker (subblock narr 0 (2*sbs-1) 0 (sbs-1)) 0
        sbb2 = checker (subblock narr (l-1-2*sbs) (l-1) (l-sbs-1) (l-1)) 0
        l = length arr
        narr = foldr (.) id (replicate k $ \x -> rmdaf dt ds dd x []) $ arr
        tochar = (map chr) . toint . (chunksOf 8) . concat
        toint msg = map (\j -> sum $ map (\(b,x) -> if b then 2^x else 0) $ zip j [0..7]) msg

displh :: ([Char] -> [Char])
displh = md ivf bf pad f ff
    where
        ivf = iv 64 65537
        bf = blockf 8
        pad = mdpad 8 '1' '0'
        f = rmdaf 17 19 4096
        ff = finalf 32 11 13 1024 8

numberOfSetBits :: Int -> Int
numberOfSetBits x
    | x == 0    = 0
    | otherwise = 1 + (numberOfSetBits (x .&. (x - 1)))

hammingDistance :: [Char] -> [Char] -> Int
hammingDistance a b = sum (map (\ (x, y) -> numberOfSetBits (xor (ord x) (ord y))) (zip a b))

{-| testing the results
let a = displh "hello world!"
let b = displh "hullo world!"
let c = displh "hello world."
hammingDistance a b
513
hammingDistance b c
491
hammingDistance a c
512
-}
