module SlotMachine(SlotMachine, normalizeSymbols, randomGrid, lines2Indexes, matchesPattern, betResult) where
import Prelude hiding (lines)
import System.Random

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.List hiding (lines)
import Data.Char (ord)
import Data.Function (on)

data SlotMachine = SlotMachine {
        symbols :: [(Char, Double)],
        wildcards :: [(Char, String)], 
        lines :: [String],
        pays :: [(String, Double)],
        spreads :: [(Char, Int, Double)],
        gambles :: [Int],
        modes :: [Int]
}

-- Examples for testing ----------------------------------------------------------------------------
slotMachine1 = SlotMachine {
    symbols = zip "9JQKA7%$#!" [10, 8, 5, 5, 5, 3, 3, 2, 2, 1], -- [(Char, Double)]
    wildcards = [ ('!', "90JQKA7%$") ], -- [(Char, String)]
    lines = [ -- [String]
        "FGHIJ", "ABCDE", "KLMNO", "AGMIE", "KGCIO", "FBCDJ", "FLMNJ", "ABHNO", "KLHDE", "FLHDJ",
        "FBHNJ", "AGHIE", "KGHIO", "AGCIE", "KGMIO", "FGCIJ", "FGMIJ", "ABMDE", "KLCNO", "ALMNE"
    ],
    pays = [ -- [(String, Double)]
        ("999", 4), ("9999", 10), ("99999", 50),
        ("JJJ", 5), ("JJJJ", 15), ("JJJJJ", 65),
        ("QQQ", 6), ("QQQQ", 20), ("QQQQQ", 75),
        ("KKK", 7), ("KKKK", 25), ("KKKKK", 100),
        ("AAA", 10), ("AAAA", 60), ("AAAAA", 200),
        ("777", 15), ("7777", 100), ("77777", 300),
        ("%%%%", 40), ("%%%%%", 150),
        ("$$", 2), ("$$$", 20), ("$$$$", 200), ("$$$$$", 1000)
    ],
    spreads = [ ('#', 3, 50), ('#', 4, 100) ], -- [(Char, Int, Double)]
    gambles = [1, 2, 5, 10, 20], -- [Int]
    modes = [1, 3, 5, 10, 20] -- [Int]
}

{- Utils -------------------------------------------------------------------------------------------

Reusable code should be here.
-}

sortDesc :: [Int] -> [Int]
sortDesc = reverse . sort

randomN :: StdGen -> Int -> (Int,StdGen)
randomN gen n = randomR (1,n) gen :: (Int,StdGen)

intTime :: IO Int
intTime = round `fmap` getPOSIXTime 

-- uncurry3 used on testing
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f = \(x, y, z) -> f x y z

-- curry3 not yet used, just as an example
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f = \x y z -> f (x, y, z)

-- TODO: your code here!

{- Classes and Instances ---------------------------------------------------------------------------
        
-}

-- TODO: your code here!

{- Functions ---------------------------------------------------------------------------------------
    
-}

----------------------------------------------------------------------------------
-- normalizeSymbols (zip "9JQKA7%$#!" [10, 8, 5, 5, 5,  3, 3, 2, 2, 1.0])

normalizeSymbols :: [(Char, Double)] -> [(Char, Double)]
normalizeSymbols list = [ (x,y/s) | (x,y) <- list ]
 where s = sumSymbols list

sumSymbols :: [(Char, Double)] -> Double
sumSymbols = sum . map snd

----------------------------------------------------------------------------------

prettyGrid :: StdGen -> SlotMachine -> IO ()
prettyGrid gen slots = putStr grid
 where
    grid = topline ++ (firstLine ++ secondLine ++ thirdLine) ++ bottomLine
    topline     = "    +-----------------+\n"
    firstLine   = "    |  " ++ putSpaces (take 5 newGrid) ++ "|\n"
    secondLine  = "    |  " ++ putSpaces (take 5 $ drop 5 $ newGrid) ++ "|\n"
    thirdLine   = "    |  " ++ putSpaces (take 5 $ drop 10 $ newGrid) ++ "|\n"
    bottomLine  = "    +-----------------+\n"
    (newGrid, next_gen) = randomGrid gen slots

putSpaces :: String -> String
putSpaces = concatMap (: "  ")

randomGrid :: StdGen -> SlotMachine -> (String, StdGen)
randomGrid gen slot = generate 15 gen
 where
    
    normalizedSlots = normalizeSymbols (symbols slot)
    
    generate 0 g = ([], g)
    generate n g = (sym ++ chain, g'') 
        where
            (sym, g') = randomSymbol g normalizedSlots
            (chain, g'') = generate (n-1) g'

{-
Recives the normalizeSymbols list, the sum of all the second terms is equal to 1 (100% getting a symbol)
Rolls cumulative probability between 0 and 1 and returns the corresponding symbol for that number

randomSymbol (mkStdGen 100) (normalizeSymbols (zip "9JQKA7%$#!" [10, 8, 5, 5, 5,  3, 3, 2, 2, 1.0]))
randomSymbol (mkStdGen 100) [('9',0.5),('3',0.5)]
-} 
randomSymbol :: StdGen -> [(Char, Double)] -> (String, StdGen)
randomSymbol gen [] = ("", gen)
randomSymbol gen ((c,value):[]) = ([c], gen)
randomSymbol gen ((c,value):(next_c,next_value):list)
    | success    = ([c], next_gen) 
    | otherwise  = randomSymbol gen ((next_c, value + next_value):list) -- Accumulate probability for next roll using the same gen
    where
        (success, next_gen) = rollSymbol gen (c,value)

-- Rolls a particular symbol's probability (between 0~1)
rollSymbol :: StdGen -> (Char,Double) -> (Bool, StdGen)
rollSymbol gen (c, value) = (value > roll, next_gen)
 where
    (roll, next_gen) = randomR (0,1) gen :: (Double,StdGen)

----------------------------------------------------------------------------------

lines2Indexes :: [String] -> [[Int]]
lines2Indexes [] = []
lines2Indexes (x:xs) = ((line2Index x):(lines2Indexes xs))

line2Index :: String -> [Int]
line2Index s = [ ord(x)-65 | x <- s]

----------------------------------------------------------------------------------

matchesPattern :: SlotMachine -> String -> String -> Bool
matchesPattern _ _"" = True
matchesPattern slot (a:as) (b:bs)
    | wildMatch (wildcards slot) a b = matchesPattern slot as bs
    | otherwise = False

wildMatch :: [(Char, String)] -> Char -> Char -> Bool
wildMatch [] _ _ = False
wildMatch ((wildcard, chain):list) c1 c2
    | c1 == c2  = True
    | matchW    = True
    | otherwise = wildMatch list c1 c2
    where
        matchW = c1 == wildcard && elem c2 chain

----------------------------------------------------------------------------------

betResult :: SlotMachine -> String -> Int -> Int -> Int
betResult slot grid numLines gamble = payment - (gamble * numLines)
 where
    payment = (betPayment slot grid numLines) * gamble

-- Sums all the payments possible given the num of lines gambled
betPayment :: SlotMachine -> String -> Int -> Int
betPayment slot grid numLines = ( sum $ take numLines $ allPayments slot grid )
 where
    spreadPay = getSpreadPayment spreadList grid
    spreadList = orderSpreads (spreads slot)

-- Returns a list with all the payments on the grid, sorted by max value first
allPayments :: SlotMachine -> String -> [Int]
allPayments slot grid = sortDesc $ getPayments slot allLines
 where
    allLines = getAllLines grid indexes
    indexes = lines2Indexes (lines slot)

getPayments :: SlotMachine -> [String] -> [Int]
getPayments _ [] = []
getPayments slot (line:other_lines)
    | gotMatch  = [payment] ++ getPayments slot other_lines
    | otherwise = getPayments slot other_lines
    where
        patterns = orderPayments (pays slot)
        (gotMatch, payment) = matchesAnyPattern slot patterns line 

getSpreadPayment :: [(Char,Int,Double)] -> String -> Int
getSpreadPayment [] _ = 0
getSpreadPayment spreadList grid
    | match = floor value
    | otherwise = getSpreadPayment sList grid
    where
        (spread, num, value) = head spreadList
        sList = tail spreadList
        match = ocurrences >= num
        ocurrences = countOccurrences grid spread

countOccurrences :: String -> Char -> Int
countOccurrences str c = length $ filter (== c) str

-- Returns wether the string matches any of the paying patterns
matchesAnyPattern :: SlotMachine -> [(String,Double)] -> String -> (Bool, Int)
matchesAnyPattern _ [] _ = (False, 0)
matchesAnyPattern slot patterns line
    | matchesPattern slot line pattern = (True, floor value)
    | otherwise = matchesAnyPattern slot pList line
    where
        (pattern, value) = head patterns
        pList = tail patterns

orderPayments :: [(String,Double)] -> [(String,Double)]
orderPayments list = reverse $ sortBy (compare `on` snd) list

orderSpreads :: [(Char,Int,Double)] -> [(Char,Int,Double)]
orderSpreads list = reverse $ sortBy (compare `on` third) list

third :: (a,b,c) -> c
third (a,b,c) = c

{- Returns all the lines of a particular grid
getAllLines "AAK9!%7A7Q%%J7K" (lines2Indexes ["FGHIJ", "ABCDE", "KLMNO", "AGMIE", "KGCIO", "FBCDJ", "FLMNJ", "ABHNO", "KLHDE", "FLHDJ", "FBHNJ", "AGHIE", "KGHIO", "AGCIE", "KGMIO", "FGCIJ", "FGMIJ", "ABMDE", "KLCNO", "ALMNE"])
-}
getAllLines :: String -> [[Int]] -> [String]
getAllLines _ [] = []
getAllLines grid (index:indexedList) = [(indexToLine grid index)] ++ (getAllLines grid indexedList)

-- Returns a line on the slot from a grid
indexToLine :: String -> [Int] -> String
indexToLine grid [] = ""
indexToLine grid (i:ix) = [ grid !! i ] ++ (indexToLine grid ix)

{- Testing and Verification ------------------------------------------------------------------------

Verify elementary functionalities. 
NOTE: Passing this tests DO NOT guarantee functionality
-}
testNormalize :: Bool    
testRandom :: Bool
testIndexes :: Bool
testMatches :: Bool
testBet :: Bool

testNormalize = [] == result
    where
        result = filter (\((ok, ov), (ek, ev)) -> and [ok == ek, abs(ov-ev) > 0.000001]) $ zip obtained expected
        obtained = normalizeSymbols (zip "9JQKA7%$#!" [10, 8, 5, 5, 5, 3, 3, 2, 2, 1.0])
        expected = [
                ('9',0.22727272727272727), ('J',0.18181818181818182), ('Q',0.11363636363636363),
                ('K',0.11363636363636363), ('A',0.11363636363636363), ('7',0.06818181818181818),
                ('%',0.06818181818181818), ('$',0.045454545454545456), ('#',0.045454545454545456),
                ('!',0.02272727272727295)
            ]
        
testRandom = [] == result
    where
        result = map fst $ filter (\(k, v) -> (length k) /= 15) $ obtained
        obtained = map (\g -> randomGrid g slotMachine1) gens
        gens = take 20 $ map mkStdGen [1..]
        
testIndexes = result == []
    where
        result = filter (\(o,e) -> o /= e) $ zip obtained expected
        obtained = concat $ lines2Indexes ["ABHNO", "KGCDE"]
        expected = concat $ [[0, 1, 7, 13, 14], [10, 6, 2, 3, 4]]


testMatches = result == []
    where
        result = filter (\(o,e) -> o /= e) $ zip obtained expected
        obtained = map (uncurry (matchesPattern slotMachine1)) $ [ 
                ("AAA7K", "AAA"), ("AAA7K", "AAAA"), ("7KAAA", "AAA"), ("!77#Q", "777"), ("!77#Q", "#77")
            ]
        expected = [True, False, False, True, False]

testBet = result == []
    where
        result = filter (uncurry (/=)) $ zip obtained expected
        obtained = map (uncurry3 (betResult slotMachine1)) $ [ 
                ("AAK9!%7A7Q%%J7K", 5, 1), ("AAK9!%7A7Q%%J7K", 10, 2), ("AAK9!%7A7Q%%J7K", 20, 2), ("AAK9!%7A7Q%%J7K", 20, 10),
                ("!77#Q7AAK##$AQQ", 1, 1), ("!77#Q7AAK##$AQQ", 5, 10), ("!77#Q7AAK##$AQQ", 10, 1), ("!77#Q7AAK##$AQQ", 20, 1), 
                ("!77#Q7AAK##$AQQ", 20, 20) 
            ]
        expected = [-5, 0, -20, -200, 49, 600, 80, 80, 1600]
        
-- Main --------------------------------------------------------------------------------------------

main = do
    putStrLn "Simple Test Verification!";
    putStrLn "=========================";
    mapM_ runTest $ zip [testNormalize, testRandom, testIndexes, testMatches, testBet] ["testNormalize", "testRandom", "testIndexes", "testMatches", "testBet"]
    putStrLn "=========================";
    where
        runTest (b, l)
            | b         = putStrLn ("OK. "++ l ++ " Passed!")
            | otherwise = putStrLn ("*** Test FAILED *** " ++ l) 
