module SlotMachine(SlotMachine, normalizeSymbols, randomGrid, lines2Indexes, matchesPattern, betResult) where
import Prelude hiding (lines)
import System.Random

data SlotMachine = SlotMachine {
        symbols :: [(Char, Double)],
        wildcards :: [(Char, String)], 
        lines :: [String],
        pays :: [(String, Double)],
        spreads :: [(Char, Int, Double)],
        gambles :: [Int],
        modes :: [Int]}

-- Examples for testing ----------------------------------------------------------------------------
slotMachine1 = SlotMachine {
    symbols = zip "9JQKA7%$#!" [10, 8, 5, 5, 5, 3, 3, 2, 2, 1.0], -- [(Char, Double)]
    wildcards = [ ('!', "90JQKA7%$") ], -- [(Char, String)]
    lines = [ -- [String]
        "FGHIJ", "ABCDE", "KLMNO", "AGMIE", "KGCIO", "FBCDJ", "FLMNJ", "ABHNO", "KLHDE", "FLHDJ",
        "FBHNJ", "AGHIE", "KGHIO", "AGCIE", "KGMIO", "FGCIJ", "FGMIJ", "ABMDE", "KLCNO", "ALMNE"
    ],
    pays = [ -- [(String, Double)]
        ("999", 4.0), ("9999", 10), ("99999", 50),
        ("JJJ", 5), ("JJJJ", 15), ("JJJJJ", 65),
        ("QQQ", 6), ("QQQQ", 20), ("QQQQQ", 75),
        ("KKK", 7), ("KKKK", 25), ("KKKKK", 100),
        ("AAA", 10), ("AAAA", 60), ("AAAAA", 200),
        ("777", 15), ("7777", 100), ("77777", 300),
        ("%%%%", 40), ("%%%%%", 150),
        ("$$", 2), ("$$$", 20), ("$$$$", 200), ("$$$$$", 1000)
    ],
    spreads = [ ('#', 3, 50.0), ('#', 4, 100) ], -- [(Char, Int, Double)]
    gambles = [1, 2, 5, 10, 20], -- [Int]
    modes = [1, 3, 5, 10, 20] -- [Int]
}

{- Utils -------------------------------------------------------------------------------------------

Reusable code should be here.
-}

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
sumSymbols [] = 0
sumSymbols ((_,value):list) = value + sumSymbols list

----------------------------------------------------------------------------------

randomGrid :: StdGen -> SlotMachine -> (String, StdGen)
randomGrid gen slot = (randomSymbol gen (symbols slot), gen)

{-
Recives the normalizeSymbols list, the sum of all the second terms is equal to 1 (100% getting a symbol)
Rolls cumulative probability between 0 and 1 and returns the corresponding symbol for that number

randomSymbol (mkStdGen 100) (normalizeSymbols (zip "9JQKA7%$#!" [10, 8, 5, 5, 5,  3, 3, 2, 2, 1.0]))
randomSymbol (mkStdGen 100) [('9',0.5),('3',0.5)]
-} 
randomSymbol :: StdGen -> [(Char, Double)] -> String
randomSymbol _ [] = ""
randomSymbol _ ((c,value):[]) = [c]
randomSymbol gen ((c,value):(next_c,next_value):list)
    | rollSymbol gen (c,value) = [c]
    | otherwise    = randomSymbol gen ((next_c, value + next_value):list) --Accumulate probability for next roll

-- Rolls a particular symbol's probability (between 0~1)
rollSymbol :: StdGen -> (Char,Double) -> Bool
rollSymbol gen (c, value) = value > roll
 where
    (roll, g2) = randomR (0,1) gen :: (Double,StdGen)

randomN :: StdGen -> Int -> (Int,StdGen)
randomN gen n = randomR (1,n) gen :: (Int,StdGen)

----------------------------------------------------------------------------------

lines2Indexes :: [String] -> [[Int]]
-- TODO: your code here!
lines2Indexes _ = [] -- Not Implemented Yet!


matchesPattern :: SlotMachine -> String -> String -> Bool
-- TODO: your code here!
matchesPattern _ _ _ = False -- Not Implemented Yet!


betResult :: SlotMachine -> String -> Int -> Int -> Int
-- TODO: your code here!
betResult _ _ _ _ = 0 -- Not Implemented Yet!


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
        result = filter (\((ok, ov), (ek, ev)) -> and [ok == ek, abs(ov-ev) < 0.000001]) $ zip obtained expected
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
        result = filter (\(o,e) -> o /= e) $ zip obtained expected
        obtained = map (uncurry3 (betResult slotMachine1)) $ [ 
                ("AAK9!%7A7Q%%J7K", 5, 1), ("AAK9!%7A7Q%%J7K", 10, 2), ("AAK9!%7A7Q%%J7K", 20, 2), ("AAK9!%7A7Q%%J7K", 20, 10),
                ("!77#Q7AAK##$AQQ", 1, 1), ("!77#Q7AAK##$AQQ", 5, 10), ("!77#Q7AAK##$AQQ", 10, 1), ("!77#Q7AAK##$AQQ", 20, 1), 
                ("!77#Q7AAK##$AQQ", 20, 20) 
            ]
        expected = [-5, 0, -20, -200, 49, 600, 80, 80, 1600]
        
-- Main --------------------------------------------------------------------------------------------

main = do
    {
        putStrLn "Simple Test Verification!";
        putStrLn "=========================";
        
        byLine$ map (\(b, l) -> 
            (if (b) 
                then putStrLn ("OK. "++ l ++ " Passed!")
                else putStrLn ("*** Test FAILED *** " ++ l)
            )) $ zip [testNormalize, testRandom, testIndexes, testMatches, testBet] ["testNormalize", "testRandom", "testIndexes", "testMatches", "testBet"];

        putStrLn "=========================";
    }
    where
        byLine [] = do { return () ; }
        byLine (l:ls) = do { l; byLine ls }
