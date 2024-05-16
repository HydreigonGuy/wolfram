module Lib
    ( getOpts, defaultConf, runWolfram, createStartArray
    ) where

myCharLenth :: [Char] -> Int
myCharLenth [] = 0
myCharLenth (x:xs) = 1 + myCharLenth xs

myIsInt :: [Char] -> Bool -> Bool
myIsInt [] _ = False
myIsInt ('-':xs) neg
    | neg = False
    | otherwise = myIsInt xs True
myIsInt (x:xs) neg
    | x < '0' = False
    | x > '9' = False
    | myCharLenth xs == 0 = True
    | otherwise = myIsInt xs True

data Conf = Conf {
    _rule :: Maybe Int,
    _start :: Int,
    _lines :: Maybe Int,
    _window :: Int,
    _move :: Int,
    _expand :: Int
} deriving (Show)

defaultConf :: Conf
defaultConf = Conf {
    _rule = Nothing,
    _start = 0,
    _lines = Nothing,
    _window = 80,
    _move = 0,
    _expand = 0
}

getNewMove :: Int -> Int -> Int
getNewMove m w
    | w < 0 = m - 1
    | otherwise = 1 + getNewMove m (w - 2)

getOpts :: Conf -> [ String ] -> Maybe Conf
getOpts (Conf Nothing _ _ _ _ _) [] = Nothing 
getOpts (Conf r s l w m _) [] = Just (Conf r s l w (getNewMove m w) 0)
getOpts (Conf r s l w m _) ("--rule":y:xs)
    | not (myIsInt y True) = Nothing
    | otherwise = getOpts (Conf (Just (read y::Int)) s l w m 0) xs
getOpts (Conf r s l w m _) ("--start":y:xs)
    | not (myIsInt y True) = Nothing
    | otherwise = getOpts (Conf r (read y::Int) l w m 0) xs
getOpts (Conf r s l w m _) ("--lines":y:xs)
    | not (myIsInt y True) = Nothing
    | otherwise = getOpts (Conf r s (Just (read y::Int)) w m 0) xs
getOpts (Conf r s l w m _) ("--window":y:xs)
    | not (myIsInt y True) = Nothing
    | otherwise = getOpts (Conf r s l (read y::Int) m 0) xs
getOpts (Conf r s l w m _) ("--move":y:xs)
    | not (myIsInt y False) = Nothing
    | otherwise = getOpts (Conf r s l w (read y::Int) 0) xs
getOpts _ _ = Nothing

checkCompatibilityFirst :: Maybe Int -> Int -> Bool
checkCompatibilityFirst (Just a) b
    | (a `mod` (b * 2)) >= b = True
    | otherwise = False
checkCompatibilityFirst _ _ = False

getNextGen :: Maybe Int -> [ Char ] -> [ Char ]
getNextGen (Just r) (' ':' ':' ':xs)
    | checkCompatibilityFirst (Just r) 1 = '*':getNextGen (Just r) (' ':' ':xs)
    | otherwise = ' ':getNextGen (Just r) (' ':' ':xs)
getNextGen (Just r) (' ':' ':'*':xs)
    | checkCompatibilityFirst (Just r) 2 = '*':getNextGen (Just r) (' ':'*':xs)
    | otherwise = ' ':getNextGen (Just r) (' ':'*':xs)
getNextGen (Just r) (' ':'*':' ':xs)
    | checkCompatibilityFirst (Just r) 4 = '*':getNextGen (Just r) ('*':' ':xs)
    | otherwise = ' ':getNextGen (Just r) ('*':' ':xs)
getNextGen (Just r) (' ':'*':'*':xs)
    | checkCompatibilityFirst (Just r) 8 = '*':getNextGen (Just r) ('*':'*':xs)
    | otherwise = ' ':getNextGen (Just r) ('*':'*':xs)
getNextGen (Just r) ('*':' ':' ':xs)
    | checkCompatibilityFirst (Just r) 16 = '*':
        getNextGen (Just r) (' ':' ':xs)
    | otherwise = ' ':getNextGen (Just r) (' ':' ':xs)
getNextGen (Just r) ('*':' ':'*':xs)
    | checkCompatibilityFirst (Just r) 32 = '*':
        getNextGen (Just r) (' ':'*':xs)
    | otherwise = ' ':getNextGen (Just r) (' ':'*':xs)
getNextGen (Just r) ('*':'*':' ':xs)
    | checkCompatibilityFirst (Just r) 64 = '*':
        getNextGen (Just r) ('*':' ':xs)
    | otherwise = ' ':getNextGen (Just r) ('*':' ':xs)
getNextGen (Just r) ('*':'*':'*':xs)
    | checkCompatibilityFirst (Just r) 128 = '*':
        getNextGen (Just r) ('*':'*':xs)
    | otherwise = ' ':getNextGen (Just r) ('*':'*':xs)
getNextGen (Just r) (' ':' ':xs)
    | checkCompatibilityFirst (Just r) 1 = ['*']
    | otherwise = [' ', ' ']
getNextGen (Just r) (' ':'*':xs)
    | checkCompatibilityFirst (Just r) 4 = ['*']
    | otherwise = [' ', ' ']
getNextGen (Just r) ('*':' ':xs)
    | checkCompatibilityFirst (Just r) 16 = ['*']
    | otherwise = [' ', ' ']
getNextGen (Just r) ('*':'*':xs)
    | checkCompatibilityFirst (Just r) 64 = ['*']
    | otherwise = [' ', ' ']
getNextGen _ (x:xs) = []
getNextGen _ _ = []

getFirstChar :: Maybe Int -> [ Char ] -> Char
getFirstChar r (' ':' ':xs)
    | checkCompatibilityFirst r 1 = '*'
    | otherwise = ' '
getFirstChar r ('*':' ':xs)
    | checkCompatibilityFirst r 4 = '*'
    | otherwise = ' '
getFirstChar r (' ':'*':xs)
    | checkCompatibilityFirst r 2 = '*'
    | otherwise = ' '
getFirstChar r ('*':'*':xs)
    | checkCompatibilityFirst r 8 = '*'
    | otherwise = ' '
getFirstChar r (' ':xs)
    | checkCompatibilityFirst r 1 = '*'
    | otherwise = ' '
getFirstChar r ('*':xs)
    | checkCompatibilityFirst r 4 = '*'
    | otherwise = ' '
getFirstChar _ _ = ' '

myPrintLine :: [ Char ] -> Int -> Int -> IO ()
myPrintLine _ _ 0 = putChar '\n'
myPrintLine (x:xs) 0 s = putChar x >> myPrintLine xs 0 (s - 1)
myPrintLine (x:xs) p s = myPrintLine xs (p - 1) s
myPrintLine [] _ _ = putChar '\n'

runWolfram :: Conf -> [ Char ] -> IO ()
runWolfram (Conf _ _ (Just 0) w _ e) str = return ()
runWolfram (Conf _ _ (Just 1) w _ e) str = myPrintLine str e w 
runWolfram (Conf r 0 (Just l) w _ e) str = myPrintLine str e w >>
    runWolfram (Conf r 0 (Just (l - 1)) w 0 (e + 1))
    (' ':getFirstChar r str:getNextGen r str)
runWolfram (Conf r 0 Nothing w _ e) str = myPrintLine str e w >>
    runWolfram (Conf r 0 Nothing w 0 (e + 1))
    (' ':getFirstChar r str:getNextGen r str)
runWolfram (Conf r s l w _ e) str = runWolfram (Conf r (s - 1) l w 0 (e + 1))
    (' ':getFirstChar r str:getNextGen r str)

createStartArray :: Conf -> [ Char ]
createStartArray (Conf _ _ _ 0 (-1) _) = []
createStartArray (Conf _ _ _ w 0 _) = '*':createStartArray
    (Conf Nothing 0 Nothing (w - 1) (-1) 0)
createStartArray (Conf _ _ _ w m _) = ' ':createStartArray
    (Conf Nothing 0 Nothing (w - 1) (m - 1) 0)
