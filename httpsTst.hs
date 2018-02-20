{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import qualified Data.Text as T
import Network.Connection
import Network.HTTP.Conduit

import qualified Data.ByteString.Lazy.Char8 as BS

data Person = P { surname    :: String
                , name       :: String
                , patronymic :: String
                , birthDate  :: Date
                , passport   :: Passport
                }
    deriving Show

data Date = D { day   :: Int
              , month :: Month
              , year  :: Int
              }

data Month = Jan | Feb | Mar | Apr | May | Jul | Jun | Aug | Sep | Oct | Nov | Dec
    deriving Eq

data Passport = Passp { serial1 :: String
                      , serial2 :: String
                      , number  :: String
                      , date    :: Date
                      }

mkDate :: Int -> Int -> Int -> Either String Date
mkDate d m y = do
    year  <- if y > 1910 && y < 2020 then Right y else Left $ "Wrong year: " ++ show y
    month <- int2Mon m
    day   <- check d month
    return $ D day month year
  where
    check d m | d > 0 = case lookup m days of
                             Just x  -> if d <= x then return d else Left $ "Wrong day: " ++ show d
                                                                         ++ " in month: " ++ show m
                             Nothing -> Left $ "Wrong month: " ++ show m
              | otherwise = Left $ "Wrong day: " ++ show d

tokenUrl   = "https://service.nalog.ru/static/captcha.html"
mainUrl    = "https://service.nalog.ru/inn.do"
requestUrl = "https://service.nalog.ru/inn-proc.do"

tmpFile    = "tmp.gif"

main = do
    request <- parseRequest mainUrl
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    res <- httpLbs request manager
    print $ (BS.take 100 $ responseBody res) `BS.append` (BS.pack " ...")
    print $ responseCookieJar res



days :: [(Month, Int)]
days = [ (Jan, 31)
       , (Feb, 29)
       , (Mar, 31)
       , (Apr, 30)
       , (May, 31)
       , (Jul, 30)
       , (Jun, 31)
       , (Aug, 31)
       , (Sep, 30)
       , (Oct, 31)
       , (Nov, 30)
       , (Dec, 31)
       ]

monthsNums = zip [Jan, Feb, Mar, Apr, May, Jul, Jun, Aug, Sep, Oct, Nov, Dec] [1..]

int2Mon :: Int -> Either String Month
int2Mon x | x > 0 && x < 13 = Right $ fst $ monthsNums !! (x - 1)
          | otherwise       = Left $ "Wrong month: " ++ show x

mon2Int :: Month -> Int
mon2Int m = case lookup m monthsNums of
                 Just x  -> x
                 Nothing -> error $ "Wrong month: " ++ show m

instance Show Month where
    show x = let i = mon2Int x
             in if i < 10 then "0" ++ show i else show i

instance Show Date where
    show (D d m y) = ds ++ "." ++ show m ++ "." ++ show y
      where ds = if d < 10 then "0" ++ show d else show d

instance Show Passport where
    show (Passp s1 s2 n d) = s1 ++ " " ++ s2 ++ " " ++ n ++ " " ++ show d

parseRecord :: String -> Either String Person
parseRecord str = do
    let _ws = _words str
    ws <- if length _ws == 8 then return _ws else Left $ "Wrong data: " ++ show _ws
    let surname      = ws !! 0
        name         = ws !! 1
        patronymic   = ws !! 2
        [p1, p2, p3, pd] = drop 4 ws
    dateB <- parseDate $ ws !! 3
    dateP <- parseDate pd
    passp <- parsePassport p1 p2 p3 dateP
    return $ P surname name patronymic dateB passp
    where
     _words = words . map (\x -> if isSpace x then ' ' else x)

parseDate :: String -> Either String Date
parseDate dt@(d1 : d2 : '.' : m1 : m2 : '.' : y1 : y2 : y3 : y4 : [])
    | allDs     = mkDate d m y
    where allDs = all isDigit [d1, d2, m1, m2, y1, y2, y3, y4]
          d =                                              digitToInt d1 * 10 + digitToInt d2
          m =                                              digitToInt m1 * 10 + digitToInt m2
          y = digitToInt y1 * 1000 + digitToInt y2 * 100 + digitToInt y3 * 10 + digitToInt y4
parseDate dt = Left $ "Wrong date: " ++ dt

parsePassport :: String -> String -> String -> Date -> Either String Passport
parsePassport p1 p2 p3 d | allDs     = Right $ Passp p1 p2 p3 d
                         | otherwise = Left $ "Wrong passport: " ++ p1 ++ " " ++ p2 ++ " " ++ p3
    where allDs = length p1 == 2
               && length p2 == 2
               && length p3 == 6
               && (all isDigit $ p1 ++ p2 ++ p3)

test = parseRecord "Исламов\tЗелимхан\tЗайндиевич\t24.02.1980\t36 14 987071\t06.05.2015"
