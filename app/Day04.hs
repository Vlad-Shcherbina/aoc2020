module Day04 (day04) where

import System.IO
import Control.Exception (assert)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.Either (isRight)
import Data.List (delete)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Util

data Passport = Passport
    { byr :: Text
    , iyr :: Text
    , eyr :: Text
    , hgt :: Text
    , hcl :: Text
    , ecl :: Text
    , pid :: Text
    , cid :: Maybe Text
    } deriving (Show)

day04 :: IO ()
day04 = withFile "data/04.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let kvss = T.splitOn "\n\n" c & map parseKvs
    let passports = [p | Right p <- map kvsToPassport kvss]
    putStr "part 1: "
    print $ length passports
    putStr "part 2: "
    print $ length $ filter (isRight . validate) passports

parseKvs :: Text -> [(Text, Text)]
parseKvs p = p
    & T.split (\c -> c == ' ' || c == '\n')
    & filter (/= "")
    & map (fromJust . partition ":")

kvsToPassport :: [(Text, Text)] -> Either Text Passport
kvsToPassport kvs = do
    let 
        getField name kvs = do
            let (mbField, kvs') = takeFromAssocList name kvs
            field <- maybeToRight ("missing " `T.append` name) mbField
            return (field, kvs')
    (byr, kvs1) <- getField "byr" kvs
    (iyr, kvs2) <- getField "iyr" kvs1
    (eyr, kvs3) <- getField "eyr" kvs2
    (hgt, kvs4) <- getField "hgt" kvs3
    (hcl, kvs5) <- getField "hcl" kvs4
    (ecl, kvs6) <- getField "ecl" kvs5
    (pid, kvs7) <- getField "pid" kvs6
    let (cid, kvs8) = takeFromAssocList "cid" kvs7
    return Passport {..} & assert (null kvs8)

takeFromAssocList :: (Eq k) => k -> [(k, v)] -> (Maybe v, [(k, v)])
takeFromAssocList key [] = (Nothing, [])
takeFromAssocList key ((k, v):kvs) = if key == k 
    then (Just v, kvs)
    else (result, (k, v):kvs') where
        (result, kvs') = takeFromAssocList key kvs

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l Nothing = Left l
maybeToRight l (Just r) = Right r

-- return error message or () if valid
validate :: Passport -> Either Text ()
validate Passport {..} = do
    let byr' = read $ T.unpack byr
    if 1920 <= byr' && byr' <= 2002
        then Right ()
        else Left $ "invalid byr " `T.append` byr

    let iyr' = read $ T.unpack iyr
    if 2010 <= iyr' && iyr' <= 2020
        then Right ()
        else Left $ "invalid iyr " `T.append` iyr

    let eyr' = read $ T.unpack eyr
    if 2020 <= eyr' && eyr' <= 2030
        then Right ()
        else Left $ "invalid eyr " `T.append` eyr

    let heightError = Left $ "invalid hgt " `T.append` hgt
    case parseHeight hgt of
        Just (Centimeters h) -> if 150 <= h && h <= 193
            then Right ()
            else heightError
        Just (Inches h) -> if 59 <= h && h <= 76
            then Right ()
            else heightError
        Nothing -> heightError

    if isValidHairColor hcl
        then Right ()
        else Left $ "invalid hcl " `T.append` hcl

    case ecl of
        "amb" -> Right ()
        "blu" -> Right ()
        "brn" -> Right ()
        "gry" -> Right ()
        "grn" -> Right ()
        "hzl" -> Right ()
        "oth" -> Right ()
        _ -> Left $ "invalid ecl " `T.append` ecl

    if T.length pid == 9 && all isDigit (T.unpack pid)
        then Right ()
        else Left $ "invalid pid " `T.append` pid

    return ()

data Height = Centimeters Int | Inches Int
parseHeight :: Text -> Maybe Height
parseHeight s = case s of
    (T.stripSuffix "cm" -> Just t) -> Just $ Centimeters $ read $ T.unpack t
    (T.stripSuffix "in" -> Just t) -> Just $ Inches $ read $ T.unpack t
    _ -> Nothing

isValidHairColor :: Text -> Bool
isValidHairColor s = case T.stripPrefix "#" s of
    Just s -> T.length s == 6 && all isLowercaseHexDigit (T.unpack s)
    Nothing -> False

isLowercaseHexDigit :: Char -> Bool
isLowercaseHexDigit c = '0' <= c && c <= '9' || 'a' <= c && c <= 'f'
