module Day18 (day18) where

import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (liftM, ap)
import Data.Char (isNumber)
import Control.Arrow (ArrowChoice(right))

data ParserState = ParserState
    { psConsumed :: Text
    , psRemaining :: Text
    }

newtype Parser a = Parser (ParserState -> (Either Text a, ParserState))

-- boilerplate
instance Functor Parser where
    fmap = liftM
instance Applicative Parser where 
    pure = return
    (<*>) = ap

instance Monad Parser where
    return x = Parser (Right x,)
    Parser f >>= g = Parser \s -> case f s of
        (Left e, s') -> (Left e, s')
        (Right x, s') -> let Parser g' = g x in g' s'

err :: Text -> Parser a
err e = Parser (Left e,)

ahead :: Parser (Maybe Char)
ahead = Parser \ps -> let
    c = case T.uncons $ psRemaining ps of
        Nothing -> Nothing
        Just (c, _) -> Just c
    in (Right c, ps)

consume :: Parser ()
consume = Parser \ps@ParserState {..} -> case T.uncons psRemaining of
    Nothing -> (Right (), ps)
    Just (c, remaining') -> (
        Right (),
        ParserState {
            psConsumed = psConsumed `T.snoc` c,
            psRemaining = remaining'
        })

skipWs :: Parser ()
skipWs = do
    c <- ahead
    case c of
        Just ' ' -> do
            consume
            skipWs
        _ -> return ()

-- only single-digit number, but that's enough
readNum :: Parser Int
readNum = do
    c <- ahead
    case c of 
        Just c | isNumber c -> return $ read [c]
        _ -> err "expected a number"

data Expr = Number Int | Add Expr Expr | Mul Expr Expr
    deriving Show

readExpr :: Parser Expr
readExpr = do
    left <- readAtom
    foldl (\x f -> f x) left <$> readExprTail

readExprTail :: Parser [Expr -> Expr]
readExprTail = do
    skipWs
    c <- ahead
    case c of
        Just '+' -> do
            consume
            e <- readAtom
            tail <- readExprTail
            return $ (`Add` e) : tail
        Just '*' -> do
            consume
            e <- readAtom
            tail <- readExprTail
            return $ (`Mul` e) : tail
        _ -> return []

readAtom :: Parser Expr
readAtom = do
    skipWs
    c <- ahead
    case c of 
        Just '(' -> do
            consume
            res <- readExpr
            skipWs
            c <- ahead
            case c of
                Just ')' -> consume
                _ -> err "expected ')'"
            return res
        Just c | isNumber c -> do
            consume
            return $ Number $ read [c]
        _ -> err "expected a number or a '('"

runParser :: Parser a -> Text -> Either Text a
runParser (Parser f) s = let
    (res, ParserState {..}) = f ParserState { psConsumed = "", psRemaining = s }
    loc = " in " `T.append` psConsumed `T.append` "<|>" `T.append` psRemaining
    in
    case res of
        Left e -> Left $ e `T.append` loc
        Right res -> if T.null psRemaining
            then Right res
            else Left $ "not all input is consumed" `T.append` loc

eval :: Expr -> Int
eval (Number x) = x
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right

day18 :: IO ()
day18 = withFile "data/18.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let lines = T.lines c

    putStr "part 1: "
    print $ sum [eval x | line <- lines, let Right x = runParser readExpr line]
