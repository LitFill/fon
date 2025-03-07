{-# LANGUAGE OverloadedStrings #-}

module Main where

import Flow

import Data.Kind            (Type)
import Data.Text            (Text, pack)
import Data.Void            (Void)
import Text.Megaparsec      (Parsec, parse, parseTest, (<|>), oneOf, noneOf, many, some, choice)
import Text.Megaparsec.Char (string, char, space)

import Text.Megaparsec.Char.Lexer qualified as L (decimal)

data Expr :: Type where
    LitInt  :: Integer -> Expr
    LitBool :: Bool    -> Expr
    Var     :: String  -> Expr
    BinOp   :: Op      -> Expr -> Expr -> Expr
    If      :: Expr    -> Expr -> Expr -> Expr
    Lambda  :: String  -> Expr -> Expr
    App     :: Expr    -> Expr -> Expr
    Let     :: String  -> Expr -> Expr -> Expr
    deriving (Show, Eq)

data Op :: Type where
    Add, Sub, Mul, Div, Eq, Lt, Gt :: Op
    deriving (Show, Eq)

type Parser = Parsec Void Text

angkaP :: Parser Integer
angkaP = L.decimal

litIntP :: Parser Expr
litIntP = LitInt <$> angkaP

trueP, falseP :: Parser Bool
trueP  = True  <$ string "benar"
falseP = False <$ string "salah"

litBoolP :: Parser Expr
litBoolP = LitBool <$> (trueP <|> falseP)

escapedCharP :: Parser Char
escapedCharP = do
    _ <- char '\\'
    c <- oneOf ("\\«»nt" :: String)
    pure <| case c of
        'n'  -> '\n'
        't'  -> '\t'
        '\\' -> '\\'
        '«'  -> '«'
        '»'  -> '»'
        _    -> error "unreachable"

normalCharP :: Parser Char
normalCharP = noneOf ("«»\\" :: String)

litStringP :: Parser String
litStringP = char '«' *> many (normalCharP <|> escapedCharP) <* char '»'

validIdentP :: Parser String
validIdentP = some <| noneOf ("«»\\ /()[]{}\n\t" :: String)

varP :: Parser Expr
varP = Var <$> validIdentP

opP :: Parser Op
opP = do
    op <- oneOf ("+-*/=<>" :: String)
    pure <| case op of
        '+' -> Add
        '-' -> Sub
        '*' -> Mul
        '/' -> Div
        '=' -> Eq
        '<' -> Lt
        '>' -> Gt
        _   -> error "unreachable"

binOpP :: Parser Expr
binOpP = do
    op <- opP
    space
    arg1 <- nonBinOpP
    space
    arg2 <- nonBinOpP
    pure <| BinOp op arg1 arg2

ifP :: Parser Expr
ifP = do
    _ <- string "jika"
    space
    cond <- exprP
    space
    _ <- string "maka"
    space
    expr1 <- exprP
    space
    _ <- string "tidak"
    space
    expr2 <- exprP
    pure <| If cond expr1 expr2

lambdaP :: Parser Expr
lambdaP = do
    _ <- char 'λ'
    name <- validIdentP
    space
    _ <- string "->"
    space
    expr <- exprP
    pure <| Lambda name expr

applyP :: Parser Expr
applyP = do
    _ <- char '('
    fun <- exprP
    space
    arg <- exprP
    _ <- char ')'
    pure <| App fun arg

letP :: Parser Expr
letP = do
    _ <- string "biar"
    space
    name <- validIdentP
    space
    _ <- char '='
    space
    expr1 <- exprP
    space
    _ <- string "di"
    space
    expr2 <- exprP
    pure <| Let name expr1 expr2

nonBinOpP :: Parser Expr
nonBinOpP = choice [literalsP, ifP, lambdaP, applyP, letP, varP]

literalsP :: Parser Expr
literalsP = choice [litIntP, litBoolP]

exprP :: Parser Expr
exprP = choice [literalsP, binOpP, nonBinOpP]

haloP :: Parser Text
haloP = string "halo"

sample :: Text
sample = "biar\nsquare = λnum -> * num num\ndi\n+ (square 8) 5"

parseFile :: FilePath -> IO ()
parseFile fname = do
    prog <- pack <$> readFile fname
    parseTest exprP prog

main :: IO ()
main = do
    let input = "halo"
    print <| parse haloP "" input
    print <| parse litStringP "" "«urmom so fat»"
    parseTest exprP sample

