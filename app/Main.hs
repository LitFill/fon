{-# LANGUAGE OverloadedStrings #-}

module Main where

import Flow

import Data.Kind            (Type)
import Data.Text            (Text, pack, replace, unpack)
import Data.Void            (Void)
import Text.Megaparsec      (Parsec, parse, parseTest, (<|>), oneOf, noneOf, many, some, choice, satisfy, between)
import Text.Megaparsec.Char (string, char, space)

import Text.Megaparsec.Char.Lexer qualified as L (decimal)
import System.Environment (getArgs)

data Expr :: Type where
    LitInt    :: Integer   -> Expr
    LitBool   :: Bool      -> Expr
    LitString :: FonString -> Expr
    Var       :: String    -> Expr
    BinOp     :: Op        -> Expr -> Expr -> Expr
    If        :: Expr      -> Expr -> Expr -> Expr
    Lambda    :: String    -> Expr -> Expr
    App       :: Expr      -> Expr -> Expr
    Let       :: String    -> Expr -> Expr -> Expr
    deriving (Show, Eq)

data Op :: Type where
    Add, Sub, Mul, Div, Eq, Lt, Gt :: Op
    deriving (Show, Eq)

data FonString where
    FonString :: [FonStringContent] -> FonString
    deriving Eq

instance Show FonString where
    show (FonString text) = show (concatMap show text)

data FonStringContent :: Type where
    FonText   :: String    -> FonStringContent
    FonNested :: FonString -> FonStringContent
    deriving Eq

instance Show FonStringContent where
    show (FonText   str) = str
    show (FonNested str) = show str

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

stringP :: Parser String
stringP = char '«' *> many (normalCharP <|> escapedCharP) <* char '»'

stringP' :: Parser String
stringP' = between (char '«') (char '»') (many (normalCharP <|> escapedCharP) <|> stringP')

litStringP :: Parser Expr
litStringP = LitString <$> fonstring

fonstring :: Parser FonString
fonstring = do
    _ <- char '«'
    content <- many foncontent
    _ <- char '»'
    pure <| FonString content

foncontent :: Parser FonStringContent
foncontent = fontext <|> fonnested
  where
    fontext = FonText . clean <$> some (satisfy (\c -> c /= '«' && c /= '»'))
    fonnested = FonNested <$> fonstring
    clean = unpack . replace "\\\n" "" . pack

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
literalsP = choice [litIntP, litBoolP, litStringP]

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

transpile :: Expr -> String
transpile (LitInt num)           = show num
transpile (LitBool bool)         = show bool
transpile (LitString str)        = show str
transpile (Var name)             = name
transpile (BinOp op arg1 arg2)   = transpile arg1 ++ " " ++ transpileOp op ++ " " ++ transpile arg2
transpile (If cond expr1 expr2)  = "if " ++ transpile cond ++ " then " ++ transpile expr1 ++ " else " ++ transpile expr2
transpile (Lambda name expr)     = "\\" ++ name ++ " -> " ++ transpile expr
transpile (App fun arg)          = "(" ++ transpile fun ++ " " ++ transpile arg ++ ")"
transpile (Let name expr1 expr2) = "let " ++ name ++ " = " ++ transpile expr1 ++ " in " ++ transpile expr2

transpileOp :: Op -> String
transpileOp = \case
    Add  -> "+"
    Sub  -> "-"
    Mul  -> "*"
    Div  -> "/"
    Eq   -> "="
    Lt   -> "<"
    Gt   -> ">"

transpileFile :: FilePath -> IO ()
transpileFile fname = do
    prog <- pack <$> readFile fname
    parseTest exprP prog
    case parse exprP "" prog of
        Right ast -> transpile ast |> ("module Main where\n\nmain :: IO ()\nmain = print $ " ++) |> writeFile (fname ++ ".hs")
        Left  _   -> putStrLn "error"

main :: IO ()
main = do
    getArgs >>= \case
        [] -> putStrLn "usage:\n\ttranspile <fname>\t\ttranspile to haskell\n\tparse <fname>\t\tparse AST"
        ["transpile", fname] -> transpileFile fname
        ["parse", fname] -> parseFile fname
        _ -> putStrLn "usage:\n\ttranspile <fname>\t\ttranspile to haskell\n\tparse <fname>\t\tparse AST"
