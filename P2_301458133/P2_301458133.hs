import Control.Applicative (Alternative(..), many)
import Data.Char
import System.Environment
import System.IO

-- Definition of Prop from the assignment
data Prop = Const Bool
          | Var String
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Iff Prop Prop
          deriving (Eq, Read, Show)

-- 1. write G1 that enforces intended precedence
-- G1:
-- Formula   ::= Formula '<->' Formula | ImplyTerm
-- ImplyTerm ::= ImplyTerm '->' ImplyTerm | OrTerm
-- OrTerm    ::= OrTerm '\/' OrTerm | AndTerm
-- AndTerm   ::= AndTerm '/\' AndTerm | NotTerm
-- NotTerm   ::= '!' NotTerm | Factor
-- Factor    ::= '(' Formula ')' | 'T' | 'F' | Ident

-- 2. write G2 that enforces right-associativity of all binary operators.
-- G2:
-- Formula   ::= ImplyTerm '<->' Formula | ImplyTerm
-- ImplyTerm ::= OrTerm '->' ImplyTerm | OrTerm
-- OrTerm    ::= AndTerm '\/' OrTerm | AndTerm
-- AndTerm   ::= NotTerm '/\' AndTerm | NotTerm
-- NotTerm   ::= '!' NotTerm | Factor
-- Factor    ::= '(' Formula ')' | 'T' | 'F' | Ident


-- 3. Implementing Functor, Applicative, Monad, and Alternative for Parser (Adapted from LEC 11 Slides)
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

instance Functor Parser where
    fmap f p = P (\input ->
        case parse p input of
            [] -> []
            [(v, out)] -> [(f v, out)])

instance Applicative Parser where
    pure v = P (\input -> [(v, input)])
    pf <*> px = P (\input ->
        case parse pf input of
            [] -> []
            [(f, out)] -> parse (fmap f px) out)

instance Monad Parser where
    p >>= f = P (\input ->
        case parse p input of
            [] -> []
            [(v, out)] -> parse (f v) out)

instance Alternative Parser where
    empty = P (\input -> [])
    p <|> q = P (\input ->
        case parse p input of
            [] -> parse q input
            [(v, out)] -> [(v, out)])

-- Basic Primitives (taken from LEC 12 Slides)
item :: Parser Char
item = P (\input ->
    case input of
        [] -> []
        (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

-- Whitespace Handlers
space :: Parser ()
space = do
    many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Identifier for Variables
ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x:xs)

-- Step 4 & 5: Parsers for constants and variables
constant :: Parser Prop
constant = do
        symbol "T"
        return (Const True)
    <|> do
        symbol "F"
        return (Const False)

var :: Parser Prop
var = do
    v <- token ident
    return (Var v)

-- Step 6: Parser for formulas (Implementing G2) ADAPTED FROM EXAMPLE IN LEC 12 SLIDES
formula :: Parser Prop
formula = do
        t <- implyTerm
        symbol "<->"
        f <- formula
        return (Iff t f)
    <|> implyTerm

implyTerm :: Parser Prop
implyTerm = do
        t <- orTerm
        symbol "->"
        f <- implyTerm
        return (Imply t f)
    <|> orTerm

orTerm :: Parser Prop
orTerm = do
        t <- andTerm
        symbol "\\/"
        f <- orTerm
        return (Or t f)
    <|> andTerm

andTerm :: Parser Prop
andTerm = do
        t <- notTerm
        symbol "/\\"
        f <- andTerm
        return (And t f)
    <|> notTerm

notTerm :: Parser Prop
notTerm = do
        symbol "!"
        f <- notTerm
        return (Not f)
    <|> factor

factor :: Parser Prop
factor = do
        symbol "("
        e <- formula
        symbol ")"
        return e
    <|> constant
    <|> var

-- Step 7: parseFormula mapping to output
parseFormula :: String -> String
parseFormula input =
    case parse formula input of
        [(v, "")] -> show v
        _ -> "Parse Error"

-- Step 8: main logic for IO
main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "Please provide a file path."
        else do
            let filename = head args
            contents <- readFile filename
            let ls = lines contents
            mapM_ (putStrLn . parseFormula) ls