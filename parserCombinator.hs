module Parser where
import Data.List
import Data.Char
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
--pairToStringLIst :: (Show a, Show b) => (a, b) -> [String]
data Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\s -> case s of
    "" -> []
    (c:cs) -> [(c,cs)])

parse (Parser p) = p

(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

instance Monad Parser where
    return a = Parser (\s -> [(a, s)])
    p >>= f = Parser (\s -> parse p s |>
        map (\ (a, s') -> parse (f a) s') |>
        concat) 

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = item >>= \c ->
    if p c then return c else (Parser (\cs -> []))


class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a


instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    mplus p q = Parser (\s -> (parse p) s ++ (parse q) s)

option :: Parser a -> Parser a -> Parser a
option p q = Parser (\s -> case parse (mplus p q) s of
    [] -> []
    (x:xs) -> [x])

char :: Char -> Parser Char
char c = satisfies (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do { char c; string cs; return (c:cs)}

many0 :: Parser a -> Parser [a]
many0 p = many1 p `option` return []

many1 :: Parser a -> Parser [a]
many1 p = do { a <- p; as <- many0 p; return (a:as)}

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) `option` return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do 
    a <- p
    as <- many0 (do {sep; p})
    return (a: as)

space :: Parser String
space = many0 (satisfies isSpace)
    where 
        isSpace ' ' = True
        isSpace '\n' = True
        isSpace '\t' = True
        isSpace _ = False

token :: Parser a -> Parser a
token p = do { a <- p; space; return a}

symb :: String -> Parser String
symb s = token (string s)

digit :: Parser Char
digit = satisfies isDigit

number :: Parser Int
number = do 
    cs <- many1 digit
    return $ read cs

expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = number `option` do { symb "("; n <- expr; symb ")"; return n}

addop = do {symb "+"; return (+)} `option` do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} `option` do {symb "/"; return (div)}

run :: String -> Int
run s = case parse expr s of
             [(num, _)] -> num

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) `option` return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where rest a = (do f <- op
                                    b <- p
                                    rest (f a b))
                                `option` return a

main = putStrLn $ show $ run "3+4"
