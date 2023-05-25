module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative (Alternative(..))
import Expr
import Data.Char

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    mp >>= f = 
        Parser $ \s ->
            case parse mp s of
                Nothing -> Nothing
                Just (x, s') -> parse (f x) s'
    return x = Parser $ \s -> Just (x, s)

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

--- type declaration over ---

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser (\s ->
        case parse p1 s of
            Nothing -> parse p2 s
            Just (x, s') -> Just (x, s'))

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr s = 
    case parse lambda_paeser s of
        Nothing -> error "parse error"
        Just (e, _) -> e

lambda_paeser :: Parser Expr
lambda_paeser = appExprParser <|> funcExprParser <|> varExprParser <|> macroParser

failParser :: Parser a
failParser = Parser $ \_ -> Nothing

charParser :: Char -> Parser Char
charParser c = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just (c, xs) else Nothing)

whitespaceParser :: Parser String
whitespaceParser = starParser $ charParser ' '

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if p x then Just (x, xs) else Nothing)

plusParser :: Parser a -> Parser [a]
plusParser p = 
    do
        x <- p
        xs <- starParser p
        return (x:xs)

starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> return []

varParser :: Parser String
varParser = 
    do
        c <- predicateParser isAlpha
        cs <- starParser (predicateParser isAlphaNum)
        return (c:cs)

varExprParser :: Parser Expr
varExprParser = 
    do
        v <- varParser
        whitespaceParser
        return (Variable v)

funcExprParser :: Parser Expr
funcExprParser = 
    do
        charParser '\\'
        var <- varParser
        charParser '.'
        expr <- parantheseParser <|> varExprParser <|> funcExprParser <|> macroParser
        return (Function var expr)

appExprParser :: Parser Expr
appExprParser = 
    do
        exprs <- plusParser (varExprParser <|> parantheseParser <|> funcExprParser <|> macroParser)
        return (foldl1 Application exprs)

parantheseParser :: Parser Expr
parantheseParser = 
    do
        charParser '('
        expr <- appExprParser <|> varExprParser <|> funcExprParser
        charParser ')'
        whitespaceParser
        return expr

macroParser :: Parser Expr
macroParser = 
    do
        charParser '$'
        var <- varParser
        whitespaceParser
        return (Macro var)

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code s = 
    case parse codeParser s of
        Nothing -> error "parse error"
        Just (c, _) -> c

codeParser :: Parser Code
codeParser = assignParser <|> evalParser

assignParser :: Parser Code
assignParser = 
    do
        var <- varParser
        whitespaceParser
        charParser '='
        whitespaceParser
        expr <- lambda_paeser
        return (Assign var expr)

evalParser :: Parser Code
evalParser = 
    do
        expr <- lambda_paeser
        return (Evaluate expr)
