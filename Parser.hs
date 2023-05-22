{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE LambdaCase #-}
module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char


-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
  return x = Parser $ \s -> Just (x, s)
  mp >>= f = Parser $ \s ->
    case parse mp s of
      Just (x, s') -> parse (f x) s'
      Nothing -> Nothing

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

-- TODO 2.1. parse a expression

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser (\s -> case parse p1 s of
                                Nothing -> parse p2 s
                                ok -> ok)

failParser :: Parser a
failParser = Parser (\s -> Nothing)

charParser :: Char -> Parser Char
charParser c = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just (c, xs)else Nothing)

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser (\s ->
    case s of
        [] -> Nothing
        (x : xs) -> if p x then Just (x, xs) else Nothing)

plusParser :: Parser a -> Parser [a]
plusParser p =
    do  x <- p
        xs <- startParser p
        return (x:xs)

startParser :: Parser a -> Parser [a]
startParser p = plusParser p <|> return []

varParser :: Parser String
varParser =
    do  x <- predicateParser isAlpha
        xs <- startParser (predicateParser isAlphaNum)
        return (x:xs)

varExprParser :: Parser Expr
varExprParser = Variable <$> varParser

funcExprParser :: Parser Expr
funcExprParser = charParser '\92' *> varParser >>= \v ->
                 charParser '.' *> atomicParser >>= \e ->
                 return (Function v e)
                 
macroParser :: Parser Expr
macroParser = do
  x <- charParser '$'
  Macro <$> varParser

parentheseExprParser :: Parser Expr
parentheseExprParser = do
  charParser '('
  expr <- exprParser
  charParser ')'
  return expr

whiteSpaceParser :: Parser String
whiteSpaceParser = startParser (charParser ' ')

spacesExprParser :: Parser Expr
spacesExprParser =
    do
        e1 <- atomicParser
        whiteSpaceParser
        e2 <- atomicParser
        return $ Application e1 e2
        
spaces_multipleExprParser :: Parser Expr
spaces_multipleExprParser = do
  expressions <- mysepBy1 atomicParser whiteSpaceParser
  whiteSpaceParser
  return $ foldl Application (head expressions) (tail expressions)


mysepBy1 :: Parser a -> Parser sep -> Parser [a]
mysepBy1 p sep = do
    x <- p
    xs <- many (sep >> p)
    return (x : xs)

atomicParser :: Parser Expr
atomicParser = macroParser <|> parentheseExprParser <|> funcExprParser <|> varExprParser

exprParser :: Parser Expr
exprParser = spaces_multipleExprParser <|> spacesExprParser <|> varExprParser <|> funcExprParser <|> parentheseExprParser <|> macroParser

parse_expr :: String -> Expr
parse_expr s = case parse exprParser  s of
    Just (e, _) ->  e
    Nothing -> error "Error."


-- TODO 4.2. parse code

assignParser :: Parser Code
assignParser =
  do
    expr <- (spacesExprParser <|> varExprParser <|> funcExprParser <|> parentheseExprParser <|> macroParser)
    whiteSpaceParser
    charParser '='
    whiteSpaceParser
    name <- varParser
    return $ Assign name expr

assign_no_spaceParser :: Parser Code
assign_no_spaceParser = do
    name <- varParser
    charParser '='
    expr <- (spacesExprParser <|> varExprParser <|> funcExprParser <|> parentheseExprParser <|> macroParser)
    return $ Assign name expr

evaluateParser :: Parser Code
evaluateParser =
   do
    Evaluate <$> (spacesExprParser <|> varExprParser <|> funcExprParser <|> parentheseExprParser <|> macroParser)

codeParser :: Parser Code
codeParser = assignParser <|> assign_no_spaceParser <|> evaluateParser

addParentheses :: String -> String
addParentheses s = aux s "" []
  where
    aux :: String -> String -> String -> String
    aux [] wordAcc acc = wordAcc ++ acc
    aux (x:xs) wordAcc acc =
      case x of
        '(' ->
          case extractParenthesesHelper xs 1 of
            (newWord, rest) -> aux rest (wordAcc ++ "(" ++ newWord ++ ")") acc
        ' ' -> aux xs "" (" " ++ wordAcc ++ acc)
        _ -> aux xs (wordAcc ++ [x]) acc

extractParentheses :: String -> [String]
extractParentheses [] = []
extractParentheses (x:xs)
  | x == '(' =
      case extractParenthesesHelper xs 1 of
        (p, rest) -> p : extractParentheses rest
  | otherwise = extractParentheses xs

extractParenthesesHelper :: String -> Int -> (String, String)
extractParenthesesHelper [] _ = ("", "")
extractParenthesesHelper (x:xs) count
  | x == '(' =
      case extractParenthesesHelper xs (count + 1) of
        (p, rest) -> ('(' : p, rest)
  | x == ')' =
    if count == 1
      then ("", xs)
      else
        case extractParenthesesHelper xs (count - 1) of
          (p, rest) -> (')' : p, rest)
  | otherwise =
      case extractParenthesesHelper xs count of
        (p, rest) -> (x : p, rest)

parse_code :: String -> Code
parse_code s = case parse codeParser (addParentheses s) of
    Just (e, _) -> e
    Nothing -> error "Invalid input string"
