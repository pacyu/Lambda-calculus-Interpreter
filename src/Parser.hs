module Parser where
import Syntax
import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isSpace)

type Parser a = String -> Maybe (a, String)

-- 跳过空白字符
skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- 解析单个字符
char :: Char -> Parser Char
char c [] = Nothing
char c (x:xs)
  | x == c    = Just (c, xs)
  | otherwise = Nothing

-- 解析标识符（变量名）
parseIdent :: Parser String
parseIdent [] = Nothing
parseIdent (c : cs)
  | isAlpha c || c == '_' = Just (rest, tail')
  | otherwise = Nothing
  where
    (rest, tail') = span (\ch -> isAlphaNum ch || ch == '_' || ch == '\'') (c : cs)

parseLet :: Parser String
parseLet s = case break (== '=') s of
  (name, '=' : rest) -> Just (trim name, trim rest)
  _ -> Nothing
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- 解析变量（转换为De Bruijn索引）
parseVar :: [String] -> Parser Expr
parseVar ctx s = do
  (name, rest) <- parseIdent s
  case elemIndex name ctx of
    Just idx -> Just (Var idx Nothing, rest)
    Nothing  -> Just (Var (length ctx) (Just name), rest)

-- 解析λ抽象
parseLam :: [String] -> Parser Expr
parseLam ctx s = case char '\\' s <|> char 'λ' s of
  Just (_, s1) -> do
    let s2 = skipSpaces s1
    (name, s3) <- parseIdent s2
    let s4 = skipSpaces s3
    case char '.' s4 of
      Just (_, s5) -> do
        let s6 = skipSpaces s5
        (body, s7) <- parseExpr (name:ctx) s6
        Just (Lam body, s7)
      _ -> Nothing
  _ -> Nothing

-- 解析括号表达式
parseParen :: [String] -> Parser Expr
parseParen ctx s = case char '(' s of
  Just (_, s1) -> do
    let s2 = skipSpaces s1
    (expr, s3) <- parseExpr ctx s2
    let s4 = skipSpaces s3
    case char ')' s4 of
      Just (_, s5) -> Just (expr, s5)
      _ -> Nothing
  _ -> Nothing

-- 解析原子表达式（变量、λ抽象、括号表达式）
parseAtom :: [String] -> Parser Expr
parseAtom ctx s =
  let s1 = skipSpaces s
  in parseLam ctx s1 <|> parseParen ctx s1 <|> parseVar ctx s1

-- 解析函数应用（左结合）
parseApp :: [String] -> Parser Expr
parseApp ctx s = do
  (func, s1) <- parseAtom ctx s
  parseAppTail ctx func s1
  where
    parseAppTail ctx func s = case parseAtom ctx s of
      Just (arg, s1) -> parseAppTail ctx (App func arg) s1
      Nothing -> Just (func, s)

-- 主解析函数
parseExpr :: [String] -> Parser Expr
parseExpr = parseApp

-- 完整解析
parse :: String -> Maybe Expr
parse input = case parseExpr [] (skipSpaces input) of
  Just (expr, rest) | all isSpace rest -> Just expr
  _ -> Nothing

-- 辅助函数：查找元素索引
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex x = go 0
  where
    go _ [] = Nothing
    go n (y:ys)
      | x == y    = Just n
      | otherwise = go (n+1) ys