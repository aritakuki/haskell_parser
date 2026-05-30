module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import AST (Expr(..))

-- ======================
-- パーサー定義
-- 文字列をASTに変換する関数群
-- ======================

-- expr: 足し算・引き算を処理するパーサー
-- chainl1: 左結合で演算子を連結する（例: 1+2+3 → ((1+2)+3)）
expr :: Parser Expr
expr = term `chainl1` addOp
  where
    addOp =
            try (spaces *> char '+' *> spaces *> pure Add)
        <|> try (spaces *> char '-' *> spaces *> pure Sub)

-- term: 掛け算・割り算を処理するパーサー
-- 優先順位: 掛け算・割り算 > 足し算・引き算
term :: Parser Expr
term = factor `chainl1` mulOp
  where
    mulOp =
            try (spaces *> char '*' *> spaces *> pure Mul)
        <|> try (spaces *> char '/' *> spaces *> pure Div)

-- factor: 累乗を処理するパーサー
factor :: Parser Expr
factor = powExpr

-- powExpr: 累乗演算子 ^ を処理するパーサー
-- 例: x^2, 2^3
powExpr :: Parser Expr
powExpr = do
    b <- factorial
    option b $
        try $ do
            spaces
            _ <- char '^'
            spaces
            e <- factorial
            return (Pow b e)

-- factorial: 階乗演算子 ! を処理するパーサー
-- 例: 3! = 6, 5! = 120
factorial :: Parser Expr
factorial = do
    e <- base
    option e $
        try $ do
            spaces
            _ <- char '!'
            return (Factorial e)

-- base: 基本的な要素をパースする（最優先順位）
-- 括弧、数値、変数、関数、微分演算子、積分演算子のいずれか
base :: Parser Expr
base =
        parens expr
    <|> func
    <|> deriv
    <|> integral
    <|> number
    <|> var

-- parens: 括弧で囲まれた式をパースする
-- 例: (1+2) → 括弧を外して中身をパース
parens :: Parser Expr -> Parser Expr
parens p =
    between
        (char '(' *> spaces)
        (char ')' *> spaces)
        p

-- number: 数値をパースする
-- 例: "123" → Lit 123
number :: Parser Expr
number =
    Lit . read <$> many1 digit <* spaces

-- var: 変数名をパースする
-- 例: "x", "y", "name" → Var "x"
var :: Parser Expr
var =
    Var <$> many1 letter <* spaces

-- func: 関数（sin, cos）をパースする
-- 例: "sin(x)" → Sin (Var "x")
func :: Parser Expr
func = do
    f <- sinP <|> cosP
    arg <- parens expr
    return (f arg)
  where
    sinP =
        string "sin" *> spaces *> pure Sin

    cosP =
        string "cos" *> spaces *> pure Cos

-- deriv: 記号的微分演算子をパースする
-- 例: "d/dx x^2" → Deriv "x" (Pow (Var "x") (Lit 2))
deriv :: Parser Expr
deriv = do
    _ <- string "d/d"
    varName <- many1 letter
    spaces
    e <- expr
    return (Deriv varName e)

-- integral: 記号的積分演算子をパースする
-- 例: "integral x x^2" → Integral "x" (Pow (Var "x") (Lit 2))
integral :: Parser Expr
integral = do
    _ <- string "integral"
    spaces
    varName <- many1 letter
    spaces
    e <- expr
    return (Integral varName e)

-- parseExpr: 文字列をパースしてASTを返す
-- 成功なら Right expr、失敗なら Left ParseError
parseExpr :: String -> Either ParseError Expr
parseExpr input =
    parse (spaces *> expr <* eof) "" input
