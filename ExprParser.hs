{-# LANGUAGE LambdaCase #-}

module ExprParser where

-- Parsec: Haskellのパーサーコンビネータライブラリ
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
-- Map: 変数の環境（変数名→値）を保持するため
import Data.Map (Map)
import qualified Data.Map as M

-- ============================================
-- AST定義（抽象構文木）
-- 数式を木構造で表現するデータ型
-- ============================================
data Expr
  = Lit Int           -- 数値リテラル (例: 5, 42)
  | Var String        -- 変数 (例: x, y, name)
  | Add Expr Expr     -- 足し算 (例: 1 + 2)
  | Mul Expr Expr     -- 掛け算 (例: 2 * 3)
  | Sub Expr Expr     -- 引き算 (例: 5 - 3)
  | Div Expr Expr     -- 割り算 (例: 10 / 2)
  | Pow Expr Expr     -- 累乗 (例: x^2, 2^3)
  | Factorial Expr    -- 階乗 (例: 3! = 6)
  | Sin Expr          -- sin関数 (例: sin(x))
  | Cos Expr          -- cos関数 (例: cos(x))
  | Deriv String Expr -- 記号的微分 (例: d/dx x^2)
  | Integral String Expr -- 記号的積分 (例: integral x x^2)
  deriving (Show, Eq)

-- ============================================
-- パーサー定義
-- 文字列をASTに変換する関数群
-- ============================================

-- expr: 足し算・引き算を処理するパーサー
-- chainl1: 左結合で演算子を連結する（例: 1+2+3 → ((1+2)+3)）
expr :: Parser Expr
expr = term `chainl1` addOp
  where
    addOp = (char '+' >> return Add) <|> (char '-' >> return Sub)

-- term: 掛け算・割り算を処理するパーサー
-- 優先順位: 掛け算・割り算 > 足し算・引き算
term :: Parser Expr
term = factor `chainl1` mulOp
  where
    mulOp = (char '*' >> return Mul) <|> (char '/' >> return Div)

-- factor: 累乗を処理するパーサー
factor :: Parser Expr
factor = powExpr

-- powExpr: 累乗演算子 ^ を処理するパーサー
-- 例: x^2, 2^3
powExpr :: Parser Expr
powExpr = do
  base <- factorial
  optional $ do
    char '^'
    exp <- factorial
    return $ Pow base exp
  return base

-- factorial: 階乗演算子 ! を処理するパーサー
-- 例: 3! = 6, 5! = 120
factorial :: Parser Expr
factorial = do
  e <- base
  optional $ do
    char '!'
    return $ Factorial e
  return e

-- base: 基本的な要素をパースする（最優先順位）
-- 括弧、数値、変数、関数、微分演算子、積分演算子のいずれか
base :: Parser Expr
base = parens expr <|> number <|> var <|> func <|> deriv <|> integral

-- parens: 括弧で囲まれた式をパースする
-- 例: (1+2) → 括弧を外して中身をパース
parens :: Parser Expr -> Parser Expr
parens p = between (char '(' >> spaces) (char ')' >> spaces) p

-- number: 数値をパースする
-- 例: "123" → Lit 123
number :: Parser Expr
number = Lit . read <$> many1 digit

-- var: 変数名をパースする
-- 例: "x", "y", "name" → Var "x"
var :: Parser Expr
var = Var <$> many1 letter

-- func: 関数（sin, cos）をパースする
-- 例: "sin(x)" → Sin (Var "x")
func :: Parser Expr
func = (string "sin" >> spaces >> return Sin <|> string "cos" >> spaces >> return Cos) <*> parens expr

-- deriv: 記号的微分演算子をパースする
-- 例: "d/dx x^2" → Deriv "x" (Pow (Var "x") (Lit 2))
deriv :: Parser Expr
deriv = do
  string "d/d"
  varName <- many1 letter
  spaces
  e <- expr
  return $ Deriv varName e

-- integral: 記号的積分演算子をパースする
-- 例: "integral x x^2" → Integral "x" (Pow (Var "x") (Lit 2))
integral :: Parser Expr
integral = do
  string "integral"
  spaces
  varName <- many1 letter
  spaces
  e <- expr
  return $ Integral varName e

-- ============================================
-- パース実行関数
-- ============================================

-- parseExpr: 文字列をパースしてASTを返す
-- 成功なら Right expr、失敗なら Left ParseError
parseExpr :: String -> Either ParseError Expr
parseExpr input = parse expr "" input

-- ============================================
-- 評価関数（数値計算）
-- ============================================

-- eval: ASTを数値として評価する
-- env: 変数の環境（例: {"x": 5}）
eval :: Map String Int -> Expr -> Int
eval env = \case
  Lit n -> n  -- 数値はそのまま返す
  Var x -> case M.lookup x env of  -- 変数は環境から値を取得
    Just v -> v
    Nothing -> error $ "Undefined variable: " ++ x
  Add e1 e2 -> eval env e1 + eval env e2  -- 足し算
  Mul e1 e2 -> eval env e1 * eval env e2  -- 掛け算
  Sub e1 e2 -> eval env e1 - eval env e2  -- 引き算
  Div e1 e2 -> eval env e1 `div` eval env e2  -- 割り算（整数）
  Pow e1 e2 -> eval env e1 ^ eval env e2  -- 累乗
  Factorial e -> product [1..eval env e]  -- 階乗（1からnまでの積）
  Sin e -> error "sin requires numeric computation (not implemented)"  -- sinは記号的微分用
  Cos e -> error "cos requires numeric computation (not implemented)"  -- cosは記号的微分用
  Deriv _ e -> error "Use differentiate for symbolic differentiation"  -- 微分はdifferentiate関数を使用
  Integral _ e -> error "Use integrate for symbolic integration"  -- 積分はintegrate関数を使用

-- ============================================
-- 記号的微分関数
-- 数式を変数で微分する（数値計算ではなく、数式変換）
-- ============================================

differentiate :: String -> Expr -> Expr
differentiate var = \case
  Lit n -> Lit 0  -- 定数の微分は0
  Var x -> if x == var then Lit 1 else Lit 0  -- 変数の微分: dx/dx=1, dy/dx=0
  Add e1 e2 -> Add (differentiate var e1) (differentiate var e2)  -- 和の微分: (f+g)' = f' + g'
  Sub e1 e2 -> Sub (differentiate var e1) (differentiate var e2)  -- 差の微分: (f-g)' = f' - g'
  Mul e1 e2 -> Add (Mul (differentiate var e1) e2) (Mul e1 (differentiate var e2))  -- 積の微分公式: (fg)' = f'g + fg'
  Div e1 e2 -> Div (Sub (Mul (differentiate var e1) e2) (Mul e1 (differentiate var e2))) (Pow e2 (Lit 2))  -- 商の微分公式: (f/g)' = (f'g - fg')/g²
  Pow e1 (Lit n) -> Mul (Lit n) (Mul (Pow e1 (Lit (n-1))) (differentiate var e1))  -- 累乗の微分公式: (x^n)' = nx^(n-1)
  Sin e -> Mul (Cos e) (differentiate var e)  -- sinの微分: (sin x)' = cos x * x'
  Cos e -> Mul (Lit (-1)) (Mul (Sin e) (differentiate var e))  -- cosの微分: (cos x)' = -sin x * x'
  Factorial e -> error "Factorial differentiation not implemented"  -- 階乗の微分は未実装
  Deriv _ e -> differentiate var e  -- ネストした微分
  Integral _ e -> e  -- 積分の微分は元の関数（微積分学の基本定理）

-- ============================================
-- 記号的積分関数
-- 数式を変数で積分する（数値計算ではなく、数式変換）
-- ============================================

integrate :: String -> Expr -> Expr
integrate var = \case
  Lit n -> Mul (Lit n) (Var var)  -- 定数の積分: ∫c dx = cx
  Var x -> if x == var 
    then Div (Pow (Var var) (Lit 2)) (Lit 2)  -- ∫x dx = x²/2
    else Mul (Var x) (Var var)  -- ∫y dx = yx (yは定数として扱う)
  Add e1 e2 -> Add (integrate var e1) (integrate var e2)  -- 和の積分: ∫(f+g) = ∫f + ∫g
  Sub e1 e2 -> Sub (integrate var e1) (integrate var e2)  -- 差の積分: ∫(f-g) = ∫f - ∫g
  Mul e1 e2 -> error "Product integration not implemented (requires integration by parts)"  -- 積の積分は部分積分が必要（未実装）
  Div e1 e2 -> error "Quotient integration not implemented"  -- 商の積分は未実装
  Pow e1 (Lit n) -> if n == -1
    then error "Integration of 1/x requires ln (not implemented)"  -- ∫1/x dx = ln|x|
    else Div (Pow e1 (Lit (n+1))) (Lit (n+1))  -- ∫x^n dx = x^(n+1)/(n+1)
  Sin e -> Mul (Lit (-1)) (Cos e)  -- ∫sin x dx = -cos x
  Cos e -> Sin e  -- ∫cos x dx = sin x
  Factorial e -> error "Factorial integration not implemented"
  Deriv _ e -> e  -- 微分の積分は元の関数
  Integral _ e -> error "Double integration not implemented"

-- ============================================
-- 式を文字列に変換（表示用）
-- ============================================

-- showExpr: ASTを読みやすい文字列に変換する
showExpr :: Expr -> String
showExpr = \case
  Lit n -> show n
  Var x -> x
  Add e1 e2 -> "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
  Mul e1 e2 -> "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
  Sub e1 e2 -> "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
  Div e1 e2 -> "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"
  Pow e1 e2 -> "(" ++ showExpr e1 ++ "^" ++ showExpr e2 ++ ")"
  Factorial e -> showExpr e ++ "!"
  Sin e -> "sin(" ++ showExpr e ++ ")"
  Cos e -> "cos(" ++ showExpr e ++ ")"
  Deriv v e -> "d/d" ++ v ++ " " ++ showExpr e
  Integral v e -> "integral " ++ v ++ " " ++ showExpr e
