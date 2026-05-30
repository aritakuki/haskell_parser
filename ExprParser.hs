{-# LANGUAGE LambdaCase #-}

module ExprParser where

-- Parsec: Haskellのパーサーコンビネータライブラリ
import Text.Parsec
import Text.Parsec.String (Parser)
-- Map: 変数の環境（変数名→値）を保持するため
import Data.Map (Map)
import qualified Data.Map as M

-- ======================
-- AST（抽象構文木）
-- 数式を木構造で表現するデータ型
-- ======================

data Expr
  = Lit Int           -- 数値リテラル (例: 5, 42)
  | Var String        -- 変数 (例: x, y, name)
  | Add Expr Expr     -- 足し算 (例: 1 + 2)
  | Sub Expr Expr     -- 引き算 (例: 5 - 3)
  | Mul Expr Expr     -- 掛け算 (例: 2 * 3)
  | Div Expr Expr     -- 割り算 (例: 10 / 2)
  | Pow Expr Expr     -- 累乗 (例: x^2, 2^3)
  | Factorial Expr    -- 階乗 (例: 3! = 6)
  | Sin Expr          -- sin関数 (例: sin(x))
  | Cos Expr          -- cos関数 (例: cos(x))
  | Deriv String Expr -- 記号的微分 (例: d/dx x^2)
  | Integral String Expr -- 記号的積分 (例: integral x x^2)
  deriving (Show, Eq)

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

-- ======================
-- 評価関数（数値計算）
-- ======================

-- eval: ASTを数値として評価する
-- env: 変数の環境（例: {"x": 5}）
eval :: Map String Int -> Expr -> Int
eval env = \case

    Lit n ->
        n  -- 数値はそのまま返す

    Var x ->
        case M.lookup x env of  -- 変数は環境から値を取得
            Just v ->
                v

            Nothing ->
                error ("Undefined variable: " ++ x)

    Add a b ->
        eval env a + eval env b  -- 足し算

    Sub a b ->
        eval env a - eval env b  -- 引き算

    Mul a b ->
        eval env a * eval env b  -- 掛け算

    Div a b ->
        eval env a `div` eval env b  -- 割り算（整数）

    Pow a b ->
        eval env a ^ eval env b  -- 累乗

    Factorial e ->
        product [1 .. eval env e]  -- 階乗（1からnまでの積）

    Sin _ ->
        error "numeric sin not implemented"  -- sinは記号的微分用

    Cos _ ->
        error "numeric cos not implemented"  -- cosは記号的微分用

    Deriv _ _ ->
        error "Use differentiate"  -- 微分はdifferentiate関数を使用

    Integral _ _ ->
        error "Use integrate"  -- 積分はintegrate関数を使用

-- ======================
-- 記号的微分関数
-- 数式を変数で微分する（数値計算ではなく、数式変換）
-- ======================

differentiate :: String -> Expr -> Expr
differentiate v = \case

    Lit _ ->
        Lit 0  -- 定数の微分は0

    Var x ->
        if x == v
        then Lit 1  -- 変数の微分: dx/dx=1
        else Lit 0  -- 変数の微分: dy/dx=0

    Add a b ->
        Add
            (differentiate v a)
            (differentiate v b)  -- 和の微分: (f+g)' = f' + g'

    Sub a b ->
        Sub
            (differentiate v a)
            (differentiate v b)  -- 差の微分: (f-g)' = f' - g'

    Mul a b ->
        Add
            (Mul (differentiate v a) b)
            (Mul a (differentiate v b))  -- 積の微分公式: (fg)' = f'g + fg'

    Div a b ->
        Div
            (Sub
                (Mul (differentiate v a) b)
                (Mul a (differentiate v b)))
            (Pow b (Lit 2))  -- 商の微分公式: (f/g)' = (f'g - fg')/g²

    Pow e (Lit n) ->
        Mul
            (Lit n)
            (Mul
                (Pow e (Lit (n-1)))
                (differentiate v e))  -- 累乗の微分公式: (x^n)' = nx^(n-1)

    Pow _ _ ->
        error "general power differentiation not implemented"

    Sin e ->
        Mul
            (Cos e)
            (differentiate v e)  -- sinの微分: (sin x)' = cos x * x'

    Cos e ->
        Mul
            (Lit (-1))
            (Mul
                (Sin e)
                (differentiate v e))  -- cosの微分: (cos x)' = -sin x * x'

    Factorial _ ->
        error "factorial differentiation not implemented"  -- 階乗の微分は未実装

    Deriv _ e ->
        differentiate v e  -- ネストした微分

    Integral _ e ->
        e  -- 積分の微分は元の関数（微積分学の基本定理）

-- ======================
-- 記号的積分関数
-- 数式を変数で積分する（数値計算ではなく、数式変換）
-- ======================

integrate :: String -> Expr -> Expr
integrate v = \case

    Lit n ->
        Mul (Lit n) (Var v)  -- 定数の積分: ∫c dx = cx

    Var x ->
        if x == v
        then
            Div
                (Pow (Var v) (Lit 2))
                (Lit 2)  -- ∫x dx = x²/2
        else
            Mul (Var x) (Var v)  -- ∫y dx = yx (yは定数として扱う)

    Add a b ->
        Add
            (integrate v a)
            (integrate v b)  -- 和の積分: ∫(f+g) = ∫f + ∫g

    Sub a b ->
        Sub
            (integrate v a)
            (integrate v b)  -- 差の積分: ∫(f-g) = ∫f - ∫g

    Mul (Lit n) e ->
        Mul
            (Lit n)
            (integrate v e)  -- 定数倍の積分: ∫cf = c∫f

    Mul e (Lit n) ->
        Mul
            (Lit n)
            (integrate v e)  -- 定数倍の積分（順序不同）

    Mul _ _ ->
        error "general product integration not implemented"  -- 積の積分は部分積分が必要（未実装）

    Div _ _ ->
        error "quotient integration not implemented"  -- 商の積分は未実装

    Pow (Var x) (Lit n)
        | x == v ->
            Div
                (Pow (Var x) (Lit (n+1)))
                (Lit (n+1))  -- 累乗の積分: ∫x^n dx = x^(n+1)/(n+1)

    Pow _ _ ->
        error "general power integration not implemented"

    Sin (Var x)
        | x == v ->
            Mul
                (Lit (-1))
                (Cos (Var x))  -- sinの積分: ∫sin x dx = -cos x

    Cos (Var x)
        | x == v ->
            Sin (Var x)  -- cosの積分: ∫cos x dx = sin x

    Sin _ ->
        error "general sin integration not implemented"

    Cos _ ->
        error "general cos integration not implemented"

    Factorial _ ->
        error "factorial integration not implemented"  -- 階乗の積分は未実装

    Deriv _ e ->
        e  -- 微分の積分は元の関数

    Integral _ _ ->
        error "double integration not implemented"  -- 二重積分は未実装

-- ======================
-- 自由変数の抽出
-- 式に含まれる変数名のリストを返す
-- ======================

freeVars :: Expr -> [String]
freeVars = uniq . go
  where

    go :: Expr -> [String]
    go = \case

        Lit _ ->
            []  -- 数値には変数がない

        Var x ->
            [x]  -- 変数名を返す

        Add a b ->
            go a ++ go b  -- 再帰的に変数を収集

        Sub a b ->
            go a ++ go b

        Mul a b ->
            go a ++ go b

        Div a b ->
            go a ++ go b

        Pow a b ->
            go a ++ go b

        Factorial e ->
            go e

        Sin e ->
            go e

        Cos e ->
            go e

        Deriv _ e ->
            go e

        Integral _ e ->
            go e

    uniq [] =
        []

    uniq (x:xs)
        | x `elem` xs =
            uniq xs  -- 重複を削除

        | otherwise =
            x : uniq xs

-- ======================
-- 式を文字列に変換（表示用）
-- ======================

-- showExpr: ASTを読みやすい文字列に変換する
showExpr :: Expr -> String
showExpr = \case

    Lit n ->
        show n

    Var x ->
        x

    Add a b ->
        "(" ++ showExpr a ++ " + " ++ showExpr b ++ ")"

    Sub a b ->
        "(" ++ showExpr a ++ " - " ++ showExpr b ++ ")"

    Mul a b ->
        "(" ++ showExpr a ++ " * " ++ showExpr b ++ ")"

    Div a b ->
        "(" ++ showExpr a ++ " / " ++ showExpr b ++ ")"

    Pow a b ->
        "(" ++ showExpr a ++ "^" ++ showExpr b ++ ")"

    Factorial e ->
        showExpr e ++ "!"

    Sin e ->
        "sin(" ++ showExpr e ++ ")"

    Cos e ->
        "cos(" ++ showExpr e ++ ")"

    Deriv v e ->
        "d/d" ++ v ++ " " ++ showExpr e

    Integral v e ->
        "integral " ++ v ++ " " ++ showExpr e
