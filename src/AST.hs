module AST where

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
