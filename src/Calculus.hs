{-# LANGUAGE LambdaCase #-}

module Calculus where

import AST (Expr(..))

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
