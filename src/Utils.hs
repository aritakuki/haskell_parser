{-# LANGUAGE LambdaCase #-}

module Utils where

import AST (Expr(..))

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
