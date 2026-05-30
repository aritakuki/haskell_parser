{-# LANGUAGE LambdaCase #-}

module Eval where

import Data.Map (Map)
import qualified Data.Map as M
import AST (Expr(..))

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
