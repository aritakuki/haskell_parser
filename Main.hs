module Main where

-- 式パーサーモジュールをインポート
import ExprParser
-- Mapモジュールをインポート（変数環境用）
import qualified Data.Map as M

-- main: デモプログラムのエントリーポイント
-- 階乗、記号的微分、記号的積分、数値評価のデモを実行
main :: IO ()
main = do

  putStrLn "=== Expression Parser Demo ==="
  putStrLn ""

  -- 階乗のデモ

  putStrLn "--- Factorial ---"

  testParse "3!"  -- 3! = 6
  testParse "5!"  -- 5! = 120
  testParse "3! + 2!"  -- 6 + 2 = 8

  putStrLn ""

  -- 記号的微分のデモ

  putStrLn "--- Symbolic Differentiation ---"

  testDifferentiate "x" "x^2"  -- 2x
  testDifferentiate "x" "x^3"  -- 3x²
  testDifferentiate "x" "sin(x)"  -- cos(x)
  testDifferentiate "x" "cos(x)"  -- -sin(x)
  testDifferentiate "x" "x^3 + 2*x"  -- 3x² + 2
  testDifferentiate "x" "x * x"  -- x + x

  putStrLn ""

  -- 記号的積分のデモ

  putStrLn "--- Symbolic Integration ---"

  testIntegrate "x" "x^2"  -- x³/3
  testIntegrate "x" "x^3"  -- x⁴/4
  testIntegrate "x" "sin(x)"  -- -cos(x)
  testIntegrate "x" "cos(x)"  -- sin(x)
  testIntegrate "x" "x^3 + 2*x"  -- x⁴/4 + x²
  testIntegrate "x" "5"  -- 5x

  putStrLn ""

  -- 数値評価のデモ

  putStrLn "--- Evaluation ---"

  testEval "3 + 2 * 5" [("x","5")]  -- 13
  testEval "x + 10" [("x","5")]  -- 15
  testEval "3!" []  -- 6

  putStrLn ""

  putStrLn "=== Done ==="

-- testParse: パースと評価のテスト関数
-- 入力文字列をパースして、ASTと評価結果を表示
testParse :: String -> IO ()
testParse input =
  case parseExpr input of

    Left err ->
      putStrLn $
        "Parse error: " ++ show err

    Right ast -> do

      putStrLn $
        "Input: " ++ input

      putStrLn $
        "AST:   " ++ show ast

      putStrLn $
        "Eval:  " ++ show (eval M.empty ast)

      putStrLn ""

-- testDifferentiate: 記号的微分のテスト関数
-- 入力文字列をパースして、指定変数で微分した結果を表示
testDifferentiate :: String -> String -> IO ()
testDifferentiate variable input =
  case parseExpr input of

    Left err ->
      putStrLn $
        "Parse error: " ++ show err

    Right ast -> do

      let derivExpr =
            differentiate variable ast

      putStrLn $
        "Input:       d/d" ++ variable ++ " " ++ input

      putStrLn $
        "Derivative:  " ++ showExpr derivExpr

      putStrLn ""

-- testIntegrate: 記号的積分のテスト関数
-- 入力文字列をパースして、指定変数で積分した結果を表示
testIntegrate :: String -> String -> IO ()
testIntegrate variable input =
  case parseExpr input of

    Left err ->
      putStrLn $
        "Parse error: " ++ show err

    Right ast -> do

      let integralExpr =
            integrate variable ast

      putStrLn $
        "Input:       integral " ++ variable ++ " " ++ input

      putStrLn $
        "Integral:    " ++ showExpr integralExpr

      putStrLn ""

-- prettyEnv: 変数環境を見やすい文字列に変換
prettyEnv :: M.Map String Int -> String
prettyEnv env =
  show (M.toList env)

-- testEval: 数値評価のテスト関数
-- 入力文字列をパースして、変数環境を与えて評価した結果を表示
testEval :: String -> [(String,String)] -> IO ()
testEval input bindings =
  case parseExpr input of

    Left err ->
      putStrLn $
        "Parse error: " ++ show err

    Right ast -> do

      let env =
            M.fromList
              [ (k, read v)
              | (k,v) <- bindings
              ]

      let vars =
            freeVars ast

      putStrLn $
        "Input:      " ++ input

      putStrLn $
        "Used Vars:  " ++ show vars

      if null vars
      then
        pure ()
      else
        putStrLn $
          "Env:        " ++ prettyEnv env

      putStrLn $
        "Result:     " ++ show (eval env ast)

      putStrLn ""
