module Main where

import ExprParser

main :: IO ()
main = do
  putStrLn "=== Expression Parser Demo ==="
  putStrLn ""
  
  -- 階乗のデモ
  putStrLn "--- Factorial ---"
  testParse "3!"
  testParse "5!"
  testParse "3! + 2!"
  putStrLn ""
  
  -- 記号的微分のデモ
  putStrLn "--- Symbolic Differentiation ---"
  testDifferentiate "x" "x^2"
  testDifferentiate "x" "x^3"
  testDifferentiate "x" "sin(x)"
  testDifferentiate "x" "cos(x)"
  testDifferentiate "x" "x^3 + 2*x"
  testDifferentiate "x" "x * x"
  putStrLn ""
  
  -- 記号的積分のデモ
  putStrLn "--- Symbolic Integration ---"
  testIntegrate "x" "x^2"
  testIntegrate "x" "x^3"
  testIntegrate "x" "sin(x)"
  testIntegrate "x" "cos(x)"
  testIntegrate "x" "x^3 + 2*x"
  testIntegrate "x" "5"
  putStrLn ""
  
  -- 数式評価のデモ
  putStrLn "--- Evaluation ---"
  testEval "3 + 2 * 5" [("x", "5")]
  testEval "x + 10" [("x", "5")]
  testEval "3!" []
  putStrLn ""
  
  putStrLn "=== Done ==="

testParse :: String -> IO ()
testParse input = do
  case parseExpr input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> do
      putStrLn $ "Input: " ++ input
      putStrLn $ "AST:   " ++ show expr
      putStrLn $ "Eval:  " ++ show (eval M.empty expr)
      putStrLn ""

testDifferentiate :: String -> String -> IO ()
testDifferentiate var input = do
  case parseExpr input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> do
      let deriv = differentiate var expr
      putStrLn $ "Input:       d/d" ++ var ++ " " ++ input
      putStrLn $ "Derivative:  " ++ showExpr deriv
      putStrLn ""

testEval :: String -> [(String, String)] -> IO ()
testEval input bindings = do
  case parseExpr input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> do
      let env = M.fromList [(k, read v) | (k, v) <- bindings]
      putStrLn $ "Input:  " ++ input
      putStrLn $ "Env:    " ++ show bindings
      putStrLn $ "Result: " ++ show (eval env expr)
      putStrLn ""

testIntegrate :: String -> String -> IO ()
testIntegrate var input = do
  case parseExpr input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> do
      let integral = integrate var expr
      putStrLn $ "Input:       integral " ++ var ++ " " ++ input
      putStrLn $ "Integral:    " ++ showExpr integral
      putStrLn ""
