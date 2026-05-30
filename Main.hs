module Main where

import ExprParser
import qualified Data.Map as M

main :: IO ()
main = do

  putStrLn "=== Expression Parser Demo ==="
  putStrLn ""

  -- Factorial

  putStrLn "--- Factorial ---"

  testParse "3!"
  testParse "5!"
  testParse "3! + 2!"

  putStrLn ""

  -- Differentiation

  putStrLn "--- Symbolic Differentiation ---"

  testDifferentiate "x" "x^2"
  testDifferentiate "x" "x^3"
  testDifferentiate "x" "sin(x)"
  testDifferentiate "x" "cos(x)"
  testDifferentiate "x" "x^3 + 2*x"
  testDifferentiate "x" "x * x"

  putStrLn ""

  -- Integration

  putStrLn "--- Symbolic Integration ---"

  testIntegrate "x" "x^2"
  testIntegrate "x" "x^3"
  testIntegrate "x" "sin(x)"
  testIntegrate "x" "cos(x)"
  testIntegrate "x" "x^3 + 2*x"
  testIntegrate "x" "5"

  putStrLn ""

  -- Evaluation

  putStrLn "--- Evaluation ---"

  testEval "3 + 2 * 5" [("x","5")]
  testEval "x + 10" [("x","5")]
  testEval "3!" []

  putStrLn ""

  putStrLn "=== Done ==="

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

prettyEnv :: M.Map String Int -> String
prettyEnv env =
  show (M.toList env)

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
