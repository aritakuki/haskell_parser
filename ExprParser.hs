{-# LANGUAGE LambdaCase #-}

module ExprParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Map (Map)
import qualified Data.Map as M

-- ======================
-- AST
-- ======================

data Expr
  = Lit Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Factorial Expr
  | Sin Expr
  | Cos Expr
  | Deriv String Expr
  | Integral String Expr
  deriving (Show, Eq)

-- ======================
-- Parser
-- ======================

expr :: Parser Expr
expr = term `chainl1` addOp
  where
    addOp =
            try (spaces *> char '+' *> spaces *> pure Add)
        <|> try (spaces *> char '-' *> spaces *> pure Sub)

term :: Parser Expr
term = factor `chainl1` mulOp
  where
    mulOp =
            try (spaces *> char '*' *> spaces *> pure Mul)
        <|> try (spaces *> char '/' *> spaces *> pure Div)

factor :: Parser Expr
factor = powExpr

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

factorial :: Parser Expr
factorial = do
    e <- base
    option e $
        try $ do
            spaces
            _ <- char '!'
            return (Factorial e)

base :: Parser Expr
base =
        parens expr
    <|> func
    <|> deriv
    <|> integral
    <|> number
    <|> var

parens :: Parser Expr -> Parser Expr
parens p =
    between
        (char '(' *> spaces)
        (char ')' *> spaces)
        p

number :: Parser Expr
number =
    Lit . read <$> many1 digit <* spaces

var :: Parser Expr
var =
    Var <$> many1 letter <* spaces

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

deriv :: Parser Expr
deriv = do
    _ <- string "d/d"
    varName <- many1 letter
    spaces
    e <- expr
    return (Deriv varName e)

integral :: Parser Expr
integral = do
    _ <- string "integral"
    spaces
    varName <- many1 letter
    spaces
    e <- expr
    return (Integral varName e)

parseExpr :: String -> Either ParseError Expr
parseExpr input =
    parse (spaces *> expr <* eof) "" input

-- ======================
-- Evaluation
-- ======================

eval :: Map String Int -> Expr -> Int
eval env = \case

    Lit n ->
        n

    Var x ->
        case M.lookup x env of
            Just v ->
                v

            Nothing ->
                error ("Undefined variable: " ++ x)

    Add a b ->
        eval env a + eval env b

    Sub a b ->
        eval env a - eval env b

    Mul a b ->
        eval env a * eval env b

    Div a b ->
        eval env a `div` eval env b

    Pow a b ->
        eval env a ^ eval env b

    Factorial e ->
        product [1 .. eval env e]

    Sin _ ->
        error "numeric sin not implemented"

    Cos _ ->
        error "numeric cos not implemented"

    Deriv _ _ ->
        error "Use differentiate"

    Integral _ _ ->
        error "Use integrate"

-- ======================
-- Symbolic Differentiation
-- ======================

differentiate :: String -> Expr -> Expr
differentiate v = \case

    Lit _ ->
        Lit 0

    Var x ->
        if x == v
        then Lit 1
        else Lit 0

    Add a b ->
        Add
            (differentiate v a)
            (differentiate v b)

    Sub a b ->
        Sub
            (differentiate v a)
            (differentiate v b)

    Mul a b ->
        Add
            (Mul (differentiate v a) b)
            (Mul a (differentiate v b))

    Div a b ->
        Div
            (Sub
                (Mul (differentiate v a) b)
                (Mul a (differentiate v b)))
            (Pow b (Lit 2))

    Pow e (Lit n) ->
        Mul
            (Lit n)
            (Mul
                (Pow e (Lit (n-1)))
                (differentiate v e))

    Pow _ _ ->
        error "general power differentiation not implemented"

    Sin e ->
        Mul
            (Cos e)
            (differentiate v e)

    Cos e ->
        Mul
            (Lit (-1))
            (Mul
                (Sin e)
                (differentiate v e))

    Factorial _ ->
        error "factorial differentiation not implemented"

    Deriv _ e ->
        differentiate v e

    Integral _ e ->
        e

-- ======================
-- Symbolic Integration
-- ======================

integrate :: String -> Expr -> Expr
integrate v = \case

    Lit n ->
        Mul (Lit n) (Var v)

    Var x ->
        if x == v
        then
            Div
                (Pow (Var v) (Lit 2))
                (Lit 2)
        else
            Mul (Var x) (Var v)

    Add a b ->
        Add
            (integrate v a)
            (integrate v b)

    Sub a b ->
        Sub
            (integrate v a)
            (integrate v b)

    Mul (Lit n) e ->
        Mul
            (Lit n)
            (integrate v e)

    Mul e (Lit n) ->
        Mul
            (Lit n)
            (integrate v e)

    Mul _ _ ->
        error "general product integration not implemented"

    Div _ _ ->
        error "quotient integration not implemented"

    Pow (Var x) (Lit n)
        | x == v ->
            Div
                (Pow (Var x) (Lit (n+1)))
                (Lit (n+1))

    Pow _ _ ->
        error "general power integration not implemented"

    Sin (Var x)
        | x == v ->
            Mul
                (Lit (-1))
                (Cos (Var x))

    Cos (Var x)
        | x == v ->
            Sin (Var x)

    Sin _ ->
        error "general sin integration not implemented"

    Cos _ ->
        error "general cos integration not implemented"

    Factorial _ ->
        error "factorial integration not implemented"

    Deriv _ e ->
        e

    Integral _ _ ->
        error "double integration not implemented"

-- ======================
-- Free Variables
-- ======================

freeVars :: Expr -> [String]
freeVars = uniq . go
  where

    go :: Expr -> [String]
    go = \case

        Lit _ ->
            []

        Var x ->
            [x]

        Add a b ->
            go a ++ go b

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
            uniq xs

        | otherwise =
            x : uniq xs

-- ======================
-- Pretty Printer
-- ======================

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
