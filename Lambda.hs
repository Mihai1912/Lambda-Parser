module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars e =
    case e of
        (Variable x) -> [x]
        (Function x e) -> delete x (free_vars e)
        (Application e1 e2) -> union (free_vars e1) (free_vars e2)

bound_vars :: Expr -> [String]
bound_vars e =
    case e of
        (Variable x) -> []
        (Function x e) -> union [x] (bound_vars e)
        (Application e1 e2) -> union (bound_vars e1) (bound_vars e2)


-- TODO 1.2. reduce a redex
rename_vars :: Expr -> String -> Expr
rename_vars e x =
    case e of
        (Variable y) -> if x == y then Variable "a" else Variable y
        (Function y e) -> if x == y then (Function "a" (rename_vars e x)) else e
        (Application e1 e2) -> (Application (rename_vars e1 x) (rename_vars e2 x))
    

reduce :: Expr -> String -> Expr -> Expr
reduce e1 x e2 = 
    case e1 of
        Variable y -> if x == y then e2 else e1
        Function y e -> if x == y then e1 else 
            if elem y $ free_vars e2 then Function "a" $ reduce (rename_vars e y) x e2 else Function y (reduce e x e2)
        Application e e' -> Application (reduce e x e2) (reduce e' x e2)

        

aux :: Expr -> String -> Expr -> String -> Expr -> Expr
aux e1 x e2 y e = 
    if (x == y) then (Function y e) else reduce (rename_vars e1 x) x e2
    
-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN ex = 
    case ex of
        Application (Function x e1) e2 -> reduce e1 x e2
        Application e1 e2 -> 
            if e1 == stepN e1
                then Application e1 (stepN e2)
                else Application (stepN e1) e2
        Function x e -> 
            if e == stepN e
                then Function x e
                else Function x (stepN e)
        Variable x -> Variable x
        _ -> ex
        

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN e = 
    if e == stepN e
        then e
        else reduceN (stepN e)

reduceAllN :: Expr -> [Expr]
reduceAllN e = 
    if e == stepN e
        then [e]
        else e : reduceAllN (stepN e)

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA ex = 
    case ex of
        Application (Function x e1) e2 -> 
            if isValue e2
                then substitute e2 x e1
                else (Application (Function x e1) (stepA e2))
        Application e1 e2 ->
            if isValue e1
                then (Application e1 (stepA e2))
                else (Application (stepA e1) e2)
        Function x e -> (Function x (stepA e))
        _ -> ex

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA e = 
    if e == stepA e
        then e
        else reduceA (stepA e)

reduceAllA :: Expr -> [Expr]
reduceAllA e =
    if isValue e
        then [e]
        else e : reduceAllA (stepA e)

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros pairs e = 
    case e of 
        Macro str -> 
            case lookup str pairs of
                Just x -> evalMacros pairs x
                Nothing -> e
        Variable x -> Variable x
        Function x e -> Function x (evalMacros pairs e)
        Application e1 e2 -> Application (evalMacros pairs e1) (evalMacros pairs e2)

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode s code = aux_evalCode s code []

aux_evalCode :: (Expr -> Expr) -> [Code] -> [(String , Expr)] -> [Expr]
aux_evalCode s code d = 
    case code of
        [] -> []
        (x:xs) -> case x of
            Evaluate e -> (s $ evalMacros d e) : (aux_evalCode s xs d)
            Assign str e -> aux_evalCode s xs $ (str , e) : d

isValue :: Expr -> Bool
isValue (Variable _) = True
isValue (Function _ _) = True
isValue _ = False

substitute :: Expr -> String -> Expr -> Expr
substitute e1 x e2 = 
    case e2 of
        (Variable y) -> if x == y then e1 else Variable y
        (Function y e) -> if x == y then Function y e else (Function y (substitute e1 x e))
        (Application e e') -> (Application (substitute e1 x e) (substitute e1 x e'))