{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise   = x : removeDuplicates xs

free_vars :: Expr -> [String]
free_vars expr = removeDuplicates $ freeVars expr []
  where
    freeVars (Variable x) env = [x | x `notElem` env]
    freeVars (Function x e) env = freeVars e (x:env)
    freeVars (Application e1 e2) env = freeVars e1 env ++ freeVars e2 env


-- TODO 1.2. reduce a redex

reduce :: Expr -> String -> Expr -> Expr
reduce (Variable e1) x e2 =
  if e1 == x
    then e2
    else Variable e1
reduce (Application e1 e2) x e3 =
  Application (reduce e1 x e3) (reduce e2 x e3)
reduce (Function y body) x e2
  | y == x = Function y body
  | y `elem` free_vars e2 = let newVarName = newVarNameWithPrefix y (Application (Function y body) e2)
                            in Function newVarName (reduce (substituteVar y newVarName body) x e2)
  | otherwise = Function y (reduce body x e2)

substituteVar :: String -> String -> Expr -> Expr
substituteVar old new expr =
  case expr of
    Variable x ->
      if x == old
        then Variable new
        else Variable x
    Function y body ->
      if y == old
        then Function y body
        else Function y (substituteVar old new body)
    Application e1 e2 ->
      Application (substituteVar old new e1) (substituteVar old new e2)

newVarNameWithPrefix :: String -> Expr -> String
newVarNameWithPrefix y e =
  let vars = map (\n -> y ++ show n) [1 ..]
      newVarNames = filter (\x -> x `notElem` free_vars e ++ [y]) vars
  in head newVarNames


-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Variable x) = Variable x
stepN (Function x e) = Function x (stepN e)
stepN (Application (Function x e1) e2) = reduce e1 x e2
stepN (Application e1 e2) =
  if hasRedex e1 then Application (stepN e1) e2
  else Application e1 (stepN e2)


hasRedex :: Expr -> Bool
hasRedex (Variable x) = False
hasRedex (Function x e) = hasRedex e
hasRedex (Application (Function x e1) e2) = True
hasRedex (Application e1 e2) = hasRedex e1 || hasRedex e2


-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN e =
  if hasRedex e then reduceN (stepN e)
  else e

reduceAllN :: Expr -> [Expr]
reduceAllN e =
  if hasRedex e then e : reduceAllN (stepN e)
  else [e]


-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation

stepA :: Expr -> Expr
stepA (Variable x) = Variable x
stepA (Function x e) = Function x (stepA e)
stepA (Application (Function x e1) e2) = if hasRedex e2 then Application (Function x e1) (stepA e2) else reduce e1 x e2
stepA (Application e1 e2) = if hasRedex e1 then   Application (stepA e1) e2 else Application e1 (stepA e2)

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA e = if hasRedex e then reduceA (stepA e) else e

reduceAllA :: Expr -> [Expr]
reduceAllA e = if hasRedex e then e : reduceAllA (stepA e) else [e]


-- TODO 3.1. make substitutions into a expression with Macros

evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros _ (Variable x) = Variable x
evalMacros context (Function x expr) = Function x (evalMacros context expr)
evalMacros context (Application expr1 expr2) = Application (evalMacros context expr1) (evalMacros context expr2)
evalMacros context (Macro m) =
  case lookup m context of
    Just expr -> evalMacros context expr
    Nothing -> Macro m

-- TODO 4.1. evaluate code sequence using given strategy

evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy code = evalHelper code []
  where
    evalHelper [] _ = []
    evalHelper (Evaluate expr : xs) context = strategy (evalMacros context expr) : evalHelper xs context
    evalHelper (Assign name expr : xs) context = evalHelper xs (updateContext name expr context)

    updateContext :: String -> Expr -> [(String, Expr)] -> [(String, Expr)]
    updateContext name expr [] = [(name, expr)]
    updateContext name expr ((n, e) : context) =
        if(name == n)  then  [(n, expr)] ++ context
        else  [(n, e)] ++  updateContext name expr context
