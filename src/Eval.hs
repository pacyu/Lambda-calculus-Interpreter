module Eval where
import Syntax


shift :: Int -> Int -> Expr -> Expr
shift d c expr = case expr of
  Var n mname
    | n < c     -> Var n mname
    | otherwise -> Var (n + d) mname
  Lam body      -> Lam (shift d (c+1) body)
  App f x       -> App (shift d c f) (shift d c x)

subst :: Int -> Expr -> Expr -> Expr
subst idx with expr = case expr of
  Var n mname
    | n == idx   -> with
    | otherwise  -> Var n mname
  Lam body      -> Lam (subst (idx+1) (shift 1 0 with) body)
  App f x       -> App (subst idx with f) (subst idx with x)

-- β归约一次（惰性求值：只归约最外层的可归约式）
betaReduce :: Expr -> Maybe Expr
betaReduce expr = case expr of
  App (Lam body) arg ->
    let arg' = shift 1 0 arg
        substituted = subst 0 arg' body
        result = shift (-1) 0 substituted
    in Just result
  App f x -> case betaReduce f of
    Just f' -> Just (App f' x)
    Nothing -> case betaReduce x of
      Just x' -> Just (App f x')
      Nothing -> Nothing
  Lam body -> case betaReduce body of
    Just body' -> Just (Lam body')
    Nothing -> Nothing
  _ -> Nothing

-- 完全归约到正规形式（惰性求值策略）
eval :: Expr -> Expr
eval expr = maybe expr eval (betaReduce expr)