module Syntax where

data Expr
  = Var Int (Maybe String)   -- 索引 和 可选的原始名称
  | Lam Expr
  | App Expr Expr
  deriving (Eq)


instance Show Expr where
  show :: Expr -> String
  show = showExpr []

-- 辅助函数：显示表达式（处理自由变量）
showExpr :: [String] -> Expr -> String
showExpr ctx (Var n mname) = case mname of
  Just name -> name
  Nothing | n < length ctx -> ctx !! n
          | otherwise      -> "#" ++ show n
showExpr ctx (Lam body) =
  let var = freshName "x" ctx
  in "λ" ++ var ++ ". " ++ showExpr (var:ctx) body
showExpr ctx (App f x) =
  "(" ++ showExpr ctx f ++ " " ++ showExpr ctx x ++ ")"

-- 生成新的变量名（避免冲突）
freshName :: String -> [String] -> String
freshName base ctx
  | base `elem` ctx = freshName (base ++ "'") ctx
  | otherwise       = base