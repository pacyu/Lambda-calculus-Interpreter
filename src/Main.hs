module Main where
import Syntax
import Parser
import Definition
import Eval
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate, stripPrefix)
import System.IO (hFlush, stdout)

-- 解释器状态
data InterpState = InterpState
  { bindings :: [(String, Expr)]
  , counter  :: Int
  }

type Interp = StateT InterpState IO

-- 查找绑定
lookupBinding :: String -> Interp (Maybe Expr)
lookupBinding name = do
  binds <- gets bindings
  return $ lookup name binds

-- 添加新绑定
addBinding :: String -> Expr -> Interp ()
addBinding name expr = do
  modify $ \s -> s { bindings = (name, expr) : bindings s }

-- 生成唯一名称
freshBindingName :: String -> Interp String
freshBindingName base = do
  cnt <- gets counter
  modify $ \s -> s { counter = cnt + 1 }
  return $ base ++ show cnt

-- 扩展表达式（展开绑定）
expand :: Expr -> Interp Expr
expand expr = case expr of
  Var n mname -> return (Var n mname)   -- 保持原样返回
  Lam body    -> Lam <$> expand body
  App f x     -> App <$> expand f <*> expand x

-- 用环境展开表达式中的自由变量（递归替换）
expandFreeVars :: [(String, Expr)] -> Expr -> Expr
expandFreeVars env expr = case expr of
  Var n (Just name) ->
    case lookup name env of
      Just def -> expandFreeVars env def -- 替换并继续展开
      Nothing -> Var n (Just name) -- 未找到则保留
  Var n Nothing -> Var n Nothing -- 绑定变量（索引）不变
  Lam body -> Lam (expandFreeVars env body)
  App f x -> App (expandFreeVars env f) (expandFreeVars env x)

-- 处理输入命令
processCommand :: String -> Interp Bool
processCommand input
  | input `elem` [":q", ":quit", ":exit"] = return False
  | input == ":help" = do
    liftIO $ putStrLn $ unlines
      [ "可用命令:"
      , "  :help         显示此帮助"
      , "  :quit         退出解释器"
      , "  :env          显示当前环境"
      , "  expr          求值λ表达式"
      , ""
      , "λ表达式语法:"
      , "  x             变量"
      , "  \\x.M 或 λx.M λ抽象"
      , "  M N           函数应用"
      , "  (M)           括号分组"
      ]
    return True
  | input == ":env" = do
    binds <- gets bindings
    if null binds
      then liftIO $ putStrLn "环境为空"
      else do
        liftIO $ putStrLn "当前绑定:"
        mapM_ (\(name, expr) -> liftIO $ putStrLn $ "  " ++ name ++ " = " ++ show expr) binds
    return True
  | Just rest <- stripPrefix ":let " input = do
      case parseLet rest of
        Nothing -> liftIO $ putStrLn "格式错误，应为: :let 变量名 = 表达式"
        Just (name, exprStr) -> do
          case parse exprStr of
            Nothing -> liftIO $ putStrLn "表达式语法错误"
            Just expr -> do
              env <- gets bindings
              let expanded = expandFreeVars env expr
              let result = eval expanded
              addBinding name result
              liftIO $ putStrLn $ "已绑定: " ++ name ++ " = " ++ show result
      return True
  | otherwise = do
    case parse input of
      Nothing -> do
        liftIO $ putStrLn "语法错误"
        return True
      Just expr -> do
        env <- gets bindings                -- 获取当前环境
        let expanded = expandFreeVars env expr  -- 展开自由变量
        let result = eval expanded
        liftIO $ putStrLn $ "结果: " ++ show result
        -- 询问是否保存为绑定
        liftIO $ putStr "是否保存为绑定？(y/N): "
        liftIO $ hFlush stdout
        response <- liftIO getLine
        when (response `elem` ["y", "Y", "yes"]) $ do
          name <- freshBindingName "res"
          addBinding name result
          liftIO $ putStrLn $ "已保存为: " ++ name
        return True

-- REPL主循环
repl :: Interp ()
repl = do
  liftIO $ putStr "λ> "
  liftIO $ hFlush stdout
  input <- liftIO getLine
  continue <- processCommand input
  when continue repl

-- 初始化环境
initialState :: InterpState
initialState = InterpState
  { bindings =
      [ ("I", i)
      , ("K", k)
      , ("S", s)
      , ("TRUE", true)
      , ("FALSE", false)
      , ("SUCC", succChurch)
      , ("PRED", predChurch)
      , ("ZERO", zero)
      ]
  , counter = 0
  }

-- 主函数
main :: IO ()
main = do
  putStrLn "λ演算解释器 (输入 :help 获取帮助)"
  evalStateT repl initialState