module Definition where
import Syntax

-- 定义常用组合子
i :: Expr  -- I = λx.x
i = Lam (Var 0 Nothing)

k :: Expr  -- K = λx.λy.x
k = Lam (Lam (Var 1 Nothing))

s :: Expr  -- S = λx.λy.λz.x z (y z)
s = Lam (Lam (Lam (App (App (Var 2 Nothing) (Var 0 Nothing))
                        (App (Var 1 Nothing) (Var 0 Nothing)))))

zero :: Expr -- λf.λx.x
zero = Lam (Lam (Var 0 Nothing))

true :: Expr
true = Lam (Lam (Var 1 Nothing))

false :: Expr
false = Lam (Lam (Var 0 Nothing))

church :: Int -> Expr
church 0 = Lam (Lam (Var 0 Nothing))
church n = Lam (Lam (App (Var 1 Nothing) (unfold n)))
  where
    unfold 1 = App (Var 1 Nothing) (Var 0 Nothing)
    unfold m = App (Var 1 Nothing) (unfold (m-1))

succChurch :: Expr
succChurch = Lam (Lam (Lam (App (Var 1 Nothing) (App (App (Var 2 Nothing) (Var 1 Nothing)) (Var 0 Nothing)))))

predChurch :: Expr -- λn.λf.λx. n (λg.λh.h (g f)) (λu.x) (λu.u)
predChurch = 
  Lam (                    -- λn (index 0)
    Lam (                  -- λf (index 0, n 变为 1)
      Lam (                -- λx (index 0, f 变为 1, n 变为 2)
        App 
          (App 
            (App (Var 2 Nothing) -- n
              (Lam (             -- λg (index 0)
                Lam (           -- λh (index 0, g 变为 1, f 变为 2)
                  App (Var 0 Nothing) -- h
                    (App (Var 1 Nothing) (Var 3 Nothing)) -- (g f)
                )
              ))
            )
            (Lam (Var 1 Nothing)) -- λu. x (x 在外层是 index 0，进一层变为 1)
          )
          (Lam (Var 0 Nothing))   -- λu. u
      )
    )
  )

  