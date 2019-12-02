module Grammar where

data Expr = Var String
          | Abs String Expr
          | App Expr Expr

instance Show Expr where
  show (Var var) = var
  show (Abs var expr) = "(\\" ++ var ++ "." ++ (show expr) ++ ")"
  show (App left right) = "(" ++ (show left) ++ " " ++ (show right) ++ ")"