module Grammar where

data Expr = Var !String
          | Abs { getName :: !String, getBody :: !Expr }
          | App !Expr !Expr
          | Def Int

instance Show Expr where
  show (Var var) = var
  show (Abs var expr) = "(\\" ++ var ++ "." ++ (show expr) ++ ")"
  show (App left right) = "(" ++ (show left) ++ " " ++ (show right) ++ ")"
  show _ = ""