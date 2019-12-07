module Grammar where

data Expr = Var String | Abs String Expr | App Expr Expr deriving (Eq, Ord)

data Type = NoType | SimpleType { getIndex :: Int } | ComplexType { getLeft :: Type, getRight :: Type } deriving (Eq, Ord)

instance Show Expr where
    show (Var var) = var
    show (Abs var expr) = "(\\" ++ var ++ "." ++ (show expr) ++ ")"
    show (App left right) = "(" ++ (show left) ++ " " ++ (show right) ++ ")"
    
instance Show Type where
    show (SimpleType t) = "t" ++ (show t)
    show (ComplexType a b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"