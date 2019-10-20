module Grammar where

data Atom = Var String
          | Atom Expression

data Space = Space (Maybe Space) Atom

data Lambda = Lambda String Expression

data Expression = Expression (Maybe Space) (Maybe Lambda)

instance Show Atom where
    show (Var a)  = a
    show (Atom e) = show e

instance Show Space where
    show (Space (Nothing)    atom) = show atom
    show (Space (Just space) atom) = "(" ++ (show space) ++ " " ++ (show atom) ++ ")"

instance Show Lambda where
    show (Lambda var expr) = "(\\" ++ var ++ "." ++ (show expr) ++ ")"

instance Show Expression where
    show (Expression (Nothing)    (Just lambda)) = show lambda
    show (Expression (Just space) (Nothing))     = show space
    show (Expression (Just space) (Just lambda)) = "(" ++ (show space) ++ " " ++ (show lambda) ++ ")"