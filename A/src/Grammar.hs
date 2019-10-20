module Grammar where

data Atom = Var String
          | Atom Expression
          deriving Show

data Expression = Lambda [Atom] String Expression
                | Space [Atom]
                deriving Show