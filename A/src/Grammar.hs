module Grammar where

data Atom = Var String
          | Atom Expression

data Space = Space Space Atom
           | SpaceAtom Atom

data Expression = Lambda (Maybe Space) String Expression
                | Expression Space

instance Show Space where
    show (Space space atom) = "(" ++ (show space) ++ " " ++ (show atom) ++ ")"
    show (SpaceAtom atom) = show atom

instance Show Expression where
    show (Lambda (Nothing) var expr) = "(\\" ++ var ++ "." ++ (show expr) ++ ")"
    show (Lambda (Just space) var expr) = "(" ++ (show space) ++ " (\\" ++ var ++ "." ++ (show expr) ++ "))"
    show (Expression space) = show space

instance Show Atom where
    show (Var a) = a
    show (Atom e) = (show e)
