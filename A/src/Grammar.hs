module Grammar where

data Expression = ApplyingLambda { getApplying :: Expression, getLambda :: Expression }
                | Lambda { getVariableName :: String, getExpression :: Expression }
                | Applying { getApplying :: Expression, getAtom :: Expression }
                | Variable { getVariableName :: String }

instance Show Expression where
  show (ApplyingLambda applying lambda) = "(" ++ (show applying) ++ " " ++ (show lambda) ++ ")"
  show (Lambda variableName expression) = "(\\" ++ variableName ++ "." ++ (show expression) ++ ")"
  show (Applying applying atom) = "(" ++ (show applying) ++ " " ++ (show atom) ++ ")"
  show (Variable variableName) = variableName