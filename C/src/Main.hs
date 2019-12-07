module Main where
import Lexer
import Parser
import Grammar
import Data.Maybe
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict
import System.Exit (exitSuccess)

showDepth 0 = putStr ""
showDepth d = do
    putStr "*   "
    showDepth (d - 1)

showContext [] = putStr ""
showContext [(a,b)] = do 
    putStr $ show a
    putStr " : "
    putStr $ show b 
    putStr " "
showContext ((a,b):xs) = do 
    putStr $ show a
    putStr " : "
    putStr $ show b 
    putStr ", "
    showContext xs

showResult [] = putStr ""
showResult ((depth, context, a, b):xs) = do
    showDepth depth
    showContext $ reverse context
    putStr "|- "
    putStr $ show a
    putStr " : "
    putStr $ show b 
    case a of
        (Var v) -> putStrLn " [rule #1]"
        (Abs v e) -> putStrLn " [rule #3]"
        (App l r) -> putStrLn " [rule #2]"
    showResult xs
    
updateFreeVars :: [(String, Type)] -> IntMap.IntMap Type -> [(String, Type)]
updateFreeVars [] solvedSystem = []
updateFreeVars ((var, herType):xs) solvedSystem = (var, fromMaybe herType (IntMap.lookup (getIndex herType) solvedSystem)) : (updateFreeVars xs solvedSystem)

getResult :: IntMap.IntMap Type -> Expr -> Map.Map String Type -> [(Expr, Type)] -> Int -> State Int [(Int, [(Expr, Type)], Expr, Type)]
getResult solvedSystem expression types context depth = do
    case expression of
        (Var var) -> do
            let oldType = fromJust $ Map.lookup var types
            return [(depth, context, expression, oldType)]
        (Abs var expr) -> do
            counter <- get
            let varType = fromMaybe (SimpleType counter) (IntMap.lookup counter solvedSystem)
            put (counter + 1)
            exprData <- getResult solvedSystem expr (Map.insert var varType types) ((Var var, varType) : context) (depth + 1)
            let exprType = case (head exprData) of
                    (_, _, _, lol) -> lol
            return $ (depth, context, expression, ComplexType varType exprType) : exprData
        (App left right) -> do
            counter <- get
            leftData <- getResult solvedSystem left types context (depth + 1)
            let leftType = case (head leftData) of
                    (_, _, _, lol) -> lol
            rightData <- getResult solvedSystem right types context (depth + 1)
            counter <- get
            let newType = case leftType of
                    (SimpleType t) -> fromMaybe (SimpleType counter) (IntMap.lookup counter solvedSystem)
                    (ComplexType a b) -> b
            put (counter + 1)
            return $ (depth, context, expression, newType) : (leftData ++ rightData) 

makeMap [] = IntMap.empty
makeMap (x:xs) = IntMap.insert (getIndex $ fst x) (snd x) (makeMap xs)

contains t t1@(SimpleType _) = if t == t1 then True else False
contains t (ComplexType left right) = (contains t left) || (contains t right)

isIncompatibleForm [] = False
isIncompatibleForm ((SimpleType t, ComplexType t1 t2):xs) = if contains (SimpleType t) (ComplexType t1 t2) then True else isIncompatibleForm xs 
isIncompatibleForm (x:xs) = isIncompatibleForm xs

add p1 p2 0 (x:xs) = p1 : p2 : xs
add p1 p2 ind (x:xs) = x : add p1 p2 (ind - 1) xs

subst a b x@(SimpleType t) = if x == a then b else x
subst a b (ComplexType type1 type2) = ComplexType (subst a b type1) (subst a b type2)

substitute a b ind [] = []
substitute a b 0 (x:xs) = x : (substitute a b (-1) xs)
substitute a b ind (x:xs) = ((subst a b (fst x)), (subst a b (snd x))) : (substitute a b (ind - 1) xs) 

remove 0 (x:xs) = xs 
remove ind (x:xs) = x : (remove (ind - 1) xs)

swap 0 (x:xs) = (snd x, fst x) : xs 
swap ind (x:xs) = x : (swap (ind - 1) xs)  

one :: State (Int, [(Type, Type)]) ()
one = do
    (ind, system) <- get
    let x = system !! ind
    let newSystem = case x of
            (ComplexType a b, SimpleType t) -> swap ind system
            (SimpleType t1, SimpleType t2) -> if t1 == t2 then remove ind system else substitute (SimpleType t1) (SimpleType t2) ind system
            (ComplexType a1 b1, ComplexType a2 b2) -> add (a1, a2) (b1, b2) ind system
            (SimpleType t, (ComplexType a b)) -> substitute (SimpleType t) (ComplexType a b) ind system
    put (ind, newSystem)

unification :: State (Int, [(Type, Type)]) (Maybe (IntMap.IntMap Type)) 
unification = do
    (ind, oldSystem) <- get
    if isIncompatibleForm oldSystem then
        return Nothing
    else
        if ind < length oldSystem then do
            one
            (_, newSystem) <- get
            if (oldSystem /= newSystem) then do
                put (0, newSystem)
                unification
            else
                if ind /= length newSystem then do
                    put (ind + 1, newSystem)
                    unification
                else
                    return $ Just $ makeMap newSystem
        else
            return $ Just $ makeMap oldSystem

getFreeVars :: Set.Set String -> Expr -> State Int (Map.Map String Type)
getFreeVars notFree (Var var) = do
    if Set.member var notFree then
        return Map.empty
    else do
        counter <- get 
        let newType = SimpleType counter
        put (counter + 1)
        return $ Map.singleton var newType
getFreeVars notFree (Abs var expr) = do
    mapExpr <- getFreeVars (Set.insert var notFree) expr
    return mapExpr
getFreeVars notFree (App left right) = do
    mapLExpr <- getFreeVars notFree left
    mapRExpr <- getFreeVars notFree right
    return $ Map.union mapLExpr mapRExpr

getSystem :: Expr -> Map.Map String Type -> State Int ([(Type, Type)], Type)   
getSystem expression types = do
    case expression of
        (Var var) -> do
            let oldType = fromJust $ Map.lookup var types
            return ([], oldType)
        (Abs var expr) -> do
            counter <- get
            let varType = SimpleType counter 
            put (counter + 1)
            (exprSystem, exprType) <- getSystem expr (Map.insert var varType types)
            return (exprSystem, ComplexType varType exprType)
        (App left right) -> do
            (leftSystem, leftType) <- getSystem left types
            (rightSystem, rightType) <- getSystem right types
            counter <- get
            let newType = SimpleType counter
            let newEquation = (leftType, ComplexType rightType newType)  
            put (counter + 1)
            return (reverse $ newEquation : (rightSystem ++ leftSystem), newType)

preparing :: Expr -> State Int ([(Type, Type)], Map.Map String Type)
preparing expr = do
    freeVarsTypes <- getFreeVars Set.empty expr
    (system, _) <- getSystem expr freeVarsTypes
    return (system, freeVarsTypes)

getType :: Expr -> IO ()
getType expr = do
    let (system, freeVarsTypes) = evalState (preparing expr) 0
    let maybeSolvedSystem = evalState unification (0, system)
    if isNothing maybeSolvedSystem then do
        putStr "Expression has no type"
        exitSuccess
    else do
        let solvedSystem = fromJust maybeSolvedSystem
        let updatedVars = updateFreeVars (Map.toList freeVarsTypes) solvedSystem
        let res = evalState (getResult solvedSystem expr (Map.fromList updatedVars) (reverse (map (\(fi, se) -> (Var fi, se)) updatedVars)) 0) (Map.size freeVarsTypes)
        showResult res

main :: IO ()
main = do
    input <- getContents
    let expr = parser $ alexScanTokens input
    getType expr