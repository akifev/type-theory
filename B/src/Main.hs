module Main where
import Data.Maybe
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict
import Lexer
import Parser
import Grammar
import System.Exit (exitSuccess)

makePlane :: Expr -> State (Int, (IntMap.IntMap Expr), Int) Expr
makePlane expression =
    case expression of
        (Var var) ->
            return $ Var var
        (Abs var expr) -> do
            newExpr <- makePlane expr
            return $ Abs var newExpr
        (App left right) -> do
            newLExpr <- makePlane left
            newRExpr <- makePlane right
            return $ App newLExpr newRExpr
        (Def key) -> do
            (_, map, _) <- get
            makePlane (fromJust $ IntMap.lookup key map)

freeVars setNotFree (Var var) =
    if Set.notMember var setNotFree then
        Set.singleton var
    else
        Set.empty
freeVars setNotFree (Abs var expr) = let newSetNotFree = Set.insert var setNotFree in freeVars newSetNotFree expr
freeVars setNotFree (App left right) = Set.union (freeVars setNotFree left) (freeVars setNotFree right)
freeVars setNotFree (Def key) = error "478"

renameAndSubstitute :: String -> Expr -> Bool -> (Set.Set String) -> (Map.Map String String) -> Expr -> State (Int, (IntMap.IntMap Expr), Int) Expr
renameAndSubstitute name term flag set names expression =
    case expression of
        (Var var) ->
            if var == name && flag then
                return term
            else
                if Map.member var names then do
                    let actualName = fromJust $ Map.lookup var names
                    return $ Var actualName
                else
                    return $ Var var
        (Abs var expr) ->
            if var == name then do
                newExpr <- renameAndSubstitute name term False set names expr
                return $ Abs var newExpr
            else
                if Set.member var set then do
                    (step, map, id) <- get
                    let newName = var ++ (show $ id + 1)
                    let newNamesMap = Map.insert var newName names
                    put (step, map, id + 1)
                    renamedExpr <- renameAndSubstitute name term flag set newNamesMap expr
                    return $ Abs newName renamedExpr
                else do
                    renamedExpr <- renameAndSubstitute name term flag set names expr
                    return $ Abs var renamedExpr
        (App left right) -> do
            newLExpr <- renameAndSubstitute name term flag set names left
            newRExpr <- renameAndSubstitute name term flag set names right
            return $ App newLExpr newRExpr
        (Def key) ->
            return $ error "578" -- unexpected case

reduce :: Expr -> State (Int, (IntMap.IntMap Expr), Int) (Expr, Bool)
reduce application@(App (left@(Def key)) right) = do
    (abstraction, isAbs) <- isAbstraction left
    if isAbs then
        reduce (App abstraction right)
    else
        reduceApplication application
reduce redex@(App abstraction@(Abs var expr) right) = do
    planeA <- makePlane expr
    planeB <- makePlane right
    (step, _, _) <- get
    substituted <- renameAndSubstitute var (Def step) True (freeVars Set.empty planeB) Map.empty planeA
    (_, map, id) <- get
    let newMap = IntMap.insert step right map
    put (step, newMap, id)
    return (substituted, True)
reduce (Var var) = return (Var var, False)
reduce (Abs var expr) = do
    (newExpr, hasRedex) <- reduce expr
    return (Abs var newExpr, hasRedex)
reduce application@(App left right) = reduceApplication application
reduce (Def key) = do
    (_, map, _) <- get
    let actualExpr = fromJust $ IntMap.lookup key map
    (newExpr, hasRedex) <- reduce actualExpr
    (step, newMap, id) <- get
    let updatedMap = IntMap.insert key newExpr newMap
    put (step, updatedMap, id)
    return (Def key, hasRedex)

isAbstraction :: Expr -> State (Int, (IntMap.IntMap Expr), Int) (Expr, Bool)
isAbstraction (Def key) = do
    (_, map, _) <- get
    let hiddenExpr = fromJust $ IntMap.lookup key map
    case hiddenExpr of
        (Abs name body) -> return (hiddenExpr, True)
        (Def k)         -> isAbstraction hiddenExpr
        _               -> return (Var "Not Abs", False)

reduceApplication :: Expr -> State (Int, (IntMap.IntMap Expr), Int) (Expr, Bool)
reduceApplication (App left right) = do
    (newLExpr, hasLRedex) <- reduce left
    if hasLRedex then
        return (App newLExpr right, True)
    else do
        (newRExpr, hasRRedex) <- reduce right
        return (App newLExpr newRExpr, hasRRedex)

loop :: Int -> Int -> Expr -> [String] -> State (Int, (IntMap.IntMap Expr), Int) [String]
loop m k expr result = do
    (newExpr, hasRedex) <- reduce expr
    (step, newMap, newId) <- get
    if step > m || hasRedex == False then
        if hasRedex == False && mod (step - 1) k /= 0 then do
            expression <- makePlane newExpr
            return $ (show expression) : result
        else
            return result
    else
        if mod step k == 0 then do
            expression <- makePlane newExpr
            let newResult = (show expression) : result
            put (step + 1, newMap, newId)
            loop m k newExpr newResult
        else do
            put (step + 1, newMap, newId)
            loop m k newExpr result

printResult :: [String] -> IO ()
printResult [] = putStr ""
printResult (x:xs) = do
    putStrLn x
    printResult xs

main :: IO ()
main = do
    numbers <- getLine
    let m = (read (takeWhile (/= ' ') numbers) :: Int)
    let k = (read (drop 1 (dropWhile (/= ' ') numbers)) :: Int)
    input <- getContents
    let expr = parser $ alexScanTokens input
    putStrLn $ show expr
    let output = reverse $ evalState (loop m k expr []) (1, IntMap.empty, 0)
    printResult output