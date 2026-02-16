import System.IO
import System.Environment
import Data.List (nub)
import qualified Data.Map.Strict as Map

type VarId = String

data Prop = Const Bool
          | Var VarId
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Iff Prop Prop
          deriving (Eq, Read, Show)

type VarAsgn = Map.Map VarId Bool

-- returns a list of distinct variable names in a propositional formula.
findVarIds :: Prop -> [VarId]
findVarIds (Const _)   = []
findVarIds (Var x)     = [x]
findVarIds (Not p)     = findVarIds p
findVarIds (And p q)   = nub (findVarIds p ++ findVarIds q)
findVarIds (Or p q)    = nub (findVarIds p ++ findVarIds q)
findVarIds (Imply p q) = nub (findVarIds p ++ findVarIds q)
findVarIds (Iff p q)   = nub (findVarIds p ++ findVarIds q)

-- that returns all possible variable assignments given a list of variable names. Hint: given n variable names, there are 2n variable assignments in total.
genVarAsgns :: [VarId] -> [VarAsgn]
genVarAsgns [] = [Map.empty]
genVarAsgns (x:xs) = 
    let subAsgns = genVarAsgns xs
    in [Map.insert x True asgn | asgn <- subAsgns] ++ 
       [Map.insert x False asgn | asgn <- subAsgns]

-- computes the truth value of a formula given a variable assignment.
eval :: Prop -> VarAsgn -> Bool
eval (Const b) _   = b
eval (Var x) asgn  = Map.findWithDefault False x asgn
eval (Not p) asgn  = not (eval p asgn)
eval (And p q) asgn = (eval p asgn) && (eval q asgn)
eval (Or p q) asgn  = (eval p asgn) || (eval q asgn)
eval (Imply p q) asgn = not (eval p asgn) || (eval q asgn)
eval (Iff p q) asgn   = eval p asgn == eval q asgn

-- returns whether a formula is satisfiable or not.
sat :: Prop -> Bool
sat p = any (eval p) (genVarAsgns (findVarIds p))

-- reads a formula string (e.g., Iff (Var "x1") (Var "x2")) to its corresponding value of type Prop.
readFormula :: String -> Prop
readFormula = read

-- takes a formula string as input and produces a string as input indicating whether the formula is satisfiable. Specifically,
-- • If the formula is satisfiable, output string SAT.
-- • If the formula is unsatisfiable, output string UNSAT.
checkFormula :: String -> String
checkFormula s = if sat (readFormula s) then "SAT" else "UNSAT"

-- handle IO and put everything together.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            fileContents <- readFile fileName
            let fileLines = lines fileContents
                results = map checkFormula fileLines
            mapM_ putStrLn results