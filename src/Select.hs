module Select
  ( Select(..)
  , SelectIdentifier
  , execute
  ) where


import System.IO
import Select.Relation
import Select.Expression
import Internal
import Data.Map as Map

import Control.Applicative ((<$>))

-- | Top level `Select` AST
newtype Select scope variable table
  = SELECT (Relation scope variable table)
  deriving (Eq,Show)

-- | Identifier for SELECT
type SelectIdentifier = Select String String FilePath
type Scope = Map.Map String String

flipScope :: Scope -> Scope
flipScope x = Map.fromList $ (\(x,y) -> (y,x)) <$> Map.toList x

-- make a map w/ the cols, a scope, and a Record to allow for variable lookup
mkEnv :: [String] -> Scope -> IRecord -> Map.Map String ICell
mkEnv cols scope (IRecord cells) =
  let envCols  = Map.fromList $ zipWith (,) cols cells -- -> (colname, ICell)
      -- envScope (colname, new col), (colname, ICell) -> (new col, ICell)
      envScope = Map.fromList $ (\x -> (Map.findWithDefault x x scope, Map.findWithDefault (ICNil "" )x envCols)) <$> Map.keys scope
  in Map.union envCols envScope

-- for And,Or
makeBinaryBool :: (Bool -> Bool -> Bool) -> (Expression String -> Expression String -> Expression String)
makeBinaryBool fn (LiteralBool b1) (LiteralBool b2) = LiteralBool $ fn b1 b2
makeBinaryBool fn x y = error $ (show x) ++ " and " ++  (show y) ++ " makeBinaryBool, wrong types"

-- for (==) to work with both integers and boolean internal values
toInt :: (Num a) =>  Bool -> a
toInt x = fromIntegral (if x then 1 else 0)

makeBinaryEq :: (Num a, Eq a) => (a -> a -> Bool) -> (Expression String -> Expression String -> Expression String)
makeBinaryEq fn (LiteralBool b1) (LiteralBool b2) = LiteralBool $ fn (toInt b1) (toInt b2)
makeBinaryEq fn (LiteralInt b1) (LiteralInt b2) = LiteralBool $ fn (fromIntegral b1) (fromIntegral b2)
makeBinaryEq fn x y = error $ (show x) ++ " and " ++  (show y) ++ " makeBinaryEq, wrong types"

-- for Gte,Gt,Lte,Le evaluation
makeBinaryNum ::(Double -> Double -> Bool) -> (Expression String -> Expression String -> Expression String)
makeBinaryNum fn (LiteralInt b1) (LiteralInt b2) =
  let b11 = fromIntegral b1
      b22 = fromIntegral b2
  in LiteralBool $ fn b11 b22
makeBinaryNum fn (LiteralReal b1) (LiteralReal b2) = LiteralBool $ fn b1 b2
{-
 eval Expression
 -}
evalExp :: Expression String -> [String] -> Scope -> IRecord -> Expression String
-- autoqute facility
evalExp (LiteralInt i)    c s r = LiteralInt i
evalExp (LiteralReal d )  c s r = LiteralReal d
evalExp (LiteralString str) c s r = LiteralString str
evalExp (LiteralBool b)   c s r = LiteralBool b
{- resolve a colum to a variable using either:
       1) the column name from the ITable
       2) the scope: which is an alias of a column name
-}
evalExp (Column var)      c s r =
  case ((mkEnv c s r) Map.! var)  of
      CInt x    -> LiteralInt $ fromIntegral x
      CFloat f  -> LiteralReal f
      CString s -> LiteralString s
      CBool b   -> LiteralBool b
      _         -> error "failed ot map internal variable to Expression Lit"
evalExp (Not (LiteralBool b ))  c s r = LiteralBool $ not b
evalExp (Not exp)  c s r = evalExp (evalExp exp c s r) c s r
evalExp (And val1 val2) c s r = makeBinaryBool ((&&)) (evalExp val1 c s r) (evalExp val2 c s r)
evalExp (Or val1 val2) c s r = makeBinaryBool ((||)) (evalExp val1 c s r) (evalExp val2 c s r)
evalExp (Equ val1 val2) c s r = makeBinaryEq ((==)) (evalExp val1 c s r) (evalExp val2 c s r)
evalExp (Neq val1 val2) c s r = evalExp (Not (And val1 val2)) c s r
evalExp (Gt val1 val2) c s r  = makeBinaryNum ((>)) (evalExp val1 c s r) (evalExp val2 c s r)
evalExp (Gte val1 val2) c s r = makeBinaryNum (>=) (evalExp val1 c s r) (evalExp val2 c s r)
evalExp (Lt val1 val2) c s r = makeBinaryNum ((<)) (evalExp val1 c s r) (evalExp val2 c s r)
evalExp (Lte val1 val2) c s r = makeBinaryNum ((<=)) (evalExp val1 c s r) (evalExp val2 c s r)
evalExp x c s r = error $ show x ++ " invalid expression"
{-
 WHERE helper functions
-}
isBoolTrue :: Expression a   -> Bool
isBoolTrue (LiteralBool True) = True
isBoolTrue _                  = False

filterRows :: ITable -> Expression String -> Scope -> ITable
filterRows (ITable name cols body) exp env =
  ITable name cols $ Prelude.filter (\x -> isBoolTrue $ evalExp exp cols env x)  body

{-
 INNER_JOIN helper functions
 -}
innerJoin :: ITable -> ITable -> Expression String -> ITable
innerJoin t1@(ITable n1 c1 b1) t2@(ITable n2 c2 b2) expr =
  let name = n1 ++ "." ++ n2
      col  = c1 ++ c2
      body = ITable name col $ concat $ (runCompare t1 c2 expr) <$> b2
  in body

evalExpJoin :: IRecord -> [String] -> IRecord -> [String] -> Expression String -> Expression String
evalExpJoin r1 c1 r2 c2 exp = evalExp exp (c1 ++ c2) Map.empty $ joinRecords r1 r2

joinRecords :: IRecord -> IRecord -> IRecord
joinRecords (IRecord cells1) (IRecord cells2) = IRecord $ cells1 ++ cells2

runCompare :: ITable -> [String] -> Expression String -> IRecord -> [IRecord]
runCompare t@(ITable _ tCols body) cols expr rec@(IRecord cells) =
 Prelude.foldr (\x acc -> if isBoolTrue (evalExpJoin x tCols rec cols expr) then (joinRecords x rec):acc else acc) [] body

namedToTuple :: Named a (Expression a) -> (a,a)
namedToTuple (AS (Column x) y) = (x,y)
namedToTuple x = error "Invalid AS data type"

fromBindingsToMap :: [Named String (Expression String)] -> Scope
fromBindingsToMap x = Map.fromList $ namedToTuple <$> x
{-
 eval relation
 -}
evalRelation :: RelationIdentifier -> Scope -> IO ITable

-- TABLE
evalRelation (TABLE inFile) _ = readTable inFile

-- INNER JOIN
evalRelation (INNER_JOIN_ON (AS tbl1 name1) (AS tbl2 name2) expr) scope = do
  table1 <-  evalRelation tbl1 scope
  table2 <-  evalRelation tbl2 scope
  return $ innerJoin (prependNameToCols $ setName table2 name2) (prependNameToCols $ setName table1 name1) $ (\(x,y) -> x ++ "." ++ y) <$> expr

-- FROM
evalRelation (FROM binds tbl)  scope = do
  table <- evalRelation tbl $ Map.union scope $ fromBindingsToMap binds
  return $ renameCols (fromBindingsToMap binds) $ selectCols ((\(x,_) -> x) <$> namedToTuple <$> binds) table

-- WHERE
evalRelation (WHERE tbl expr) scope = do
  table <- evalRelation tbl scope
  return $ filterRows table expr scope

-- UNION
evalRelation (UNION tbl1 tbl2) scope = do
  table1 <- evalRelation tbl1 scope
  table2 <- evalRelation tbl2 scope
  return $ unionTable table1 table2


-- eval w/ a default, empty scope
evalR :: RelationIdentifier -> IO ITable
evalR ri = evalRelation ri Map.empty
{-
 READ/WRITE ITables
-}
readTable :: FilePath -> IO ITable
readTable file =
  withFile file ReadMode $ \handle -> do
    str <- hGetContents handle
    seq (length str) (return ()) -- force IO
    return $ getITable str

writeTable :: FilePath -> ITable -> IO ()
writeTable file tbl = writeFile file (showITable tbl)
{-
 top level function for testing
-}
execute
  :: SelectIdentifier  -- Select String String FilePath
  -> FilePath -- output
  -> IO ()
execute (SELECT relI) outFile = evalR relI >>= writeTable outFile
