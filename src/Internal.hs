module Internal where

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L
import Control.Monad (void)
import Control.Applicative ((<$>))
import Data.Map as Map
import Data.List.Split (splitOn)
import Data.List (intercalate,elemIndices,concat)

import Control.Applicative

{-
 Parsing CSV files into ITABLE
 -}

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "#"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

iString :: Parser ICell
iString = CString <$> some letterChar

integer :: Parser ICell
integer = CInt <$> lexeme L.integer

float :: Parser ICell
float = CFloat <$> lexeme L.float

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

bool :: Parser ICell
bool = (reserved "True" >> return (CBool True))
  <|> (reserved "False" >> return (CBool False))


parseICell =
  bool
  <|> try float
  <|> integer
  <|> iString

pRec :: String -> ICell
pRec cell = case parse parseICell "<stdin>" cell of
                 Left err -> ICNil $ show err
                 Right val -> val

pLine ::  String -> IRecord
pLine line = case parse parseRec "<stdin>" line of
                 Left err -> IRNil $ show err
                 Right val -> val

parseRec :: Parser IRecord
parseRec = IRecord . concat <$> (many parseICell `sepBy` char ',')

{-
 ITable  - contains a name :: String, a list of columns :: [String]
             then a list of of records
 IRecord - the record, contains just a list of ICells
 ICell   - the actual values stored in an ITABLE
 -}

data ICell = ICNil String
    | CInt Integer
    | CFloat Double
    | CString String
    | CBool Bool deriving (Eq,Show)

data IRecord = IRecord [ICell]
               | IRNil String deriving (Eq,Show)
data ITable = ITable { name     :: String
                     , colNames :: [String]
                     , body     :: [IRecord] }
              | ITNil String deriving (Eq,Show)


{-
 show for ICell, IRecord, ITable
 -}

showCell :: ICell -> String
showCell cell =
  case cell of
      ICNil  _   -> ""
      CInt x    -> show x
      CFloat x  -> show x
      CString x -> x
      CBool x   -> show x

showRecord :: IRecord -> String
showRecord (IRecord list) = intercalate "," $ showCell <$>  list
showRecord (IRNil _) = "#skipped"

showITable :: ITable -> String
showITable (ITNil _) = "#epicFail"
showITable (ITable name cols body) =
  let header = intercalate "," cols
  in intercalate "\n" $ (:) header $ showRecord <$> body

{-
 ICell: Helper functions
 -}

cellTypeMatch :: ICell -> ICell -> Bool
cellTypeMatch (ICNil _) (ICNil _)     = True
cellTypeMatch (CInt _) (CInt _)       = True
cellTypeMatch (CFloat _) (CFloat _)   = True
cellTypeMatch (CString _) (CString _) = True
cellTypeMatch (CBool _) (CBool _)     = True
cellTypeMatch _ _                     = False

{-
 IRecord : Helper functions
 -}

recTypeMatch :: IRecord-> IRecord-> Bool
recTypeMatch (IRNil _) (IRNil _) = True
recTypeMatch (IRecord c1) (IRecord c2) = (length c1 == length c2) && (foldl1 (==) $ zipWith (cellTypeMatch) c1 c2)
recTypeMatch _ _ = False

selR :: IRecord -> Int -> [ICell]
selR (IRecord arr) i = if (i < (length arr)) then [arr !! i] else []

selRmany :: [Int] -> IRecord -> IRecord
selRmany inds arr =
  IRecord $ concat $ selR arr <$> inds

rLength :: IRecord -> Int
rLength (IRecord rec) = length rec

{-
 ITable: Helper functions
 -}


validateTable :: ITable -> Bool
validateTable (ITable name cols body@(x:xs)) =
  let test1 = foldr1 (&&) $ zipWith (==) (rLength <$> body) $ repeat $ length cols
      test2 = foldr1 (&&) $ (recTypeMatch x) <$> xs
  in  test1 && test2


getITable :: String -> ITable
getITable str =
  case splitOn "\n" str of
      (names:rest) -> ITable "default" (splitOn "," names) $ pLine <$> rest
      _            -> ITNil "number of lines"


selectCols :: [String] -> ITable -> ITable
selectCols scols (ITable name cols body) =
  let selInd = concat $ (flip elemIndices cols) <$> scols
  in  ITable name ((\x -> cols !! x) <$> selInd) $ (selRmany selInd) <$> body


renameCols :: Map.Map String String -> ITable -> ITable
renameCols env (ITable name cols body)= ITable name ((\x -> findWithDefault x x env) <$> cols) body


setName :: ITable -> String -> ITable
setName (ITable n c b) name = ITable name c b
setName (ITNil s) name = ITNil s

prependNameToCols :: ITable -> ITable
prependNameToCols (ITable name cols body) =
  ITable name ((\x -> name ++ "." ++ x) <$> cols) body
prependNameToCols (ITNil s) = ITNil s


sameCols :: ITable -> ITable -> Bool
sameCols t1@(ITable n1 c1 b1) t2@(ITable n2 c2 b2) =
  (length c1 == length c2) && (foldr1 (&&) $ zipWith (==) c1 c2)

-- works for two tables with the same cols, in the same order
unionTable :: ITable -> ITable -> ITable
unionTable (ITNil s) _ = ITNil s
unionTable _ (ITNil s) = ITNil s
unionTable t1@(ITable n1 c1 b1) t2@(ITable n2 c2 b2) =
  if (sameCols t1 t2) then ITable n1 c1 (b1 ++ b2) else ITNil "union fail"
