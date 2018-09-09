module Main (main) where

import Select
import Select.Expression
import Select.Relation
import System.IO
import Test.Hspec
import System.Directory

import Internal

main :: IO ()
main = do
  --print $ getITable testTable
  createDirectoryIfMissing False "./tmp"

  hspec $ describe "execute" $ do
    it "can ingest and print out table w/ all types" $ do
      (showITable $ getITable testTable) `shouldBe` testTable

    it "can select a table" $ do
      execute selectTable "./tmp/output_people.csv"
      "./tmp/output_people.csv" `shouldHaveSameContentAs` "data/people.csv"

    it "can select a column using as" $ do
      execute selectID "./tmp/output_name_all.csv"
      "./tmp/output_name_all.csv" `shouldHaveSameContentAs` "data/expected_output_name_all.csv"

    it "performs filtering and comparisons correctly" $ do
      execute selectName "./tmp/output_name.csv"
      "./tmp/output_name.csv" `shouldHaveSameContentAs` "data/expected_output_name.csv"

    it "performs joins correctly" $ do
      execute selectJoin "./tmp/output_join.csv"
      "./tmp/output_join.csv" `shouldHaveSameContentAs` "data/expected_output_join.csv"

    it "performs joins union" $ do
      execute unionTables "./tmp/output_union.csv"
      "./tmp/output_union.csv" `shouldHaveSameContentAs` "data/langs.csv"

testTable :: String
testTable = "bool,float,integer,string\nTrue,1.0,42,AdamWespiser\nFalse,10.01,4132412322,HaskellCurry"

testFloat = "colFloat\n1.0000"

selectTable :: SelectIdentifier
selectTable = SELECT (TABLE "data/people.csv")

selectID :: SelectIdentifier
selectID =
  SELECT $ FROM [Column "id" `AS` "ID_MODIFIED"] $ TABLE "data/people.csv"

selectName :: SelectIdentifier
selectName =
  SELECT $ [Column "first_name" `AS` "name"]
    `FROM` TABLE "data/people.csv" `WHERE` (Column "age" `Gte` LiteralInt 40)


unionTables :: SelectIdentifier
unionTables = SELECT $ UNION (TABLE "data/dynamic.csv") $ TABLE "data/static.csv"

selectJoin :: SelectIdentifier
selectJoin = SELECT $
  [ Column "orders.order_id" `AS` "order_id"
  , Column "customers.customer_name" `AS` "customer_name"
  ] `FROM`
  ( TABLE "data/orders_table.csv" `AS` "orders"
    `INNER_JOIN_ON`
    TABLE "data/customer_table.csv" `AS` "customers"
  ) (Column ("orders","customer_id") `Equ` Column ("customers","customer_id"))


shouldHaveSameContentAs :: FilePath -> FilePath -> Expectation
file1 `shouldHaveSameContentAs` file2 =
  withFile file1 ReadMode $ \handle1 ->
    withFile file2 ReadMode $ \handle2 -> do
      contents1 <- hGetContents handle1
      contents2 <- hGetContents handle2
      contents1 `shouldBe` contents2
