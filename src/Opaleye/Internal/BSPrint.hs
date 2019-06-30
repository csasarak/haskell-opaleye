{-# LANGUAGE OverloadedStrings #-}
{-|
 Module : Opaleye.Internal.BSPrint

Description : Print functions to a bytestring

-}

module Opaleye.Internal.BSPrint where

import qualified Data.ByteString as BS
import Data.ByteString.Builder

import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.Sql as Sql
import           Opaleye.Internal.Sql (Select(SelectFrom,
                                              Table,
                                              RelExpr,
                                              SelectJoin,
                                              SelectValues,
                                              SelectBinary,
                                              SelectLabel,
                                              SelectExists),
                                       From, Join, Values, Binary, Label, Exists)
import qualified Data.List.NonEmpty as NEL
import qualified Opaleye.Internal.BSHPrint as HPrint

type TableAlias = Builder

-- | Replace instances of string 1 with string 2 in the bytestring
replace :: BS.ByteString -> BS.ByteString -> BS.ByteString
replace find inBS = let (prefix, suffix) = breakSubstring find inBs
                        suffix' = drop (BS.length prefix) suffix
                    in prefix <> inBS <> suffix'
  
(<+>) :: Builder -> Builder -> Builder
a <+> b = " " <> a <> " " <> b
infixl 6 <+>

parens :: Builder -> Builder -> Builder
parens b = "(" <> b <> ")"
  
-- | Append a comma to each builder
commaV :: [Builder] -> Builder
commaV = fmap (<> ",")

bsSql :: Select -> BS.ByteString
bsSql (SelectFrom s)   = bsSelectFrom s
bsSql (Table table)    = HPrint.bsTable table
bsSql (RelExpr expr)   = HPrint.bsSqlExpr expr
bsSql (SelectJoin j)   = bsSelectJoin j
bsSql (SelectValues v) = bsSelectValues v
bsSql (SelectBinary v) = bsSelectBinary v
bsSql (SelectLabel v)  = bsSelectLabel v
bsSql (SelectExists v) = bsSelectExists v

bsDistinctOn :: Maybe (NEL.NonEmpty HSql.SqlExpr) -> Builder
bsDistinctOn = maybe mempty $ \nel ->
     "DISTINCT ON" <+>
         "(" <+> commaV HPrint.bsSqlExpr (NEL.toList nel) <+>  ")"

bsSelectFrom :: From -> Builder
bsSelectFrom s = "SELECT"
                 <+> bsDistinctOn (Sql.distinctOn s)
                 <+>  bsAttrs (Sql.attrs s)
                 <+>  bsTables (Sql.tables s)
                 <+>  HPrint.bsWhere (Sql.criteria s)
                 <+>  bsGroupBy (Sql.groupBy s)
                 <+>  HPrint.bsOrderBy (Sql.orderBy s)
                 <+>  bsLimit (Sql.limit s)
                 <+>  bsOffset (Sql.offset s)

bsSelectJoin :: Join -> Builder
bsSelectJoin j = "SELECT * FROM"
                 <+>  bsTable (tableAlias 1 s1)
                 <+>  bsJoinType (Sql.jJoinType j)
                 <+>  bsTable (tableAlias 2 s2)
                 <+>  "ON"
                 <+>  HPrint.bsSqlExpr (Sql.jCond j)
  where (s1, s2) = Sql.jTables j

bsSelectValues :: Values -> Builder
bsSelectValues v = "SELECT"
                   <+> bsAttrs (Sql.vAttrs v)
                   <+> "FROM"
                   <+>  bsValues (Sql.vValues v)

bsSelectBinary :: Binary -> Builder
bsSelectBinary b = bsSql (Sql.bSelect1 b)
                   <+> bsBinOp (Sql.bOp b)
                   <+> bsSql (Sql.bSelect2 b)

bsSelectLabel :: Label -> Builder
bsSelectLabel l = "/*" <+> bytestring (defuseComments (Sql.lLabel l)) <+> "*/"
                  <+> bsSql (Sql.lSelect l)
  where
    defuseComments = replace (BS.pack "--") (BS.pack " - - ")
                   . replace (BS.pack "/*") (BS.pack " / * ")
                   . replace (BS.pack "*/") (BS.pack " * / ")
                   . BS.pack -- TODO: Change Label to use bytestring or text, can't use builder for interpolation

bsSelectExists :: Exists -> Builder
bsSelectExists v =
  "SELECT *"
  <+> text "FROM"
  <+> bsTable (tableAlias 1 (Sql.existsTable v))
  <+> case Sql.existsBool v of
       True -> "WHERE EXISTS"
       False -> "WHERE NOT EXISTS"
  <+> parens (bsSql (Sql.existsCriteria v))

bsJoinType :: Sql.JoinType -> Builder
bsJoinType Sql.LeftJoin = "LEFT OUTER JOIN"
bsJoinType Sql.RightJoin = "RIGHT OUTER JOIN"
bsJoinType Sql.FullJoin = "FULL OUTER JOIN"

-- This is pretty much just nameAs from Print.hs (pretty)
nameAs :: (HSql.SqlExpr, Maybe HSql.SqlColumn) -> Builder
nameAs (expr, name) = HPrint.bsAs (fmap unColumn name) (HPrint.bsSqlExpr expr)
  where unColumn (HSql.SqlColumn s) = s

bsAttrs :: Sql.SelectAttrs -> Builder
bsAttrs Sql.Star                 = "*"
bsAttrs (Sql.SelectAttrs xs)     = (commaV nameAs . NEL.toList) xs
bsAttrs (Sql.SelectAttrsStar xs) =
  commaV id ((map nameAs . NEL.toList) xs <> ["*"])

tableAlias :: Int -> Select -> (TableAlias, Select)
tableAlias i select = ("T" <> intDec i, select)

bsTables :: [Select] -> Builder
bsTables [] = mempty
bsTables ts = "FROM" <+> commaV bsTable (zipWith tableAlias [1..] ts)

bsTable :: (TableAlias, Select) -> Builder
bsTable (alias, select) = HPrint.bsAs (Just alias) $ case select of
  Table table           -> HPrint.bsTable table
  RelExpr expr          -> HPrint.bsSqlExpr expr
  SelectFrom selectFrom -> parens (bsSelectFrom selectFrom)
  SelectJoin slj        -> parens (bsSelectJoin slj)
  SelectValues slv      -> parens (bsSelectValues slv)
  SelectBinary slb      -> parens (bsSelectBinary slb)
  SelectLabel sll       -> parens (bsSelectLabel sll)
  SelectExists saj      -> parens (bsSelectExists saj)

bsGroupBy :: Maybe (NEL.NonEmpty HSql.SqlExpr) -> Builder
bsGroupBy = maybe mempty (HPrint.bsGroupBy . NEL.toList xs)

bsLimit :: Maybe Int -> Builder
bsLimit = maybe mempty (\n' -> "LIMIT" <+> intDec n)

bsOffset :: Maybe Int -> Builder
bsOffset = maybe mempty (\n' -> "OFFSET" <+> intDec n)

bsValues :: [[HSql.SqlExpr]] -> Builder
bsValues v = HPrint.bsAs (Just "V") (parens ("VALUES" <+> commaV bsValuesRow v))

ppValuesRow :: [HSql.SqlExpr] -> Builder
ppValuesRow = parens . HPrint.commaH HPrint.ppSqlExpr

bsBinOp :: Sql.BinOp -> Builder
bsBinOp o = case o of
  Sql.Union        -> "UNION"
  Sql.UnionAll     -> "UNION ALL"
  Sql.Except       -> "EXCEPT"
  Sql.ExceptAll    -> "EXCEPT ALL"
  Sql.Intersect    -> "INTERSECT"
  Sql.IntersectAll -> "INTERSECT ALL"

bsInsertReturning :: Sql.Returning HSql.SqlInsert -> Builder
bsInsertReturning (Sql.Returning insert returnExprs) =
  HPrint.bsInsert insert
  <+> "RETURNING"
  <+> commaV HPrint.bsSqlExpr (NEL.toList returnExprs)

bsUpdateReturning :: Sql.Returning HSql.SqlUpdate -> Builder
bsUpdateReturning (Sql.Returning update returnExprs) =
  HPrint.bsUpdate update
  <+> "RETURNING"
  <+> commaV HPrint.bsSqlExpr (NEL.toList returnExprs)

bsDeleteReturning :: Sql.Returning HSql.SqlDelete -> Builder
bsDeleteReturning (Sql.Returning delete returnExprs) =
  HPrint.bsDelete delete
  <+> "RETURNING"
  <+> commaV HPrint.bsSqlExpr (NEL.toList returnExprs)