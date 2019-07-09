-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

module Opaleye.Internal.HaskellDB.Sql where


import Data.Text
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text.Prettyprint.Doc (Doc)

-----------------------------------------------------------
-- * SQL data type
-----------------------------------------------------------

data SqlTable = SqlTable
  { sqlTableSchemaName :: Maybe Text
  , sqlTableName       :: Text
  } deriving Show

newtype SqlColumn = SqlColumn Text deriving Show

-- | A valid SQL name for a parameter.
type SqlName = Text

data SqlOrderNulls = SqlNullsFirst | SqlNullsLast
                   deriving Show

data SqlOrderDirection = SqlAsc | SqlDesc
                       deriving Show

data SqlOrder = SqlOrder { sqlOrderDirection :: SqlOrderDirection
                         , sqlOrderNulls     :: SqlOrderNulls }
  deriving Show

data SqlRangeBound = Inclusive SqlExpr | Exclusive SqlExpr | PosInfinity | NegInfinity
                   deriving Show

data SqlDistinct = SqlDistinct | SqlNotDistinct
                 deriving Show

-- | Expressions in SQL statements.
data SqlExpr = ColumnSqlExpr  SqlColumn
             | CompositeSqlExpr SqlExpr Text
             | BinSqlExpr     Text SqlExpr SqlExpr
             | SubscriptSqlExpr SqlExpr SqlExpr
             | PrefixSqlExpr  Text SqlExpr
             | PostfixSqlExpr Text SqlExpr
             | FunSqlExpr     Text [SqlExpr]
             | AggrFunSqlExpr Text [SqlExpr] [(SqlExpr, SqlOrder)] SqlDistinct -- ^ Aggregate functions separate from normal functions.
             | ConstSqlExpr   Text
             | CaseSqlExpr    (NEL.NonEmpty (SqlExpr,SqlExpr)) SqlExpr
             | ListSqlExpr    (NEL.NonEmpty SqlExpr)
             | ParamSqlExpr (Maybe SqlName) SqlExpr
             | PlaceHolderSqlExpr
             | ParensSqlExpr SqlExpr
             | CastSqlExpr Text SqlExpr
             | DefaultSqlExpr
             | ArraySqlExpr [SqlExpr]
             | RangeSqlExpr Text SqlRangeBound SqlRangeBound
  deriving Show

-- | Data type for SQL UPDATE statements.
data SqlUpdate  = SqlUpdate SqlTable [(SqlColumn,SqlExpr)] [SqlExpr]

-- | Data type for SQL DELETE statements.
data SqlDelete  = SqlDelete SqlTable [SqlExpr]

data OnConflict = DoNothing
                -- ^ @ON CONFLICT DO NOTHING@

--- | Data type for SQL INSERT statements.
data SqlInsert  = SqlInsert SqlTable [SqlColumn] (NEL.NonEmpty [SqlExpr]) (Maybe OnConflict)

-- type use purely for annotating Docs in the print function
data SqlStatement
type Doc = Data.Text.Prettyprint.Doc.Doc SqlStatement
