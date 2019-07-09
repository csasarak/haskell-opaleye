{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


{- |

 Fields can be required or optional and, independently, nullable or
 non-nullable.

 A required non-nullable @SqlInt4@ (for example) is defined with
 'required' and gives rise to a

 @
 TableFields (Field SqlInt4) (Field SqlInt4)
 @

 The leftmost argument is the type of writes. When you insert or
 update into this column you must give it a @Field SqlInt4@ (which you
 can define with @sqlInt4 :: Int -> Field SqlInt4@).

 A required nullable @SqlInt4@ is defined with 'required' and gives rise
 to a

 @
 TableFields (FieldNullable SqlInt4) (FieldNullable SqlInt4)
 @

 When you insert or update into this column you must give it a
 @FieldNullable SqlInt4@, which you can define either with @sqlInt4@ and
 @toNullable :: Field a -> FieldNullable a@, or with @null ::
 FieldNullable a@.

 An optional non-nullable @SqlInt4@ is defined with 'optional' and gives
 rise to a

 @
 TableFields (Maybe (Field SqlInt4)) (Field SqlInt4)
 @

 Optional columns are those that can be omitted on writes, such as
 those that have @DEFAULT@s or those that are @SERIAL@.
 When you insert or update into this column you must give it a @Maybe
 (Field SqlInt4)@. If you provide @Nothing@ then the column will be
 omitted from the query and the default value will be used. Otherwise
 you have to provide a @Just@ containing a @Field SqlInt4@.

 An optional nullable @SqlInt4@ is defined with 'optional' and gives
 rise to a

 @
 TableFields (Maybe (FieldNullable SqlInt4)) (FieldNullable SqlInt4)
 @

 Optional columns are those that can be omitted on writes, such as
 those that have @DEFAULT@s or those that are @SERIAL@.
 When you insert or update into this column you must give it a @Maybe
 (FieldNullable SqlInt4)@. If you provide @Nothing@ then the default
 value will be used. Otherwise you have to provide a @Just@ containing
 a @FieldNullable SqlInt4@ (which can be null).

-}

module Opaleye.Table (-- * Defining tables
                      table,
                      tableWithSchema,
                      T.Table,
                      T.tableField,
                      T.optional,
                      T.required,
                      -- * Querying tables
                      selectTable,
                      -- * Other
                      T.TableColumns,
                      TableFields,
                      -- * Deprecated
                      T.tableColumn,
                      View,
                      Writer,
                      T.Table(T.Table, T.TableWithSchema),
                      -- * Module reexport
                      module Opaleye.Table) where

import qualified Data.Text as SText
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Table as T
import           Opaleye.Internal.Table (View, Table, Writer,
                                         TableFields)

import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.Unpackspec as U

import qualified Opaleye.Select                  as S

import qualified Data.Profunctor.Product.Default as D

-- | Example type specialization:
--
-- @
-- selectTable :: Table w (Field a, Field b)
--             -> Select (Field a, Field b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- selectTable :: Table w (Foo (Field a) (Field b) (Field c))
--             -> Select (Foo (Field a) (Field b) (Field c))
-- @
selectTable :: D.Default U.Unpackspec fields fields
            => Table a fields
            -- ^
            -> S.Select fields
selectTable = selectTableExplicit D.def

-- | Define a table with an unqualified name.
table :: String
      -- ^ Table name
      -> TableFields writeFields viewFields
      -> Table writeFields viewFields
table s = T.Table s

-- | Define a table with a qualified name.
tableWithSchema :: String
                -- ^ Schema name
                -> String
                -- ^ Table name
                -> TableFields writeFields viewFields
                -> Table writeFields viewFields
tableWithSchema sc t = T.TableWithSchema sc t

-- * Explicit versions

selectTableExplicit :: U.Unpackspec tablefields fields
                    -- ^
                    -> Table a tablefields
                    -- ^
                    -> S.Select fields
selectTableExplicit cm table' = Q.simpleQueryArr f where
  f ((), t0) = (retwires, primQ, Tag.next t0) where
    (retwires, primQ) = T.queryTable cm table' t0

-- * Deprecated versions

-- | Use 'selectTable' instead.  Will be deprecated in version 0.7.
queryTable :: D.Default U.Unpackspec fields fields =>
              Table a fields -> S.Select fields
queryTable = selectTable

-- | Use 'selectTableExplicit' instead.  Will be deprecated in version
-- 0.7.
queryTableExplicit :: U.Unpackspec tablefields fields ->
                     Table a tablefields -> S.Select fields
queryTableExplicit = selectTableExplicit
