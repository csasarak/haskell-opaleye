{-# LANGUAGE OverloadedStrings #-}
{-|
 Module : Opaleye.Internal.BSPrint

Description : Print functions to a bytestring

-}
module Opaleye.Internal.BSHPrint where

-- TODO: explicit export lists, also find out why this can't just be in BSPrint?

import qualified Data.ByteString hiding (intersperse) as BS
import Data.ByteString.Builder

import Opaleye.Internal.HaskellDB.Sql (SqlColumn(..), SqlDelete(..),
                               SqlExpr(..), SqlOrder(..), SqlInsert(..),
                               SqlUpdate(..), SqlTable(..), SqlRangeBound(..),
                               OnConflict(..))

import qualified Opaleye.Internal.HaskellDB.Sql as Sql

-- this stuff again...
(<+>) :: Builder -> Builder -> Builder
a <+> b = " " <> a <> " " <> b
infixl 6 <+>

hsep :: [Builder] -> Builder
hsep = foldr1 (<+>)

parens :: Builder -> Builder -> Builder
parens b = "(" <> b <> ")"
  
-- | Append a comma to each builder
commaV :: [Builder] -> Builder
commaV bs = let inits = foldr (<> ",")

intersperse :: Builder -> [Builder] -> Builder
intersperse del = foldr (<+> del)

-- Silliness to avoid "ORDER BY 1" etc. meaning order by the first
-- column.  We need an identity function, but due to
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/100 we need
-- to be careful not to be over enthusiastic.  Just apply COALESCE to
-- literals.
deliteral :: SqlExpr -> SqlExpr
deliteral expr@(ConstSqlExpr _) = FunSqlExpr "COALESCE" [expr]
deliteral expr                  = expr

bsWhere :: [SqlExpr] -> Builder
bsWhere [] = mempty
bsWhere es = "WHERE"
             <+> hsep (intersperse (text "AND")
                       (map (parens . bsSqlExpr) es))