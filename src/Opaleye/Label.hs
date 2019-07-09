module Opaleye.Label where

import qualified Data.Text as SText
import qualified Opaleye.Internal.Label as L
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Select            as S

-- | Add a commented label to the generated SQL.
label :: String -> S.Select a -> S.Select a
label l a = Q.simpleQueryArr (L.label' (SText.pack l) . Q.runSimpleQueryArr a)
