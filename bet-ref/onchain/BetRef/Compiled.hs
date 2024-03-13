{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module BetRef.Compiled
    ( betRefValidator
    , BetRefParams (..)
    , OracleAnswerDatum (..)
    , BetRefDatum (..)
    , BetRefAction (..)
    ) where

import           PlutusCore.Version    (plcVersion100)
import qualified PlutusTx

import           BetRef

-- | Generates validator given params.
betRefValidator :: BetRefParams -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
betRefValidator betRefParams =
    $$(PlutusTx.compile [|| mkBetRefValidator||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 betRefParams
