{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}


module Validators.SimpleV where
-- module Validators.MyLib (someFunc) where


-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"



-- Sectios of a Plutus Contract

-- 1. Extensions -- very top
-- 2. Imports
import PlutusTx
import PlutusTx.Prelude -- keeps giving warning because you are not using this prelude so comment
import qualified Ledger
import qualified Plutus.V2.Ledger.Api as V2LedgerApi
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts


-- 3. OnChain code


-- what we really need - remmember that any valiator that is going to call SImpleV takes 3 arguments.

-- - datum
-- - redeemer 
-- - script context


--Actual validator logic
simpleV :: BuiltinData -> BuiltinData -> BuiltinData -> ()
simpleV _ _ _ = ()     -- not considering anything and making it succesful. Most successful one.
-- simpleV _ _ _ = error () -- if i do this it will always fail to retrieve 

--Boilerplate
simpleVCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- simpleVCompiled = compile [|| simpleV ||]     -- this is still quoted.
simpleVCompiled = $$(compile [|| simpleV ||])    -- we add $$ to get back for plutus

simpleValidator :: V2UtilsScripts.Validator
simpleValidator = V2LedgerApi.mkValidatorScript simpleVCompiled


validatorHash :: Ledger.ValidatorHash
validatorHash = Ledger.validatorHash simpleValidator   -- this is in Base16 format

address :: Ledger.Address
address = Ledger.scriptHashAddress validatorHash 


-- 4. Offchain code