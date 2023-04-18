module Echidna.SymExec where

import Control.Monad (forM)
import Control.Monad.State.Strict (evalStateT)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)

import EVM (StorageModel(..))
import EVM.Expr (simplify)
import EVM.Fetch qualified as Fetch
import EVM.Solidity (SolcContract(..))
import EVM.Solvers (withSolvers, Solver(Z3), CheckSatResult(Sat))
import EVM.SymExec (interpret, runExpr, abstractVM, mkCalldata, produceModels)
import EVM.Types
import EVM.SMT

import Echidna.Types.Tx

exploreContract :: Addr -> SolcContract -> IO [Tx]
exploreContract dst contract = do
  let
    calldata = mkCalldata Nothing [] -- fully abstract calldata, solution in buffers.txdata
    -- Alternatively, more concrete calldata with a specific function,
    -- not sure what is better yet
    -- calldata = mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
    vmSym = abstractVM calldata contract.runtimeCode Nothing SymbolicS
    maxIter = Just 10
    askSmtIters = Just 5
    rpcInfo = Nothing
    timeout = Nothing -- Just 1000 -- is it seconds?

  withSolvers Z3 2 timeout $ \solvers -> do
    exprInter <- evalStateT (interpret (Fetch.oracle solvers rpcInfo) maxIter askSmtIters runExpr) vmSym
    models <- produceModels solvers (simplify exprInter)

    txs <- forM models $ \(_end, result) ->
      case result of
        Sat cex ->
          case Map.lookup (AbstractBuf "txdata") cex.buffers of
            Just (Flat cd) -> do
              let value = fromMaybe 0 $ Map.lookup (CallValue 0) cex.txContext
              pure $ Just $
                Tx { call = SolCalldata cd
                   , src = 0
                   , dst = dst
                   , gasprice = 0
                   , gas = maxGasPerBlock
                   , value = value
                   , delay = (0, 0)
                   }
            Just (Comp _) -> pure Nothing -- TODO: compressed, implement me
            Nothing -> pure Nothing

        _ -> pure Nothing

    pure $ catMaybes txs
