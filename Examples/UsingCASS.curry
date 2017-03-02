--- A simple program to show the usage of the API mode of CASS
--- to access the demanded values of the operation Rev.main:

import AnalysisServer   (analyzeGeneric)
import GenericProgInfo  (lookupProgInfo)
import Demandedness     (demandAnalysis)

demandedArgumentsOf :: String -> String -> IO [Int]
demandedArgumentsOf modname fname = do
  deminfo <- analyzeGeneric demandAnalysis modname >>= return . either id error
  return $ maybe [] id (lookupProgInfo (modname,fname) deminfo)

main = demandedArgumentsOf "Rev" "main"
