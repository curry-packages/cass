--------------------------------------------------------------------------
--- This module supports the configuration of the analysis system
--- and provides access to some values in Config file.
---
--- It also provides an operation to get the port number of
--- the analysis server (which is implicitly started if necessary).
---
--- @author Michael Hanus
--- @version January 2025
--------------------------------------------------------------------------

module CASS.Configuration
 ( systemBanner, baseDir, docDir, executableName
 , curryInfoAnalyses
 , CConfig(..), defaultCConfig, debugLevel, setDebugLevel
 , getServerAddress, readRCFile, updateProperty
 , useCurryInfo, useCurryInfoCGI, fixpointMethod, withPrelude
 , storeServerPortNumber, removeServerPortNumber
 , getDefaultPath, waitTime, numberOfWorkers
 ) where

import Control.Monad               ( unless, when )
import Curry.Compiler.Distribution ( curryCompiler )
import Data.List                   ( sort )
import Numeric                     ( readInt )
import System.Environment          ( getEnv )
import System.FilePath             ( FilePath, (</>), (<.>) )
import System.IO                   ( hPutStrLn, stderr )

import System.Process
import System.Directory

import Analysis.Logging   ( DLevel(..), debugMessage )
import CASS.PackageConfig ( packagePath, packageExecutable, packageVersion )
import Data.PropertyFile  ( readPropertyFile, updatePropertyFile )

--- The banner of the CASS system.
systemBanner :: String
systemBanner =
  let bannerText = "CASS: Curry Analysis Server System (Version " ++
                   packageVersion ++ " of 08/01/2025 for " ++
                   curryCompiler ++ ")"
      bannerLine = take (length bannerText) (repeat '=')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine


--- The base directory of the analysis tool containing all programs
--- and documentations.
--- It is used to copy the configuration file, to the find executables
--- of the server and the workers, and to find the documentation
--- of the various analyses.
baseDir :: String
baseDir = packagePath

--- The directory containing the documentations of the various analyses.
docDir :: String
docDir = baseDir </> "docs"

--- The name of the main executable. Used to start workers in `CASS.Server`.
executableName :: String
executableName = packageExecutable

--- The address of the server when it is connected from the worker clients.
getServerAddress :: IO String
getServerAddress = return "127.0.0.1" -- run only on local machine

--- timeout for network message passing: -1 is wait time infinity
waitTime :: Int
waitTime = -1

--- Default number of workers (if the number is not found in the
--- configuration file).
defaultWorkers :: Int
defaultWorkers = 0

--- Analysis names currently supported by CurryInfo.
curryInfoAnalyses :: [String]
curryInfoAnalyses =
  [ "Deterministic"
  , "Demand"
  , "Indeterministic"
  , "SolComplete"
  , "Terminating"
  , "Total"
  ]

--------------------------------------------------------------------------
--- Configuration info used during execution of CASS.
--- It contains the properties from the rc file and the current debug level.
data CConfig = CConfig { ccProps :: [(String,String)], ccDebugLevel :: DLevel }

--- The default configuration has no properties and is quiet.
defaultCConfig :: CConfig
defaultCConfig = CConfig [] Quiet

--- Returns the debug level from the current configuration.
debugLevel :: CConfig -> DLevel
debugLevel = ccDebugLevel

--- Returns the debug level from the current configuration.
setDebugLevel :: Int -> CConfig -> CConfig
setDebugLevel dl cc = cc { ccDebugLevel = toEnum dl }

--- Returns the curryinfo flag from the current configuration.
useCurryInfo :: CConfig -> Bool
useCurryInfo cc =
  maybe False (`elem` ["yes","cgi"]) (lookup "curryinfo" (ccProps cc))

--- Returns the curryinfo CGI flag from the current configuration.
useCurryInfoCGI :: CConfig -> Bool
useCurryInfoCGI cc = maybe False (=="cgi") (lookup "curryinfo" (ccProps cc))

--- Returns the fixpoint computation method from Config file
fixpointMethod :: CConfig -> String
fixpointMethod cc = maybe "wlist" id  (lookup "fixpoint" (ccProps cc))

--- Gets the option to analyze also the prelude from Config file
withPrelude :: CConfig -> Bool
withPrelude cc = maybe True (/="no") (lookup "prelude" (ccProps cc))

--- Gets the default load path from the property file (added at the end
--- of CURRYPATH).
getDefaultPath :: CConfig -> IO String
getDefaultPath cc = do
  currypath <- getEnv "CURRYPATH"
  return $ case lookup "path" (ccProps cc) of
    Just value -> if all isSpace value
                    then currypath
                    else if null currypath then value
                                           else currypath ++ ':' : value
    Nothing    -> currypath

--- number of worker threads running at the same time
numberOfWorkers :: CConfig -> Int
numberOfWorkers cc = do
  case lookup "numberOfWorkers" (ccProps cc) of
    Just value -> case readInt value of
                    [(int,_)] -> int
                    _         -> defaultWorkers
    Nothing    -> defaultWorkers

--------------------------------------------------------------------------
-- Name of user property file:
propertyFileName :: IO String
propertyFileName = getHomeDirectory >>= return . (</> ".curryanalysisrc")

defaultPropertyFileName :: String
defaultPropertyFileName = baseDir </> "curryanalysisrc"

--- Install user property file if it does not exist.
installPropertyFile :: IO ()
installPropertyFile = do
  fname <- propertyFileName
  pfexists <- doesFileExist fname
  dpfexists <- doesFileExist defaultPropertyFileName
  when (not pfexists && dpfexists) $ do
    copyFile defaultPropertyFileName fname
    hPutStrLn stderr $
      "New analysis configuration file '" ++ fname ++ "' installed."

--- Reads the rc file (and try to install a user copy of it if it does not
--- exist) and returns its definition. Additionally, the definitions
--- are compared with the default property file of the CASS distribution.
--- If the set of variables is different, the rc file of the user is updated
--- with the distribution but the user's definitions are kept.
readRCFile :: IO CConfig
readRCFile = do
  hashomedir <- getHomeDirectory >>= doesDirectoryExist
  if not hashomedir
   then readPropertiesAndStoreLocally
   else do
     installPropertyFile
     cc@(CConfig userprops dl) <- readPropertiesAndStoreLocally
     distprops <- readPropertyFile defaultPropertyFileName
     unless (rcKeys userprops == rcKeys distprops) $ do
       rcName <- propertyFileName
       debugMessage dl 1 $ "Updating '" ++ rcName ++ "'..."
       renameFile rcName $ rcName <.> "bak"
       dpfexists <- doesFileExist defaultPropertyFileName
       when dpfexists $ copyFile defaultPropertyFileName rcName
       mapM_ (\ (n, v) -> maybe (return ())
                 (\uv -> if uv == v then return ()
                                    else updatePropertyFile rcName n uv)
                 (lookup n userprops))
             distprops
     return cc

rcKeys :: [(String, String)] -> [String]
rcKeys = sort . map fst

--- Reads the user property file or, if it does not exist,
--- the default property file of CASS,
--- and return the configuration with the properties.
readPropertiesAndStoreLocally :: IO CConfig
readPropertiesAndStoreLocally = do
  userpfn    <- propertyFileName
  hasuserpfn <- doesFileExist userpfn
  props      <- readPropertyFile
                  (if hasuserpfn then userpfn else defaultPropertyFileName)
  return $ updateDebugLevel (CConfig props Quiet)

--- Updates the debug level from the current properties.
updateDebugLevel :: CConfig -> CConfig
updateDebugLevel cc =
  case lookup "debugLevel" (ccProps cc) of
    Just value -> case readInt value of
                    [(dl,_)] -> setDebugLevel dl cc
                    _        -> cc
    Nothing    -> cc

--- Updates a property.
updateProperty :: String -> String -> CConfig -> CConfig
updateProperty pn pv cc = cc { ccProps = replaceKeyValue pn pv (ccProps cc) }

replaceKeyValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replaceKeyValue k v []            = [(k,v)]
replaceKeyValue k v ((k1,v1):kvs) =
  if k == k1 then (k,v) : kvs
             else (k1,v1) : replaceKeyValue k v kvs


--------------------------------------------------------------------------
--- Gets the name of file containing the current server port and pid
--- ($HOME has to be set)
getServerPortFileName :: IO String
getServerPortFileName = do
  homeDir <- getHomeDirectory
  return $ homeDir++"/.curryanalysis.port"

--- Stores the current server port number together with the pid of
--- the server process.
storeServerPortNumber :: Int -> IO ()
storeServerPortNumber portnum = do
  mypid <- getPID
  serverPortFileName <- getServerPortFileName
  writeFile serverPortFileName (show (portnum,mypid))

--- Removes the currently stored server port number.
removeServerPortNumber :: IO ()
removeServerPortNumber = getServerPortFileName >>= removeFile

readServerPortPid :: IO (Int,Int)
readServerPortPid = getServerPortFileName >>= readFile >>= return . read

--------------------------------------------------------------------------
