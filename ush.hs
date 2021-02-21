import System.Directory
import System.Environment
import System.Process
import System.IO
import Data.List.Split
import Control.Exception
import Control.Concurrent
import Data.Either
import Data.Maybe

{- TODO if it's a directory ignore the result -}
findInPath :: String -> IO (Maybe String)
findInPath s = do
  path <- try $ getEnv "PATH"
  splitPath <- return $ splitOn ":" $ copeWithTry $ path
  searchResults <- sequence $ map (try . getDirectoryContents) splitPath
  searchResultsFound <- return $ map ((/= []) . filter (== s)) $ map copeWithTry $ searchResults
  searchResults <- return $ map ((++ "/" ++ s) . fst) $ filter snd $ zip splitPath searchResultsFound
  return $ case searchResults of
    a:rest -> Just a
    otherwise -> Nothing
  where
    copeWithTry :: Either IOError [a] -> [a]
    copeWithTry = either (const []) id

splitConcat :: String -> [String] -> [String]
splitConcat on s = concat $ map (splitOn on) s

removeEmpty :: [String] -> [String]
removeEmpty = filter (/= "")

pipe :: Handle -> Handle -> IO ()
pipe h1 h2 = do
  h1Open <- hIsOpen h1
  h2Open <- hIsOpen h2
  if (h1Open && h2Open) then do
    contents <- hGetContents h1
    hPutStr h2 contents
    hFlush h2
    pipe h1 h2
  else return ()

execProgram :: String -> [String] -> IO ()
execProgram prog args = do
  (Just hIn, Just hOut, Just hErr, hProc) <- createProcess (proc prog args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  forkIO $ pipe stdin hIn
  forkIO $ pipe hOut stdout
  forkIO $ pipe hErr stderr
  return ()
  -- output <- hGetContents hOut
  -- putStrLn output
  -- hDuplicateTo stdout (fromJust hOut)

-- readProcess prog args "" >>= putStrLn

execCmd :: [String] -> IO ()
execCmd [] = return ()
execCmd (cmd:args) = do
  program <- findInPath cmd
  maybe (return ()) (\p -> execProgram p args) program

shellRoutine = do
  putStr "$ "
  hFlush stdout
  inp <- getLine
  inpSplit <- return $ removeEmpty $ splitConcat "\t" $ splitConcat " " $ [inp]
  execCmd inpSplit
  shellRoutine

main = shellRoutine
