import System.Directory
import System.Environment
import System.Process
import System.IO
import Data.List.Split
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Either
import Data.Maybe

copeWithTry :: Either IOError [a] -> [a]
copeWithTry = either (const []) id

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

splitConcat :: String -> [String] -> [String]
splitConcat on s = concat $ map (splitOn on) s

removeEmpty :: [String] -> [String]
removeEmpty = filter (/= "")

pipeInp :: MVar Char -> MVar () -> Handle -> IO ()
pipeInp inpMV exitMV h = do
  inp <- takeMVar inpMV
  hOpen <- hIsOpen h
  if hOpen then do
    hPutChar h inp
    hFlush h

    keepGoing <- isEmptyMVar exitMV
    if keepGoing then pipeInp inpMV exitMV h else return ()
  else return ()

pipeOtp :: Handle -> Handle -> MVar () -> IO ()
pipeOtp h1 h2 done = do
  h1Open <- hIsOpen h1
  h2Open <- hIsOpen h2
  if (h1Open && h2Open) then do
    eiContents <- try $ hGetContents h1
    if (isRight eiContents) then do
      contents <- return $ copeWithTry $ eiContents
      hPutStr h2 contents
      hFlush h2
      pipeOtp h1 h2 done
    else putMVar done ()
  else   putMVar done ()

execProgram :: MVar Char -> String -> [String] -> IO ()
execProgram inpMV prog args = do
  (Just hIn, Just hOut, Just hErr, hProc) <- createProcess (proc prog args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  inpQuitMV <- newEmptyMVar
  outMV <- newEmptyMVar
  errMV <- newEmptyMVar

  forkIO $ pipeInp inpMV inpQuitMV hIn
  forkIO $ pipeOtp hOut stdout outMV
  forkIO $ pipeOtp hErr stderr errMV

  waitForProcess hProc

  takeMVar outMV
  takeMVar errMV

  putMVar inpQuitMV ()
  putMVar inpMV '\0'

  -- killThread inID
  hClose hIn
  -- hClose hOut
  -- hClose hErr
  return ()
  -- output <- hGetContents hOut
  -- putStrLn output
  -- hDuplicateTo stdout (fromJust hOut)

-- readProcess prog args "" >>= putStrLn

inpManager :: MVar Char -> IO ()
inpManager mv = do
  inp <- hGetChar stdin
  putMVar mv inp
  inpManager mv

getInpLine :: String -> MVar Char -> IO String
getInpLine soFar inpMV = do
  inp <- takeMVar inpMV
  hPutChar stdout inp
  hFlush stdout
  if inp == '\n' then return soFar else getInpLine (soFar ++ [inp]) inpMV

execCmd :: MVar Char -> [String] -> IO ()
execCmd _ [] = return ()
execCmd inpMV (cmd:args) = do
  program <- findInPath cmd
  maybe (return ()) (\p -> execProgram inpMV p args) program

shellRoutine :: MVar Char -> IO ()
shellRoutine inpMV = do
  putStr "$ "
  hFlush stdout
  inp <- getInpLine "" inpMV
  inpSplit <- return $ removeEmpty $ splitConcat "\t" $ splitConcat " " $ [inp]
  execCmd inpMV inpSplit
  shellRoutine inpMV

main = do
  inpMV <- newEmptyMVar
  forkIO $ inpManager inpMV
  shellRoutine inpMV
