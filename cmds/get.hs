import System.Environment
import Control.Exception
import Data.Either

copeWithTry :: Either IOError [a] -> [a]
copeWithTry = either (const []) id

main = do
  args <- getArgs
  if args == [] then putStrLn "" else do
    val <- try $ getEnv $ head args
    putStrLn $ copeWithTry val
