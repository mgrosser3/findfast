module Main where

import Data.Time (diffUTCTime, getCurrentTime)
import FindFast (findFast)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
  startTime <- getCurrentTime

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  args <- getArgs
  case args of
    [regex_pattern] -> findFast "." regex_pattern
    [regex_pattern, path] -> findFast path regex_pattern
    _ -> showUsageAndExit
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  putStrLn $ "\nDuration: " ++ show (realToFrac duration * 1000 :: Double) ++ " ms"

showUsageAndExit :: IO ()
showUsageAndExit = do
  hPutStrLn stderr "Usage: ff <regex-pattern> <path>"
  exitFailure

-- handleFile :: String -> FilePath -> IO ()
--   handleFile pattern path = do
--     result <- try (Data.ByteString.Char8.readFile path) :: IO (Either IOException ByteString)
--     case result of
--     Left err -> putStrLn $ "Error: Could not read file: " ++ path ++ " (" ++ show err ++ ")"
--     Right content -> do
--       let matches = getAllMatches (content =~ pattern :: AllMatches [] (MatchOffset, MatchLength))
--       unless (null matches) $ do
--         putStrLn $ "\n" ++ makeSafe path ++ " (" ++ show (Data.ByteString.Char8.length content) ++ " bytes)"
--         mapM_
--         ( \(offset, matchLength) -> do
--         when (matchLength > 0) $ do
--         let lineNum = getLineNumber content offset + 1
--         let lineContent = getLineContent content lineNum
--         putStrLn $ show lineNum ++ ": " ++ Data.ByteString.Char8.unpack lineContent
--         )
--         matches

-- determinePathType :: String -> IO (Either String FilePath)
-- determinePathType path = do
--     exists <- doesPathExist path
--     if exists
--         then return $ Right path  -- Echter Pfad
--         else if any (`elem` path) ['*', '?', '[', ']']
--             then return $ Left "Error: Glob patterns not supported, use a valid file path"
--             else return $ Left $ "Error: Path doesn't exist: " ++ path
--
--           exitFailure

-- handleDirectory :: String -> FilePath -> IO ()
-- handleDirectory pattern path = do
--   result <- try $ listDirectory path :: IO (Either IOException [String])
--   case result of
--     Left err -> putStrLn $ "Error: Could not read directory: " ++ path ++ " (" ++ show err ++ ")"
--     Right subpathes -> do
--       mapM_
--         ( \subpath -> do
--             unless (isHidden subpath) $
--               processPath (handleFile pattern) (handleDirectory pattern) $
--                 path </> subpath
--         )
--         subpathes
