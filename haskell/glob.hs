-- | Haskell implementation using String
module Main where

import Control.Exception (SomeException, handle)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)

-- | Globing
-- >>> glob "" ""
-- True
-- >>> map (flip glob "hello") ["h??lo", "h*o", "h*ello", "*hello*"]
-- [True,True,True,True]
-- >>> map (flip glob "hello wolrd") ["*h*o*", "h*o*", "*h*d", "*h*l*w*d", "*h?l*w*d"]
-- [True,True,True,True,True]
-- >>> glob "hello" "hi"
-- False
-- >>> glob "h?i" "hi"
-- False
-- >>> glob "h*l" "hello"
-- False
glob :: String -> String -> Bool
glob ('*' : ps) xs = wildcardMatch ps xs
  where
    wildcardMatch (p' : ps') (x' : xs') = glob (if p' == x' then ps' else '*' : ps) xs'
    wildcardMatch [] _ = True
    wildcardMatch _ _ = False
glob ('?' : ps) (_ : xs) = glob ps xs
glob (p : ps) (x : xs) = p == x && glob ps xs
glob [] [] = True
glob _ _ = False

-- | Walking
walk :: String -> FilePath -> IO ()
walk pat = goDir
  where
    goDir root = do
      paths <- listDirectory root
      mapM_ (goPath . mappend (root <> "/")) paths
    goPath path = do
      isDir <- doesDirectoryExist path
      if isDir
        then goDir path
        else ignoreException (goFile path)
    goFile path = do
      content <- zip [0 :: Integer ..] . lines <$> readFile path
      mapM_ (display path) (filter (glob pat . snd) content)
    display path (ln, line) = putStrLn (path <> ":" <> show ln <> "\t" <> line)

-- | Ignore exceptions thrown when file contains invalid byte sequence
ignoreException :: IO () -> IO ()
ignoreException = handle ignoreAll
  where
    ignoreAll :: SomeException -> IO ()
    ignoreAll = const (pure ())

-- | CLI
main :: IO ()
main = do
  args <- getArgs
  case args of
    [pat] -> walk pat "."
    _ -> putStrLn "USAGE: glob <pattern>"
