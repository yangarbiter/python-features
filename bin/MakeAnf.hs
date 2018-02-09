{-# LANGUAGE OverloadedStrings #-}
module Main ( main
            ) where

import Anf
import Control.Monad
import Control.Monad.Loops (whileM_)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (pack, unpack)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    (inFile : outFile : _) -> convertFile inFile outFile
    _                      -> putStrLn "Missing input or output file"

convertFile :: FilePath -> FilePath -> IO ()
convertFile inFp outFp = withFile inFp ReadMode $ \inFile ->
  withFile outFp WriteMode $ \outFile ->
  whileM_ (fmap not $ hIsEOF inFile) $
  BS.hGetLine inFile >>= processLine >>= BS8.hPutStrLn outFile

processLine :: ByteString -> IO ByteString
processLine = fmap (BSL.toStrict . J.encode) . (decodeForSure >=> transformJson)
  where decodeForSure :: ByteString -> IO J.Value
        decodeForSure s = case J.decodeStrict s of
          Nothing   -> ioError (userError "Bad JSON line")
          Just json -> pure json

        transformJson :: J.Value -> IO J.Value
        transformJson j@(J.Object m) =
          case HM.lookup "user_script" m of
            Nothing -> pure j
            Just (J.String s) -> case makeAnfSource (T.unpack s) of
              Left err -> ioError . userError . show $ err
              Right newS -> pure $
                J.Object (HM.insert "user_script" (J.String (T.pack newS)) m)
