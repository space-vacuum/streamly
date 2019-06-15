-- |
-- Module      : Main
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}

import Control.DeepSeq (NFData)
import Data.Functor.Identity (runIdentity)
import System.IO (openFile, IOMode(..), Handle, hClose)
import System.Process.Typed (shell, runProcess_)

import Data.IORef
import Gauge

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Mem.Array as A
import qualified Streamly.Prelude as S

#ifdef DEVBUILD
import Data.Char (ord, chr)
import qualified Streamly.Fold as FL
import qualified Streamly.String as SS
#endif

-- Input and output file handles
data Handles = Handles Handle Handle

scratchDir :: String
scratchDir = "benchmark/scratch/"

infile :: String
infile = scratchDir ++ "in-100MB.txt"

outfile :: String
outfile = scratchDir ++ "out.txt"

blockSize, blockCount :: Int
blockSize = 32768
blockCount = 3200

#ifdef DEVBUILD
fileSize :: Int
fileSize = blockSize * blockCount

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

-- Code copied from base/Data.Char to INLINE it
{-# INLINE isSpace #-}
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- The magic 0x377 isn't really that magical. As of 2014, all the codepoints
-- at or below 0x377 have been assigned, so we shouldn't have to worry about
-- any new spaces appearing below there. It would probably be best to
-- use branchless ||, but currently the eqLit transformation will undo that,
-- so we'll do it like this until there's a way around that.
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  | otherwise = iswspace (ord c) /= 0
  where
    uc = fromIntegral (ord c) :: Word
#endif

main :: IO ()
main = do
#ifdef DEVBUILD
    -- This is a 500MB text file for text processing benchmarks.  We cannot
    -- have it in the repo, therefore we use it locally with DEVBUILD
    -- conditional (enabled by "dev" cabal flag). Some tests that depend on
    -- this file are available only in DEVBUILD mode.
    inHandle <- openFile "benchmark/text-processing/gutenberg-500.txt" ReadMode
#else
    -- XXX will this work on windows/msys?
    let cmd = "mkdir -p " ++ scratchDir
                ++ "; test -e " ++ infile
                ++ " || { echo \"creating input file " ++ infile
                ++ "\" && dd if=/dev/random of=" ++ infile
                ++ " bs=" ++ show blockSize
                ++ " count=" ++ show blockCount
                ++ ";}"

    runProcess_ (shell cmd)
    inHandle <- openFile infile ReadMode
#endif
    outHandle <- openFile outfile WriteMode
    href <- newIORef $ Handles inHandle outHandle

    defaultMain
        [ bgroup "readArray"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                let s = FH.readArrays inh
                larr <- S.last s
                case larr of
                    Nothing -> return Nothing
                    Just arr -> A.readIndex arr (A.length arr - 1)
            -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
            -- wc uses lseek to just determine the file size rather than reading
            -- and counting characters.
            , mkBench "length (bytecount)" href $ do
                Handles inh _ <- readIORef href
                let s = FH.readArrays inh
                S.sum (S.map A.length s)
            , mkBench "sum" href $ do
                let foldlArr' f z = runIdentity . S.foldl' f z . A.read
                Handles inh _ <- readIORef href
                let s = FH.readArrays inh
                S.foldl' (\acc arr -> acc + foldlArr' (+) 0 arr) 0 s
            ]
        , bgroup "readStream"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                S.last $ FH.read inh
            , mkBench "length (bytecount)" href $ do
                Handles inh _ <- readIORef href
                S.length $ FH.read inh
            , mkBench "sum" href $ do
                Handles inh _ <- readIORef href
                S.sum $ FH.read inh
            ]
        , bgroup "copyArray"
            [ mkBench "copy" href $ do
                Handles inh outh <- readIORef href
                let s = FH.readArrays inh
                FH.writeArrays outh s
            ]
#ifdef DEVBUILD
        -- This takes a little longer therefore put under the dev conditional
        , bgroup "copyStream"
            [ mkBench "fromToHandle" href $ do
                Handles inh outh <- readIORef href
                FH.write outh (FH.read inh)
            ]
        -- This needs an ascii file, as decode just errors out.
        , bgroup "decode-encode"
           [ mkBench "char8" href $ do
               Handles inh outh <- readIORef href
               FH.write outh
                 $ SS.encodeChar8
                 $ SS.decodeChar8
                 $ FH.read inh
           , mkBench "utf8" href $ do
               Handles inh outh <- readIORef href
               FH.write outh
                 $ SS.encodeUtf8
                 $ SS.decodeUtf8
                 $ FH.read inh
           ]
        , bgroup "grouping"
            [ mkBench "chunksOf 1 (toArray)" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.chunksOf fileSize (A.toArrayN fileSize)
                                (FH.read inh)

            , mkBench "chunksOf 1" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.chunksOf 1 FL.drain (FH.read inh)
            , mkBench "chunksOf 10" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.chunksOf 10 FL.drain (FH.read inh)
            , mkBench "chunksOf 1000" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.chunksOf 1000 FL.drain (FH.read inh)
            ]
        , bgroup "group-ungroup"
            [ mkBench "lines-unlines" href $ do
                Handles inh outh <- readIORef href
                FH.write outh
                  $ SS.encodeChar8
                  $ SS.unlines
                  $ SS.lines
                  $ SS.decodeChar8
                  $ FH.read inh
            , mkBench "lines-unlines-arrays" href $ do
                Handles inh outh <- readIORef href
                FH.writeArraysPackedUpto (1024*1024) outh
                    $ S.insertAfterEach (return $ A.fromList [10])
                    $ A.splitArraysOn 10
                    $ FH.readArraysOfUpto (1024*1024) inh
            , mkBench "words-unwords" href $ do
                Handles inh outh <- readIORef href
                FH.write outh
                  $ SS.encodeChar8
                  $ SS.unwords
                  $ SS.words
                  $ SS.decodeChar8
                  $ FH.read inh
            ]

        , let lf = fromIntegral (ord '\n')
              lfarr = A.fromList [lf]
              isSp = isSpace . chr . fromIntegral
              toarr = A.fromList . map (fromIntegral . ord)
          in bgroup "splitting"
            [ bgroup "predicate"
                [ mkBench "splitBy \\n (line count)" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitBy (== lf) FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitSuffixBy \\n (line count)" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitSuffixBy (== lf) FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "wordsBy isSpace (word count)" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.wordsBy isSp FL.drain
                        $ FH.read inh) -- >>= print
                ]

            , bgroup "empty-pattern"
                [ mkBench "splitOn \"\"" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn (A.fromList []) FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitSuffixOn \"\"" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitSuffixOn (A.fromList []) FL.drain
                        $ FH.read inh) -- >>= print
                ]
            , bgroup "short-pattern"
                [ mkBench "splitOn \\n (line count)" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn lfarr FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitSuffixOn \\n (line count)" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitSuffixOn lfarr FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitOn a" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn (toarr "a") FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitOn \\r\\n" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn (toarr "\r\n") FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitSuffixOn \\r\\n)" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitSuffixOn (toarr "\r\n") FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitOn aa" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn (toarr "aa") FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitOn aaaa" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn (toarr "aaaa") FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitOn abcdefgh" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn (toarr "abcdefgh") FL.drain
                        $ FH.read inh) -- >>= print
                ]
            , bgroup "long-pattern"
                [ mkBench "splitOn abcdefghi" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn (toarr "abcdefghi") FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitOn catcatcatcatcat" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn (toarr "catcatcatcatcat") FL.drain
                        $ FH.read inh) -- >>= print
                , mkBench "splitOn abc...xyz" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitOn
                                    (toarr "abcdefghijklmnopqrstuvwxyz")
                                    FL.drain
                            $ FH.read inh) -- >>= print
                , mkBench "splitSuffixOn abc...xyz" href $ do
                    Handles inh _ <- readIORef href
                    (S.length $ FL.splitSuffixOn
                                    (toarr "abcdefghijklmnopqrstuvwxyz")
                                    FL.drain
                            $ FH.read inh) -- >>= print
                ]
            ]
#endif
        ]

    where

    mkBench :: NFData b => String -> IORef Handles -> IO b -> Benchmark
    mkBench name ref action =
        bench name $ perRunEnv (do
                (Handles inh outh) <- readIORef ref
                hClose inh
                hClose outh
                inHandle <- openFile infile ReadMode
                outHandle <- openFile outfile WriteMode
                writeIORef ref (Handles inHandle outHandle)
            )
            (\_ -> action)
