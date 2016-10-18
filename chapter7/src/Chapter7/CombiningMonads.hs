{-# LANGUAGE FlexibleContexts #-}

module Chapter7.CombiningMonads where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS

pathsWriter :: [(Int,Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end = map execWriter (pathsWriter' edges start end)

pathsWriter' :: [(Int,Int)] -> Int -> Int -> [Writer [Int] ()]
pathsWriter' edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- pathsWriter' edges e_end end
                   return $ do tell [start]
                               subpath
   in if start == end then tell [start] : e_paths else e_paths

graph1 :: [(Int, Int)]
graph1 = [(2013,501),(2013,1004),(501,2558),(1004,2558)]

pathsWriterT :: [(Int,Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end)

pathsWriterT' :: [(Int,Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriterT' edges start end =
  let e_paths = do (e_start, e_end) <- lift edges
                   guard $ e_start == start
                   tell [start]
                   pathsWriterT' edges e_end end
   in if start == end then tell [start] `mplus` e_paths else e_paths

readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do x <- ask
                         lift . tell $ show x
                         return $ x + 1

readerWriterExampleG :: (MonadReader Int m, MonadWriter String m) => m Int
readerWriterExampleG = do x <- ask
                          tell $ show x
                          return $ x + 1

factorial' :: StateT Integer (State Integer) Integer
factorial' = do n <- get
                if n == 0
                   then lift get
                   else do lift . modify $ (*n)
                           modify $ \x -> x - 1
                           factorial'

factorial :: Integer -> Integer
factorial x = execState (execStateT factorial' x) 1
