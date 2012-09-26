{-# LANGUAGE CPP #-}
-- {-# LANGUAGE QuasiQuotes #-}
#define USE_STENCIL  0
#define USE_CONVOLVE 0
module Main where
import Data.Array.Repa as R hiding (map)
#if   USE_STENCIL
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
#elif USE_CONVOLVE
import Data.Array.Repa.Algorithms.Convolve
#endif
import Data.Array.Repa.Algorithms.Randomish as R (randomishIntArray)
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, utctDayTime)

type Board t = R.Array t DIM2 Int
type BoardSize = (Int, Int)
type BoardIx = (Int, Int)
type CellStatus = Int

boardSize :: BoardSize
--boardSize = (30,80)
boardSize = (47,193)
--boardSize = (1000,1000)

initLifeGame :: BoardSize -> IO (Board U)
initLifeGame (x,y) = do
    seed <- getCurrentTime
    let seed' = fromEnum $ utctDayTime seed
    return $ R.randomishIntArray (ix2 x y) 0 1 seed'

#if USE_STENCIL
step :: (Monad m) => Board U -> m (Board U)
step board = R.computeP
           $ szipWith lifeCheck board
           $ mapStencil2 (BoundConst 0) sumAround
           $ board
  where
    {-# INLINE sumAround #-}
    sumAround =
{-
      -- We can use QuasiQuote for generating below code.
      [stencil2|  1 1 1
                  1 0 1
                  1 1 1 |]
-}
      -- makeStencil (Z :. 3 :. 3)
      makeStencil2 3 3 -- this function doesn't work when stencil size > 7 x 7
      $ \ix -> case ix of
               Z :. -1 :. -1 -> Just 1
               Z :. -1 :.  0 -> Just 1
               Z :. -1 :.  1 -> Just 1
               Z :.  0 :. -1 -> Just 1
               Z :.  0 :.  1 -> Just 1
               Z :.  1 :. -1 -> Just 1
               Z :.  1 :.  0 -> Just 1
               Z :.  1 :.  1 -> Just 1
               _             -> Nothing

#elif USE_CONVOLVE

step :: (Monad m) => Board U -> m (Board U)
step board = do
  board' <- convolveOutP (outAs 0) sumAround board
  computeP $ szipWith lifeCheck board board'
  where
    {-# INLINE sumAround #-}
    sumAround = fromListUnboxed (Z :. 3 :. 3)
                 [1,1,1
                 ,1,0,1
                 ,1,1,1]

#else

step :: (Monad m) => Board U -> m (Board U)
step board = R.computeP
           $ szipWith lifeCheck board
           $ R.traverse board id sumAround
  where
    _ :. height :. width = extent board

    -- This INLINE exhaust simplifier ticks.
    -- {-# INLINE sumAround #-}
    -- check around cells, count living cells
    sumAround get (Z :. i :. j)
{-
      = lifeCheck (get curIx)
            $ outIsZero get (Z :. (i-1) :. (j-i))
-}
            = outIsZero get (Z :. (i-1) :. (j-i))
            + outIsZero get (Z :. (i-1) :. j)
            + outIsZero get (Z :. (i-1) :. (j+1))
            + outIsZero get (Z :. i     :. (j-1))
            + outIsZero get (Z :. i     :. (j+1))
            + outIsZero get (Z :. (i+1) :. (j-1))
            + outIsZero get (Z :. (i+1) :. j)
            + outIsZero get (Z :. (i+1) :. (j+1))

    outIsZero get ix@(Z :. i :. j)
      = if isInternal i j
          then 0
          else get ix

    isInternal i j
        =  (i <= 0) || (i >= height - 1)
        || (j <= 0) || (j >= width  - 1)
#endif

-- return next status
lifeCheck :: CellStatus -> Int -> CellStatus
lifeCheck a 2 = a
lifeCheck _ 3 = 1
lifeCheck _ _ = 0
{-# INLINE lifeCheck #-}

repeatStep :: BoardSize -> Board U -> IO ()
repeatStep siz board = do
    -- board' <- R.computeP $ step board
    board' <- step board
--    threadDelay 10000
    threadDelay 120000
    putStr "\x1b[2J"
    putStr "\x1b[1;1H"
    printListToRect siz $ R.toList board'
    repeatStep siz board'

    where
        printListToRect :: BoardSize -> [Int] -> IO ()
--        printListToRect _ _ = print =<< getCurrentTime
        printListToRect (x,y) ls
            | length ls < y = return ()
            | otherwise = do
                mapM_ (putStr.show) $ take y ls
                putChar '\n'
                printListToRect (x,y) $ drop y ls

main :: IO ()
main = do
    board <- initLifeGame boardSize
    repeatStep boardSize board
