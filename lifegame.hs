module Main where
import Data.Array.Repa as R hiding (map)
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
