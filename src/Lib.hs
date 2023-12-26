{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns#-}
module Lib where

import AI.Clustering.KMeans (kmeans, KMeansOpts (KMeansOpts, kmeansClusters), defaultKMeansOpts, KMeans (..))
import qualified Data.Matrix.Unboxed as UM
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
    ( fo_size, renderableToFile, toFile )
import GHC.Float (double2Int)
import HaskelyzerTemplate.MainTemplate
import Criterion.Main (whnfIO, defaultMain)
import Criterion (bench)
import GHC.IO (unsafePerformIO)
import Text.Read (readMaybe)
import GHC.Float.RealFracMethods (int2Double)
import qualified Data.Text.IO as T
import Control.Concurrent (getNumCapabilities)

$(generateHaskalyzer "calcclusters.haskelyzer")

insertElementToVectorMap:: UV.Unbox a => Map.Map Int (UV.Vector a) -> Int -> a -> Map.Map Int (UV.Vector a)
insertElementToVectorMap mapToInsert key val =
    case Map.lookup key mapToInsert of
        Nothing -> Map.insert key (UV.singleton val) mapToInsert
        Just vector -> Map.insert key (vector `UV.snoc` val) mapToInsert


mainData:: UM.Matrix Double
mainData = UM.fromLists testData

calcKMeansForConcurrent:: IO(Int -> KMeans (UV.Vector Double))
calcKMeansForConcurrent = do
    return calcKMeans

calcKMeans:: Int -> KMeans (UV.Vector Double)
calcKMeans numOfClusters =
    let options = defaultKMeansOpts { kmeansClusters = True} in
        kmeans numOfClusters mainData options

plotClustering:: KMeans (UV.Vector Double) -> FilePath -> EC (Layout Double Double) ()
plotClustering kMeansResult fileName = do

    let zippedCats = zipWith (\(a:b:_) y -> (a,b,y)) testData ((UV.toList . membership) kMeansResult)

    let result = foldr (\(x,y,cat) mapToInsert -> insertElementToVectorMap mapToInsert cat (x,y)) Map.empty zippedCats
    layout_title .= "Clusters " ++ fileName
    setColors [opaque blue, opaque red, opaque green, opaque black, opaque greenyellow, opaque grey]
    -- plot (line "am" [signal [0,(0.5)..400]])
    sequence_ $ Map.mapWithKey (\key v ->
        plot (points ("cluster " ++ show key) (UV.toList v))
        ) result

someFunc :: IO ()
someFunc = do
    -- plot (points "am points" (map (\(a:b:_) -> (a,b)) testData))
    let numberOfPointsToRender = 65.0
    -- let plots = map (\x -> StackedLayout $ execEC $ plotClustering x ("example_linear_" ++ show x)) ([2..(double2Int numberOfPointsToRender)] :: [Int])

    -- let a = StackedLayouts { _slayouts_layouts = plots, _slayouts_compress_legend = False }
    -- let renderedLayouts = renderStackedLayouts a

    -- _ <- renderableToFile (def & fo_size .~ (500,300 * floor numberOfPointsToRender)) "example_linear.png" renderedLayouts


    let singleThreadBenchmark = whnfIO $ do 
            _ <- mapM (return . calcKMeans) [2..(double2Int numberOfPointsToRender)]
            return ()

    numberOfGroups <- calcClusters >>= return . length
    let groupedInput = inputHelper [2..(double2Int numberOfPointsToRender)] numberOfGroups

    print groupedInput

    let concurrentBenchmark =  whnfIO $ calcClusters >>= \fs -> return $ concat $ zipWith (\f inputs -> map f inputs ) fs groupedInput

    let bench1 = bench "singleThreadBenchmark" singleThreadBenchmark
    let bench2 = bench "concurrentBenchmark" concurrentBenchmark

    defaultMain [bench1, bench2]

    return ()
    -- mapM_ (\x -> runKMeansAndSaveToSvg x ("example_linear" ++ show x ++ ".svg")) [1..10]
    where
        inputHelper:: [Int] -> Int -> [[Int]]
        inputHelper indices numOfThreads = inputHelper' indices [] numOfThreads
            where
                inputHelper':: [Int] -> [[Int]] -> Int -> [[Int]]
                inputHelper' [] acc _ = acc 
                inputHelper' indices' acc numOfThreads = let (x, xs) = splitAt numOfThreads indices' in inputHelper' xs (x:acc) numOfThreads


testData:: [[Double]]
{-# NOINLINE testData#-}
testData= unsafePerformIO $ do

    file <- T.readFile "worms_64d.txt"

    let v = T.split ((==) '\n') file
    let readDouble x = case (readMaybe x :: Maybe Double) of
            Just a -> a 
            Nothing -> int2Double (read x :: Int)

    -- print $ let [x,y] = T.split ((==) ' ') $ head v in [T.unpack x, T.unpack y]

    x <- mapM (\line -> let vals = T.split ((==) ' ') line in 
                return $ map (readDouble . T.unpack) vals ) v

    print "parsed"

    return x