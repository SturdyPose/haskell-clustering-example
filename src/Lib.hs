module Lib
    ( someFunc
    ) where
import AI.Clustering.KMeans (kmeans, KMeansOpts (KMeansOpts, kmeansClusters), defaultKMeansOpts, KMeans (..))
import qualified Data.Matrix.Unboxed as UM
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map.Strict as Map

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import GHC.Float (double2Int)

testData:: [[Double]]
testData = [
       [ 19.44228191,  10.01954739],
       [ 19.53244135,   9.27926943],
       [ 19.47813524,  10.11702067],
       [ 19.53707403,  10.34821127],
       [ 20.22351335,  10.15144988],
       [ 19.28171056,   9.97469793],
       [ 20.02206671,  10.44740153],
       [ 20.50197584,  10.21343973],
       [ 19.21130219,  10.12152178],
       [ 20.12801058,   9.72609094],
       [ 19.83785357,  10.26330778],
       [ 20.41310726,   9.46206339],
       [ 19.45552404,   9.40547838],
       [ 19.89360546,  10.44014115],
       [ 20.36125385,   9.90567986],
       [ 20.35849112,  10.48796981],
       [ 20.88746346,  10.35747471],
       [ 20.49813333,  10.65385512],
       [ 20.62931967,  10.31353257],
       [ 19.71910217,  10.11721853],
       [ 23.88999494,   9.49780921],
       [ 23.53417881,  10.22200125],
       [ 24.20515374,  10.20352821],
       [ 24.27042841,   8.94589297],
       [ 24.33017732,  10.46796864],
       [ 24.1848164 ,   9.39154275],
       [ 24.25760608,  10.19827234],
       [ 24.14990874,   9.95071613],
       [ 23.79546847,   9.40557273],
       [ 24.12813273,   9.45732106],
       [ 24.88221126,  10.67626518],
       [ 24.24725526,   9.38441264],
       [ 23.70951565,   9.48528594],
       [ 23.84057452,  10.4090588 ],
       [ 25.30846271,  10.06039913],
       [ 23.88388447,   9.42833493],
       [ 23.77114657,   9.98375689],
       [ 23.40834507,  10.49376609],
       [ 23.98728878,  10.22910904],
       [ 22.96215117,   9.97424991],
       [ 19.58555678,  11.8102289 ],
       [ 20.03125014,  10.6541982 ],
       [ 20.61266943,  11.811877  ],
       [ 19.81763777,  12.87517492],
       [ 19.70835143,  11.64469843],
       [ 20.08416497,  11.49784966],
       [ 19.82591625,  11.04150988],
       [ 19.28604287,  12.57047196],
       [ 19.60033648,  12.37376337],
       [ 19.73600258,  11.13350926],
       [ 20.6532054 ,  12.40243406],
       [ 19.88155388,  11.5102145 ],
       [ 20.16608498,  11.52421444],
       [ 19.84189865,  12.16468441],
       [ 20.07146249,  11.9723343 ],
       [ 19.80540283,  12.25633198],
       [ 20.36592307,  12.04606745],
       [ 19.76154363,  12.04326451],
       [ 21.13411946,  11.97343609],
       [ 20.00825767,  12.22197451]]

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

insertElementToVectorMap:: UV.Unbox a => Map.Map Int (UV.Vector a) -> Int -> a -> Map.Map Int (UV.Vector a)
insertElementToVectorMap mapToInsert key val = 
    case Map.lookup key mapToInsert of 
        Nothing -> Map.insert key (UV.singleton val) mapToInsert
        Just vector -> Map.insert key (vector `UV.snoc` val) mapToInsert

plotClustering:: Int -> FilePath -> EC (Layout Double Double) ()
plotClustering numberOfClusters fileName = do 
    let mat = UM.fromLists testData
    let options = defaultKMeansOpts { kmeansClusters = False }

    let kMeansResult = kmeans numberOfClusters mat options

    let zippedCats = zipWith (\(a:b:_) y -> (a,b,y)) testData ((UV.toList . membership) kMeansResult)

    let result = foldr (\(x,y,cat) mapToInsert -> insertElementToVectorMap mapToInsert cat (x,y)) Map.empty zippedCats 
    layout_title .= "Clusters " ++ fileName
    setColors [opaque blue, opaque red, opaque green, opaque black, opaque greenyellow, opaque grey]
    -- plot (line "am" [signal [0,(0.5)..400]])
    sequence_ $ Map.mapWithKey (\key v -> 
        plot (points ("cluster " ++ show key) (UV.toList v))
        ) result

runKMeansAndSaveToSvg:: Int -> FilePath -> IO()
runKMeansAndSaveToSvg numberOfClusters fileName = do 

    toFile def fileName (plotClustering numberOfClusters fileName)

someFunc :: IO ()
someFunc = do 
    -- plot (points "am points" (map (\(a:b:_) -> (a,b)) testData))
    let numberOfPointsToRender = 10.0
    let plots = map (\x -> StackedLayout $ execEC $ plotClustering x ("example_linear_" ++ show x)) ([2..(double2Int numberOfPointsToRender)] :: [Int])
    
    let a = StackedLayouts { _slayouts_layouts = plots, _slayouts_compress_legend = False }
    let renderedLayouts = renderStackedLayouts a 

    _ <- renderableToFile (def & fo_size .~ (500,300 * numberOfPointsToRender)) "example_linear.svg" renderedLayouts 

    return ()
    -- mapM_ (\x -> runKMeansAndSaveToSvg x ("example_linear" ++ show x ++ ".svg")) [1..10]
