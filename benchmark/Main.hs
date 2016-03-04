module Main (main) where



import           Criterion.Main
import qualified Data.Vector    as V

import qualified Sort        as Subject



main :: IO ()
main = defaultMain
    [ env (pure (V.reverse (V.generate 48 id))) (\ ~vec ->
        bgroup "Array of 48 elements, reversed"
            [ bench "Quicksort"
                    (nf Subject.quicksort vec)
            , bench "Bubblesort"
                    (nf Subject.bubblesort vec)
            , bench "Selection sort"
                    (nf Subject.selectionsort vec)
            , bench "Slowsort"
                    (nf Subject.slowsort vec)
            ] )
    , env (pure (V.reverse (V.generate 1000 id))) (\ ~vec ->
        bgroup "Array of 1000 elements, reversed"
            [ bench "Quicksort"
                    (nf Subject.quicksort vec)
            , bench "Bubblesort"
                    (nf Subject.bubblesort vec)
            , bench "Selection sort"
                    (nf Subject.selectionsort vec)
            ] )
    ]
