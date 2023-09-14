module Logic.RandomGen exposing (..)

import Random

-- import Types

oneToCustom : Int -> Random.Generator Int
oneToCustom n =
    Random.int 1 n

oneToHundred : Random.Generator Int
oneToHundred =
    Random.int 1 100

oneToTwenty : Random.Generator Int
oneToTwenty =
    Random.int 1 20

zeroToNine : Random.Generator Int
zeroToNine =
    Random.int 0 9

oneToFive : Random.Generator Int
oneToFive =
    Random.int 1 5

zeroToOne : Random.Generator Int
zeroToOne =
    Random.int 0 1

randomListGen : Int -> Random.Generator Int -> Random.Generator (List Int)
randomListGen n gen =
    Random.list n gen