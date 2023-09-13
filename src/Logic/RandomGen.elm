module Logic.RandomGen exposing (..)

import Random

-- import Types

-- type Msg = NewNumber Int

oneToTwenty : Random.Generator Int
oneToTwenty =
    Random.int 1 20

zeroToNine : Random.Generator Int
zeroToNine =
    Random.int 0 9

oneToFive : Random.Generator Int
oneToFive =
    Random.int 1 5

randomListGen : Int -> Random.Generator Int -> Random.Generator (List Int)
randomListGen n gen =
    Random.list n gen