module Types.RandomGen exposing (..)

import Random

import Types

type Msg = NewNumber Int

oneToTwenty : Random.Generator Int
oneToTwenty =
    Random.int 1 20

oneToFive : Random.Generator Int
oneToFive =
    Random.int 1 5