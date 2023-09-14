module Types.Levels exposing (..)

import Types.Enemy as Enemy

import Logic.RandomGen as RNG

import Random

import Array

type Item
    = Stapler
    | StandingFan
    | Chair
    | Mug

type RoomEntryType
    = DexSneak
    | ChrSneak
    | FailedSneak
    | Rush
    | Normal

type alias Room =
    { num : Int
    , items : List Item
    , enemies : List Enemy.Enemy
    }

type alias Level =
    { level : Int
    , image : String
    , rooms : List Room
    }

level0 : Level
level0 = 
  { level = 0
  , image = ""
  , rooms = [defaultRoom]
  }

level1 : Random.Seed -> (Level, Random.Seed)
level1 seed =
  let
    (list_enemies, new_seed) = Random.step (RNG.randomListGen (6) RNG.zeroToThree) seed
    enemy_array = Array.fromList list_enemies
    enemy1 = enemyHelper (Array.get 0 enemy_array |> Maybe.withDefault 5) 1 1
    enemy2 = enemyHelper (Array.get 1 enemy_array |> Maybe.withDefault 5) 1 1
    enemy3 = enemyHelper (Array.get 2 enemy_array |> Maybe.withDefault 5) 1 1
    enemy4 = enemyHelper (Array.get 3 enemy_array |> Maybe.withDefault 5) 1 1
    enemy5 = enemyHelper (Array.get 4 enemy_array |> Maybe.withDefault 5) 1 1
    enemy6 = enemyHelper (Array.get 5 enemy_array |> Maybe.withDefault 5) 1 2
  in
    ({ level = 1
    , image = "" -- TODO add image
    , rooms =
        [ { num = 1
          , items = [ Stapler ]
          , enemies = [ enemy1
                      -- , Enemy.calculateEnemyHP <| Enemy.baseEnemySpy 1 2
                      ] -- TODO make these random from a pool
          }
        , { num = 2
          , items = [ StandingFan ]
          , enemies = [ enemy2 ]
          }
        , { num = 3
          , items = [ Chair ]
          , enemies = [ enemy3 ]
          }
        , { num = 4
          , items = [ Mug ]
          , enemies = [ enemy4 ]
          }
        , { num = 5
          , items = [ Stapler ]
          , enemies = [ enemy5
                      , enemy6
                      ]
          }
        , { num = 6
          , items = [ StandingFan ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyBoss 1 1]
          }
        ]
    }, new_seed)

level2 : Random.Seed -> (Level, Random.Seed)
level2 seed =
  let
    (list_enemies, new_seed) = Random.step (RNG.randomListGen (6) RNG.zeroToThree) seed
    enemy_array = Array.fromList list_enemies
    enemy1 = enemyHelper (Array.get 0 enemy_array |> Maybe.withDefault 5) 2 1
    enemy2 = enemyHelper (Array.get 1 enemy_array |> Maybe.withDefault 5) 2 1
    enemy3 = enemyHelper (Array.get 2 enemy_array |> Maybe.withDefault 5) 2 2
    enemy4 = enemyHelper (Array.get 3 enemy_array |> Maybe.withDefault 5) 2 1
    enemy5 = enemyHelper (Array.get 4 enemy_array |> Maybe.withDefault 5) 2 1
    enemy6 = enemyHelper (Array.get 5 enemy_array |> Maybe.withDefault 5) 2 2
  in
    ({ level = 2
    , image = "" -- TODO add image
    , rooms =
        [ { num = 1
          , items = [ Stapler ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue 2 1
                      , enemy1
                      ] -- TODO make these random from a pool
          }
        , { num = 2
          , items = [ StandingFan ]
          , enemies = [ enemy2
                      , enemy3
                      ]
          }
        , { num = 3
          , items = [ Chair ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyWarrior 2 1
                      , enemy4
                      ]
          }
        , { num = 4
          , items = [ Mug ]
          , enemies = [ enemy5
                      , enemy6
                      ]
          }
        , { num = 5
          , items = [ Stapler ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyBoss 2 1]
          }
        ]
    }, new_seed)

level3 : Random.Seed -> (Level, Random.Seed)
level3 seed =
  let
    (list_enemies, new_seed) = Random.step (RNG.randomListGen (5) RNG.zeroToThree) seed
    enemy_array = Array.fromList list_enemies
    enemy1 = enemyHelper (Array.get 0 enemy_array |> Maybe.withDefault 5) 3 1
    enemy2 = enemyHelper (Array.get 1 enemy_array |> Maybe.withDefault 5) 3 1
    enemy3 = enemyHelper (Array.get 2 enemy_array |> Maybe.withDefault 5) 3 2
    enemy4 = enemyHelper (Array.get 3 enemy_array |> Maybe.withDefault 5) 3 1
    enemy5 = enemyHelper (Array.get 4 enemy_array |> Maybe.withDefault 5) 3 2
  in
    ({ level = 3
    , image = "" -- TODO add image
    , rooms =
        [ { num = 1
          , items = [ Stapler ]
          , enemies = [ enemy1
                      ] -- TODO make these random from a pool
          }
        , { num = 2
          , items = [ StandingFan ]
          , enemies = [ enemy2
                      , enemy3
                      ]
          }
        , { num = 3
          , items = [ Chair ]
          , enemies = [ enemy4
                      , enemy5
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue 3 3
                      ]
          }
        , { num = 5
          , items = [ Stapler ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyBoss 2 1]
          }
        ]
    }, new_seed)

level4 : Random.Seed -> (Level, Random.Seed)
level4 seed =
    ({ level = 4
    , image = "" -- TODO add image
    , rooms =
        [ { num = 1
          , items = [ ]
          , enemies = [ Enemy.calculateEnemyHP 
                            <| Enemy.getCaptainConstitution
                            <| Enemy.baseEnemyCaptain 4 1
                      ] 
          }
        ]
    }, seed)

itemToString : Item -> String
itemToString item =
  case item of
      Stapler ->
          "Stapler"

      StandingFan ->
          "Standing Fan"

      Chair ->
          "Chair"

      Mug ->
          "Mug"

defaultRoom : Room
defaultRoom =
  { num = -1
  , items = []
  , enemies = []
  }

enemyHelper : Int -> Int -> Int -> Enemy.Enemy
enemyHelper rng_num level id =
  case rng_num of
    0 -> Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue level id

    1 -> Enemy.calculateEnemyHP <| Enemy.baseEnemySpy level id

    2 -> Enemy.calculateEnemyHP <| Enemy.baseEnemyWarrior level id

    3 -> Enemy.calculateEnemyHP <| Enemy.baseEnemyTank level id

    _ -> Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue level id