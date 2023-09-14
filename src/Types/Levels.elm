module Types.Levels exposing (..)

import Types.Enemy as Enemy

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

level1 : Level
level1 =
    { level = 1
    , image = "" -- TODO add image
    , rooms =
        [ { num = 1
          , items = [ Stapler ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemySpy 1 1
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemySpy 1 2
                      ] -- TODO make these random from a pool
          }
        , { num = 2
          , items = [ StandingFan ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyWarrior 1 1]
          }
        , { num = 3
          , items = [ Chair ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue 1 1]
          }
        , { num = 4
          , items = [ Mug ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyTank 1 1]
          }
        , { num = 5
          , items = [ Stapler ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue 1 1
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemySpy 1 2
                      ]
          }
        , { num = 6
          , items = [ StandingFan ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyBoss 1 1]
          }
        ]
    }

level2 : Level
level2 =
    { level = 2
    , image = "" -- TODO add image
    , rooms =
        [ { num = 1
          , items = [ Stapler ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue 2 1
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemySpy 2 2
                      ] -- TODO make these random from a pool
          }
        , { num = 2
          , items = [ StandingFan ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyTank 2 1
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemyWarrior 2 2
                      ]
          }
        , { num = 3
          , items = [ Chair ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyWarrior 2 1
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemySpy 2 2 
                      ]
          }
        , { num = 4
          , items = [ Mug ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue 2 1
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue 2 2
                      ]
          }
        , { num = 5
          , items = [ Stapler ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyBoss 2 1]
          }
        ]
    }

level3 : Level
level3 =
    { level = 3
    , image = "" -- TODO add image
    , rooms =
        [ { num = 1
          , items = [ Stapler ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyTank 3 1
                      ] -- TODO make these random from a pool
          }
        , { num = 2
          , items = [ StandingFan ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemySpy 3 1
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemySpy 3 2
                      ]
          }
        , { num = 3
          , items = [ Chair ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyWarrior 3 1
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemySpy 3 2
                      , Enemy.calculateEnemyHP <| Enemy.baseEnemyRogue 3 3
                      ]
          }
        , { num = 5
          , items = [ Stapler ]
          , enemies = [ Enemy.calculateEnemyHP <| Enemy.baseEnemyBoss 2 1]
          }
        ]
    }

level4 : Level
level4 =
    { level = 4
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
    }

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