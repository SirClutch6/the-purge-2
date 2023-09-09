module Types.Levels exposing (..)

import Enemy as Enemy

type Item
    = Stapler
    | StandingFan
    | Chair
    | Mug

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
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyWarrior 1 ] -- TODO make these random from a pool
          }
        , { num = 2
          , items = [ StandingFan ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemySpy 1 ]
          }
        , { num = 3
          , items = [ Chair ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyRogue 1 ]
          }
        , { num = 4
          , items = [ Mug ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyTank 1 ]
          }
        , { num = 5
          , items = [ Stapler ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyRogue 1 
                      , calculateEnemyHP <| Enemy.baseEnemySpy 1 
                      ]
          }
        , { num = 6
          , items = [ StandingFan ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyBoss 1 ]
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
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyRogue 2
                      , calculateEnemyHP <| Enemy.baseEnemySpy 2 
                      ] -- TODO make these random from a pool
          }
        , { num = 2
          , items = [ StandingFan ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyTank 2
                      , calculateEnemyHP <| Enemy.baseEnemyWarrior 2 
                      ]
          }
        , { num = 3
          , items = [ Chair ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyWarrior 2
                      , calculateEnemyHP <| Enemy.baseEnemySpy 1 
                      ]
          }
        , { num = 4
          , items = [ Mug ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyRogue 2
                      , calculateEnemyHP <| Enemy.baseEnemyRogue 2 
                      ]
          }
        , { num = 5
          , items = [ Stapler ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyBoss 2 ]
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
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyTank 3
                      ] -- TODO make these random from a pool
          }
        , { num = 2
          , items = [ StandingFan ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemySpy 3
                      , calculateEnemyHP <| Enemy.baseEnemySpy 3
                      ]
          }
        , { num = 3
          , items = [ Chair ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyWarrior 3
                      , calculateEnemyHP <| Enemy.baseEnemySpy 3
                      , calculateEnemyHP <| Enemy.baseEnemyRogue 3
                      ]
          }
        , { num = 5
          , items = [ Stapler ]
          , enemies = [ calculateEnemyHP <| Enemy.baseEnemyBoss 2 ]
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
          , enemies = [ calculateEnemyHP 
                            <| getCaptainConstitution
                            <| Enemy.baseEnemyCaptain 4 
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