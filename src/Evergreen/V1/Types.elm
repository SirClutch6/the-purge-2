module Evergreen.V1.Types exposing (..)

import Browser
import Evergreen.V1.Frontend.Model
import Evergreen.V1.Types.Actions
import Evergreen.V1.Types.Enemy
import Evergreen.V1.Types.Levels
import Evergreen.V1.Types.Player
import Time
import Url


type alias FrontendModel =
    Evergreen.V1.Frontend.Model.Model


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | StartGame
    | ChoseRogue
    | ChoseSpy
    | ChoseWarrior
    | ChoseTank
    | ReturnToClassChoice
    | AdjustAttr Evergreen.V1.Types.Player.Attribute Int
    | CalculateHP
    | ConfirmPointsBuyInitial
    | ConfirmPointsBuyBetweenLevel
    | EnterBuilding
    | SearchForAnotherWayIn
    | EnterRoom
    | StartRound
    | NextTurn
    | FinishRoom
    | FinishLevel
    | BetweenRoomRest Evergreen.V1.Types.Levels.Room
    | BetweenRoomLoot Evergreen.V1.Types.Levels.Room
    | BetweenRoomRush Evergreen.V1.Types.Levels.Room
    | PurchaseWater
    | PurchaseJuice
    | PurchaseHotChocolate
    | PurchaseProteinShake
    | BetweenLevelPurchaseFinished
    | SelectEnemy Int
    | PlayerAttack Evergreen.V1.Types.Actions.Distance Int
    | PlayerMove Evergreen.V1.Types.Actions.Direction
    | PlayerTaunt
    | PlayerFuriousAttack
    | PlayerStealth
    | PlayerHeal
    | EnemyAction Evergreen.V1.Types.Enemy.Enemy
    | ShowHelp
    | GameOver
    | ResetGame
    | JumpToFinish
    | GetCleanTime Time.Posix
    | TestChangeDistance


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
