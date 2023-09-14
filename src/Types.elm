module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Task

import Frontend.Model as FM

import Types.Player as Player
import Types.Levels as Level
import Types.Actions as Action
import Types.Enemy as Enemy
import Time
import Types.Player exposing (Player)


type alias FrontendModel =
    FM.Model


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | StartGame
    | ChoseRogue
    | ChoseSpy
    | ChoseWarrior
    | ChoseTank
    | AdjustAttr Player.Attribute Int
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
    | BetweenRoomRest Level.Room
    | BetweenRoomLoot Level.Room
    | BetweenRoomRush Level.Room
    | PurchaseWater
    | PurchaseJuice
    | PurchaseHotChocolate
    | PurchaseProteinShake
    | BetweenLevelPurchaseFinished
    -- Player Actions
    | PlayerAttack Action.Distance Int
    | PlayerMove Action.Direction
    | PlayerTaunt
    | PlayerFuriousAttack
    | PlayerStealth
    | PlayerHeal
    | EnemyAction Enemy.Enemy
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

performMessage : FrontendMsg -> Cmd FrontendMsg
performMessage msg =
    Task.perform identity <| Task.succeed msg