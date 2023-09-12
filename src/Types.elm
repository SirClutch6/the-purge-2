module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Task

import Frontend.Model as FM

import Types.Player as Player


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
    | ConfirmPointsBuy
    | EnterRoom
    | FinishRoom
    | FinishLevel
    | BetweenRoomRest
    | BetweenRoomLoot
    | BetweenRoomRush


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend

performMessage : FrontendMsg -> Cmd FrontendMsg
performMessage msg =
    Task.perform identity <| Task.succeed msg