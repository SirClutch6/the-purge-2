module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Task

import Frontend.Model as FM


type alias FrontendModel =
    FM.Model


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend

performMessage : FrontendMsg -> Cmd FrontendMsg
performMessage msg =
    Task.perform identity <| Task.succeed msg