module Evergreen.V7.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type UiPath
    = AskingUsername
    | UsernameReceived


type alias Vote =
    { clientId : Lamdera.ClientId
    , score : Int
    , name : String
    }


type alias Question =
    { question : String
    , votes : List Vote
    }


type RefinementState
    = NoQuestion
    | Voting Question
    | VoteComplete Question


type alias User =
    { id : Lamdera.ClientId
    , name : String
    , lastPing : Maybe Int
    }


type alias BackendModel =
    { currentQuestion : String
    , state : RefinementState
    , currentUsers : Dict.Dict Lamdera.ClientId User
    , currentTime : Maybe Int
    }


type alias BackendClientState =
    { id : Lamdera.ClientId
    , backendModel : BackendModel
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , username : String
    , path : UiPath
    , url : Url.Url
    , refinementState : Maybe BackendClientState
    , proposedQuestion : String
    , userListVisible : Bool
    , settingsMenuVisible : Bool
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UsernameChanged String
    | Join
    | QuestionChanged String
    | NoOpFrontendMsg
    | SubmitQuestion
    | SubmitVote Int
    | NewTime Time.Posix
    | UserListToggle
    | SettingsMenuToggle
    | RequestServerReset


type ToBackend
    = ClientJoin String
    | StartVote String
    | Ping Time.Posix String
    | ClientVote Int
    | RequestReset


type BackendMsg
    = NoOpBackendMsg
    | Tick Time.Posix
    | CheckVoting Time.Posix


type ToFrontend
    = NoOpToFrontend
    | ServerState BackendClientState
    | Reset
