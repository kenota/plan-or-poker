module Evergreen.Type.V4 exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type UiPath
    = AskingUsername
    | UsernameReceived


type alias BackendClientState =
    { id : ClientId
    , backendModel : BackendModel
    }


type alias FrontendModel =
    { key : Key
    , username : String
    , path : UiPath
    , url : Url.Url
    , refinementState : Maybe BackendClientState
    , proposedQuestion : String
    , userListVisible : Bool
    , settingsMenuVisible : Bool
    }


type alias Vote =
    { clientId : ClientId
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
    { id : ClientId
    , name : String
    , lastPing : Maybe Int
    }


type alias BackendModel =
    { currentQuestion : String
    , state : RefinementState
    , currentUsers : Dict ClientId User
    , currentTime : Maybe Int
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
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


initFrontend : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
initFrontend url key =
    ( { key = key
      , url = url
      , username = ""
      , path = AskingUsername
      , refinementState = Nothing
      , proposedQuestion = ""
      , settingsMenuVisible = False
      , userListVisible = False
      }
    , Cmd.none
    )


emptyBackendModel : BackendModel
emptyBackendModel =
    { currentQuestion = ""
    , state = NoQuestion
    , currentUsers = Dict.empty
    , currentTime = Nothing
    }


initBackend : ( BackendModel, Cmd BackendMsg )
initBackend =
    ( emptyBackendModel
    , Cmd.none
    )
