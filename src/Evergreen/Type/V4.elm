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
    , url : Url.Url
    , username : String
    , path : UiPath
    , refinementState : Maybe BackendClientState
    , proposedQuestion : String
    }


initFrontend : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
initFrontend url key =
    ( { key = key
      , url = url
      , username = ""
      , path = AskingUsername
      , refinementState = Nothing
      , proposedQuestion = ""
      }
    , Cmd.none
    )


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


initBackend : ( BackendModel, Cmd BackendMsg )
initBackend =
    ( { currentQuestion = ""
      , state = NoQuestion
      , currentUsers = Dict.empty
      , currentTime = Nothing
      }
    , Cmd.none
    )


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


type ToBackend
    = ClientJoin String
    | StartVote String
    | Ping Time.Posix String
    | ClientVote Int


type BackendMsg
    = NoOpBackendMsg
    | Tick Time.Posix
    | CheckVoting Time.Posix


type ToFrontend
    = NoOpToFrontend
    | ServerState BackendClientState
