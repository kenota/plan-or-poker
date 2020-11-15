module Evergreen.V1.Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
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
    , refinementState : Maybe BackendClientState
    , proposedQuestion : String
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
    , lastPong : Time.Posix
    }


type alias BackendModel =
    { currentQuestion : String
    , state : RefinementState
    , currentUsers : List User
    , currentTime : Time.Posix
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


type ToBackend
    = ClientJoin String
    | StartVote String
    | Pong Time.Posix
    | ClientVote Int


type BackendMsg
    = NoOpBackendMsg
    | Tick Time.Posix


type ToFrontend
    = NoOpToFrontend
    | ServerState BackendClientState
    | Ping Time.Posix
