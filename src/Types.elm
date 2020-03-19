module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
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
