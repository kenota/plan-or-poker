module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type UiPath
    = AskingUsername
    | UsernameReceived


type alias FrontendModel =
    { key : Key
    , username : String
    , path : UiPath
    , refinementState : Maybe BackendModel
    }


type alias Vote =
    { user : String
    , score : Int
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
    }


type alias BackendModel =
    { currentQuestion : String
    , state : RefinementState
    , currentUsers : List User
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | UsernameChanged String
    | Join
    | NoOpFrontendMsg


type ToBackend
    = ClientJoin String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | List User
