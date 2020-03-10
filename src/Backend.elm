module Backend exposing (..)

import Dict exposing (Dict)
import Html
import Lamdera exposing (ClientId, SessionId)
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Time.every 1000 Tick


init : ( Model, Cmd BackendMsg )
init =
    ( { currentQuestion = ""
      , state = NoQuestion
      , currentUsers = Dict.empty
      , currentTime = Nothing
      }
    , Cmd.none
    )


isUserActive : User -> Int -> Int -> Bool
isUserActive user currentTime timeout =
    case user.lastPing of
        Nothing ->
            False

        Just lastPing ->
            currentTime - lastPing < timeout


cleanupLostClients : Dict ClientId User -> Maybe Int -> Int -> Dict ClientId User
cleanupLostClients users currentTime timeoutMs =
    case currentTime of
        Nothing ->
            users

        Just t ->
            Dict.filter (\k v -> isUserActive v t timeoutMs) users


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        Tick t ->
            let
                newTime =
                    Just (Time.posixToMillis t)

                newUserMap =
                    cleanupLostClients model.currentUsers newTime (5 * 1000)

                newModel =
                    { model | currentUsers = newUserMap, currentTime = newTime }
            in
            ( newModel
            , Cmd.batch
                [ broadcastCurrentState newModel.currentUsers newModel
                ]
            )


getClientIdList : List User -> List ClientId
getClientIdList l =
    List.map (\e -> e.id) l


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ClientJoin s ->
            let
                m =
                    { model
                        | currentUsers =
                            Dict.insert clientId
                                { name = s
                                , id = clientId
                                , lastPing = model.currentTime
                                }
                                model.currentUsers
                    }

                sendHello =
                    Lamdera.sendToFrontend clientId (ServerState { id = clientId, backendModel = m })
            in
            ( m
            , Cmd.batch [ sendHello, broadcastCurrentState m.currentUsers m ]
            )

        StartVote q ->
            let
                m =
                    { model
                        | currentQuestion = q
                        , state = Voting { question = q, votes = [] }
                    }
            in
            ( m, broadcastCurrentState m.currentUsers m )

        ClientVote score ->
            case model.state of
                Voting q ->
                    let
                        updatedVotes =
                            saveVote model q.votes clientId score

                        newModel =
                            if List.length updatedVotes /= Dict.size model.currentUsers then
                                { model | state = Voting { question = q.question, votes = updatedVotes } }

                            else
                                { model | state = VoteComplete { question = q.question, votes = updatedVotes } }
                    in
                    ( newModel
                    , broadcastCurrentState model.currentUsers newModel
                    )

                NoQuestion ->
                    ( model, Cmd.none )

                VoteComplete q ->
                    ( model, Cmd.none )

        Ping t name ->
            let
                newModel =
                    { model
                        | currentUsers =
                            Dict.insert clientId
                                { id = clientId
                                , lastPing = model.currentTime
                                , name = name
                                }
                                model.currentUsers
                    }
            in
            ( newModel, Cmd.none )


broadcast clients msg =
    clients
        |> List.map (\c -> Lamdera.sendToFrontend c msg)
        |> Cmd.batch


broadcastCurrentState : Dict ClientId User -> BackendModel -> Cmd BackendMsg
broadcastCurrentState clients m =
    clients
        |> Dict.map (\clientId conn -> Lamdera.sendToFrontend conn.id (ServerState { id = conn.id, backendModel = m }))
        |> Dict.values
        |> Cmd.batch


clientNameById : BackendModel -> ClientId -> String
clientNameById model clientId =
    let
        u =
            Dict.get clientId model.currentUsers
    in
    case u of
        Just user ->
            user.name

        Nothing ->
            "Unknown"


saveVote : BackendModel -> List Vote -> ClientId -> Int -> List Vote
saveVote m votes id score =
    let
        found =
            List.head <| List.filter (\x -> x.clientId == id) votes

        newScore =
            { clientId = id, score = score, name = clientNameById m id }
    in
    case found of
        Nothing ->
            newScore :: votes

        Just vote ->
            newScore :: List.filter (\x -> x.clientId /= id) votes


upsertUser : List User -> User -> List User
upsertUser l u =
    let
        found =
            List.head <| List.filter (\x -> x.id == u.id) l
    in
    case found of
        Nothing ->
            u :: l

        Just existing ->
            u :: List.filter (\x -> x.id /= u.id) l
