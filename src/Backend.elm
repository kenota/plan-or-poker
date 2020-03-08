module Backend exposing (..)

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
      , currentUsers = []
      , currentTime = Time.millisToPosix 0
      }
    , Cmd.none
    )


cleanupLostClients : List User -> Time.Posix -> Int -> List User
cleanupLostClients users t timeoutMs =
    List.filter
        (\u ->
            Time.posixToMillis u.lastPong
                == 0
                || Time.posixToMillis t
                - Time.posixToMillis u.lastPong
                < timeoutMs
        )
        users


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        Tick t ->
            let
                newUserList =
                    cleanupLostClients model.currentUsers t (5 * 1000)

                newModel =
                    { model | currentUsers = newUserList }
            in
            ( newModel
            , Cmd.batch
                [ broadcast (getClientIdList newModel.currentUsers) (Ping t)
                , broadcastCurrentState newModel.currentUsers newModel
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
                            upsertUser model.currentUsers
                                { name = s
                                , id = clientId
                                , lastPong = model.currentTime
                                }
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
                            if List.length updatedVotes /= List.length model.currentUsers then
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

        Pong t ->
            let
                maybeUser =
                    List.head (List.filter (\x -> x.id == clientId) model.currentUsers)
            in
            case maybeUser of
                Nothing ->
                    ( model, Cmd.none )

                Just u ->
                    let
                        updated =
                            { u | lastPong = t }
                    in
                    ( { model | currentUsers = upsertUser model.currentUsers updated }, Cmd.none )


broadcast clients msg =
    clients
        |> List.map (\c -> Lamdera.sendToFrontend c msg)
        |> Cmd.batch


broadcastCurrentState clients m =
    clients
        |> List.map (\conn -> Lamdera.sendToFrontend conn.id (ServerState { id = conn.id, backendModel = m }))
        |> Cmd.batch


clientNameById : BackendModel -> ClientId -> String
clientNameById model clientId =
    let
        u =
            List.head <| List.filter (\x -> x.id == clientId) model.currentUsers
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
