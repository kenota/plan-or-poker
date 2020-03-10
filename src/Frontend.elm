module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict as Dict
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Lamdera
import Time
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Time.every 1000 NewTime
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , username = ""
      , path = AskingUsername
      , refinementState = Nothing
      , proposedQuestion = ""
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        UsernameChanged s ->
            ( { model | username = s }, Cmd.none )

        QuestionChanged s ->
            ( { model | proposedQuestion = s }, Cmd.none )

        Join ->
            ( { model | path = UsernameReceived }, Lamdera.sendToBackend (ClientJoin model.username) )

        SubmitQuestion ->
            let
                q =
                    model.proposedQuestion
            in
            ( { model | proposedQuestion = "" }, Lamdera.sendToBackend (StartVote q) )

        SubmitVote v ->
            ( model, Lamdera.sendToBackend (ClientVote v) )

        NewTime t ->
            case model.path of
                Types.AskingUsername ->
                    ( model, Cmd.none )

                UsernameReceived ->
                    ( model, Lamdera.sendToBackend (Ping t model.username) )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ServerState s ->
            ( { model | refinementState = Just s }, Cmd.none )


manualCss =
    Html.node "style"
        []
        [ Html.text <|
            """
        @import url('https://fonts.googleapis.com/css?family=Open+Sans&display=swap')
        html { font-family 'Open Sans', sans; }
        """
        ]


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Planning poker"
    , body = [ mainLayout model ]
    }


headerStyle =
    [ Font.size 32
    , E.width E.fill
    , E.height (E.px 60)
    , Background.color currentTheme.primary
    , Font.color currentTheme.invertedText
    , Border.shadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 1
        , color = currentTheme.text
        }
    ]


heading1 =
    [ Font.size 25
    ]


normalText =
    [ Font.size 12 ]


joinBlock : FrontendModel -> E.Element FrontendMsg
joinBlock m =
    E.row [ E.centerX, E.width (E.fill |> E.maximum 800), E.spacing 10, E.padding 10 ]
        [ Input.text []
            { label = Input.labelLeft [ E.padding 10 ] (E.text "Name")
            , onChange = newName
            , placeholder = Nothing
            , text = m.username
            }
        , Input.button
            [ Background.color currentTheme.secondary
            , Font.color currentTheme.invertedText
            , E.padding 10
            , E.height E.fill
            , Border.rounded 2
            ]
            { label = E.text "Join"
            , onPress = Just Join
            }
        ]


askQuestionBlock : FrontendModel -> E.Element FrontendMsg
askQuestionBlock m =
    E.row
        [ E.centerX
        , E.width E.fill
        , E.spacing 10
        , E.padding 10
        ]
        [ Input.text
            []
            { label = Input.labelLeft [ E.padding 10 ] (E.text "Question")
            , onChange = newQuestion
            , placeholder = Nothing
            , text = m.proposedQuestion
            }
        , Input.button
            [ Background.color currentTheme.secondary
            , Font.color currentTheme.invertedText
            , E.padding 10
            , E.height E.fill
            , Border.rounded 2
            ]
            { label = E.text "Submit"
            , onPress = Just SubmitQuestion
            }
        ]


renderQuestion : Question -> E.Element FrontendMsg
renderQuestion q =
    E.row []
        [ E.text ("Question: " ++ q.question)
        ]


renderVotingStatus : Question -> Int -> E.Element FrontendMsg
renderVotingStatus q totalUsers =
    E.row []
        [ E.text "Voted "
        , E.text <| String.fromInt <| List.length q.votes
        , E.text " / "
        , E.text <| String.fromInt totalUsers
        ]


renderVoteButtons : E.Element FrontendMsg
renderVoteButtons =
    E.row [ E.width E.fill, E.spacing 10, E.spaceEvenly ]
        (List.map
            (\score -> voteButton score)
            cards
        )


voteButton : Int -> E.Element FrontendMsg
voteButton score =
    Input.button
        [ Background.color currentTheme.secondary
        , Font.color currentTheme.invertedText
        , E.padding 20
        ]
        { onPress = Just (SubmitVote score)
        , label = E.text <| String.fromInt score
        }


renderStatusBar : FrontendModel -> E.Element FrontendMsg
renderStatusBar m =
    E.row []
        [ E.text "hello" ]


renderServerState : FrontendModel -> BackendClientState -> E.Element FrontendMsg
renderServerState f b =
    case b.backendModel.state of
        NoQuestion ->
            E.column [ E.width E.fill, E.spacing 10, E.padding 10 ]
                [ askQuestionBlock f
                , E.el heading1 (E.text "Current Users")
                , listOfUsers <| Dict.values b.backendModel.currentUsers
                ]

        Voting q ->
            E.column [ E.width E.fill ]
                [ E.row [ E.padding 10, E.width E.fill ]
                    [ renderQuestion q
                    , E.el [ E.alignRight ] (renderVotingStatus q <| Dict.size b.backendModel.currentUsers)
                    ]
                , if didIVote q.votes b.id then
                    E.el [ E.padding 10 ] (E.text "You have voted")

                  else
                    renderVoteButtons
                ]

        VoteComplete q ->
            E.column [ E.width E.fill, E.spacing 10, E.padding 10 ]
                [ renderQuestion q
                , renderVoteResults q.votes
                , askQuestionBlock f
                ]


newName : String -> FrontendMsg
newName x =
    UsernameChanged x


newQuestion : String -> FrontendMsg
newQuestion q =
    QuestionChanged q


mainLayout : FrontendModel -> Html FrontendMsg
mainLayout m =
    E.layout
        [ Background.color currentTheme.background ]
        (rootContainer m)


rootContainer : FrontendModel -> E.Element FrontendMsg
rootContainer m =
    E.column [ E.centerX, E.width E.fill ]
        [ header, selectView m ]


selectView : FrontendModel -> E.Element FrontendMsg
selectView m =
    case m.path of
        AskingUsername ->
            joinBlock m

        UsernameReceived ->
            case m.refinementState of
                Nothing ->
                    E.row [] [ E.text "No server state" ]

                Just s ->
                    E.column
                        [ E.centerX
                        , E.width (E.fill |> E.maximum 800)
                        ]
                        [ renderServerState m s ]


renderVoteResults : List Vote -> E.Element FrontendMsg
renderVoteResults votes =
    let
        sortedVotes =
            List.sortBy .score votes
    in
    E.wrappedRow [ E.spacing 10, E.width E.fill ] (List.concat <| List.map (\score -> voteBlock votes score) cards)


onlyScoredAs : Int -> Vote -> Maybe Vote
onlyScoredAs score vote =
    if vote.score == score then
        Just vote

    else
        Nothing


didIVote : List Vote -> Lamdera.ClientId -> Bool
didIVote votes clientId =
    let
        u =
            List.head <| List.filter (\x -> x.clientId == clientId) votes
    in
    case u of
        Just _ ->
            True

        Nothing ->
            False


voteBlock : List Vote -> Int -> List (E.Element FrontendMsg)
voteBlock votes score =
    let
        filteredVotes =
            List.filterMap (onlyScoredAs score) votes
    in
    if List.length filteredVotes == 0 then
        []

    else
        [ E.column
            [ E.spacing 10
            , E.padding 10
            , Border.width 1
            , Border.rounded 4
            ]
            (List.append
                [ E.el [ E.centerX ] (E.text <| String.fromInt score) ]
                (List.map
                    (\v -> E.el [] (E.text v.name))
                    filteredVotes
                )
            )
        ]


renderSpecificResults : List Vote -> Int -> List (E.Element FrontendMsg)
renderSpecificResults votes score =
    [ E.wrappedRow [] [] ]


header : E.Element FrontendMsg
header =
    E.row
        headerStyle
        [ E.el [ E.centerX ] (E.text "Plan or poker") ]


type alias Theme =
    { text : E.Color
    , primary : E.Color
    , secondary : E.Color
    , background : E.Color
    , invertedText : E.Color
    }


currentTheme : Theme
currentTheme =
    { primary = E.rgb255 52 168 83
    , secondary = E.rgb255 26 115 232
    , text = E.rgb255 37 42 52
    , background = E.rgb255 255 255 255
    , invertedText = E.rgb255 255 255 255
    }


listOfUsers : List User -> E.Element FrontendMsg
listOfUsers l =
    let
        nameList =
            List.sort (List.map (\x -> x.name) l)
    in
    E.column [] (List.map renderUser nameList)


renderUser : String -> E.Element FrontendMsg
renderUser s =
    E.row [] [ E.text s ]


cards : List Int
cards =
    [ 1, 2, 3, 5, 8, 13 ]
