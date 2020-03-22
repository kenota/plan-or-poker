module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict as Dict
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera
import Time
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = Types.initFrontend
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Time.every 1000 NewTime
        , view = view
        }


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

        UserListToggle ->
            ( { model | userListVisible = not model.userListVisible }, Cmd.none )

        SettingsMenuToggle ->
            ( { model | settingsMenuVisible = not model.settingsMenuVisible }, Cmd.none )

        RequestServerReset ->
            ( { model | settingsMenuVisible = False }, Lamdera.sendToBackend RequestReset )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

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

        Reset ->
            let
                newModel =
                    { model
                        | proposedQuestion = ""
                        , refinementState = Nothing
                        , userListVisible = False
                        , settingsMenuVisible = False
                        , username = ""
                        , path = AskingUsername
                    }
            in
            ( newModel, Cmd.none )


manualCss =
    Html.node "link"
        [ Html.Attributes.property "href" (Encode.string "https://fonts.googleapis.com/css2?family=Roboto&family=Rubik:wght@300&display=swap")
        , Html.Attributes.property "rel" (Encode.string "stylesheet")
        ]
        []


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Planning poker"
    , body = [ manualCss, mainLayout model ]
    }


headerStyle =
    [ Font.size 24
    , Font.family
        [ Font.typeface "Roboto" ]
    , E.width E.fill
    , E.height (E.px 48)
    , Font.color currentTheme.secondary
    , Border.shadow
        { offset = ( 0, 0 )
        , size = 0.2
        , blur = 2
        , color = currentTheme.text
        }
    ]


heading1 =
    [ Font.size 25
    ]


normalText =
    [ Font.size 12 ]


onEnter : msg -> E.Attribute msg
onEnter msg =
    E.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


joinBlock : FrontendModel -> E.Element FrontendMsg
joinBlock m =
    E.row [ E.centerX, E.width (E.fill |> E.maximum 800), E.spacing 10, E.padding 10 ]
        [ Input.text [ onEnter Join ]
            { label = Input.labelLeft [ E.padding 10 ] (E.text "Name")
            , onChange = newName
            , placeholder = Just (Input.placeholder [] (E.text "Hello"))
            , text = m.username
            }
        , Input.button
            mainButtonStyle
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
            mainButtonStyle
            { label = E.text "Submit"
            , onPress = Just SubmitQuestion
            }
        ]


mainButtonStyle : List (E.Attribute msg)
mainButtonStyle =
    [ Background.color currentTheme.secondary
    , Font.color currentTheme.invertedText
    , Font.family [ Font.typeface "Rubik" ]
    , Font.size 14
    , Font.letterSpacing 1.2
    , Border.rounded 4
    , E.height (E.px 44)
    , E.paddingEach
        { top = 0
        , right = 24
        , bottom = 0
        , left = 24
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
    E.row [ E.width E.fill, E.spacing 10, E.spaceEvenly, E.paddingEach { top = 0, right = 14, bottom = 10, left = 14 } ]
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
    E.column [ E.centerX, E.width E.fill, E.height E.fill ]
        [ E.row headerStyle [ header m ]
        , E.column
            [ E.centerX
            , E.width (E.fill |> E.maximum 800)
            , E.height E.fill
            ]
            [ E.el [ E.width E.fill, E.height (E.fillPortion 1) ] (selectView m)
            ]
        ]


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


header : FrontendModel -> E.Element FrontendMsg
header m =
    E.row
        [ E.width (E.fill |> E.maximum 800), E.centerX ]
        [ E.el [ E.paddingEach { top = 0, right = 0, bottom = 0, left = 24 } ] (E.text "Plan or poker")
        , E.el [ E.alignRight, E.paddingEach { top = 0, right = 5, bottom = 0, left = 0 } ] (userCounter m)
        ]


maybeUserList : FrontendModel -> Maybe (List (E.Attribute FrontendMsg))
maybeUserList m =
    case m.refinementState of
        Just backend ->
            case m.userListVisible of
                True ->
                    Just
                        [ E.below
                            (E.column
                                [ Background.color currentTheme.invertedText
                                , Border.color currentTheme.primary
                                , Border.width 1
                                , Border.rounded 3
                                , E.alignRight
                                ]
                                [ listOfUsers <| Dict.values backend.backendModel.currentUsers ]
                            )
                        ]

                False ->
                    Nothing

        Nothing ->
            Nothing


maybeSettingsMenu : FrontendModel -> Maybe (List (E.Attribute FrontendMsg))
maybeSettingsMenu m =
    case m.refinementState of
        Just backend ->
            case m.settingsMenuVisible of
                True ->
                    Just
                        [ E.below
                            (E.column
                                [ Background.color currentTheme.invertedText
                                , Border.color currentTheme.primary
                                , Border.width 1
                                , Border.rounded 3
                                , E.alignRight
                                , E.padding 10
                                ]
                                [ Input.button
                                    mainButtonStyle
                                    { label = E.text "Reset"
                                    , onPress = Just RequestServerReset
                                    }
                                ]
                            )
                        ]

                False ->
                    Nothing

        Nothing ->
            Nothing


userCounter : FrontendModel -> E.Element FrontendMsg
userCounter m =
    case m.refinementState of
        Just backend ->
            E.row
                (Maybe.withDefault [] (maybeUserList m) ++ [ E.spacing 8 ])
                [ E.el [] userIcon
                , E.el
                    [ Font.color currentTheme.invertedText
                    , Background.color currentTheme.primary
                    , E.padding 6
                    , Border.rounded 10
                    , Events.onClick UserListToggle
                    ]
                    (E.text (String.fromInt <| Dict.size backend.backendModel.currentUsers))
                , E.el
                    (Maybe.withDefault
                        []
                        (maybeSettingsMenu m)
                        ++ [ Events.onClick SettingsMenuToggle ]
                    )
                    settingsIcon
                ]

        Nothing ->
            E.el [] (E.text "")


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
    E.column [ E.padding 12 ] (List.map renderUser nameList)


renderUser : String -> E.Element FrontendMsg
renderUser s =
    E.row [ E.paddingEach { top = 0, right = 0, bottom = 0, left = 12 } ] [ E.text s ]


cards : List Int
cards =
    [ 1, 2, 3, 5, 8, 13 ]
