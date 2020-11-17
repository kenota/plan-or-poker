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
import Html.Attributes as A
import Html.Events as HE
import Icons exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera
import TW
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
        [ A.property "href" (Encode.string "https://fonts.googleapis.com/css2?family=Roboto&family=Rubik:wght@300&display=swap")
        , A.property "rel" (Encode.string "stylesheet")
        ]
        []


tailwindCss =
    Html.node "link"
        [ A.property "href" (Encode.string "/public/main.css")
        , A.property "rel" (Encode.string "stylesheet")
        ]
        []


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Planning poker"
    , body = [ manualCss, tailwindCss, mainLayout model ]
    }


headerStyle =
    [ Font.size 24
    , Font.family
        [ Font.typeface "Roboto" ]
    , E.width E.fill
    , E.height (E.px 48)
    , Font.color currentTheme.secondary
    , E.paddingEach { top = 0, right = 16, bottom = 0, left = 16 }
    ]


heading1 =
    [ Font.size 25
    ]


normalText =
    [ Font.size 12 ]


onEnter : msg -> E.Attribute msg
onEnter msg =
    E.htmlAttribute
        (onEnterHTMLInput msg)


onEnterHTMLInput : msg -> Html.Attribute msg
onEnterHTMLInput msg =
    HE.on "keyup"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

                    else
                        Decode.fail "Not the enter key"
                )
        )


joinBlock : FrontendModel -> E.Element FrontendMsg
joinBlock m =
    E.row [ E.centerX, E.width (E.fill |> E.maximum 800), E.spacing 10 ]
        [ Input.text [ onEnter Join ]
            { label = Input.labelLeft [ E.centerY, E.alignLeft ] (E.text "Name")
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
        ]
        [ Input.text
            []
            { label = Input.labelLeft [ E.centerY ] (E.text "Question")
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
            E.column [ E.width E.fill, E.spacing 10 ]
                [ askQuestionBlock f
                ]

        Voting q ->
            E.column [ E.width E.fill ]
                [ E.row [ E.width E.fill, E.paddingEach { top = 10, right = 0, bottom = 10, left = 0 } ]
                    [ renderQuestion q
                    , E.el [ E.alignRight ] (renderVotingStatus q <| Dict.size b.backendModel.currentUsers)
                    ]
                , if didIVote q.votes b.id then
                    E.el [ E.padding 10 ] (E.text "You have voted")

                  else
                    renderVoteButtons
                ]

        VoteComplete q ->
            E.column [ E.width E.fill, E.spacing 10 ]
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
    case m.path of
        AskingUsername ->
            Html.div
                [ TW.min_h_screen
                , TW.flex
                , TW.items_center
                , TW.justify_center
                , TW.bg_gray_100
                , TW.py_12
                , TW.px_4
                , TW.sm__px_6
                , TW.lg__px_8
                ]
                [ Html.div
                    [ TW.max_w_md
                    , TW.w_full
                    ]
                    [ Html.div
                        []
                        [ Html.h1 [ TW.mt_6, TW.text_center, TW.text_3xl, TW.leading_9, TW.font_extrabold, TW.text_gray_900 ] [ Html.text "Welcome to Plan or Poker" ] ]
                    , Html.div
                        [ TW.mt_8 ]
                        [ Html.div
                            [ TW.rounded_md, TW.shadow_sm ]
                            [ Html.input [ onEnterHTMLInput Join, A.placeholder "Your name", HE.onInput newName, TW.appearance_none, TW.rounded_md, TW.relative, TW.block, TW.w_full, TW.px_3, TW.py_2, TW.border, TW.border_gray_300, TW.placeholder_gray_500, TW.text_gray_900, TW.focus__outline_none, TW.z_10 ] [] ]
                        , Html.div
                            [ TW.mt_6 ]
                            [ Html.button
                                [ HE.onClick Join, TW.relative, TW.w_full, TW.flex, TW.justify_center, TW.py_2, TW.px_4, TW.border, TW.border_transparent, TW.text_sm, TW.leading_5, TW.font_medium, TW.rounded_md, TW.text_white, TW.bg_indigo_600, TW.hover__bg_indigo_500, TW.focus__outline_none, TW.focus__border_indigo_700 ]
                                [ Html.text "Join" ]
                            ]
                        ]
                    ]
                ]

        UsernameReceived ->
            Html.div
                [ TW.flex, TW.flex_row, TW.h_screen, TW.w_screen, TW.justify_center, TW.bg_gray_100 ]
                [ Html.div
                    [ TW.flex, TW.flex_row, TW.h_screen, TW.w_5over6, TW.max_w_4xl ]
                    [ Html.div
                        [ TW.flex_grow, TW.m_2 ]
                        (questionForm m.proposedQuestion
                            ++ [ Html.div
                                    [ TW.bg_white, TW.shadow, TW.overflow_hidden, TW.sm__rounded_lg ]
                                    [ Html.div
                                        [ TW.px_4, TW.py_5, TW.border_b, TW.border_gray_200 ]
                                        [ Html.h3 [ TW.text_lg, TW.leading_6, TW.font_medium, TW.text_gray_900 ] [ Html.text <| getQuestionOrPlaceholder m.refinementState ] ]
                                    , Html.div
                                        [ TW.flex, TW.flex_col ]
                                        [ Html.div
                                            [ TW.px_4, TW.py_5, TW.text_sm, TW.leading_5, TW.font_medium, TW.text_gray_500 ]
                                            [ Html.text "Your vote" ]
                                        , renderNewVoteBlock m
                                        , Html.div
                                            [ TW.px_4, TW.py_5, TW.text_sm, TW.leading_5, TW.font_medium, TW.text_gray_500 ]
                                            [ Html.text "Voting progress" ]
                                        , Html.div
                                            [ TW.px_4, TW.pt_0, TW.pb_5, TW.text_sm, TW.leading_5, TW.text_gray_900, TW.sm__mt_0 ]
                                            [ renderNewVoteProgress m.refinementState ]
                                        , Html.div
                                            [ TW.px_4, TW.py_5, TW.text_sm, TW.leading_5, TW.font_medium, TW.text_gray_500 ]
                                            [ Html.text "Voting results" ]
                                        , Maybe.withDefault (Html.div [] []) (Maybe.map renderNewVotingResults (getBackendModel m))
                                        ]
                                    ]
                               ]
                        )
                    , Html.div
                        [ TW.w_48, TW.flex_none, TW.m_2, TW.rounded_md ]
                        [ Html.h3
                            [ TW.text_center, TW.text_gray_900, TW.text_lg, TW.leading_6, TW.font_medium ]
                            [ Html.text "User List" ]
                        , renderNewUserList m.refinementState
                        ]
                    ]
                ]


renderNewVoteProgress : Maybe BackendClientState -> Html FrontendMsg
renderNewVoteProgress maybe =
    case maybe of
        Nothing ->
            Html.div [] [ Html.text "" ]

        Just backendState ->
            case backendState.backendModel.state of
                NoQuestion ->
                    Html.div [] [ Html.text "" ]

                Voting q ->
                    renderVoteProgressBar (Dict.size backendState.backendModel.currentUsers) (List.length q.votes)

                VoteComplete q ->
                    Html.div [] [ Html.text "Vote complete" ]


renderVoteProgressBar : Int -> Int -> Html FrontendMsg
renderVoteProgressBar totalUsers voted =
    let
        pct =
            (voted * 100) // totalUsers
    in
    Html.div
        [ TW.relative, TW.pt_1 ]
        [ Html.div
            [ TW.overflow_hidden, TW.h_2, TW.mb_4, TW.text_xs, TW.flex, TW.rounded, TW.bg_indigo_300 ]
            [ Html.div
                [ A.style "width" (String.fromInt pct ++ "%"), TW.shadow_none, TW.flex, TW.flex_col, TW.bg_indigo_600 ]
                []
            ]
        ]


getQuestionOrPlaceholder : Maybe BackendClientState -> String
getQuestionOrPlaceholder maybe =
    case maybe of
        Nothing ->
            "Connecting"

        Just wrapper ->
            case wrapper.backendModel.state of
                NoQuestion ->
                    ""

                Voting q ->
                    q.question

                VoteComplete q ->
                    q.question


questionForm : String -> List (Html FrontendMsg)
questionForm proposedQuestion =
    [ Html.div
        [ TW.flex, TW.flex_row, TW.space_x_3, TW.pt_5, TW.pb_5 ]
        [ Html.div
            [ TW.flex_grow, TW.text_sm, TW.leading_5, TW.text_gray_900, TW.sm__mt_0 ]
            [ Html.input [ A.value proposedQuestion, onEnterHTMLInput SubmitQuestion, A.placeholder "Your queston", HE.onInput newQuestion, TW.appearance_none, TW.rounded_md, TW.relative, TW.block, TW.w_full, TW.px_3, TW.py_2, TW.border, TW.border_gray_300, TW.placeholder_gray_500, TW.text_gray_900, TW.focus__outline_none, TW.z_10 ] [] ]
        , Html.div
            [ TW.flex_none, TW.w_32 ]
            [ Html.button
                [ HE.onClick SubmitQuestion, TW.relative, TW.w_full, TW.flex, TW.justify_center, TW.py_2, TW.px_4, TW.border, TW.border_transparent, TW.text_sm, TW.leading_5, TW.font_medium, TW.rounded_md, TW.text_white, TW.bg_indigo_600, TW.hover__bg_indigo_500, TW.focus__outline_none, TW.focus__border_indigo_700 ]
                [ Html.text "Submit" ]
            ]
        ]
    ]


getBackendModel : FrontendModel -> Maybe BackendModel
getBackendModel m =
    case m.refinementState of
        Nothing ->
            Nothing

        Just wrapper ->
            Just wrapper.backendModel


renderNewVoteBlock : FrontendModel -> Html FrontendMsg
renderNewVoteBlock m =
    case m.refinementState of
        Nothing ->
            Html.div [] [ Html.text "loading" ]

        Just s ->
            case s.backendModel.state of
                NoQuestion ->
                    Html.div
                        [ TW.px_4, TW.pt_0, TW.pb_5, TW.text_sm, TW.leading_5, TW.text_gray_900, TW.sm__mt_0 ]
                        [ Html.text "No vote is in progress" ]

                Voting q ->
                    let
                        v =
                            getMyVote q.votes s.id
                    in
                    Html.div
                        [ TW.px_4, TW.pt_0, TW.pb_5, TW.text_sm, TW.leading_5, TW.text_gray_900, TW.sm__mt_0 ]
                        [ Html.div
                            [ TW.flex, TW.flex_row, TW.space_x_6 ]
                            (List.map (renderNewVoteButton v) cards)
                        ]

                VoteComplete q ->
                    let
                        v =
                            getMyVote q.votes s.id
                    in
                    Html.div
                        [ TW.px_4, TW.pt_0, TW.pb_5, TW.text_sm, TW.leading_5, TW.text_gray_900, TW.sm__mt_0 ]
                        [ Html.div
                            [ TW.flex, TW.flex_row, TW.space_x_6 ]
                            (List.map (renderNewVoteButton v) cards)
                        ]


renderNewVoteButton : Maybe Vote -> Int -> Html FrontendMsg
renderNewVoteButton myVote score =
    let
        bg =
            case myVote of
                Nothing ->
                    TW.bg_indigo_300

                Just v ->
                    if v.score == score then
                        TW.bg_indigo_700

                    else
                        TW.bg_indigo_300
    in
    Html.div
        [ HE.onClick <| SubmitVote score, TW.w_10, TW.h_10, bg, TW.hover__bg_indigo_700, TW.cursor_pointer, TW.rounded_md, TW.flex, TW.flex_col, TW.justify_center, TW.text_center ]
        [ Html.span [ TW.text_white, TW.text_lg, TW.font_bold ] [ Html.text (String.fromInt score) ] ]


renderNewUserList : Maybe BackendClientState -> Html msg
renderNewUserList m =
    case m of
        Nothing ->
            Html.h2
                [ TW.text_center, TW.text_gray_500, TW.leading_6, TW.font_medium ]
                [ Html.text "Loading" ]

        Just serverState ->
            Html.div
                [ TW.flex, TW.flex_col, TW.text_sm, TW.leading_5, TW.border, TW.border_gray_200, TW.rounded_md ]
                (renderUsers
                    serverState.backendModel.currentUsers
                )


renderUsers : Dict.Dict Lamdera.ClientId User -> List (Html msg)
renderUsers d =
    let
        users =
            Dict.values d
    in
    List.map renderNewUserRow (List.sortBy .name users)


renderNewUserRow : User -> Html msg
renderNewUserRow u =
    Html.div
        [ TW.flex, TW.flex_row, TW.w_auto, TW.pl_3, TW.pr_4, TW.py_3 ]
        [ Html.div
            [ TW.flex_grow, TW.text_gray_700, TW.truncate ]
            [ Html.text u.name ]
        ]


renderNewVotingResults : BackendModel -> Html FrontendMsg
renderNewVotingResults backendModel =
    case backendModel.state of
        VoteComplete question ->
            Html.div
                [ TW.space_x_3, TW.px_4, TW.pt_0, TW.pb_5, TW.leading_5, TW.text_gray_900 ]
                (listNewVotes question.votes)

        _ ->
            Html.div [] []


listNewVotes : List Vote -> List (Html FrontendMsg)
listNewVotes votes =
    List.concat <|
        List.map (listVotesForScore votes) cards


listVotesForScore : List Vote -> Int -> List (Html FrontendMsg)
listVotesForScore votes score =
    let
        filteredVotes =
            List.filterMap (onlyScoredAs score) votes
    in
    if List.length filteredVotes == 0 then
        []

    else
        [ Html.div
            [ TW.inline_block, TW.border, TW.align_top, TW.border_gray_200, TW.rounded_md, TW.overflow_hidden, TW.w_32 ]
            [ Html.div
                [ TW.flex, TW.flex_col ]
                [ Html.div
                    [ TW.bg_indigo_700, TW.text_white, TW.text_sm, TW.font_bold, TW.pt_1, TW.pb_1, TW.text_center ]
                    [ Html.text <| String.fromInt score ]
                , Html.div
                    [ TW.flex, TW.flex_col ]
                    (List.map voteAuthorInResult filteredVotes)
                ]
            ]
        ]


voteAuthorInResult : Vote -> Html FrontendMsg
voteAuthorInResult vote =
    Html.div
        [ TW.border_gray_200, TW.flex, TW.flex_row, TW.w_auto, TW.pl_3, TW.pr_4, TW.py_1 ]
        [ Html.div
            [ TW.flex_grow, TW.text_gray_900, TW.truncate, TW.text_xs ]
            [ Html.text vote.name ]
        ]


rootContainer : FrontendModel -> E.Element FrontendMsg
rootContainer m =
    E.column [ E.centerX, E.width E.fill, E.height E.fill ]
        [ E.row
            [ E.width E.fill
            , E.height (E.px 48)
            , Border.shadow
                { offset = ( 0, 0 )
                , size = 0.01
                , blur = 1
                , color = currentTheme.text
                }
            ]
            [ E.el
                [ E.centerX
                , E.centerY
                , E.width (E.fill |> E.maximum 800)
                , E.height E.fill
                ]
                (header m)
            ]
        , E.el
            [ E.centerX
            , E.width (E.fill |> E.maximum 800)
            , E.height E.fill
            , E.paddingEach { top = 12, right = 16, bottom = 0, left = 16 }
            ]
            (E.row [ E.width E.fill ] [ selectView m ])
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
                        , E.width E.fill
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


getMyVote : List Vote -> Lamdera.ClientId -> Maybe Vote
getMyVote votes clientId =
    List.head <| List.filter (\x -> x.clientId == clientId) votes


didIVote : List Vote -> Lamdera.ClientId -> Bool
didIVote votes clientId =
    case getMyVote votes clientId of
        Nothing ->
            False

        Just _ ->
            True


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
            [ E.alignTop
            ]
            (List.append
                [ E.el
                    [ Background.color currentTheme.secondary
                    , Border.roundEach { topLeft = 3, topRight = 3, bottomRight = 0, bottomLeft = 0 }
                    , E.width E.fill
                    , E.paddingEach { top = 5, right = 0, bottom = 5, left = 0 }
                    , Font.color currentTheme.invertedText
                    ]
                    (E.el [ E.centerX ] (E.text <| String.fromInt score))
                ]
                [ E.column
                    [ Border.widthEach { top = 0, right = 1, bottom = 1, left = 1 }
                    , Border.roundEach { topLeft = 0, topRight = 0, bottomRight = 3, bottomLeft = 3 }
                    , Border.color currentTheme.border
                    , E.paddingEach { top = 5, right = 0, bottom = 0, left = 0 }
                    ]
                    (List.map
                        (\v -> E.el [ E.paddingEach { top = 0, right = 16, bottom = 5, left = 16 } ] (E.text v.name))
                        filteredVotes
                    )
                ]
            )
        ]


renderSpecificResults : List Vote -> Int -> List (E.Element FrontendMsg)
renderSpecificResults votes score =
    [ E.wrappedRow [] [] ]


header : FrontendModel -> E.Element FrontendMsg
header m =
    E.row
        headerStyle
        [ E.el [] (E.text "Plan or poker")
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
    , border : E.Color
    }


currentTheme : Theme
currentTheme =
    { primary = E.rgb255 52 168 83
    , secondary = E.rgb255 26 115 232
    , text = E.rgb255 37 42 52
    , background = E.rgb255 255 255 255
    , invertedText = E.rgb255 255 255 255
    , border = E.rgb255 200 200 200
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
    [ 0, 1, 2, 3, 5, 8, 13 ]
