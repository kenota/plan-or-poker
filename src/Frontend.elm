module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Lamdera
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
        , subscriptions = \m -> Sub.none
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


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ServerState s ->
            ( { model | refinementState = Just s }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Planning poker"
    , body = [ mainLayout model ]
    }


headerStyle =
    [ Font.family
        [ Font.external
            { name = "Open Sans"
            , url = "https://fonts.googleapis.com/css?family=Open+Sans&display=swap"
            }
        , Font.sansSerif
        ]
    , Font.size 38
    , E.padding 30
    ]


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
            , E.padding 10
            , E.height E.fill
            , Border.rounded 2
            ]
            { label = E.text "Join"
            , onPress = Just Join
            }
        ]


renderServerState : FrontendModel -> BackendModel -> E.Element FrontendMsg
renderServerState f b =
    case b.state of
        NoQuestion ->
            askQuestionBlock f

        Voting _ ->
            E.row [] [ E.text "Voting" ]

        VoteComplete _ ->
            E.row [] [ E.text "Vote complete" ]


askQuestionBlock : FrontendModel -> E.Element FrontendMsg
askQuestionBlock m =
    E.row [ E.centerX, E.width (E.fill |> E.maximum 800), E.spacing 10, E.padding 10 ]
        [ Input.text []
            { label = Input.labelLeft [ E.padding 10 ] (E.text "Question")
            , onChange = newQuestion
            , placeholder = Nothing
            , text = m.proposedQuestion
            }
        , Input.button
            [ Background.color currentTheme.secondary
            , E.padding 10
            , E.height E.fill
            , Border.rounded 2
            ]
            { label = E.text "Submit"
            , onPress = Just SubmitQuestion
            }
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
                    renderServerState m s


header : E.Element FrontendMsg
header =
    E.row [ E.width E.fill, E.height (E.px 60), Background.color (E.rgb255 8 217 214) ]
        [ E.el [ E.centerX ] (E.text "Plan or poker") ]


usernameInput : String -> Html FrontendMsg
usernameInput s =
    Html.input [ A.placeholder "Username", A.value s, E.onInput UsernameChanged ] []


joinButton : Html FrontendMsg
joinButton =
    Html.button [ E.onClick Join ] [ Html.text "Join" ]


renderListOfUsers : List User -> Html FrontendMsg
renderListOfUsers l =
    Html.ul
        []
        (List.map renderUser l)


renderUser : User -> Html FrontendMsg
renderUser u =
    Html.li [] [ Html.text u.name ]


type alias Theme =
    { text : E.Color
    , primary : E.Color
    , secondary : E.Color
    , background : E.Color
    }


currentTheme : Theme
currentTheme =
    { primary = E.rgb255 8 217 214
    , secondary = E.rgb255 255 46 99
    , text = E.rgb255 37 42 52
    , background = E.rgb255 234 234 234
    }
