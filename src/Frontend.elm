module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
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

        Types.UsernameChanged s ->
            case model.path of
                AskingUsername ->
                    ( { model | username = s }, Cmd.none )

                UsernameReceived ->
                    ( { model | username = s }
                    , Cmd.none
                    )

        Join ->
            ( { model | path = UsernameReceived }, Lamdera.sendToBackend (ClientJoin model.username) )


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
    , body =
        case
            model.path
        of
            AskingUsername ->
                [ Html.div [] [ Html.text ("Enter your name " ++ model.username) ]
                , usernameInput model.username
                , joinButton
                ]

            UsernameReceived ->
                [ Html.div []
                    [ Html.text ("Got your name: " ++ model.username)
                    , renderServerState model.refinementState
                    ]
                ]
    }


usernameInput : String -> Html FrontendMsg
usernameInput s =
    Html.input [ A.placeholder "Username", A.value s, E.onInput UsernameChanged ] []


joinButton : Html FrontendMsg
joinButton =
    Html.button [ E.onClick Join ] [ Html.text "Join" ]


renderServerState : Maybe BackendModel -> Html FrontendMsg
renderServerState m =
    case m of
        Nothing ->
            Html.div [] [ Html.text "No Server state" ]

        Just s ->
            Html.div [] [ Html.text "Got state", renderListOfUsers s.currentUsers ]


renderListOfUsers : List User -> Html FrontendMsg
renderListOfUsers l =
    Html.ul
        []
        (List.map renderUser l)


renderUser : User -> Html FrontendMsg
renderUser u =
    Html.li [] [ Html.text u.name ]
