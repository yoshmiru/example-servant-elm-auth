module Main exposing (FromServer(..), FromUi(..), Model, Msg(..), fromServer, init, main, update, view, viewItem)

import Api exposing (..)
import CustomApi as CApi
import Browser
import Dict exposing (Dict)
import Debug exposing (log)
import Html exposing (Html, button, div, input, label, li, text, ul)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Regex


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { items : Dict Int Item
    , addItemInput : String
    , error : Maybe String
    , token : String
    , userIdInput   : String
    , passwordInput : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Dict.empty "" Nothing "" "" ""
    , Cmd.none
    )



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = Initial (List ItemId)
    | NewItem Item
    | Delete ItemId
    | LoggedIn String


type FromUi
    = AddItemInputChange String
    | AddItemButton
    | UserIdInputChange   String
    | PasswordInputChange String
    | Done ItemId
    | Login


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromServer fromServerMsg ->
            case fromServerMsg of
                Initial itemIds ->
                    ( model
                    , itemIds
                        |> List.map (\id -> CApi.getApiItemByItemId model.token id (fromServer NewItem))
                        |> Cmd.batch
                    )

                NewItem item ->
                    ( { model | items = Dict.insert item.id item model.items }
                    , Cmd.none
                    )

                Delete id ->
                    ( { model | items = Dict.remove id model.items }
                    , Cmd.none
                    )
                LoggedIn line ->
                    let regex : Regex.Regex
                        regex = Maybe.withDefault Regex.never <| Regex.fromString "JWT-Cookie=([A-Za-z0-9._-]*);"
                        words : List Regex.Match
                        words = Regex.find regex (log "line" line)
                    in
                        case List.head (List.map .submatches words) of
                            Just [Just token] -> ({model|token = log "token" token}, CApi.getApiItem token (fromServer Initial))
                            _  -> (model, Cmd.none)

        FromUi fromUi ->
            case fromUi of
                AddItemButton ->
                    let
                        itemName = model.addItemInput
                        token    = model.token
                    in
                    if itemName == "" then
                        update (Error "empty field") model

                    else
                        ( { model | addItemInput = "" }
                        , CApi.postApiItem token itemName (fromServer (\id -> NewItem (Item id itemName)))
                        )

                AddItemInputChange t ->
                    ( { model | addItemInput = t, error = Nothing }
                    , Cmd.none
                    )
                UserIdInputChange uid ->
                    ( { model | userIdInput = uid, error = Nothing }
                    , Cmd.none
                    )
                PasswordInputChange password ->
                    ( { model | passwordInput = password, error = Nothing }
                    , Cmd.none
                    )

                Done id ->
                    ( model
                    , deleteApiItemByItemId id (fromServer (\() -> Delete id))
                    )
                Login ->
                    let name = model.userIdInput
                        password = model.passwordInput
                        form = LoginForm name password
                    in
                        ( model, Api.postLogin form (fromServer LoggedIn))

        Error error ->
            ( { model | error = Just error }, Cmd.none )


fromServer : (a -> FromServer) -> Result Http.Error a -> Msg
fromServer msgConstructor result =
    case result of
        Ok content ->
            FromServer <| msgConstructor content

        Err error ->
            Error <| httpErrorToString error


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl s ->
            "bad url: " ++ s

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "bad status: " ++ String.fromInt status

        Http.BadBody response ->
            "bad payload: " ++ response



-- VIEW


view : Model -> Html Msg
view model =
    case model.token of
        "" ->
            div []
                [
                div []
                    [
                        label [] [ text "Name" ]
                      , input [ onInput (FromUi << UserIdInputChange), value model.userIdInput ] []
                    ]
              , div []
                    [
                        label [] [ text "Password" ]
                      , input [ type_ "password", onInput (FromUi << PasswordInputChange), value model.passwordInput ] []
                    ]
              , div []
                    [
                        button [ onClick (FromUi Login) ] [ text "Login" ]
                    ]
                ]
        token ->
            let
                items =
                    List.map (viewItem << Tuple.second) (Dict.toList model.items)

                error =
                    model.error
                        |> Maybe.map viewError
                        |> Maybe.withDefault (Html.text "")
            in
            div []
                [ ul [] items
                , input [ onInput (FromUi << AddItemInputChange), value model.addItemInput ] []
                , button [ onClick (FromUi AddItemButton) ] [ text "add item" ]
                , error
                ]


viewItem : Item -> Html Msg
viewItem item =
    li []
        [ text item.text
        , text " - "
        , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]


viewError : String -> Html msg
viewError error =
    div
        []
        [ text <| "Error: " ++ error ]
