module Main exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import WebSocket


type Model
    = NotConnected
        { server : String
        , name : String
        }
    | Connected
        { server : String
        , name : String
        , chat : String
        , messages : List String
        }


type Message
    = SetServer String
    | SetName String
    | SetChat String
    | Connect
    | Recv String
    | Send


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : ( Model, Cmd Message )
init =
    ( NotConnected
        { name = "Mitchell"
        , server = "ws://127.0.0.1:8080"
        }
    , Cmd.none
    )


subscriptions : Model -> Sub Message
subscriptions model =
    case model of
        NotConnected _ ->
            Sub.none

        Connected { server } ->
            WebSocket.listen server Recv


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case model of
        NotConnected model ->
            case message of
                SetServer server ->
                    ( NotConnected { server = server, name = model.name }
                    , Cmd.none
                    )

                SetName name ->
                    ( NotConnected { server = model.server, name = name }
                    , Cmd.none
                    )

                Connect ->
                    ( Connected
                        { server = model.server
                        , name = model.name
                        , chat = ""
                        , messages = []
                        }
                    , Cmd.none
                    )

                SetChat _ ->
                    Debug.crash "NotConnected/SetChat"

                Recv _ ->
                    Debug.crash "NotConnected/Recv"

                Send ->
                    Debug.crash "NotConnected/Send"

        Connected model ->
            case message of
                Recv s ->
                    ( Connected
                        { server = model.server
                        , name = model.name
                        , chat = model.chat
                        , messages = s :: model.messages
                        }
                    , Cmd.none
                    )

                Send ->
                    ( Connected
                        { server = model.server
                        , name = model.name
                        , chat = ""
                        , messages =
                            (model.name ++ ": " ++ model.chat)
                                :: model.messages
                        }
                    , WebSocket.send "ws://127.0.0.1:8080"
                        (model.name ++ ": " ++ model.chat)
                    )

                SetChat chat ->
                    ( Connected
                        { server = model.server
                        , name = model.name
                        , chat = chat
                        , messages = model.messages
                        }
                    , Cmd.none
                    )

                Connect ->
                    Debug.crash "Connected/Connect"

                SetServer _ ->
                    Debug.crash "Connected/SetServer"

                SetName _ ->
                    Debug.crash "Connected/SetName"


view : Model -> Html Message
view model =
    case model of
        NotConnected model ->
            Html.div []
                [ Html.text "Server"
                , Html.input
                    [ Html.Attributes.value model.server
                    , Html.Events.onInput SetServer
                    ]
                    []
                , Html.text "Username"
                , Html.input
                    [ Html.Attributes.value model.name
                    , Html.Events.onInput SetName
                    ]
                    []
                , Html.div
                    []
                    [ Html.button
                        [ Html.Events.onClick Connect ]
                        [ Html.text "Connect" ]
                    ]
                ]

        Connected model ->
            Html.div []
                [ Html.p [] [ Html.text ("Hi there, " ++ model.name ++ "!") ]
                , Html.input
                    [ Html.Attributes.value model.chat
                    , Html.Events.onInput SetChat
                    ]
                    []
                , Html.button [ Html.Events.onClick Send ] [ Html.text "Send" ]
                , Html.div [] <|
                    List.map (Html.div [] << List.singleton << Html.text) <|
                        List.reverse model.messages
                ]
