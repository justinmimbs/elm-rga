module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init "hello, world", Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }


init : String -> Model
init string =
    Model 0 (String.length string) (string |> String.toList)


type alias Model =
    { cursor : Int
    , length : Int
    , array : List Char
    }



-- update


type Msg
    = PressedArrow Direction


type Direction
    = Left
    | Right


update : Msg -> Model -> Model
update msg model =
    case msg of
        PressedArrow direction ->
            let
                cursor =
                    case direction of
                        Left ->
                            model.cursor - 1 |> max 0

                        Right ->
                            model.cursor + 1 |> min model.length
            in
            { model | cursor = cursor }



-- events


decodeKeydown : Decoder Msg
decodeKeydown =
    Decode.map4
        (\_ _ _ key -> key)
        (Decode.field "altKey" decodeFalse)
        (Decode.field "ctrlKey" decodeFalse)
        (Decode.field "metaKey" decodeFalse)
        (Decode.field "key" Decode.string)
        |> Decode.andThen
            decodeMsgFromKey


decodeFalse : Decoder ()
decodeFalse =
    Decode.bool
        |> Decode.andThen
            (bool (Decode.fail "no-op") (Decode.succeed ()))


decodeMsgFromKey : String -> Decoder Msg
decodeMsgFromKey key =
    case key of
        "ArrowLeft" ->
            Decode.succeed (PressedArrow Left)

        "ArrowRight" ->
            Decode.succeed (PressedArrow Right)

        _ ->
            Decode.fail "no-op"



-- view


view : Model -> Browser.Document Msg
view { cursor, length, array } =
    Browser.Document
        "test"
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "../app/css/style.css" ] []
        , Html.div
            [ Html.Attributes.class "textarea"
            , Html.Attributes.tabindex 0
            , Html.Events.on "keydown" decodeKeydown
            ]
            [ Html.div
                [ Html.Attributes.class "textline"
                ]
                [ Html.text (String.fromList array |> String.left cursor)
                , Html.span [ Html.Attributes.class "cursor insert" ] []
                , Html.text (String.fromList array |> String.dropLeft cursor)
                ]
            ]
        ]



-- helpers


bool : a -> a -> Bool -> a
bool t f x =
    if x then
        t

    else
        f
