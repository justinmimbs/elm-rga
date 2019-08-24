module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


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
    Model Insert 0 (String.length string) (string |> String.toList)


type alias Model =
    { mode : Mode
    , cursor : Int
    , length : Int
    , array : List Char
    }



-- update


type Msg
    = PressedArrow Direction
    | SelectedMode Mode


type Direction
    = Left
    | Right


type Mode
    = Insert
    | Replace


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

        SelectedMode mode ->
            { model | mode = mode }



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
view { mode, cursor, length, array } =
    Browser.Document
        "test"
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "../app/css/style.css" ] []
        , Html.div
            [ Html.Attributes.class "editor"
            , Html.Attributes.tabindex 0
            , Html.Events.on "keydown" decodeKeydown
            ]
            [ Html.div
                [ Html.Attributes.class "textarea"
                ]
                [ Html.div
                    [ Html.Attributes.class "textline"
                    ]
                    [ Html.text (String.fromList array |> String.left cursor)
                    , Html.span [ Html.Attributes.class ("cursor" ++ (mode == Insert |> bool " insert" "")) ] []
                    , Html.text (String.fromList array |> String.dropLeft cursor)
                    ]
                ]
            , Html.div
                [ Html.Attributes.class "controls"
                ]
                [ viewSelect modeToString modeFromString mode [ Insert, Replace ]
                    |> Html.map SelectedMode
                ]
            ]
        ]


modeToString : Mode -> String
modeToString mode =
    case mode of
        Insert ->
            "Insert"

        Replace ->
            "Replace"


modeFromString : String -> Maybe Mode
modeFromString string =
    case string of
        "Insert" ->
            Just Insert

        "Replace" ->
            Just Replace

        _ ->
            Nothing



-- view select


viewSelect : (a -> String) -> (String -> Maybe a) -> a -> List a -> Html a
viewSelect toString fromString selected items =
    Html.ul
        [ Html.Attributes.class "select"
        , Html.Events.on "click" (decodeSelectedItem fromString)
        ]
        (List.map (viewSelectItem toString selected) items)


viewSelectItem : (a -> String) -> a -> a -> Html b
viewSelectItem toString selected item =
    let
        label =
            item |> toString

        ( class, key ) =
            if item == selected then
                ( "selected", Encode.null )

            else
                ( "", Encode.string label )
    in
    Html.li
        [ Html.Attributes.class class
        , Html.Attributes.property "data-key" key
        ]
        [ Html.text label
        ]


decodeSelectedItem : (String -> Maybe a) -> Decoder a
decodeSelectedItem fromString =
    Decode.at [ "target", "data-key" ] Decode.string
        |> Decode.andThen (fromString >> maybeToDecoder)


maybeToDecoder : Maybe a -> Decoder a
maybeToDecoder m =
    case m of
        Just x ->
            Decode.succeed x

        Nothing ->
            Decode.fail "no-op"



-- helpers


bool : a -> a -> Bool -> a
bool t f x =
    if x then
        t

    else
        f
