module Editor exposing (Editor, Msg, init, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Rga exposing (Rga)
import Set


type alias Editor =
    { mode : Mode
    , cursor : Int
    , length : Int
    , array : Rga Char
    }


init : String -> Editor
init string =
    { mode = Insert
    , cursor = 0
    , length = String.length string
    , array = string |> String.toList |> Rga.fromList 0 Set.empty
    }



-- update


type Msg
    = PressedArrow Direction
    | PressedDelete Direction
    | EnteredChar Char
    | SelectedMode Mode
    | ToggledMode


type Direction
    = Left
    | Right


type Mode
    = Insert
    | Replace


update : Msg -> Editor -> Editor
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

        PressedDelete direction ->
            let
                indexAndCursor =
                    if direction == Left && 0 < model.cursor then
                        Just ( model.cursor, model.cursor - 1 )

                    else if direction == Right && model.cursor < model.length then
                        Just ( model.cursor + 1, model.cursor )

                    else
                        Nothing
            in
            case indexAndCursor of
                Just ( index, cursor ) ->
                    case model.array |> Rga.delete index of
                        Just ( array, _ ) ->
                            { model
                                | cursor = cursor
                                , length = model.length - 1
                                , array = array
                            }

                        Nothing ->
                            model

                Nothing ->
                    model

        EnteredChar char ->
            let
                ( f, index, length ) =
                    if model.mode == Insert || model.cursor == model.length then
                        ( Rga.insert, model.cursor, model.length + 1 )

                    else
                        ( Rga.update, model.cursor + 1, model.length )
            in
            case model.array |> f index char of
                Just ( array, _ ) ->
                    { model
                        | cursor = model.cursor + 1
                        , length = length
                        , array = array
                    }

                Nothing ->
                    model

        SelectedMode mode ->
            { model | mode = mode }

        ToggledMode ->
            { model | mode = model.mode == Insert |> bool Replace Insert }



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

        "Backspace" ->
            Decode.succeed (PressedDelete Left)

        "Delete" ->
            Decode.succeed (PressedDelete Right)

        "Insert" ->
            Decode.succeed ToggledMode

        _ ->
            case String.uncons key of
                Just ( char, "" ) ->
                    Decode.succeed (EnteredChar char)

                _ ->
                    Decode.fail "no-op"



-- view


view : Editor -> Html Msg
view { mode, cursor, length, array } =
    let
        string =
            array |> Rga.toList |> String.fromList |> String.replace " " "\u{00A0}"
    in
    Html.div
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
                [ Html.text (string |> String.left cursor)
                , Html.span [ Html.Attributes.class ("cursor" ++ (mode == Insert |> bool " insert" "")) ] []
                , Html.text (string |> String.dropLeft cursor)
                ]
            ]
        , Html.div
            [ Html.Attributes.class "controls"
            ]
            [ viewSelect modeToString modeFromString mode [ Insert, Replace ]
                |> Html.map SelectedMode
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
