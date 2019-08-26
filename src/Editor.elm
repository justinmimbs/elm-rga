module Editor exposing (Editor, Msg, apply, init, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Rga exposing (RemoteOp, Rga)
import Set exposing (Set)


type alias Editor =
    { mode : Mode
    , cursor : Int
    , length : Int
    , array : Rga Char
    }


init : Int -> Set Int -> String -> Editor
init site sites string =
    { mode = Insert
    , cursor = 0
    , length = String.length string
    , array = string |> String.toList |> Rga.fromList site sites
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


type alias Op =
    RemoteOp Char


update : Msg -> Editor -> ( Editor, Maybe Op )
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
                |> noOp

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
                        Just ( array, op ) ->
                            { model
                                | cursor = cursor
                                , length = model.length - 1
                                , array = array
                            }
                                |> withOp op

                        Nothing ->
                            model |> noOp

                Nothing ->
                    model |> noOp

        EnteredChar char ->
            let
                ( f, index, length ) =
                    if model.mode == Insert || model.cursor == model.length then
                        ( Rga.insert, model.cursor, model.length + 1 )

                    else
                        ( Rga.update, model.cursor + 1, model.length )
            in
            case model.array |> f index char of
                Just ( array, op ) ->
                    { model
                        | cursor = model.cursor + 1
                        , length = length
                        , array = array
                    }
                        |> withOp op

                Nothing ->
                    model |> noOp

        SelectedMode mode ->
            { model | mode = mode }
                |> noOp

        ToggledMode ->
            { model | mode = model.mode == Insert |> bool Replace Insert }
                |> noOp


noOp : a -> ( a, Maybe Op )
noOp x =
    ( x, Nothing )


withOp : Op -> a -> ( a, Maybe Op )
withOp op x =
    ( x, Just op )


apply : Op -> Editor -> Editor
apply op editor =
    let
        array =
            editor.array |> Rga.apply op

        length =
            array |> Rga.length

        cursor =
            Rga.translateIndex editor.cursor editor.array array
                |> Maybe.withDefault (editor.cursor |> min length)
    in
    { mode = editor.mode
    , cursor = cursor
    , length = length
    , array = array
    }



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

        "Enter" ->
            Decode.succeed (EnteredChar '\n')

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
view { mode, cursor, array } =
    let
        lines =
            array |> Rga.foldl String.cons "" |> String.reverse |> String.replace " " "\u{00A0}" |> String.lines
    in
    Html.div
        [ Html.Attributes.class "editor"
        , Html.Attributes.tabindex 0
        , Html.Events.on "keydown" decodeKeydown
        ]
        [ Html.div
            [ Html.Attributes.class "textarea"
            ]
            (viewLines mode cursor lines [])
        , Html.div
            [ Html.Attributes.class "controls"
            ]
            [ viewSelect modeToString modeFromString mode [ Insert, Replace ]
                |> Html.map SelectedMode
            ]
        ]


viewLines : Mode -> Int -> List String -> List (Html a) -> List (Html a)
viewLines mode cursor lines nodes =
    case lines of
        [] ->
            List.reverse nodes

        line :: rest ->
            let
                length =
                    String.length line
            in
            Html.div
                [ Html.Attributes.class "line"
                ]
                (if 0 <= cursor && cursor <= length then
                    [ Html.text (line |> String.left cursor)
                    , Html.span [ Html.Attributes.class ("cursor" ++ (mode == Insert |> bool " insert" "")) ] []
                    , Html.text (line |> String.dropLeft cursor)
                    ]

                 else
                    [ Html.text line
                    ]
                )
                :: nodes
                |> viewLines mode (cursor - (length + 1)) rest


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
