module Main exposing (main)

import Browser
import Editor exposing (Editor)
import Html exposing (Html)
import Html.Attributes
import Set


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init "hello, world", Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { editor : Editor
    }


init : String -> Model
init string =
    { editor = Editor.init string
    }


type Msg
    = EditorMsg Editor.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditorMsg editorMsg ->
            { model | editor = model.editor |> Editor.update editorMsg }


view : Model -> Browser.Document Msg
view { editor } =
    Browser.Document
        "test"
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "../app/css/style.css" ] []
        , Editor.view editor |> Html.map EditorMsg
        ]
