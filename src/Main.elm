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
    { editor1 : Editor
    , editor2 : Editor
    }


init : String -> Model
init string =
    { editor1 = Editor.init 1 (Set.fromList [ 1, 2 ]) string
    , editor2 = Editor.init 2 (Set.fromList [ 1, 2 ]) string
    }


type Msg
    = Editor1 Editor.Msg
    | Editor2 Editor.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Editor1 editorMsg ->
            let
                ( editor1, editor2 ) =
                    ( model.editor1, model.editor2 ) |> updateEditors editorMsg
            in
            { model
                | editor1 = editor1
                , editor2 = editor2
            }

        Editor2 editorMsg ->
            let
                ( editor2, editor1 ) =
                    ( model.editor2, model.editor1 ) |> updateEditors editorMsg
            in
            { model
                | editor1 = editor1
                , editor2 = editor2
            }


updateEditors : Editor.Msg -> ( Editor, Editor ) -> ( Editor, Editor )
updateEditors msg ( local, remote ) =
    case Editor.update msg local of
        ( local_, Just op ) ->
            ( local_, remote |> Editor.apply op )

        ( local_, Nothing ) ->
            ( local_, remote )


view : Model -> Browser.Document Msg
view { editor1, editor2 } =
    Browser.Document
        "test"
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "../app/css/style.css" ] []
        , Editor.view editor1 |> Html.map Editor1
        , Editor.view editor2 |> Html.map Editor2
        ]
