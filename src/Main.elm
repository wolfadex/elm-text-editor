module Main exposing (main)

import Browser
import Css
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Nonempty.List
import PieceTable
import String.Graphemes
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { table : PieceTable.Table
    , cursors : Nonempty.List.NonemptyList Cursor
    }


type alias Cursor =
    { offset : Int
    , length : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        table =
            PieceTable.init "Hello, World!"
    in
    ( { table =
            table

      -- |> PieceTable.insert 12 " Bob"
      -- |> Result.andThen (PieceTable.delete 7 6)
      -- |> Result.andThen (PieceTable.delete 0 5)
      -- |> Result.andThen (PieceTable.insert 0 "Goodbye")
      -- |> Result.andThen (PieceTable.replace 4 3 " morning")
      -- |> Result.andThen (PieceTable.insert 12 "\n")
      -- |> Result.toMaybe
      -- |> Maybe.withDefault table
      , cursors =
            Nonempty.List.singleton
                { offset = 0, length = Nothing }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = NoOp
    | KeyDown String
    | Paste String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyDown key ->
            case key of
                "ArrowRight" ->
                    ( { model
                        | cursors =
                            model.cursors
                                |> Nonempty.List.map
                                    (\cursor ->
                                        { cursor
                                            | offset =
                                                min
                                                    (PieceTable.toString model.table |> String.Graphemes.length)
                                                    (cursor.offset + 1)
                                        }
                                    )
                      }
                    , Cmd.none
                    )

                "ArrowLeft" ->
                    ( { model
                        | cursors =
                            model.cursors
                                |> Nonempty.List.map
                                    (\cursor ->
                                        { cursor
                                            | offset = max 0 (cursor.offset - 1)
                                        }
                                    )
                      }
                    , Cmd.none
                    )

                "Backspace" ->
                    ( { model
                        | table =
                            model.cursors
                                |> Nonempty.List.toList
                                |> List.sortBy .offset
                                |> List.foldr
                                    (\cursor table ->
                                        table
                                            |> Result.andThen
                                                (case cursor.length of
                                                    Just length ->
                                                        PieceTable.delete cursor.offset length

                                                    Nothing ->
                                                        PieceTable.delete (cursor.offset - 1) 1
                                                )
                                    )
                                    (Ok model.table)
                                |> Result.toMaybe
                                |> Maybe.withDefault model.table
                        , cursors =
                            model.cursors
                                |> Nonempty.List.toList
                                |> List.sortBy .offset
                                |> List.foldl
                                    (\cursor ( additionalOffset, cursors ) ->
                                        ( case cursor.length of
                                            Nothing ->
                                                additionalOffset + 1

                                            Just length ->
                                                additionalOffset + length
                                        , { cursor
                                            | length = Nothing
                                            , offset =
                                                case cursor.length of
                                                    Nothing ->
                                                        cursor.offset - 1 - additionalOffset

                                                    Just _ ->
                                                        cursor.offset - additionalOffset
                                          }
                                            :: cursors
                                        )
                                    )
                                    ( 0, [] )
                                |> Tuple.second
                                |> Nonempty.List.fromList
                                |> Maybe.withDefault model.cursors
                      }
                    , Cmd.none
                    )

                "ArrowUp" ->
                    let
                        _ =
                            Debug.log "todo" key
                    in
                    ( model, Cmd.none )

                "ArrowDown" ->
                    let
                        _ =
                            Debug.log "todo" key
                    in
                    ( model, Cmd.none )

                "Tab" ->
                    let
                        _ =
                            Debug.log "todo" key
                    in
                    ( model, Cmd.none )

                "Shift" ->
                    let
                        _ =
                            Debug.log "todo" key
                    in
                    ( model, Cmd.none )

                "Control" ->
                    let
                        _ =
                            Debug.log "todo" key
                    in
                    ( model, Cmd.none )

                "Enter" ->
                    let
                        _ =
                            Debug.log "todo" key
                    in
                    ( model, Cmd.none )

                "Alt" ->
                    let
                        _ =
                            Debug.log "todo" key
                    in
                    ( model, Cmd.none )

                "Meta" ->
                    let
                        _ =
                            Debug.log "todo" key
                    in
                    ( model, Cmd.none )

                "Escape" ->
                    let
                        _ =
                            Debug.log "todo" key
                    in
                    ( model, Cmd.none )

                _ ->
                    ( addText key model
                    , Cmd.none
                    )

        Paste text ->
            ( addText text model
            , Cmd.none
            )


addText : String -> Model -> Model
addText text model =
    let
        _ =
            Debug.log "addText" text
    in
    { model
        | table =
            model.cursors
                |> Nonempty.List.toList
                |> List.sortBy .offset
                |> List.foldr
                    (\cursor table ->
                        table
                            |> Result.andThen
                                (case cursor.length of
                                    Just length ->
                                        PieceTable.replace cursor.offset length text

                                    Nothing ->
                                        PieceTable.insert cursor.offset text
                                )
                    )
                    (Ok model.table)
                |> Result.toMaybe
                |> Maybe.withDefault model.table
        , cursors =
            let
                baseOffset =
                    String.Graphemes.length text
            in
            model.cursors
                |> Nonempty.List.toList
                |> List.sortBy .offset
                |> List.foldl
                    (\cursor ( additionalOffset, cursors ) ->
                        ( case cursor.length of
                            Nothing ->
                                additionalOffset + baseOffset

                            Just length ->
                                additionalOffset + (baseOffset - length)
                        , { cursor
                            | length = Nothing
                            , offset = cursor.offset + baseOffset + additionalOffset
                          }
                            :: cursors
                        )
                    )
                    ( 0, [] )
                |> Tuple.second
                |> Nonempty.List.fromList
                |> Maybe.withDefault model.cursors
    }


view : Model -> Browser.Document Msg
view model =
    { title = "Text Editor"
    , body =
        [ Html.div
            []
            [ Html.button
                []
                [ Html.text "Undo" ]
            , Html.button
                []
                [ Html.text "Repo" ]
            ]
        , model.table
            |> PieceTable.toString
            |> String.Graphemes.toList
            |> (\chars -> chars ++ [ "" ])
            |> List.indexedMap
                (\index char ->
                    let
                        cursorStyle =
                            if
                                Nonempty.List.any (\cursor -> cursor.offset == index)
                                    model.cursors
                            then
                                Css.cursor

                            else
                                emptyAttribute
                    in
                    case char of
                        "\n" ->
                            [ Html.span [ cursorStyle ] [], Html.br [] [] ]

                        " " ->
                            [ Html.span
                                [ cursorStyle
                                , Css.space
                                ]
                                [ Html.text char ]
                            ]

                        _ ->
                            [ Html.span [ cursorStyle ] [ Html.text char ] ]
                )
            |> List.concat
            |> Html.p
                [ Html.Attributes.tabindex 0
                , Html.Events.on "keydown" decodeKeyDown
                , Html.Events.custom "paste" decodePaste
                ]
        ]
    }


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.map3 (\key meta control -> ( KeyDown key, meta, control ))
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)
        |> Json.Decode.andThen
            (\( keydown, meta, control ) ->
                if meta || control then
                    Json.Decode.fail "ignore"

                else
                    Json.Decode.succeed keydown
            )


decodePaste :
    Json.Decode.Decoder
        { message : Msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
decodePaste =
    Json.Decode.map
        (\text ->
            { message = Paste text
            , stopPropagation = True
            , preventDefault = True
            }
        )
        (Json.Decode.field "__clipboardData" Json.Decode.string)


emptyAttribute : Html.Attribute msg
emptyAttribute =
    Html.Attributes.class ""
