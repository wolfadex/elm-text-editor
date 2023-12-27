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
            PieceTable.init "Hello,\nWorld and friends!"
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
    | SetCursor Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetCursor offset ->
            ( { model
                | cursors =
                    Nonempty.List.singleton
                        { offset = offset
                        , length = Nothing
                        }
              }
            , Cmd.none
            )

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
                    ( moveUp 1 model
                    , Cmd.none
                    )

                "ArrowDown" ->
                    ( moveDown 1 model
                    , Cmd.none
                    )

                "Enter" ->
                    ( addText "\n" model, Cmd.none )

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


moveUp : Int -> Model -> Model
moveUp amount model =
    let
        lines : List String
        lines =
            model.table
                |> PieceTable.toString
                |> String.Graphemes.split "\n"
    in
    { model
        | cursors =
            model.cursors
                |> Nonempty.List.map
                    (\cursor ->
                        { cursor
                            | offset =
                                let
                                    ( currentLine, localOffset, currentLineLength ) =
                                        findLine 0 lines cursor.offset

                                    endOfPreviousLine =
                                        List.take (currentLine - 1) lines
                                            |> List.foldl (\line total -> String.Graphemes.length line + total) 0

                                    endOfLine =
                                        List.take currentLine lines
                                            |> List.foldl (\line total -> String.Graphemes.length line + total) 0
                                in
                                min (endOfPreviousLine + localOffset - 1) endOfLine
                                    |> max 0
                            , length = Nothing
                        }
                    )
    }


moveDown : Int -> Model -> Model
moveDown amount model =
    let
        lines : List String
        lines =
            model.table
                |> PieceTable.toString
                |> String.Graphemes.split "\n"
    in
    { model
        | cursors =
            model.cursors
                |> Nonempty.List.map
                    (\cursor ->
                        { cursor
                            | offset =
                                let
                                    ( currentLine, localOffset, currentLineLength ) =
                                        findLine 0 lines cursor.offset

                                    goalPosition =
                                        cursor.offset + currentLineLength + 1

                                    linesToCount =
                                        List.take (currentLine + 2) lines

                                    endOfLine =
                                        linesToCount
                                            |> List.foldl (\line total -> String.Graphemes.length line + total) 0
                                            |> (+) (List.length linesToCount - 1)
                                in
                                min goalPosition endOfLine
                            , length = Nothing
                        }
                    )
    }


findLine : Int -> List String -> Int -> ( Int, Int, Int )
findLine row lines_ offset =
    case lines_ of
        [] ->
            ( row, offset, 0 )

        line :: [] ->
            ( row, offset, String.Graphemes.length line )

        line :: lines__ ->
            let
                lineLength =
                    String.Graphemes.length line
            in
            if offset <= lineLength then
                ( row, offset, lineLength )

            else
                findLine (row + 1) lines__ (offset - lineLength)


addText : String -> Model -> Model
addText text model =
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



----------
-- VIEW --
----------


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
        , viewTable model.table model.cursors
        ]
    }


viewTable : PieceTable.Table -> Nonempty.List.NonemptyList Cursor -> Html.Html Msg
viewTable table cursors =
    let
        tableRows =
            table
                |> PieceTable.toString
                |> String.Graphemes.split "\n"

        rowCount =
            List.length tableRows
    in
    tableRows
        |> List.foldl
            (\rowText ( rowIndex, rowOffset, rows ) ->
                ( rowIndex + 1
                , rowOffset
                    + String.Graphemes.length rowText
                    + (if rowIndex + 1 == rowCount then
                        0

                       else
                        1
                      )
                , rows
                    ++ [ viewRow
                            rowText
                            (if rowIndex + 1 == rowCount then
                                String.Graphemes.toList rowText ++ [ "" ]

                             else
                                String.Graphemes.toList (rowText ++ "\n")
                            )
                            rowOffset
                            cursors
                       ]
                )
            )
            ( 0, 0, [] )
        |> (\( _, _, rows ) -> rows)
        |> Html.p
            [ Html.Attributes.tabindex 0
            , Html.Events.on "keydown" decodeKeyDown
            , Html.Events.custom "paste" decodePaste
            ]


viewRow : String -> List String -> Int -> Nonempty.List.NonemptyList Cursor -> Html.Html Msg
viewRow rowText rowGrahpemes rowOffset cursors =
    rowGrahpemes
        |> List.indexedMap
            (\rowIndex char ->
                let
                    index =
                        rowOffset + rowIndex

                    cursorStyle =
                        if Nonempty.List.any (\cursor -> cursor.offset == index) cursors then
                            Css.cursor

                        else
                            emptyAttribute

                    setCursor =
                        Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( SetCursor index, True ))
                in
                case char of
                    "\n" ->
                        [ Html.span
                            [ cursorStyle
                            , setCursor
                            ]
                            []
                        , Html.br [] []
                        ]

                    " " ->
                        [ Html.span
                            [ cursorStyle
                            , Css.space
                            , setCursor
                            ]
                            [ Html.text char ]
                        ]

                    _ ->
                        [ Html.span [ cursorStyle, setCursor ] [ Html.text char ] ]
            )
        |> List.concat
        |> Html.div
            [ Html.Events.stopPropagationOn "click"
                (Json.Decode.succeed
                    ( SetCursor
                        (rowOffset + String.Graphemes.length rowText)
                    , True
                    )
                )
            ]


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
