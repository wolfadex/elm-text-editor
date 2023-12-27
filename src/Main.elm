module Main exposing (main)

import Browser
import Css
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Nonempty.List
import PieceTable
import String.Extra
import String.Graphemes
import Unicode


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-----------
-- MODEL --
-----------


type alias Model =
    { table : PieceTable.Table
    , cursors : Nonempty.List.NonemptyList Cursor
    }


type alias Cursor =
    { offset : Int
    , length : Maybe Int
    }



----------
-- INIT --
----------


init : () -> ( Model, Cmd Msg )
init _ =
    ( { table = PieceTable.init ""
      , cursors =
            Nonempty.List.singleton
                { offset = 0, length = Nothing }
      }
    , Cmd.none
    )



-------------------
-- SUBSCRIPTIONS --
-------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



------------
-- UPDATE --
------------


type Msg
    = KeyDown Bool String
    | Paste String
    | SetCursor Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        KeyDown alt key ->
            case key of
                "ArrowRight" ->
                    ( if alt then
                        jumpRight 1 model

                      else
                        moveRight 1 model
                    , Cmd.none
                    )

                "ArrowLeft" ->
                    ( if alt then
                        jumpLeft 1 model

                      else
                        moveLeft 1 model
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


moveRight : Int -> Model -> Model
moveRight amount model =
    { model
        | cursors =
            model.cursors
                |> Nonempty.List.map
                    (moveCursorBy
                        (model.table
                            |> PieceTable.toString
                            |> String.Graphemes.length
                        )
                        1
                    )
    }


moveCursorBy : Int -> Int -> Cursor -> Cursor
moveCursorBy maximum amount cursor =
    { cursor
        | offset =
            (cursor.offset + amount)
                |> min maximum
                |> max 0
    }


jumpRight : Int -> Model -> Model
jumpRight amount model =
    jumpSideways
        (\content cursor ->
            moveCursorBy
                (model.table
                    |> PieceTable.toString
                    |> String.Graphemes.length
                )
                (content
                    |> String.Graphemes.dropLeft cursor.offset
                    |> findNSeparatorIndex amount
                )
                cursor
        )
        model


jumpLeft : Int -> Model -> Model
jumpLeft amount model =
    jumpSideways
        (\content cursor ->
            moveCursorBy
                (model.table
                    |> PieceTable.toString
                    |> String.Graphemes.length
                )
                (content
                    |> String.Graphemes.left cursor.offset
                    |> String.Graphemes.reverse
                    |> findNSeparatorIndex amount
                    |> negate
                )
                cursor
        )
        model


jumpSideways : (String -> Cursor -> Cursor) -> Model -> Model
jumpSideways jumpFn model =
    let
        content : String
        content =
            model.table
                |> PieceTable.toString
    in
    { model
        | cursors =
            model.cursors
                |> Nonempty.List.map (jumpFn content)
    }


findNSeparatorIndex : Int -> String -> Int
findNSeparatorIndex separators str =
    let
        graphemes : List String
        graphemes =
            str
                |> String.Graphemes.toList

        stopFn : String -> Bool
        stopFn =
            case graphemes of
                [] ->
                    String.Extra.isBlank

                firstGrapheme :: _ ->
                    if String.Extra.isBlank firstGrapheme then
                        String.Extra.isBlank >> not

                    else
                        case String.uncons firstGrapheme of
                            Just ( firstChar, _ ) ->
                                if Unicode.isAlphaNum firstChar then
                                    isNonAlphaNum

                                else
                                    \s -> String.Extra.isBlank s || not (isNonAlphaNum s)

                            Nothing ->
                                String.Extra.isBlank
    in
    findNSeparatorIndexHelper stopFn separators 0 graphemes


isNonAlphaNum : String -> Bool
isNonAlphaNum =
    String.uncons
        >> Maybe.map (Tuple.first >> Unicode.isAlphaNum >> not)
        >> Maybe.withDefault False


findNSeparatorIndexHelper : (String -> Bool) -> Int -> Int -> List String -> Int
findNSeparatorIndexHelper stopFn separators index graphemes =
    case graphemes of
        [] ->
            index

        grapheme :: rest ->
            if stopFn grapheme && separators < 2 then
                index

            else
                findNSeparatorIndexHelper stopFn (separators - 1) (index + 1) rest


moveLeft : Int -> Model -> Model
moveLeft amount model =
    { model
        | cursors =
            model.cursors
                |> Nonempty.List.map
                    (\cursor ->
                        { cursor
                            | offset = max 0 (cursor.offset - amount)
                        }
                    )
    }


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
                                    ( currentLine, localOffset, _ ) =
                                        findLine 0 lines cursor.offset

                                    endOfPreviousLine : Int
                                    endOfPreviousLine =
                                        List.take (currentLine - 1) lines
                                            |> List.foldl (\line total -> String.Graphemes.length line + total) 0

                                    endOfLine : Int
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
                                    ( currentLine, _, currentLineLength ) =
                                        findLine 0 lines cursor.offset

                                    goalPosition : Int
                                    goalPosition =
                                        cursor.offset + currentLineLength + 1

                                    linesToCount : List String
                                    linesToCount =
                                        List.take (currentLine + 2) lines

                                    endOfLine : Int
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
                lineLength : Int
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
                baseOffset : Int
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
        tableRows : List String
        tableRows =
            table
                |> PieceTable.toString
                |> String.Graphemes.split "\n"

        rowCount : Int
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
                    index : Int
                    index =
                        rowOffset + rowIndex

                    cursorStyle : Html.Attribute Msg
                    cursorStyle =
                        if Nonempty.List.any (\cursor -> cursor.offset == index) cursors then
                            Css.cursor

                        else
                            emptyAttribute

                    setCursor : Html.Attribute Msg
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
    Json.Decode.map4 (\key meta control alt -> ( KeyDown alt key, meta, control ))
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)
        (Json.Decode.field "altKey" Json.Decode.bool)
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
