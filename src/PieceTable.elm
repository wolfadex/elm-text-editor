module PieceTable exposing
    ( Table, init
    , insert, delete, replace
    , toString
    )

{-|

@docs Table, init

@docs insert, delete, replace

@docs toString

-}

import String.Graphemes


type Table
    = Table TableInternal


type alias TableInternal =
    { original : String
    , add : String
    , pieces : List Piece
    }


type alias Piece =
    { offset : Int
    , length : Int
    , source : Source
    }


type Source
    = Original
    | Add


init : String -> Table
init original =
    Table
        { original = original
        , add = ""
        , pieces =
            [ { offset = 0, length = String.Graphemes.length original, source = Original }
            ]
        }


toString : Table -> String
toString (Table table) =
    table.pieces
        |> List.foldl
            (\piece str ->
                str
                    ++ String.Graphemes.slice
                        piece.offset
                        (piece.offset + piece.length)
                        (case piece.source of
                            Original ->
                                table.original

                            Add ->
                                table.add
                        )
            )
            ""


type Error
    = OutOfBounds


insert : Int -> String -> Table -> Result Error Table
insert offset str (Table table) =
    if String.Graphemes.isEmpty str then
        table
            |> Table
            |> Ok

    else if offset < 0 then
        Err OutOfBounds

    else
        sequenceOffsetToPieceIndexAndBufferOffset offset table
            |> Result.map
                (\( pieceIndex, originalPiece, bufferOffset ) ->
                    let
                        addBufferOffset =
                            String.Graphemes.length table.add
                    in
                    if originalPiece.source == Add && bufferOffset == originalPiece.offset + originalPiece.length && originalPiece.offset + originalPiece.length == addBufferOffset then
                        Table
                            { table
                                | add = table.add ++ str
                                , pieces =
                                    List.indexedMap
                                        (\index piece ->
                                            if index == pieceIndex then
                                                { piece | length = piece.length + String.Graphemes.length str }

                                            else
                                                piece
                                        )
                                        table.pieces
                            }

                    else
                        let
                            insertPieces =
                                [ { source = originalPiece.source
                                  , offset = originalPiece.offset
                                  , length = bufferOffset - originalPiece.offset
                                  }
                                , { source = Add
                                  , offset = addBufferOffset
                                  , length = String.Graphemes.length str
                                  }
                                , { source = originalPiece.source
                                  , offset = bufferOffset
                                  , length = originalPiece.length - (bufferOffset - originalPiece.offset)
                                  }
                                ]
                                    |> List.filter (\piece -> piece.length > 0)
                        in
                        Table
                            { table
                                | add = table.add ++ str
                                , pieces =
                                    List.take pieceIndex table.pieces
                                        ++ insertPieces
                                        ++ List.drop (pieceIndex + 1) table.pieces
                            }
                )


delete : Int -> Int -> Table -> Result Error Table
delete offset length (Table table) =
    if length == 0 then
        table
            |> Table
            |> Ok

    else if length < 0 then
        delete (offset + length) -length (Table table)

    else if offset < 0 then
        Err OutOfBounds

    else
        Result.map2
            (\( initialAffectedPieceIndex, initialPiece, initialBufferOffset ) ( finalAffectedPieceIndex, finalPiece, finalBufferOffset ) ->
                -- Is the delete at the beginning of the piece?
                if initialBufferOffset == initialPiece.offset then
                    Table
                        { table
                            | pieces =
                                List.indexedMap
                                    (\index piece ->
                                        if index == initialAffectedPieceIndex then
                                            { piece | offset = piece.offset + length, length = piece.length - length }

                                        else
                                            piece
                                    )
                                    table.pieces
                        }
                    -- Or at the end of the initialPiece?

                else if initialAffectedPieceIndex == finalAffectedPieceIndex && finalBufferOffset == initialPiece.offset + initialPiece.length then
                    Table
                        { table
                            | pieces =
                                List.indexedMap
                                    (\index piece ->
                                        if index == initialAffectedPieceIndex then
                                            { piece | length = piece.length - length }

                                        else
                                            piece
                                    )
                                    table.pieces
                        }

                else
                    let
                        deletePieces =
                            [ { source = initialPiece.source
                              , offset = initialPiece.offset
                              , length = initialBufferOffset - initialPiece.offset
                              }
                            , { source = finalPiece.source
                              , offset = finalBufferOffset
                              , length = finalPiece.length - (finalBufferOffset - finalPiece.offset)
                              }
                            ]
                                |> List.filter (\piece -> piece.length > 0)
                    in
                    Table
                        { table
                            | pieces =
                                List.take initialAffectedPieceIndex table.pieces
                                    ++ deletePieces
                                    ++ List.drop (finalAffectedPieceIndex - initialAffectedPieceIndex + 1) table.pieces
                        }
            )
            (sequenceOffsetToPieceIndexAndBufferOffset offset table)
            (sequenceOffsetToPieceIndexAndBufferOffset (offset + length) table)


replace : Int -> Int -> String -> Table -> Result Error Table
replace offset length str table =
    table
        |> delete offset length
        |> Result.andThen (insert offset str)



-- INTERNAL


sequenceOffsetToPieceIndexAndBufferOffset : Int -> TableInternal -> Result Error ( Int, Piece, Int )
sequenceOffsetToPieceIndexAndBufferOffset offset table =
    if offset < 0 then
        Err OutOfBounds

    else
        sequenceOffsetToPieceIndexAndBufferOffsetHelper 0 offset table.pieces


sequenceOffsetToPieceIndexAndBufferOffsetHelper : Int -> Int -> List Piece -> Result Error ( Int, Piece, Int )
sequenceOffsetToPieceIndexAndBufferOffsetHelper index remainingOffset pieces =
    case pieces of
        [] ->
            Err OutOfBounds

        piece :: rest ->
            if remainingOffset <= piece.length then
                Ok ( index, piece, piece.offset + remainingOffset )

            else
                sequenceOffsetToPieceIndexAndBufferOffsetHelper (index + 1) (remainingOffset - piece.length) rest
