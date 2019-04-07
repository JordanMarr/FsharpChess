namespace Chess.Domain

module Api = 
    open Chess.Domain.Entities

    type CellDTO = { Coord: string; IsOccupied: bool; Color: string; Rank: string }
    
    /// cellStr ex: "{ coord: "A1", piece { rank: "Bishop", color: "White" }
    let deserializeCoord (cellStr: string) : Cell = 
        let colMap = List.zip ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H";] Column.List |> Map.ofList
        let rowMap = List.zip ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8";] Row.List |> Map.ofList
        match cellStr.ToCharArray() with
        | [| col; row |] -> { Col = colMap.[col.ToString()]; Row = rowMap.[row.ToString()] }
        | _ -> failwith "Invalid cell"
    
    let serializeCell cell =
        let colMap = List.zip Column.List ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H";] |> Map.ofList
        let rowMap = List.zip Row.List ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8";] |> Map.ofList
        colMap.[cell.Col] + rowMap.[cell.Row]

    let boardToCellsDto (board: Board) =
        let cell_piece_list = Map.toList board
        let cells = cell_piece_list |> List.map (fun (cell, piece) -> 
            match piece with
            | Some (color,rank) -> 
                { 
                    Coord = serializeCell cell
                    IsOccupied = true
                    Color = match color with | White -> "white" | Black -> "black"
                    Rank = match rank with | Pawn _ -> "pawn" | Rook -> "rook" | Bishop -> "bishop" | King -> "king" | Queen -> "queen" | Knight -> "knight"
                }
            | None ->
                {
                    Coord = serializeCell cell
                    IsOccupied = false
                    Color = null
                    Rank = null
                })
        cells

    type ChessApi() =
        
        let mutable gameState = Implementation.initGame()
        
        member this.InitGame() = gameState <- Implementation.initGame()

        member this.Cells with get() = boardToCellsDto gameState.Board |> List.toArray

        member this.Message with get() = gameState.Message

        member this.Move(fromCell: string, toCell: string) =
            let cell1 = deserializeCoord fromCell
            let cell2 = deserializeCoord toCell
            gameState <- Implementation.move gameState { FromCell = cell1; ToCell = cell2 }
            ()