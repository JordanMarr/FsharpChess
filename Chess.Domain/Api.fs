namespace Chess.Domain

module Api = 
    open Chess.Domain.Entities

    type CellDTO = { coord: string; isOccupied: bool; color: string; rank: string }
    
    // Helpers
    let listColumns = [A; B; C; D; E; F; G; H]
    let listRows = [One; Two; Three; Four; Five; Six; Seven; Eight]

    // cellStr ex: "{ coord: "A1", piece { rank: "Bishop", color: "White" }
    let deserializeCoord (cellStr: string) : Cell = 
        let colMap = List.zip ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H";] listColumns |> Map.ofList
        let rowMap = List.zip ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8";] listRows |> Map.ofList
        match cellStr.ToCharArray() with
        | [| col; row |] -> (colMap.[col.ToString()],rowMap.[row.ToString()])
        | _ -> failwith "Invalid cell"
    
    let serializeCell cell =
        let (col,row) = cell
        let colMap = List.zip listColumns ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H";] |> Map.ofList
        let rowMap = List.zip listRows ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8";] |> Map.ofList
        colMap.[col] + rowMap.[row]

    let boardToCellsDto (board: Board) =
        let cell_piece_list = Map.toList board
        let cells = cell_piece_list |> List.map (fun (cell, piece) -> 
            match piece with
            | Some (color,rank) -> 
                { 
                    coord = serializeCell cell
                    isOccupied = true
                    color = match color with | White -> "white" | Black -> "black"
                    rank = match rank with | Pawn _ -> "pawn" | Rook -> "rook" | Bishop -> "bishop" | King -> "king" | Queen -> "queen" | Knight -> "knight"
                }
            | None ->
                {
                    coord = serializeCell cell
                    isOccupied = false
                    color = null
                    rank = null
                })
        cells

    type ChessApi() =
        
        let mutable gameState = Implementation.initGame()
        
        member this.InitGame() = gameState <- Implementation.initGame()

        member this.Cells with get() = boardToCellsDto gameState.board |> List.toArray

        member this.Message with get() = gameState.message

        member this.Move(fromCell: string, toCell: string) =
            let cell1 = deserializeCoord fromCell
            let cell2 = deserializeCoord toCell
            gameState <- Implementation.move gameState (cell1,cell2)
            ()