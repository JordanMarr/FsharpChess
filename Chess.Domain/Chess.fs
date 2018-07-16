namespace Chess.Domain
open System

module Entities =    
    type Color = | White | Black
    type HasMoved = | NotMoved | Moved
    type Rank = | Pawn of HasMoved | Rook | Bishop | Knight | Queen | King
    type Piece = Color * Rank    
    type Column = | A | B | C | D | E | F | G | H    
    type Row = | One | Two | Three | Four | Five | Six | Seven | Eight
    let columns = [A;B;C;D;E;F;G;H]
    let rows = [One; Two; Three; Four; Five; Six; Seven; Eight]
    type Cell = Column * Row
    type Board = Map<Cell, Piece option>
    type GameProgress = | InProgress | WhiteWins | BlackWins
    type GameState = { board: Board; nextMove: Color; message: string }
    type AttemptedMove = Cell * Cell
    type ValidatedMoveFrom = Piece * Cell * Cell
    
module Implementation =
    open Validation
    open Entities

    let initGame () = 
        let blackPawn = Some (Black, Pawn NotMoved)
        let whitePawn = Some (White, Pawn NotMoved)
        let white rank = Some (White, rank)
        let black rank = Some (Black, rank)

        let board = 
            Map [
                (A,Eight), black Rook; (B,Eight), black Knight; (C,Eight), black Bishop; (D,Eight), black King; (E,Eight), black Queen; (F,Eight), black Bishop; (G,Eight), black Knight; (H,Eight), black Rook;
                (A,Seven),  blackPawn; (B,Seven), blackPawn; (C,Seven), blackPawn; (D,Seven), blackPawn; (E,Seven), blackPawn; (F,Seven), blackPawn; (G,Seven), blackPawn; (H,Seven), blackPawn; 
                (A,Six), None; (B,Six), None; (C,Six), None; (D,Six), None; (E,Six), None; (F,Six), None; (G,Six), None; (H,Six), None;
                (A,Five), None; (B,Five), None; (C,Five), None; (D,Five), None; (E,Five), None; (F,Five), None; (G,Five), None; (H,Five), None;
                (A,Four), None; (B,Four), None; (C,Four), None; (D,Four), None; (E,Four), None; (F,Four), None; (G,Four), None; (H,Four), None;
                (A,Three), None; (B,Three), None; (C,Three), None; (D,Three), None; (E,Three), None; (F,Three), None; (G,Three), None; (H,Three), None;
                (A,Two), whitePawn; (B,Two), whitePawn; (C,Two), whitePawn; (D,Two), whitePawn; (E,Two), whitePawn; (F,Two), whitePawn; (G,Two), whitePawn; (H,Two), whitePawn; 
                (A,One), white Rook; (B,One), white Knight; (C,One), white Bishop; (D,One), white King; (E,One), white Queen; (F,One), white Bishop; (G,One), white Knight; (H,One), white Rook;
            ]

        {   board = board; 
            nextMove = White; 
            message = "Welcome to F# Chess!" }

    let validateTurn gameState move =
        let fromCell, toCell = move
        match gameState.board.[fromCell] with
        | Some (fromColor, fromRank) -> 
            if fromColor = gameState.nextMove
            then Ok ((fromColor, fromRank), fromCell, toCell)
            else Error "It's not your turn"
        | None -> 
            Error "No piece was selected to move"

    let validateNotFriendlyTarget gameState move =
        let fromPiece, fromCell, toCell = move
        match gameState.board.[toCell] with
        | Some (toColor, toRank) -> 
            if gameState.nextMove = toColor
            then Error "Can not take a friendly piece"
            else Ok move
        | None -> Ok move
        
    let getHorizDist (fromCol, fromRow) (toCol, toRow) =
        let fromHorizIndex = List.findIndex (fun c -> c = fromCol) columns
        let toHorizIndex = List.findIndex (fun c -> c = toCol) columns
        toHorizIndex - fromHorizIndex

    let getVertDist (fromCol, fromRow) (toCol, toRow) =
        let fromVertIndex = List.findIndex (fun r -> r = fromRow) rows
        let toVertIndex = List.findIndex (fun r -> r = toRow) rows
        toVertIndex - fromVertIndex

    let tryGetCell (colIdx,rowIdx) =
        if colIdx < columns.Length && colIdx >= 0 && rowIdx < columns.Length && rowIdx >= 0
        then Some (columns.[colIdx], rows.[rowIdx])
        else None
    
    let getCoords cell = 
        let col,row = cell
        let colIdx = List.findIndex (fun c -> c = col) columns
        let rowIdx = List.findIndex (fun r -> r = row) rows
        (colIdx, rowIdx)
        
    let validateMoveShape gameState move : Result<ValidatedMoveFrom, string> =
        let fromPiece, fromCell, toCell = move
        let (fromPieceColor, fromPieceRank) = fromPiece
        let toPieceOpt = gameState.board.Item toCell
                
        let xDelta = getHorizDist fromCell toCell
        let yDelta = getVertDist fromCell toCell
        
        let validateKnight () =
            let isL = match (abs xDelta, abs yDelta) with | (1,2) -> true | (2,1) -> true | _ -> false
            if isL 
            then Ok move
            else Error "Knight can only move in an L pattern"

        let validateRook () =
            let isUpDownLeftRight = (abs xDelta > 0 && yDelta = 0) || (xDelta = 0 && abs yDelta > 0)
            if isUpDownLeftRight 
            then Ok move
            else Error "Rook can only move up, down, left or right"

        let validateBishop () =
            let isDiag = (abs xDelta = abs yDelta)
            if isDiag 
            then Ok move
            else Error "Bishop can only move diagonally"

        let validateKing () =
            let isAnyDirectionOneSpace = (abs xDelta = 1 || xDelta = 0) && (abs yDelta = 1 || yDelta = 0)
            if isAnyDirectionOneSpace 
            then Ok move
            else Error "King can only move one space in any direction"

        let validateQueen () =
            let isAnyDirection = (abs xDelta = abs yDelta) || (abs xDelta > 0 && yDelta = 0) || (xDelta = 0 && abs yDelta > 0)
            if isAnyDirection 
            then Ok move
            else Error "Queen can only move diagonally, up, down, left or right"
        
        let validatePawn hasMoved =   
            match toPieceOpt with
            | Some toPiece -> // Moving to an occupied cell
                // Check for diagonal captures
                match (fromPieceColor, abs xDelta, yDelta) with
                | (White, 1, 1) 
                | (Black, 1, -1) 
                    -> Ok move
                | _ -> Error "Pawn can only capture moving one space diagonally"
            | None ->  // Moving to an empty cell
                // Check for straight non-captures
                match (fromPieceColor, xDelta, yDelta, hasMoved) with
                | (White, 0, 1, _) -> Ok move            // can always move forward one space to an empty cell
                | (White, 0, 2, NotMoved) -> Ok move     // can move forward two spaces only if pawn has not yet moved
                | (Black, 0, -1, _) -> Ok move           // can always move forward one space to an empty cell
                | (Black, 0, -2, NotMoved) -> Ok move    // can move forward two spaces only if pawn has not yet moved
                | _ -> Error "Pawn can move forward one space (or two spaces on the first move)"

        match fromPieceRank with
        | Bishop -> validateBishop ()
        | Rook -> validateRook ()
        | King -> validateKing ()
        | Queen -> validateQueen ()
        | Knight -> validateKnight ()
        | Pawn hasMoved -> validatePawn hasMoved    
    
    let validateNoInterposition gameState move =
        let fromPiece, fromCell, toCell = move
        let fromColor, fromRank = fromPiece
        let fromCol, fromRow = fromCell

        match fromRank with
        | Knight -> Ok move
        | _ -> 
            let xDelta = getHorizDist fromCell toCell
            let yDelta = getVertDist fromCell toCell

            let normalize n = if n > 0 then 1 elif n < 0 then -1 else 0
            let addVectors v1 v2 = (fst v1 + fst v2, snd v1 + snd v2)
            let unitVector = (normalize xDelta, normalize yDelta)

            let rec moveSeq startCell vector = 
                seq {
                    let nextCell = startCell
                                   |> getCoords
                                   |> addVectors vector
                                   |> tryGetCell
                
                    if nextCell.IsSome then
                        yield nextCell.Value
                        yield! moveSeq nextCell.Value vector
                }    

            let valid = moveSeq fromCell unitVector 
                        |> Seq.takeWhile (fun move -> move <> toCell)
                        |> Seq.forall (fun move -> gameState.board.[move].IsNone)
            
            if valid
            then Ok move
            else Error "Another piece is blocking this move"
                    
    let updateBoard (board: Board) move =
        let fromPiece, fromCell, toCell = move
        let fromPieceColor, fromPieceRank = fromPiece
        match fromPieceRank with
        | Pawn pi ->
            board.Add(fromCell, None).Add(toCell, Some (fromPieceColor, Pawn Moved))
        | _ -> 
            board.Add(fromCell, None).Add(toCell, Some (fromPieceColor, fromPieceRank))

    let updateNextMoveColor color = 
        match color with
        | Black -> White
        | White -> Black
        
    let validateMove gameState attemptedMove =
        attemptedMove
        |> validateTurn gameState
        >>= validateNotFriendlyTarget gameState
        >>= validateMoveShape gameState
        >>= validateNoInterposition gameState

    let move gameState attemptedMove =
        let validatedMove = validateMove gameState attemptedMove
        match validatedMove with
        | Ok move -> 
            { gameState with 
                        board = updateBoard gameState.board move
                        nextMove = updateNextMoveColor(gameState.nextMove) 
                        message = "" }
        | Error msg ->
            { gameState with message = msg }
