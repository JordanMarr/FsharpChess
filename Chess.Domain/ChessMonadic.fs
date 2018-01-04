namespace Chess.Domain

open System
module Entities2 =
    
    type Color = 
    | White 
    | Black

    type Pawn =
    | NotMoved
    | Moved

    type Rank = 
    | Pawn of Pawn
    | Rook 
    | Bishop 
    | Knight 
    | Queen 
    | King

    type Piece = Color * Rank
    
    type Column = | A | B | C | D | E | F | G | H    
    type Row = | One | Two | Three | Four | Five | Six | Seven | Eight
    type Cell = Column * Row
    type Board = Map<Cell, Piece option>
    type GameState = { board: Board; nextMove: Color; message: string }

    type AttemptedMove = Cell * Cell
    type ValidatedMoveFrom = Piece * Cell * Cell
    type ValidationResult<'T> = | Valid of 'T | Invalid of string
    
    type ValidationBuilder() =
        member this.Bind(v, f) =
            match v with
            | Valid t -> f t
            | Invalid msg -> Invalid msg
        
        member this.Return(x) =
            Valid x                     

    (*
        USE CASES
    *)
    type InitBoard = unit -> Board
    type Move = GameState -> AttemptedMove -> ValidationResult<GameState>
    

module Implementation2 =
    open Entities2
            
    let initBoard : Entities2.InitBoard = fun () -> 
        let blackPawn = Some (Black, Pawn NotMoved)
        let whitePawn = Some (White, Pawn NotMoved)
        let white rank = Some (White, rank)
        let black rank = Some (Black, rank)

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

    let validateMoveFrom (gameState: GameState) (move: AttemptedMove) : ValidationResult<ValidatedMoveFrom> =
        let fromCell, toCell = move
        match gameState.board.[fromCell] with
        | Some (fromColor, fromRank) -> 
            if fromColor = gameState.nextMove
            then Valid ((fromColor, fromRank), fromCell, toCell)
            else Invalid "It's not your turn"
        | None -> 
            Invalid "No piece was selected to move"
        
    let validateMoveTo (gameState: GameState) (move: ValidatedMoveFrom) =
        let fromPiece, fromCell, toCell = move

        let getHorizDist (fromCol, fromRow) (toCol, toRow) =
            let columns = [A;B;C;D;E;F;G;H]
            let fromHorizIndex = List.findIndex (fun c -> c = fromCol) columns
            let toHorizIndex = List.findIndex (fun c -> c = toCol) columns
            toHorizIndex - fromHorizIndex

        let getVertDist (fromCol, fromRow) (toCol, toRow) =
            let rows = [One; Two; Three; Four; Five; Six; Seven; Eight]
            let fromVertIndex = List.findIndex (fun r -> r = fromRow) rows
            let toVertIndex = List.findIndex (fun r -> r = toRow) rows
            toVertIndex - fromVertIndex
                
        let x = getHorizDist fromCell toCell
        let y = getVertDist fromCell toCell

        let validatePawn (fromColor: Color) (pawn: Pawn) toPieceOption =   
            match toPieceOption with
            | Some toPiece -> // Moving to an occupied cell
                let (toColor, toRank) = toPiece
                let isEnemy = fromColor <> toColor
                // Check for diagonal captures
                match (fromColor, x, y, isEnemy) with
                | (White, 1, 1, true) -> Valid move
                | (White, -1, 1, true) -> Valid move
                | (Black, 1, -1, true) -> Valid move
                | (Black, -1, -1, true) -> Valid move
                | _ -> Invalid "Invalid move - can only capture an enemy piece"
            | None ->  // Moving to an empty cell
                // Check for straight non-captures
                match (fromColor, x, y, pawn) with
                | (White, 0, 1, _) -> Valid move         // can always move forward one space to an empty cell
                | (White, 0, 2, NotMoved) -> Valid move     // can move forward two spaces only if pawn has not yet moved
                | (Black, 0, -1, _) -> Valid move        // can always move forward one space to an empty cell
                | (Black, 0, -2, NotMoved) -> Valid move    // can move forward two spaces only if pawn has not yet moved
                | _ -> Invalid "Invalid move"

        let validateKnight fromPiece toPieceOption =
            let (fromColor, fromRank) = fromPiece
            let isL = match (abs x, abs y) with | (1,2) -> true | (2,1) -> true | _ -> false
            if isL then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid move else Invalid "Can only capture an enemy piece"
                | None -> // Moving to an empty cell
                    Valid move
            else
                Invalid "Knight can only move in an L pattern"

        let validateRook (fromColor, fromRank) toPieceOption =
            let isUpDownLeftRight = (abs x > 0 && y = 0) || (x = 0 && abs y > 0)
            if isUpDownLeftRight then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid move else Invalid "Can only capture an enemy piece"
                | None -> 
                    Valid move
            else
                Invalid "Rook can only move up, down, left or right"

        let validateBishop (fromColor, fromRank) toPieceOption =
            let isDiag = (abs x = abs y)
            if isDiag then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid move else Invalid "Can only capture an enemy piece"
                | None -> 
                    Valid move
            else
                Invalid "Bishop can only move diagonally"

        let validateKing (fromColor, fromRank) toPieceOption =
            let isAnyDirectionOneSpace = (abs x = 1 || x = 0) && (abs y = 1 || y = 0)
            if isAnyDirectionOneSpace then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid move else Invalid "Can only capture an enemy piece"
                | None -> 
                    Valid move
            else
                Invalid "King can only move one space in any direction"

        let validateQueen (fromColor, fromRank) toPieceOption =
            let isAnyDirection = (abs x = abs y) || (abs x > 0 && y = 0) || (x = 0 && abs y > 0)
            if isAnyDirection then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid move else Invalid "Can only capture an enemy piece"
                | None -> 
                    Valid move
            else
                Invalid "Queen can only move diagonally, up, down, left or right"
        
        let (fromPieceColor, fromPieceRank) = fromPiece
        let toPieceOpt = gameState.board.Item toCell

        match fromPieceRank with
        | Bishop -> validateBishop fromPiece toPieceOpt
        | Rook -> validateRook fromPiece toPieceOpt
        | King -> validateKing fromPiece toPieceOpt
        | Queen -> validateQueen fromPiece toPieceOpt
        | Knight -> validateKnight fromPiece toPieceOpt
        | Pawn p -> validatePawn fromPieceColor p toPieceOpt
                
    let updateBoard (board: Board) (move: ValidatedMoveFrom) : Board =
        let fromPiece, fromCell, toCell = move
        board.Add(toCell, Some fromPiece).Add(fromCell, None)

    let validation = new ValidationBuilder()     

    let doMove : Entities2.Move = fun (gameState: GameState) (move: AttemptedMove) ->
        validation {
            let! validatedFrom = validateMoveFrom gameState move
            let! validatedMove = validateMoveTo gameState validatedFrom
            let updatedBoard = updateBoard gameState.board validatedMove
            return { gameState with board = updatedBoard }
        }