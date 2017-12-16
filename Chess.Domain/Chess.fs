namespace Chess.Domain

module Entities =
    
    type Color = 
    | White 
    | Black

    type PawnInfo = { HasMoved: bool; }

    type Rank = 
    | Pawn of PawnInfo
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

    type MoveValidationResult = | Valid | Invalid of string
    
module UseCases =
    open Entities

    type InitBoard = unit -> Board
    type Move = Board * Color * Cell * Cell  -> (Board * MoveValidationResult)
    type IsMoveValid = Board * Color * Cell * Cell -> MoveValidationResult

module Implementation =
    open Entities

    let blackPawn = Some (Black, Pawn { HasMoved = false })
    let whitePawn = Some (White, Pawn { HasMoved = false })
    
    let initBoard : UseCases.InitBoard = fun () -> 
        let cell col row = col, row
        let has color rank = Some (color, rank)

        Map [
                    (A,Eight), Some (Black,Rook); (B,Eight), Some(Black,Knight); cell C Eight, has Black Bishop; cell D Eight, has Black King; cell E Eight, has Black Queen; cell F Eight, has Black Bishop; cell G Eight, has Black Knight; cell H Eight, has Black Rook;
                    cell A Seven, blackPawn; cell B Seven, blackPawn; cell C Seven, blackPawn; cell D Seven, blackPawn; cell E Seven, blackPawn; cell F Seven, blackPawn; cell G Seven, blackPawn; cell H Seven, blackPawn; 
                    cell A Six, None; cell B Six, None; cell C Six, None; cell D Six, None; cell E Six, None; cell F Six, None; cell G Six, None; cell H Six, None;
                    cell A Five, None; cell B Five, None; cell C Five, None; cell D Five, None; cell E Five, None; cell F Five, None; cell G Five, None; cell H Five, None;
                    cell A Four, None; cell B Four, None; cell C Four, None; cell D Four, None; cell E Four, None; cell F Four, None; cell G Four, None; cell H Four, None;
                    cell A Three, None; cell B Three, None; cell C Three, None; cell D Three, None; cell E Three, None; cell F Three, None; cell G Three, None; cell H Three, None;
                    cell A Two, whitePawn; cell B Two, whitePawn; cell C Two, whitePawn; cell D Two, whitePawn; cell E Two, whitePawn; cell F Two, whitePawn; cell G Two, whitePawn; cell H Two, whitePawn; 
                    cell A One, has White Rook; cell B One, has White Knight; cell C One, has White Bishop; cell D One, has White King; cell E One, has White Queen; cell F One, has White Bishop; cell G One, has White Knight; cell H One, has White Rook;
        ]
    
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
        
    let isMoveValid : UseCases.IsMoveValid = fun (board, nextTurn, fromCell, toCell) ->
        
        let x = getHorizDist fromCell toCell
        let y = getVertDist fromCell toCell

        let validatePawn pi (fromColor, fromRank) toPieceOption =
            match toPieceOption with
            | Some toPiece -> // Moving to an occupied cell
                let (toColor, toRank) = toPiece
                let isEnemy = fromColor <> toColor
                // Check for diagonal captures
                match (fromColor, x, y, isEnemy) with
                | (White, 1, 1, true) -> Valid
                | (White, -1, 1, true) -> Valid
                | (Black, 1, -1, true) -> Valid
                | (Black, -1, -1, true) -> Valid
                | _ -> Invalid "Invalid move - can only capture an enemy piece"
            | None ->  // Moving to an empty cell
                // Check for straight non-captures
                match (fromColor, x, y, pi.HasMoved) with
                | (White, 0, 1, _) -> Valid         // can always move forward one space to an empty cell
                | (White, 0, 2, false) -> Valid     // can move forward two spaces only if pawn has not yet moved
                | (Black, 0, -1, _) -> Valid        // can always move forward one space to an empty cell
                | (Black, 0, -2, false) -> Valid    // can move forward two spaces only if pawn has not yet moved
                | _ -> Invalid "Invalid move"

        let validateKnight (fromColor, fromRank) toPieceOption =
            let isL = match (abs x, abs y) with | (1,2) -> true | (2,1) -> true | _ -> false
            if isL then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid else Invalid "Can only capture an enemy piece"
                | None -> // Moving to an empty cell
                    Valid
            else
                Invalid "Knight can only move in an L pattern"

        let validateRook (fromColor, fromRank) toPieceOption =
            let isUpDownLeftRight = (abs x > 0 && y = 0) || (x = 0 && abs y > 0)
            if isUpDownLeftRight then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid else Invalid "Can only capture an enemy piece"
                | None -> 
                    Valid
            else
                Invalid "Rook can only move up, down, left or right"

        let validateBishop (fromColor, fromRank) toPieceOption =
            let isDiag = (abs x = abs y)
            if isDiag then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid else Invalid "Can only capture an enemy piece"
                | None -> 
                    Valid
            else
                Invalid "Bishop can only move diagonally"

        let validateKing (fromColor, fromRank) toPieceOption =
            let isAnyDirectionOneSpace = (abs x = 1 || x = 0) && (abs y = 1 || y = 0)
            if isAnyDirectionOneSpace then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid else Invalid "Can only capture an enemy piece"
                | None -> 
                    Valid
            else
                Invalid "King can only move one space in any direction"

        let validateQueen (fromColor, fromRank) toPieceOption =
            let isAnyDirection = (abs x = abs y) || (abs x > 0 && y = 0) || (x = 0 && abs y > 0)
            if isAnyDirection then
                match toPieceOption with
                | Some toPiece -> // Moving to an occupied cell
                    let (toColor, toRank) = toPiece
                    let isEnemy = fromColor <> toColor
                    if isEnemy then Valid else Invalid "Can only capture an enemy piece"
                | None -> 
                    Valid
            else
                Invalid "Queen can only move diagonally, up, down, left or right"

        let pieceToMove = board.Item fromCell

        match pieceToMove with
        | Some fromPiece -> 
            let (fromPieceColor,fromPieceRank) = fromPiece
            if fromPieceColor = nextTurn then
                let toPiece = board.Item toCell
                match fromPieceRank with
                | Bishop -> validateBishop fromPiece toPiece
                | Rook -> validateRook fromPiece toPiece
                | King -> validateKing fromPiece toPiece
                | Queen -> validateQueen fromPiece toPiece
                | Knight -> validateKnight fromPiece toPiece
                | Pawn pi -> validatePawn pi fromPiece toPiece
            else
                Invalid "It is not your turn"

        | None -> Invalid "No piece was selected to move"
    
    let move : UseCases.Move = fun (board, nextTurn, fromCell, toCell) ->
        let validationResult = isMoveValid(board, nextTurn, fromCell, toCell)
        match validationResult with
        | Valid ->
            let fromPiece = board.Item fromCell
            match fromPiece with
            | Some (fromPieceColor,fromPieceRank) ->
                match fromPieceRank with
                | Pawn pi -> // If this is a pawn, set HasMoved = true
                    (board.Add(toCell, Some (fromPieceColor,Pawn { HasMoved = true})).Add(fromCell, None), validationResult)
                | _ -> 
                    (board.Add(toCell, Some (fromPieceColor,fromPieceRank)).Add(fromCell, None), validationResult)
            | None ->
                (board, validationResult)
        | Invalid err ->
            (board, validationResult)


        
