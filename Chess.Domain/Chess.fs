namespace Chess.Domain

module Entities =    
    type Color = | White | Black
    type HasMoved = | NotMoved | Moved
    type Rank = | Pawn of HasMoved | Rook | Bishop | Knight | Queen | King
    type Piece = Color * Rank    
    type Column = | A | B | C | D | E | F | G | H  
        with static member List = [A;B;C;D;E;F;G;H]
    type Row = | One | Two | Three | Four | Five | Six | Seven | Eight 
        with static member List = [One; Two; Three; Four; Five; Six; Seven; Eight] 
    type Cell = { Col: Column; Row: Row }
    type Board = Map<Cell, Piece option>
    type GameProgress = | InProgress | WhiteWins | BlackWins
    type GameState = { Board: Board; NextMove: Color; Message: string }
    type AttemptedMove = { FromCell: Cell; ToCell: Cell }
    type Move = { FromPiece: Piece; FromCell: Cell; ToCell: Cell }
    
module Implementation =
    open Entities

    let initGame () = 
        let blackPawn = Some (Black, Pawn NotMoved)
        let whitePawn = Some (White, Pawn NotMoved)
        let white rank = Some (White, rank)
        let black rank = Some (Black, rank)

        let createRow row pieces =
            let cells = Column.List |> List.map (fun col -> { Col = col; Row = row })
            List.zip cells pieces 

        let board = 
            Map (   (createRow Eight    [black Rook;   black Knight;   black Bishop;   black King;     black Queen;    black Bishop;   black Knight;   black Rook]) @
                    (createRow Seven    [blackPawn;    blackPawn;      blackPawn;      blackPawn;      blackPawn;      blackPawn;      blackPawn;      blackPawn]) @
                    (createRow Six      [None;         None;           None;           None;           None;           None;           None;           None]) @
                    (createRow Five     [None;         None;           None;           None;           None;           None;           None;           None]) @
                    (createRow Four     [None;         None;           None;           None;           None;           None;           None;           None]) @
                    (createRow Three    [None;         None;           None;           None;           None;           None;           None;           None]) @
                    (createRow Two      [whitePawn;    whitePawn;      whitePawn;      whitePawn;      whitePawn;      whitePawn;      whitePawn;      whitePawn]) @
                    (createRow One      [white Rook;   white Knight;   white Bishop;   white King;     white Queen;    white Bishop;   white Knight;   white Rook]) )

        {   Board = board; 
            NextMove = White; 
            Message = "Welcome to F# Chess!" }

    let validateFromPieceTurn gameState (attemptedMove: AttemptedMove) : Result<Move, string> =
        match gameState.Board.[attemptedMove.FromCell] with
        | Some (fromColor, fromRank) -> 
            if fromColor = gameState.NextMove
            then Ok { FromPiece = (fromColor, fromRank); FromCell = attemptedMove.FromCell; ToCell = attemptedMove.ToCell }
            else Error "It's not your turn"
        | None -> 
            Error "No piece was selected to move"

    let validateNotFriendlyTarget gameState move =
        match gameState.Board.[move.ToCell] with
        | Some (toColor, toRank) -> 
            if gameState.NextMove = toColor
            then Error "Can not take a friendly piece"
            else Ok move
        | None -> Ok move
        
    let getHorizDist fromCell toCell =
        let toIdx = Column.List |> List.findIndex (fun c -> c = toCell.Col)
        let fromIdx = Column.List |> List.findIndex (fun c -> c = fromCell.Col)
        toIdx - fromIdx

    let getVertDist fromCell toCell =
        let toIdx = Row.List |> List.findIndex (fun r -> r = toCell.Row)
        let fromIdx = Row.List |> List.findIndex (fun r -> r = fromCell.Row)
        toIdx - fromIdx

    let tryGetCell (colIdx,rowIdx) =
        if colIdx < Column.List.Length && colIdx >= 0 && rowIdx < Column.List.Length && rowIdx >= 0
        then Some { Col = Column.List.[colIdx]; Row = Row.List.[rowIdx] }
        else None
    
    let getCoords cell = 
        let colIdx = Column.List |> List.findIndex (fun c -> c = cell.Col)
        let rowIdx = Row.List |> List.findIndex (fun r -> r = cell.Row)
        (colIdx, rowIdx)
        
    let validateMoveShape gameState move : Result<Move, string> =
        let (fromPieceColor, fromPieceRank) = move.FromPiece
        let toPieceOpt = gameState.Board.Item move.ToCell
                
        let xDelta = getHorizDist move.FromCell move.ToCell
        let yDelta = getVertDist move.FromCell move.ToCell
        
        let validateKnight() =
            let isL = match (abs xDelta, abs yDelta) with | (1,2) -> true | (2,1) -> true | _ -> false
            if isL 
            then Ok move
            else Error "Knight can only move in an L pattern"

        let validateRook() =
            let isUpDownLeftRight = (abs xDelta > 0 && yDelta = 0) || (xDelta = 0 && abs yDelta > 0)
            if isUpDownLeftRight 
            then Ok move
            else Error "Rook can only move up, down, left or right"

        let validateBishop() =
            let isDiag = (abs xDelta = abs yDelta)
            if isDiag 
            then Ok move
            else Error "Bishop can only move diagonally"

        let validateKing() =
            let isAnyDirectionOneSpace = (abs xDelta = 1 || xDelta = 0) && (abs yDelta = 1 || yDelta = 0)
            if isAnyDirectionOneSpace 
            then Ok move
            else Error "King can only move one space in any direction"

        let validateQueen() =
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
        | Bishop -> validateBishop()
        | Rook -> validateRook()
        | King -> validateKing()
        | Queen -> validateQueen()
        | Knight -> validateKnight()
        | Pawn hasMoved -> validatePawn hasMoved    
    
    let validateNoInterposition gameState move =
        match move.FromPiece with
        | color, Knight -> Ok move
        | _ -> 
            let xDelta = getHorizDist move.FromCell move.ToCell
            let yDelta = getVertDist move.FromCell move.ToCell

            let normalize n = if n > 0 then 1 elif n < 0 then -1 else 0
            let addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
            let unitVector = (normalize xDelta, normalize yDelta)

            let rec moveSeq startCell vector = 
                seq {
                    let nextCell = 
                        startCell
                        |> getCoords
                        |> addVectors vector
                        |> tryGetCell
                
                    if nextCell.IsSome then
                        yield nextCell.Value
                        yield! moveSeq nextCell.Value vector
                }    

            let valid = 
                moveSeq move.FromCell unitVector 
                |> Seq.takeWhile (fun nextCell -> nextCell <> move.ToCell)
                |> Seq.forall (fun nextCell -> gameState.Board.[nextCell].IsNone)
            
            if valid
            then Ok move
            else Error "Another piece is blocking this move"
                    
    let updateBoard (board: Board) move =
        let fromPieceColor, fromPieceRank = move.FromPiece
        match fromPieceRank with
        | Pawn pi ->
            board.Add(move.FromCell, None).Add(move.ToCell, Some (fromPieceColor, Pawn Moved))
        | _ -> 
            board.Add(move.FromCell, None).Add(move.ToCell, Some (fromPieceColor, fromPieceRank))

    let updateNextMoveColor color = 
        match color with
        | Black -> White
        | White -> Black
        
    let validateMove (gameState: GameState) (attemptedMove: AttemptedMove) =
        attemptedMove
        |> validateFromPieceTurn gameState
        |> Result.bind (validateNotFriendlyTarget gameState)
        |> Result.bind (validateMoveShape gameState)
        |> Result.bind (validateNoInterposition gameState)

    let move (gameState: GameState) (attemptedMove: AttemptedMove) =
        let validatedMove = validateMove gameState attemptedMove
        match validatedMove with
        | Ok move -> 
            { gameState with 
                Board = updateBoard gameState.Board move
                NextMove = updateNextMoveColor(gameState.NextMove) 
                Message = "" }
        | Error msg ->
            { gameState with Message = msg }
