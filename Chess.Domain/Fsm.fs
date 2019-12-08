namespace Chess.Domain

/// Finite State Machine
module Fsm =
    open Entities

    // Aliases
    type NextMove = Color
    type Winner = Color
    type Message = string
    type TotalMoves = int

    type State = 
        | InProgress of Board * NextMove * TotalMoves * Message
        | Checkmate of Board * Winner
        | Stalemate of Board

    type Event =
        | Initialize
        | Move of Cell * Cell

    let update state event =
        match state, event with
        | _, Initialize -> 
            let gs = Implementation.initGame()
            InProgress (gs.Board, gs.NextMove, 0, gs.Message)

        | InProgress (board, thisMove, totalMoves, _), Move (fromCell, toCell) ->
            let gs = { Board = board; NextMove = thisMove; Message = "" }
            let attemptedMove = { FromCell = fromCell; ToCell = toCell }

            match Implementation.validateMove gs attemptedMove with
            | Ok validatedMove -> 
                // Update board
                let board = Implementation.updateBoard board validatedMove

                // Evaluate board
                if Implementation.isCheckmate board thisMove then
                    let winner = thisMove
                    Checkmate (board, winner)

                elif Implementation.isStalemate board thisMove then
                    Stalemate board

                else
                    let nextMove = Implementation.toggleColor thisMove
                    let msg = 
                        if Implementation.isCheck board thisMove
                        then sprintf "Check!  %A moves next." nextMove
                        else sprintf "%A moves next." nextMove
                    InProgress (board, nextMove, totalMoves + 1, sprintf "%A moves next." msg)

            | Error errorMessage ->
                InProgress (board, thisMove, totalMoves, errorMessage)
                
        | Checkmate (board, winner), _ ->
            Checkmate (board, winner)

        | Stalemate (board), _ ->
            Stalemate board
