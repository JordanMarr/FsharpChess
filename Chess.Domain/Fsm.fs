namespace Chess.Domain

/// Finite State Machine
module Fsm =
    open Entities

    // Aliases
    type Winner = Color

    type State = 
        | NewGame
        | InProgress of GameState
        | Checkmate of Board * Winner
        | Stalemate of Board

    type Event =
        | Initialize
        | Move of AttemptedMove

    let update state event =
        match state, event with
        | NewGame, _
        | _, Initialize -> 
            Implementation.initGame() |> InProgress

        | InProgress gs, Move attemptedMove ->
            match Implementation.validateMove gs attemptedMove with
            | Ok validatedMove -> 
                // Update board
                let board = Implementation.updateBoard gs.Board validatedMove

                // Evaluate board
                if Implementation.isCheckmate board gs.NextMove then
                    let winner = gs.NextMove
                    Checkmate (board, winner)

                elif Implementation.isStalemate board gs.NextMove then
                    Stalemate board

                else
                    let nextMove = Implementation.toggleColor gs.NextMove
                    InProgress
                        { gs with 
                            NextMove = nextMove
                            Message =
                                if Implementation.isCheck board gs.NextMove
                                then sprintf "Check!  %A moves next." nextMove
                                else sprintf "%A moves next." nextMove }

            | Error errorMessage ->
                InProgress { gs with Message = errorMessage }
                
        | Checkmate (board, winner), _ ->
            Checkmate (board, winner)

        | Stalemate (board), _ ->
            Stalemate board
