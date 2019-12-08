namespace Tests

open NUnit.Framework
open Chess.Domain
open Chess.Domain.Entities
open Chess.Domain.Fsm
open FsUnit

module FSMTests =

    let cell = Api.deserializeCoord
    let move c1 c2 = Move { FromCell = cell c1; ToCell = cell c2 }

    [<Test>]
    let ``Test FSM`` () =
        let state =
            NewGame
            |> update Initialize
            |> update (move "A2" "A4")
            |> update (move "B7" "B5")
            |> update (move "A4" "B5")
        
        match state with
        | InProgress gs ->
            let piece = gs.Board.Item(cell "B5")
            piece |> should equal (Some (White, Pawn Moved))
        | _ ->
            failwith "Unexpected state"

