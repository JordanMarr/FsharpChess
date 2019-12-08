namespace Tests

open NUnit.Framework
open Chess.Domain
open Chess.Domain.Entities
open Chess.Domain.Fsm
open FsUnit

module FSMTests =

    let cell = Api.deserializeCoord
    let attemptedMove c1 c2 = { FromCell = cell c1; ToCell = cell c2 }
    let moveEvent c1 c2 = Move (attemptedMove c1 c2)

    [<Test>]
    let ``Test FSM`` () =
        let state =
            NewGame
            |> update Initialize
            |> update (moveEvent "A2" "A4")
            |> update (moveEvent "B7" "B5")
            |> update (moveEvent "A4" "B5")
        
        match state with
        | InProgress gs ->
            gs.Board.Item(cell "B5") |> should equal (Some (White, Pawn Moved))
        | _ ->
            failwith "Unexpected state"

    [<Test>]
    let ``Test without FSM``() =
        let gs = Implementation.initGame()
        let gs = Implementation.move gs (attemptedMove "A2" "A4")
        let gs = Implementation.move gs (attemptedMove "B7" "B5")
        let gs = Implementation.move gs (attemptedMove "A4" "B5")

        gs.Board.Item(cell "B5") |> should equal (Some (White, Pawn Moved))
