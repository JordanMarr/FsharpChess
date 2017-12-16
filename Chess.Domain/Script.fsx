// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open Chess.Domain

// Define your library scripting code here
open Types
open Implementation

let board = initBoard()

let newBoard = move(board, (B,One), (C,Three))
