# FsharpChess
Chess with an F# domain engine and a simple WPF UI.

I created this project as part of my ongoing effort to understand F#.  I am a full time C# developer, so I am currently trying to rethink my imperative approach to designing software of doing everything with mutable state. 

I would really love it if anyone has any ideas on how to make this better, more "functional", or anything, really.

# F# Domain

## Chess.fs
This contains the domain entities (nouns), use cases (actions or methods), and the implementation methods (moving, validation, etc).

## Api.fs
This provides a "ChessApi" class that allows the C# code to interface with the F# backend.  The ChessApi class contains a mutable copy of the game state (the board, which is just a dictionary/map of cells and optional pieces, whose turn it is, etc).  This is currently the only mutable state in the F# library.  The idea is that I didn't want to make the C# calling code responsible for receiving the state and passing in back in for each move, as that seems like a very functional way of doing things.  Instead, I wanted to "encapsulate" mutable state management within the F# ChessApi class. 

# C# WPF UI
The WPF UI is a very simple code-behind design (no MVVM here), because my purpose was to focus on the F# bits.
It creates an instance of the F# ChessApi class and it refreshes the UI after each move based on the newly generated game state.

# TODO
- [x] Implement "interposition" (or "blocking") - all pieces that capture linearly (all except the knight) should not be able to skip past enemy pieces.  They should only be allowed the capture the first piece in their movement path.
- [ ] Implement GameProgress to handle "check" and "checkmate" (game over + victor) and "stalement" status
- [ ] Create checkmate detection - engine needs to be able to detect when a move results in the other color's king having no valid escape moves
- [ ] Implement "legal" move detection - a player cannot move their own king into danger
- [ ] Implement stalemate detection - when a player is not in check, but has no legal moves
- [ ] If a player's king is in check, their next move MUST remove them from check state
