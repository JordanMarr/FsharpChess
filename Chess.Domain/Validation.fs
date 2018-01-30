namespace Chess.Domain

module Validation =

    let (>>=) result f =
        match result with
        | Ok t -> f t
        | Error msg -> Error msg
