namespace Chess.Domain

module Validation =
    type ValidationResult<'T> = | Valid of 'T | Invalid of string
        with 
            static member bind result f =
                match result with
                | Valid t -> f t
                | Invalid msg -> Invalid msg

    let (>>=) = ValidationResult<_>.bind

    type ValidationBuilder() =
        member this.Bind(v, f) =
            match v with
            | Valid t -> f t
            | Invalid msg -> Invalid msg
        
        member this.Return(x) =
            Valid x                     

        member this.ReturnFrom(x) =
            x
    
    let validation = new ValidationBuilder()     
    