[<AutoOpen>]
module prelude

module Array = 
    let cast a = Array.map unbox a

let assertThrows<'e when 'e :> exn> f =
    let action = Gallio.Common.Action f
    MbUnit.Framework.Assert.Throws<'e> action |> ignore