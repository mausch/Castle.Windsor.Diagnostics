module Main

open Castle.MicroKernel
open Castle.Windsor

let loadKernel (xmlFile: string) =
    let container = new WindsorContainer(xmlFile)
    container.Kernel



[<EntryPoint>]
let main args = 
    let compile s = printfn "Compiling %s..." s
    let outputName = ref "a.out"
    let verbose = ref false
    let warningLevel = ref 0
    let kk = ArgInfo("", ArgType.String ignore, "")
    let setref d s = d := s
    let specs = [
                "-o", ArgType.String (setref outputName), "Name of the output"
                "-v", ArgType.Set verbose, "Display additional information"
                "--warn", ArgType.Int (setref warningLevel), "Set warning level"
                "--", ArgType.Rest compile, "Stop parsing command line"
                ] |> List.map (fun (sh, ty, desc) -> ArgInfo(sh, ty, desc))
    0

