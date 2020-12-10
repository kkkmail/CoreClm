open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type Tracer() =
    member _.doTrace(message: string,
                      [<CallerMemberName; Optional; DefaultParameterValue("")>] memberName: string,
                      [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string,
                      [<CallerLineNumber; Optional; DefaultParameterValue(0)>] line: int) =
        printfn "%s" (sprintf "\nMessage: %s" message)
        printfn "%s" (sprintf "Member name: %s" memberName)
        printfn "%s" (sprintf "Source file path: %s" path)
        printfn "%s" (sprintf "Source line number: %d" line)


let tracer = new Tracer()

tracer.doTrace("Test message.")

let mult x y =
    tracer.doTrace("From local function.")
    x * y

type TestClass() =
    let doNothingImpl() =
        tracer.doTrace("From class member implementation.")
        4

    let add x y = x + y

    let add2 y =
        tracer.doTrace("From partialy applied function.")
        add 2 y

    member this.doNothing() =
        tracer.doTrace("From class member.")
        let x = doNothingImpl() |> mult 3
        add2 x


let t = TestClass()
let x = t.doNothing()

