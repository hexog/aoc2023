module [<AutoOpen>] Aoc2023.Test.Common

open System
open System.IO

let enumerateLines (text: string) = seq {
    use reader = new StringReader(text)
    let mutable line = null
    while (line <- reader.ReadLine(); line <> null) do
        if not (String.IsNullOrWhiteSpace(line)) then
            yield line
}