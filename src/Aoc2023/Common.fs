module [<AutoOpen>] Aoc2023.Common

open System
open System.Buffers
open System.Runtime.CompilerServices
open System.Xml

[<Struct; IsByRefLike>]
type SpanEnumerator =

    val mutable input: ReadOnlySpan<char>
    val mutable current: ReadOnlySpan<char>
    val separators: SearchValues<char>

    new(input: ReadOnlySpan<char>, separators: SearchValues<char>) =
        { input = input
          current = ReadOnlySpan<char>.Empty
          separators = separators }

    member this.Current = this.current

    member this.MoveNext() =
        if this.input.IsEmpty then
            false
        else

        let nextLineIndex = this.input.IndexOfAny(this.separators)
        if nextLineIndex < 0 then
            this.current <- this.input
            this.input <- ReadOnlySpan<char>.Empty
            true
        else

        if nextLineIndex = 0 then
            this.input <- this.input.Slice(1)
            this.MoveNext()
        else

        this.current <- this.input.Slice(0, nextLineIndex).Trim()
        this.input <- this.input.Slice(nextLineIndex + 1)
        true

    member this.GetEnumerator() = this


let enumerateSpanValues (input: ReadOnlySpan<char>) (separators: SearchValues<char>) =
    SpanEnumerator(input, separators)

let private lineSearchValues = SearchValues.Create("\n")
let enumerateSpanLines (input: ReadOnlySpan<char>) : SpanEnumerator = enumerateSpanValues input lineSearchValues