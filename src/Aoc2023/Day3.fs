module Aoc2023.Day3

open System
open System.Collections.Generic
open System.Globalization
open System.Runtime.InteropServices

[<Struct>]
type EnginePartPosition = {
    Top: int
    Left: int
}

[<Struct>]
type EnginePart = {
    Position: EnginePartPosition
    Number: int
}

let numberBuffer = ResizeArray()

let numberCharsBuffer = ResizeArray<char>()

let getAdjacentNumbers (input: string[]) (top: int) (left: int) =
    numberBuffer.Clear()

    let readNumber (input: string[]) (top: int) (left: int) : EnginePart =
        assert Char.IsDigit(input[top][left])
        numberCharsBuffer.Clear()

        let rec loop (input: string[]) (top: int) (mostLeft: int) (i: int) (searching: bool) =
            if searching then
                if i < 0 then
                    loop input top (i + 1) (i + 1) false
                else

                let char = input[top][i]
                if Char.IsDigit(char) then
                    loop input top i (i - 1) true
                else
                    loop input top (i + 1) (i + 1) false
            else

            if i >= input[0].Length then
                assert(numberCharsBuffer.Count > 0)
                let value = Int32.Parse(CollectionsMarshal.AsSpan(numberCharsBuffer), NumberStyles.Integer)
                struct (value, mostLeft)
            else

            let char = input[top][i]
            if Char.IsDigit(char) then
                numberCharsBuffer.Add(char)
                loop input top mostLeft (i + 1) false
            else
                assert(numberCharsBuffer.Count > 0)
                let value = Int32.Parse(CollectionsMarshal.AsSpan(numberCharsBuffer), NumberStyles.Integer)
                struct (value, mostLeft)

        let struct(number, mostLeft) = loop input top left left true
        { Position = { Top = top; Left = mostLeft }
          Number = number }

    let readNumberIntoBuffer input top left =
        readNumber input top left
        |> numberBuffer.Add

    if top > 0 then
        //>. . .
        // . . .
        // . . .
        if left > 0 then
            if Char.IsDigit(input[top - 1][left - 1]) then
                readNumberIntoBuffer input (top - 1) (left - 1)

        // .>. .
        // . . .
        // . . .
        if Char.IsDigit(input[top - 1][left]) then
            readNumberIntoBuffer input (top - 1) left

        // . .>.
        // . . .
        // . . .
        if left < input[0].Length - 1 then
            if Char.IsDigit(input[top - 1][left + 1]) then
                readNumberIntoBuffer input (top - 1) (left + 1)
    // . . .
    //>. . .
    // . . .
    if left > 0 then
        if Char.IsDigit(input[top][left - 1]) then
            readNumberIntoBuffer input top (left - 1)

    // . . .
    // .>. .
    // . . .
    // nothing

    // . . .
    // . .>.
    // . . .
    if left < input[0].Length - 1 then
        if Char.IsDigit(input[top][left + 1]) then
            readNumberIntoBuffer input top (left + 1)

    if top < input[0].Length - 1 then
        // . . .
        // . . .
        //>. . .
        if left > 0 then
            if Char.IsDigit(input[top + 1][left - 1]) then
                readNumberIntoBuffer input (top + 1) (left - 1)

        // . . .
        // . . .
        // .>. .
        if Char.IsDigit(input[top + 1][left]) then
            readNumberIntoBuffer input (top + 1) left

        // . . .
        // . . .
        // . .>.
        if left < input[0].Length - 1 then
            if Char.IsDigit(input[top + 1][left + 1]) then
                readNumberIntoBuffer input (top + 1) (left + 1)

    numberBuffer

let getEnginePartSum (input: string) =

    let countedNumberSet = HashSet()

    let lines = input.Split("\n", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    assert (lines |> Seq.forall (_.Length >> (=) lines[0].Length))

    let mutable enginePartSum = 0
    let addEnginePart (value: EnginePart) = enginePartSum <- enginePartSum + value.Number
    let safeAddEnginePart (value: EnginePart) =
        if countedNumberSet.Add(value.Position) then
            addEnginePart value

    for top = 0 to lines.Length - 1 do
        for left = 0 to lines[0].Length - 1 do
            let currentSymbol = lines[top][left]
            if not (currentSymbol = '.') && not (Char.IsDigit(currentSymbol)) then
                let engineParts = getAdjacentNumbers lines top left
                for enginePart in engineParts do
                    safeAddEnginePart enginePart

    enginePartSum
