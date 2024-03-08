module Aoc2023.Day4

open System
open System.Buffers
open System.Collections.Generic

let numberSeparators = SearchValues.Create(" ")

let parseCardInfo (line: ReadOnlySpan<char>) (winningNumberHandler: int -> unit) (numberHandler: int -> unit) : int =

    // parse header <Card X:>
    assert line.Slice(0, 4).SequenceEqual("Card")
    let titleSeparator = line.IndexOf(':')
    let cardNumber = Int32.Parse(line.Slice(5, titleSeparator - 5).TrimStart())

    // parse card winning numbers and numbers you have
    let cardData = line.Slice(titleSeparator + 1)
    let mutable readingWinningNumbers = true
    for number in enumerateSpanValues cardData numberSeparators do
        if number.SequenceEqual("|") then
            readingWinningNumbers <- false
        else

        let number = Int32.Parse(number)
        if readingWinningNumbers then
            winningNumberHandler number
        else
            numberHandler number

    cardNumber

let calculatePoints (input: string) =

    let cardWinningNumberSet = HashSet()
    let addWinningCard (value: int) = cardWinningNumberSet.Add(value) |> ignore

    let mutable pointsTotal = 0
    let mutable cardPoints = 0
    let safeAddWinningNumberPoints (number: int) =
        if cardWinningNumberSet.Contains(number) then
            cardPoints <- if cardPoints = 0 then 1 else cardPoints * 2
        ()

    let addCardPointsToTotal () =
        if cardPoints > 0 then
            pointsTotal <- pointsTotal + cardPoints
            cardPoints <- 0
        cardWinningNumberSet.Clear()

    for line in enumerateSpanLines (input.AsSpan()) do
        parseCardInfo line addWinningCard safeAddWinningNumberPoints |> ignore
        addCardPointsToTotal ()

    pointsTotal