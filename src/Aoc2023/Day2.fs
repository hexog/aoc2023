module Aoc2023.Day2

open System
open System.IO

[<Measure>]
type red

[<Measure>]
type blue

[<Measure>]
type green

type Game = {
    Id: int
    Pulls: BallSet array
    SmallestPlayableSet: BallSet
}

and BallSet = {
    Red: int<red>
    Blue: int<blue>
    Green: int<green>
}

let parseGame (line: string) =
    let parts = line.Split([| ':'; ';' |], StringSplitOptions.TrimEntries)
    assert (parts.Length >= 2)
    assert (parts[0].StartsWith("Game "))

    let id = Int32.Parse(parts[0].AsSpan("Game ".Length))
    let mutable maxRed = 0<red>
    let mutable maxBlue = 0<blue>
    let mutable maxGreen = 0<green>
    let pulls = [|
        for i = 1 to parts.Length - 1 do
            let colors = parts[i].Split(',', StringSplitOptions.TrimEntries)
            assert (colors.Length > 0)
            let mutable red = 0<red>
            let mutable blue = 0<blue>
            let mutable green = 0<green>

            for color in colors do
                let number = Int32.Parse(color.AsSpan(0, color.IndexOf(' ')))
                if color.EndsWith("red") then
                    red <- number * 1<red>
                    if red > maxRed then
                        maxRed <- red
                if color.EndsWith("blue") then
                    blue <- number * 1<blue>
                    if blue > maxBlue then
                        maxBlue <- blue
                if color.EndsWith("green") then
                    green <- number * 1<green>
                    if green > maxGreen then
                        maxGreen <- green

            yield { Red = red; Blue = blue; Green = green }
    |]

    { Id = id
      Pulls = pulls
      SmallestPlayableSet = { Red = maxRed; Blue = maxBlue; Green = maxGreen } }

let isPossibleSetForGame (ballSet: BallSet) (game: Game) =
    game.SmallestPlayableSet.Red <= ballSet.Red
    && game.SmallestPlayableSet.Blue <= ballSet.Blue
    && game.SmallestPlayableSet.Green <= ballSet.Green

let sumPlayableWithSetGameIds (input: string) =

    let gameSet = { Red = 12<red>; Blue = 14<blue>; Green = 13<green> }

    use reader = new StringReader(input)
    let mutable line = null
    let mutable sum = 0
    while (line <- reader.ReadLine(); line <> null) do
        let game = parseGame line
        if isPossibleSetForGame gameSet game then
            sum <- sum + game.Id
    sum