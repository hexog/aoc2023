module Aoc2023.Day1

open System
open System.Buffers
open System.IO
open System.Text.RegularExpressions

let digitsSearchValues = SearchValues.Create("123456789")

let sumFirstAndLastDigit (line: ReadOnlySpan<char>) =
    let firstDigitIndex = line.IndexOfAny(digitsSearchValues)
    let lastDigitIndex = line.LastIndexOfAny(digitsSearchValues)
    assert (firstDigitIndex >= 0)
    assert (lastDigitIndex >= 0)
    int (line[firstDigitIndex] - '0') * 10 + int (line[lastDigitIndex] - '0')

let sumAllCalibrationValues (text: string) =
    let mutable buffer = text.AsSpan()
    let mutable sum = 0

    while buffer.Length > 0 do
        let lineSeparatorIndex = buffer.IndexOf(Environment.NewLine)
        if lineSeparatorIndex >= 0 then
            let line = buffer.Slice(0, lineSeparatorIndex)
            sum <- sum + sumFirstAndLastDigit line
            buffer <- buffer.Slice(lineSeparatorIndex + Environment.NewLine.Length)
        else
            sum <- sum + sumFirstAndLastDigit buffer
            buffer <- ReadOnlySpan<char>()

    sum

let digitStringStartRegex =
    Regex(
        "(one|two|three|four|five|six|seven|eight|nine|[1-9])",
        RegexOptions.Compiled ||| RegexOptions.Singleline
    )

let digitStringEndRegex =
    Regex(
        "(one|two|three|four|five|six|seven|eight|nine|[1-9])",
        RegexOptions.Compiled ||| RegexOptions.Singleline ||| RegexOptions.RightToLeft
    )

let digitStringToInt (digitString: ReadOnlySpan<char>) =
    if digitString.SequenceEqual("one") then 1
    elif digitString.SequenceEqual("two") then 2
    elif digitString.SequenceEqual("three") then 3
    elif digitString.SequenceEqual("four") then 4
    elif digitString.SequenceEqual("five") then 5
    elif digitString.SequenceEqual("six") then 6
    elif digitString.SequenceEqual("seven") then 7
    elif digitString.SequenceEqual("eight") then 8
    elif digitString.SequenceEqual("nine") then 9
    else failwith ""


let sumFirstAndLastDigitString (line: string) =
    let firstDigitMatch = digitStringStartRegex.Match(line)
    assert firstDigitMatch.Success
    let firstDigit =
        let value = firstDigitMatch.ValueSpan
        if value.Length = 1 then
            int value[0] - int '0'
        else
            digitStringToInt value
    let lastDigitMatch = digitStringEndRegex.Match(line)
    let lastDigit =
        let value = lastDigitMatch.ValueSpan
        if value.Length = 1 then
            int value[0] - int '0'
        else
            digitStringToInt value
    assert (firstDigit > 0 && firstDigit < 10)
    assert (lastDigit > 0 && lastDigit < 10)
    firstDigit * 10 + lastDigit

let sumAllCalibrationStringValues (text: string) =
    use reader = new StringReader(text)
    let mutable line = null
    let mutable sum = 0
    while (line <- reader.ReadLine(); line <> null) do
        sum <- sum + sumFirstAndLastDigitString line
    sum

let sumAllCalibrationStringValuesStupid (text: string) =
    let mutable sum = 0

    for line in text.Split(Environment.NewLine) do
        let mutable firstDigit = -1

        for i in 0 .. line.Length - 1 do
            let lineStart = line.Substring(i)

            if (lineStart[0] = '1' || lineStart.StartsWith("one")) && firstDigit = -1 then firstDigit <- 1
            elif (lineStart[0] = '2' || lineStart.StartsWith("two")) && firstDigit = -1 then firstDigit <- 2
            elif (lineStart[0] = '3' || lineStart.StartsWith("three")) && firstDigit = -1 then firstDigit <- 3
            elif (lineStart[0] = '4' || lineStart.StartsWith("four")) && firstDigit = -1 then firstDigit <- 4
            elif (lineStart[0] = '5' || lineStart.StartsWith("five")) && firstDigit = -1 then firstDigit <- 5
            elif (lineStart[0] = '6' || lineStart.StartsWith("six")) && firstDigit = -1 then firstDigit <- 6
            elif (lineStart[0] = '7' || lineStart.StartsWith("seven")) && firstDigit = -1 then firstDigit <- 7
            elif (lineStart[0] = '8' || lineStart.StartsWith("eight")) && firstDigit = -1 then firstDigit <- 8
            elif (lineStart[0] = '9' || lineStart.StartsWith("nine")) && firstDigit = -1 then firstDigit <- 9

        let mutable lastDigit = -1

        for i = line.Length downto 1 do
            let lineEnd = line.Substring(0, i)

            if (lineEnd[i - 1] = '1' || lineEnd.EndsWith("one")) && lastDigit = -1 then lastDigit <- 1
            elif (lineEnd[i - 1] = '2' || lineEnd.EndsWith("two")) && lastDigit = -1 then lastDigit <- 2
            elif (lineEnd[i - 1] = '3' || lineEnd.EndsWith("three")) && lastDigit = -1 then lastDigit <- 3
            elif (lineEnd[i - 1] = '4' || lineEnd.EndsWith("four")) && lastDigit = -1 then lastDigit <- 4
            elif (lineEnd[i - 1] = '5' || lineEnd.EndsWith("five")) && lastDigit = -1 then lastDigit <- 5
            elif (lineEnd[i - 1] = '6' || lineEnd.EndsWith("six")) && lastDigit = -1 then lastDigit <- 6
            elif (lineEnd[i - 1] = '7' || lineEnd.EndsWith("seven")) && lastDigit = -1 then lastDigit <- 7
            elif (lineEnd[i - 1] = '8' || lineEnd.EndsWith("eight")) && lastDigit = -1 then lastDigit <- 8
            elif (lineEnd[i - 1] = '9' || lineEnd.EndsWith("nine")) && lastDigit = -1 then lastDigit <- 9

        sum <- sum + (firstDigit * 10 + lastDigit)

    sum