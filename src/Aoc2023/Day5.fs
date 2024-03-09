module Aoc2023.Day5

open System
open System.Threading.Tasks
open FSharp.Core.Operators.Checked

[<Measure>]
type seed

[<Measure>]
type soil

[<Measure>]
type fertilizer

[<Measure>]
type water

[<Measure>]
type light

[<Measure>]
type temperature

[<Measure>]
type humidity

[<Measure>]
type location

[<Measure>]
type range

type Mapping<[<Measure>] 'source, [<Measure>] 'destination> = {
    DestinationRangeStart: uint64<'destination>
    SourceRangeStart: uint64<'source>
    RangeLength: uint64<range>
}

type SeedMappings = {
    SeedToSoil: Mapping<seed, soil> list
    SoilToFertilizer: Mapping<soil, fertilizer> list
    FertilizerToWater: Mapping<fertilizer, water> list
    WaterToLight: Mapping<water, light> list
    LightToTemperature: Mapping<light, temperature> list
    TemperatureToHumidity: Mapping<temperature, humidity> list
    HumidityToLocation: Mapping<humidity, location> list
}

let calculateMappedValue<[<Measure>] 'source, [<Measure>] 'destination>
    (mapping: Mapping<'source, 'destination>)
    (sourceValue: uint64<'source>)
    : uint64<'destination> voption
    =
    let calculateRangeMaxValue (minValue: uint64<'source>) (range: uint64<range>) : uint64<'source> =
        (uint64 minValue) + (uint64 range - 1UL) |> LanguagePrimitives.UInt64WithMeasure

    let calculateDistance (minValue: uint64<'source>) (value: uint64<'source>) : uint64<range> =
        (uint64 value) - (uint64 minValue) |> LanguagePrimitives.UInt64WithMeasure

    let calculatePositionOnDestinationRange (minValue: uint64<'destination>) (distance: uint64<range>) : uint64<'destination> =
        (uint64 minValue) + (uint64 distance) |> LanguagePrimitives.UInt64WithMeasure

    let sourceMinValue = mapping.SourceRangeStart
    if sourceValue < sourceMinValue then
        ValueNone
    else

    let sourceMaxValue = calculateRangeMaxValue sourceMinValue mapping.RangeLength
    if sourceValue > sourceMaxValue then
        ValueNone
    else

    let distance = calculateDistance sourceMinValue sourceValue
    let destinationMinValue = mapping.DestinationRangeStart
    let destination = calculatePositionOnDestinationRange destinationMinValue distance

    ValueSome destination

let calculateFirstMapping
    (mappings: Mapping<'source, 'destination> list)
    (sourceValue: uint64<'source>)
    : uint64<'destination> =
    let convert (value: uint64<'source>) : uint64<'destination> =
        (uint64 value) |> LanguagePrimitives.UInt64WithMeasure

    let rec tryUseCorrectMapping (mappings: Mapping<'source, 'destination> list) (sourceValue: uint64<'source>) =
        match mappings with
        | [] -> ValueNone
        | mapping :: mappings ->
            match calculateMappedValue mapping sourceValue with
            | ValueSome _ as success -> success
            | ValueNone -> tryUseCorrectMapping mappings sourceValue

    match tryUseCorrectMapping mappings sourceValue with
    | ValueSome result -> result
    | ValueNone -> convert sourceValue

let mapper (mappings: SeedMappings) (input: uint64<seed>) : uint64<location> =
    input
    |> calculateFirstMapping mappings.SeedToSoil
    |> calculateFirstMapping mappings.SoilToFertilizer
    |> calculateFirstMapping mappings.FertilizerToWater
    |> calculateFirstMapping mappings.WaterToLight
    |> calculateFirstMapping mappings.LightToTemperature
    |> calculateFirstMapping mappings.TemperatureToHumidity
    |> calculateFirstMapping mappings.HumidityToLocation


let parseMapping<[<Measure>] 'source, [<Measure>] 'destination> (input: string list) =
    let parseLine (line: string) : Mapping<'source, 'destination> =
        let parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        assert (parts.Length = 3)
        let destinationRangeStart = UInt64.Parse(parts[0])
        let sourceRangeStart = UInt64.Parse(parts[1])
        let rangeLength = UInt64.Parse(parts[2])
        { DestinationRangeStart = destinationRangeStart |> LanguagePrimitives.UInt64WithMeasure
          SourceRangeStart = sourceRangeStart |> LanguagePrimitives.UInt64WithMeasure
          RangeLength = rangeLength * 1UL<range> }

    let rec loop input mappings =
        match input with
        | [] -> mappings
        | line :: input ->
            let mapping = parseLine line
            loop input (mapping :: mappings)

    loop input []

let parseMappings (input: string) =
    let parts =
        input.Split("""

""",
        StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

    let seeds =
        let seedsString = parts[0]
        assert seedsString.StartsWith("seeds: ")
        seedsString.Substring("seeds: ".Length)
            .Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.map (fun x -> UInt64.Parse(x) * 1UL<seed>)
    assert (seeds.Length > 0)

    let mutable mappings : SeedMappings =
        { SeedToSoil = []
          SoilToFertilizer = []
          FertilizerToWater = []
          WaterToLight = []
          LightToTemperature = []
          TemperatureToHumidity = []
          HumidityToLocation = [] }

    let parseMappingAndUpdateMappings
        (input: string list)
        (apply: SeedMappings -> Mapping<'source, 'destination> list -> SeedMappings)
        =
        mappings <-
            parseMapping input
            |> apply mappings

    for i = 1 to parts.Length - 1 do
        let partLines = parts[i].Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        assert (partLines.Length >= 2)
        let input = partLines |> Array.toList |> List.tail
        let header = partLines[0]

        match header with
        | "seed-to-soil map:" -> parseMappingAndUpdateMappings input (fun m s -> { m with SeedToSoil =s })
        | "soil-to-fertilizer map:" -> parseMappingAndUpdateMappings input (fun m s -> { m with SoilToFertilizer = s })
        | "fertilizer-to-water map:" -> parseMappingAndUpdateMappings input (fun m s -> { m with FertilizerToWater = s })
        | "water-to-light map:" -> parseMappingAndUpdateMappings input (fun m s -> { m with WaterToLight = s })
        | "light-to-temperature map:" -> parseMappingAndUpdateMappings input (fun m s -> { m with LightToTemperature = s })
        | "temperature-to-humidity map:" -> parseMappingAndUpdateMappings input (fun m s -> { m with TemperatureToHumidity = s })
        | "humidity-to-location map:" -> parseMappingAndUpdateMappings input (fun m s -> { m with HumidityToLocation = s })
        | _ -> failwith header

    struct(seeds, mappings)

let findLowestLocationNumberForSeeds (input: string) =
    let struct(seeds, mappings) = parseMappings input

    seeds
    |> Seq.map (mapper mappings)
    |> Seq.min

let findLowestLocationNumberForSeedRanges (input: string) =
    let struct(seeds, mappings) = parseMappings input

    let unwrapSeedRangesBuffered (seeds: uint64<seed> seq) : array<uint64<seed>> seq = seq {
        let buffer = Array.zeroCreate 2000
        let mutable bufferIndex = 0
        for pair in seeds |> Seq.chunkBySize 2 do
            assert (pair.Length = 2)
            match pair with
            | [| left; right |] ->
                for i = uint64 left to uint64 left + uint64 right - 1UL do
                    buffer[bufferIndex] <- i * 1UL<seed>
                    bufferIndex <- bufferIndex + 1
                    if bufferIndex = buffer.Length then
                        yield buffer
                        bufferIndex <- 0
            | _ -> failwith ""
        yield buffer[.. bufferIndex - 1]
    }

    let mutable min = UInt64.MaxValue * 1UL<location>
    let updateMin (value: uint64<location>) =
        if value < min then
            min <- value

    unwrapSeedRangesBuffered seeds
    |> Seq.iter (fun x-> Parallel.ForEach(x, (mapper mappings >> updateMin)) |> ignore)

    min

// # Version 2

type MappedRange<[<Measure>] 'step> = {
    Start: uint64<'step>
    Length: uint64<range>
}

module MappedRange =
    let convert<[<Measure>] 'source, [<Measure>] 'destination> (range: MappedRange<'source>) : MappedRange<'destination> =
        { Start = (uint64 range.Start) |> LanguagePrimitives.UInt64WithMeasure
          Length = range.Length }

let calculateMappedRange<[<Measure>] 'source, [<Measure>] 'destination>
    (mapping: Mapping<'source, 'destination>)
    (sourceRange: MappedRange<'source>)
    : MappedRange<'source> Set * MappedRange<'destination> Set
    =
    let convert = MappedRange.convert<'source, 'destination>

    let buildMappedRange (start: uint64) (length: uint64) : MappedRange<'source> =
        { Start = start |> LanguagePrimitives.UInt64WithMeasure
          Length = length |> LanguagePrimitives.UInt64WithMeasure }

    let map (source: MappedRange<'source>) : MappedRange<'destination> =
        let start = int64 source.Start
        let difference = int64 mapping.DestinationRangeStart - int64 mapping.SourceRangeStart
        let newStart = start + difference |> uint64
        buildMappedRange newStart (uint64 source.Length)
        |> convert

    let x1 = uint64 sourceRange.Start
    let y1 = uint64 sourceRange.Start + uint64 sourceRange.Length - 1UL

    let x2 = uint64 mapping.SourceRangeStart
    let y2 = uint64 mapping.SourceRangeStart + uint64 mapping.RangeLength  - 1UL

    if y1 < x2 || x1 > y2 then
        Set.singleton sourceRange, Set.empty
    else

    let sourceRanges = ResizeArray()
    let destinationRanges = ResizeArray()
    if x1 >= x2 && y1 <= y2 then
        destinationRanges.Add(map sourceRange)
    else
        if x1 < x2 then
            sourceRanges.Add(buildMappedRange x1 (x2 - x1))
        else
            destinationRanges.Add(buildMappedRange x1 (min y1 y2 - x1 + 1UL) |> map)
        if y1 <= y2 then
            let start = max x1 x2
            destinationRanges.Add(buildMappedRange start (y1 - start + 1UL) |> map)
        else
            sourceRanges.Add(buildMappedRange (y2 + 1UL (*edge*)) (y1 - y2))
        if x1 < x2 && y1 > y2 then
            destinationRanges.Add(buildMappedRange x2 (y2 - x2 + 1UL) |> map)

    sourceRanges |> Set.ofSeq,
    destinationRanges |> Set.ofSeq

let calculateMappedRanges
    (mappings: Mapping<'source, 'destination> list)
    (sourceRange: MappedRange<'source> Set)
    : MappedRange<'destination> Set =

    let rec findFirstMapping
        (mappings: Mapping<'source, 'destination> list)
        (sourceRanges: MappedRange<'source> Set)
        (destinationRanges: MappedRange<'destination> Set)
        : MappedRange<'source> Set * MappedRange<'destination> Set
        =
        match mappings with
        | [] -> sourceRanges, destinationRanges
        | _ when sourceRanges.Count = 0 -> sourceRanges, destinationRanges
        | mapping :: mappings ->
            let sourceRanges, newMappedRanges =
                sourceRanges
                |> Seq.map (calculateMappedRange mapping)
                |> Seq.reduce (fun (l1, r1) (l2, r2) -> Set.union l1 l2, Set.union r1 r2 )

            findFirstMapping
                mappings
                sourceRanges
                (Set.union newMappedRanges destinationRanges)

    let convert = MappedRange.convert<'source, 'destination>

    let unmapped, mapped = findFirstMapping mappings sourceRange Set.empty
    let result =
        mapped
        |> Seq.append (unmapped |> Seq.map convert)
        |> Set.ofSeq

    result

let removeZeroRanges (ranges: MappedRange<_> Set) = ranges |> Set.filter (fun r -> r.Length > 0UL<range>)

let rangeMap (mappings: SeedMappings) (input: MappedRange<seed> Set) : MappedRange<location> Set =
    input
    |> calculateMappedRanges mappings.SeedToSoil
    |> removeZeroRanges
    |> calculateMappedRanges mappings.SoilToFertilizer
    |> removeZeroRanges
    |> calculateMappedRanges mappings.FertilizerToWater
    |> removeZeroRanges
    |> calculateMappedRanges mappings.WaterToLight
    |> removeZeroRanges
    |> calculateMappedRanges mappings.LightToTemperature
    |> removeZeroRanges
    |> calculateMappedRanges mappings.TemperatureToHumidity
    |> removeZeroRanges
    |> calculateMappedRanges mappings.HumidityToLocation
    |> removeZeroRanges

let findLowestLocationNumberForSeedRangesV2 (input: string) =
    let struct(seeds, mappings) = parseMappings input

    let unwrapSeedRanges (seeds: uint64<seed> seq) : MappedRange<seed> seq = seq {
        for pair in seeds |> Seq.chunkBySize 2 do
            assert (pair.Length = 2)
            match pair with
            | [| left; right |] ->
                yield
                    { Start = left
                      Length = uint64 right * 1UL<range> }
            | _ -> failwith ""
    }

    unwrapSeedRanges seeds
    |> Set.ofSeq
    |> rangeMap mappings
    |> Seq.map (_.Start >> uint64)
    |> Seq.min
