module Aoc2023.Day5

open System

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
    DestinationRangeStart: uint<'destination>
    SourceRangeStart: uint<'source>
    RangeLength: uint<range>
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
    (sourceValue: uint<'source>)
    : uint<'destination> voption
    =
    let calculateRangeMaxValue (minValue: uint<'source>) (range: uint<range>) : uint<'source> =
        (uint minValue) + (uint range - 1u) |> LanguagePrimitives.UInt32WithMeasure

    let calculateDistance (minValue: uint<'source>) (value: uint<'source>) : uint<range> =
        (uint value) - (uint minValue) |> LanguagePrimitives.UInt32WithMeasure

    let calculatePositionOnDestinationRange (minValue: uint<'destination>) (distance: uint<range>) : uint<'destination> =
        (uint minValue) + (uint distance) |> LanguagePrimitives.UInt32WithMeasure

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
    (sourceValue: uint<'source>)
    : uint<'destination> =
    let convert (value: uint<'source>) : uint<'destination> =
        (uint value) |> LanguagePrimitives.UInt32WithMeasure

    let rec tryUseCorrectMapping (mappings: Mapping<'source, 'destination> list) (sourceValue: uint<'source>) =
        match mappings with
        | [] -> ValueNone
        | mapping :: mappings ->
            match calculateMappedValue mapping sourceValue with
            | ValueSome _ as success -> success
            | ValueNone -> tryUseCorrectMapping mappings sourceValue

    match tryUseCorrectMapping mappings sourceValue with
    | ValueSome result -> result
    | ValueNone -> convert sourceValue

let mapper (mappings: SeedMappings) (input: uint<seed>) : uint<location> =
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
        let destinationRangeStart = UInt32.Parse(parts[0])
        let sourceRangeStart = UInt32.Parse(parts[1])
        let rangeLength = UInt32.Parse(parts[2])
        { DestinationRangeStart = destinationRangeStart |> LanguagePrimitives.UInt32WithMeasure
          SourceRangeStart = sourceRangeStart |> LanguagePrimitives.UInt32WithMeasure
          RangeLength = rangeLength * 1u<range> }

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
        |> Array.map (fun x -> UInt32.Parse(x) * 1u<seed>)
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
