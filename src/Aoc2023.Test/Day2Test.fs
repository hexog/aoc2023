﻿module rec Aoc2023.Test.Day2Test

open Aoc2023
open NUnit.Framework

[<Test>]
let Test1() =
    let result = Day2.sumPlayableWithSetGameIds input
    Assert.Pass(result.ToString())

let input = """Game 1: 4 green, 3 blue, 11 red; 7 red, 5 green, 10 blue; 3 green, 8 blue, 8 red; 4 red, 12 blue; 15 red, 3 green, 10 blue
Game 2: 3 red, 1 blue, 2 green; 1 blue, 9 green; 1 red, 10 green
Game 3: 5 green, 9 red, 4 blue; 3 green, 7 blue; 12 blue, 3 green, 3 red; 3 blue, 7 red, 2 green; 7 blue, 3 green, 10 red
Game 4: 2 green, 2 blue; 12 red, 9 green, 2 blue; 13 green, 15 red, 4 blue; 14 red, 3 green, 5 blue; 6 red, 1 green; 1 blue, 2 red, 2 green
Game 5: 2 green, 6 blue; 1 red, 3 green, 5 blue; 3 green, 4 blue; 3 blue, 5 green, 1 red; 5 blue
Game 6: 5 green, 1 blue, 3 red; 8 green, 15 red; 16 green, 5 red, 1 blue
Game 7: 1 blue, 3 red, 11 green; 18 red, 16 blue, 5 green; 13 blue, 5 green; 1 red, 8 green, 15 blue
Game 8: 1 green, 14 blue, 1 red; 10 blue; 1 green
Game 9: 4 green, 12 blue, 1 red; 14 blue; 2 blue, 4 green; 4 green, 1 red, 10 blue
Game 10: 11 green, 9 red; 12 red, 9 green; 5 red, 7 blue, 5 green; 6 green, 1 blue, 12 red; 3 red, 3 blue; 16 red, 9 blue, 7 green
Game 11: 11 green, 1 red, 9 blue; 2 red, 13 green, 5 blue; 5 green, 2 red, 5 blue; 5 green, 7 blue; 1 red, 5 blue, 1 green
Game 12: 5 green, 1 red; 1 red, 4 green; 1 blue, 12 green; 15 green, 4 blue; 4 blue, 19 green; 16 green, 4 blue
Game 13: 1 red, 9 green, 5 blue; 10 blue, 7 green, 1 red; 3 green, 2 red, 14 blue; 16 blue, 3 red
Game 14: 9 red, 1 blue, 2 green; 16 blue, 7 red; 2 green, 3 red, 14 blue; 1 green, 9 blue
Game 15: 6 blue; 4 blue; 1 red, 16 blue, 3 green
Game 16: 14 green, 5 red, 1 blue; 1 red, 1 blue; 5 blue
Game 17: 1 blue, 1 green, 3 red; 2 red, 2 blue, 2 green; 1 blue, 1 red; 1 red, 2 green, 2 blue; 2 blue; 1 green, 2 red, 1 blue
Game 18: 4 blue, 2 green, 1 red; 1 green, 1 red, 10 blue; 1 green, 1 red, 2 blue; 1 red, 5 blue; 3 green, 6 blue; 1 red, 1 green, 7 blue
Game 19: 1 blue, 13 green, 12 red; 7 blue, 2 green, 1 red; 1 blue, 3 red, 3 green; 3 blue, 8 green, 10 red; 7 blue, 2 green
Game 20: 1 red, 17 blue; 10 blue, 5 green; 9 green, 1 red, 3 blue; 1 red, 5 green, 1 blue
Game 21: 3 red, 6 blue, 5 green; 4 blue, 1 red, 7 green; 6 blue, 4 red, 9 green
Game 22: 11 blue, 2 red, 6 green; 16 blue, 5 red, 6 green; 12 red, 2 green, 10 blue; 14 blue, 2 green, 11 red
Game 23: 3 red, 5 green; 10 blue, 1 green, 9 red; 2 red, 10 green, 9 blue; 9 blue, 7 green
Game 24: 8 blue, 1 red; 3 red, 9 blue; 9 green, 2 red, 8 blue
Game 25: 2 red, 1 green, 1 blue; 1 green, 12 blue, 2 red; 2 red, 1 blue; 2 blue; 1 green, 10 blue; 6 blue
Game 26: 2 red; 4 green, 1 red, 7 blue; 11 blue, 2 red, 4 green; 1 red, 1 blue; 1 red, 5 green, 12 blue
Game 27: 1 red, 7 green, 8 blue; 13 green, 12 blue, 1 red; 6 red, 1 green, 10 blue; 8 red, 2 blue, 2 green; 11 blue, 4 green, 4 red
Game 28: 1 red, 8 blue, 3 green; 12 green, 4 blue; 1 red, 4 blue, 11 green; 7 blue, 10 green, 10 red; 11 blue, 7 red, 8 green; 10 red, 2 green, 2 blue
Game 29: 4 green, 2 red; 1 blue, 11 red; 2 blue, 3 green, 1 red; 16 red; 3 green, 8 red, 1 blue; 2 blue, 7 green, 12 red
Game 30: 1 blue, 3 green; 4 green, 2 blue; 3 red, 5 blue; 4 green, 1 red
Game 31: 2 red, 2 blue, 3 green; 2 green, 3 blue, 8 red; 7 red, 16 blue, 2 green; 5 red, 20 blue, 2 green
Game 32: 2 red, 1 green, 4 blue; 4 green, 4 red, 1 blue; 4 red, 4 blue; 1 blue, 4 red, 2 green; 4 blue, 3 green, 4 red
Game 33: 11 green, 4 blue, 10 red; 2 green, 13 red, 7 blue; 13 red, 2 blue, 8 green; 15 red, 9 blue, 12 green; 14 red, 10 green, 2 blue; 13 red, 7 green
Game 34: 11 red, 6 blue, 4 green; 16 red, 7 blue, 4 green; 6 red, 18 green, 6 blue; 3 blue, 16 red, 3 green; 2 red, 3 blue, 17 green; 3 green, 9 red, 6 blue
Game 35: 6 green, 10 red, 12 blue; 4 red, 1 blue, 2 green; 3 green, 8 blue, 7 red; 6 red, 12 blue, 2 green
Game 36: 4 green, 2 blue, 2 red; 3 green, 10 red, 1 blue; 1 blue, 3 green, 2 red; 2 green, 1 red; 1 blue, 5 red
Game 37: 3 blue, 1 red, 2 green; 8 red, 4 green, 10 blue; 4 red, 4 green
Game 38: 13 green, 3 red, 2 blue; 1 red, 13 green, 2 blue; 20 green, 3 red, 2 blue; 1 red, 2 blue, 12 green
Game 39: 13 blue, 1 red, 8 green; 5 red, 3 green, 8 blue; 6 blue, 4 green; 18 blue, 7 green, 1 red; 4 green, 3 blue, 5 red; 6 blue, 4 red, 1 green
Game 40: 2 red, 2 blue, 9 green; 1 blue, 2 red, 12 green; 16 green, 11 blue, 1 red; 1 green, 2 red; 3 blue, 2 red
Game 41: 7 blue, 1 red; 4 blue, 1 red; 3 blue, 1 red, 2 green; 13 blue
Game 42: 18 red, 1 green, 13 blue; 2 blue, 2 green, 7 red; 16 red, 12 blue; 1 green, 10 blue, 14 red
Game 43: 15 red, 6 green, 2 blue; 3 blue, 9 red, 3 green; 13 red
Game 44: 2 blue, 5 green, 3 red; 4 red, 4 blue, 19 green; 5 red, 3 blue, 9 green; 19 green, 6 red, 5 blue
Game 45: 5 red, 4 green, 13 blue; 12 red, 10 blue; 3 green, 9 blue, 5 red; 10 blue, 18 red, 5 green; 16 red, 6 green, 17 blue
Game 46: 3 green; 3 green, 2 blue; 4 blue, 2 red, 3 green; 5 blue, 3 green, 4 red; 1 green, 1 blue
Game 47: 2 blue, 1 red, 10 green; 2 red; 6 red, 1 blue; 16 red, 2 blue, 8 green; 5 blue, 8 red, 7 green
Game 48: 11 green, 4 red, 2 blue; 2 blue, 5 green, 8 red; 9 green, 6 red; 3 red, 3 green, 1 blue; 2 blue, 12 green, 17 red
Game 49: 10 blue, 4 green, 1 red; 10 red, 10 blue; 12 blue, 7 red; 13 blue, 6 green
Game 50: 1 red, 19 green, 7 blue; 4 red, 1 green, 5 blue; 16 green, 8 red, 8 blue
Game 51: 12 green, 18 blue; 13 green, 14 blue, 4 red; 7 green, 4 red, 14 blue; 8 green, 2 blue, 3 red; 16 blue, 8 green
Game 52: 9 blue, 9 green, 3 red; 8 blue, 1 green, 13 red; 2 red, 8 blue, 9 green; 13 red, 4 green; 6 green, 15 red; 11 blue, 11 red, 9 green
Game 53: 2 red, 4 green, 3 blue; 5 blue, 16 green; 4 blue, 8 red, 12 green
Game 54: 6 red, 16 green; 6 red, 15 green; 8 green, 8 red, 2 blue
Game 55: 9 red, 2 green; 4 blue; 2 green, 2 red, 7 blue; 1 red, 16 blue, 1 green; 17 blue, 5 red
Game 56: 14 green, 3 red, 9 blue; 14 blue, 15 green, 2 red; 8 red, 13 blue, 15 green; 15 blue, 2 red, 12 green; 3 red, 7 blue, 10 green; 10 blue, 13 green
Game 57: 1 blue, 10 green, 2 red; 4 blue, 9 green, 11 red; 2 blue
Game 58: 4 red, 2 blue, 5 green; 1 blue, 5 green, 4 red; 3 green, 4 red, 8 blue; 4 blue, 7 green; 5 green, 4 blue; 1 blue, 6 red
Game 59: 5 blue, 4 red, 3 green; 8 blue, 12 green, 5 red; 5 red, 8 blue, 15 green
Game 60: 6 red, 12 blue, 1 green; 10 blue, 20 green, 4 red; 6 blue, 1 green, 5 red; 9 red, 12 blue, 14 green; 15 green, 1 red, 14 blue; 10 green, 13 blue
Game 61: 1 blue, 12 green, 3 red; 4 green, 1 red, 4 blue; 8 red, 4 green, 6 blue
Game 62: 6 blue, 7 green, 3 red; 6 blue, 3 red, 3 green; 11 green, 6 red, 2 blue; 2 red, 6 blue, 3 green; 2 green, 3 blue, 3 red; 3 blue, 11 green, 11 red
Game 63: 5 green, 6 blue, 4 red; 6 green, 12 blue; 3 green, 9 blue, 10 red; 1 blue, 4 red, 5 green
Game 64: 10 green, 14 red; 1 blue, 9 red; 3 green, 10 blue, 14 red; 5 green, 3 blue, 12 red; 5 blue, 12 red, 13 green
Game 65: 1 red, 5 green, 10 blue; 14 red, 5 green, 10 blue; 10 blue, 10 red
Game 66: 9 green, 8 blue, 1 red; 8 red, 14 blue; 8 red, 7 blue, 2 green; 4 blue, 3 green, 5 red; 2 red, 8 green, 8 blue
Game 67: 4 red, 3 green, 3 blue; 4 green, 1 blue, 4 red; 1 blue, 3 red; 10 blue; 16 blue, 6 red, 4 green
Game 68: 6 blue, 6 green, 9 red; 4 blue, 9 red, 3 green; 3 blue, 8 red
Game 69: 4 green, 12 red, 3 blue; 2 red, 3 blue; 2 blue, 4 red, 2 green; 1 blue, 3 red
Game 70: 4 red, 3 green, 15 blue; 1 green, 4 red; 1 red, 1 green, 5 blue
Game 71: 4 blue, 2 red, 10 green; 7 red, 6 blue, 11 green; 4 blue, 7 red, 8 green
Game 72: 9 red, 9 blue, 1 green; 4 red, 6 green, 5 blue; 3 green, 7 red, 2 blue
Game 73: 3 green, 9 red; 4 green, 15 red; 12 red, 2 blue; 14 red, 3 green
Game 74: 2 red, 6 blue, 1 green; 3 red, 6 blue; 1 green, 12 blue, 14 red
Game 75: 3 green, 18 red; 1 green, 7 red, 1 blue; 2 red, 2 green, 3 blue; 11 red; 2 red, 3 green, 2 blue
Game 76: 6 green, 2 red, 5 blue; 13 green, 5 blue; 5 blue, 1 red, 1 green
Game 77: 4 blue, 6 green, 3 red; 15 red, 1 green; 4 green, 11 red, 13 blue; 8 blue, 6 green, 9 red; 3 blue, 1 green, 11 red; 3 green, 3 red
Game 78: 11 green, 1 blue, 2 red; 7 red, 16 blue, 11 green; 9 blue, 10 red, 6 green; 1 green, 8 blue, 10 red; 8 blue, 6 red, 1 green
Game 79: 2 blue, 5 green, 4 red; 1 blue, 1 red, 1 green; 1 blue, 5 red, 10 green; 6 red, 3 green, 3 blue; 8 red, 9 green, 6 blue; 7 blue, 6 green, 13 red
Game 80: 10 green, 7 blue, 5 red; 5 red, 1 green, 6 blue; 8 blue, 2 red, 8 green
Game 81: 3 green, 10 red; 6 blue, 8 green, 14 red; 4 green, 4 blue, 13 red; 5 blue, 11 green, 6 red; 16 red, 8 green, 5 blue; 6 green, 18 red, 6 blue
Game 82: 13 red, 1 green, 7 blue; 8 green, 4 blue, 12 red; 18 red, 5 green, 3 blue; 13 red, 4 green, 9 blue
Game 83: 1 red, 3 green, 4 blue; 5 blue, 4 green, 1 red; 3 green, 1 red, 12 blue; 4 green, 11 blue
Game 84: 3 blue, 10 green, 2 red; 3 red, 8 blue; 11 blue, 12 red, 14 green; 2 red, 11 green, 2 blue
Game 85: 8 blue, 2 green, 1 red; 13 blue, 6 red; 3 blue, 5 green
Game 86: 16 red, 8 blue; 7 blue; 16 red, 16 blue, 1 green; 15 blue, 11 red; 2 green, 7 red, 5 blue
Game 87: 6 green, 9 blue, 4 red; 1 red, 1 green, 4 blue; 5 blue, 13 green, 3 red; 2 green, 4 red; 16 blue, 10 green, 3 red
Game 88: 1 blue, 14 red; 14 red, 3 blue, 8 green; 1 blue, 5 green
Game 89: 12 green, 14 blue, 3 red; 2 red, 3 blue, 3 green; 2 blue, 8 green; 1 red, 3 green, 15 blue; 3 red, 5 blue
Game 90: 3 blue, 17 red, 11 green; 2 red, 2 blue, 7 green; 7 blue; 8 blue, 4 green, 10 red; 1 blue, 4 red
Game 91: 10 red, 9 blue, 8 green; 5 blue, 10 red, 2 green; 11 red, 17 green, 7 blue; 12 blue, 16 red, 18 green; 20 green, 5 blue, 15 red
Game 92: 1 green, 14 red, 1 blue; 2 blue, 6 green; 9 red, 6 green; 5 blue, 5 red, 2 green; 3 blue, 3 green, 10 red; 5 blue, 1 red
Game 93: 10 green, 1 red, 6 blue; 16 red, 5 blue, 2 green; 3 red, 7 green, 11 blue; 12 green, 5 blue, 4 red; 8 green, 7 blue, 10 red; 1 red, 5 blue
Game 94: 3 blue, 1 red, 3 green; 1 blue, 4 green, 4 red; 9 green
Game 95: 3 green, 5 blue, 9 red; 2 green, 9 red, 2 blue; 12 red, 9 green; 11 green, 9 red, 9 blue; 9 blue, 6 green, 10 red; 13 red, 2 blue, 5 green
Game 96: 2 red, 19 blue, 2 green; 10 blue, 1 red, 2 green; 9 blue, 1 red; 2 green, 3 blue; 1 green, 1 red, 11 blue
Game 97: 6 green, 7 blue, 5 red; 7 green, 1 red, 11 blue; 6 green, 6 red, 5 blue; 2 red, 9 blue, 1 green
Game 98: 5 green, 8 red, 15 blue; 16 green, 9 blue, 8 red; 5 blue, 3 red, 2 green; 13 blue, 12 green, 4 red; 2 red, 15 green, 3 blue; 1 green, 11 blue, 2 red
Game 99: 1 green, 7 blue, 6 red; 16 blue, 9 red; 1 green, 17 red, 12 blue; 15 red, 7 blue; 8 blue, 14 red
Game 100: 5 blue, 11 red, 6 green; 11 red, 2 blue, 5 green; 6 blue, 6 green; 2 blue, 6 red, 15 green; 7 red, 4 blue, 7 green"""