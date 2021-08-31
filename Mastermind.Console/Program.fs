open Mastermind.Core
open Spectre.Console
open System.Text.RegularExpressions

let readGuess () =
    let getCodePeg n =
        match n with
        | '0' -> CodePeg.Red
        | '1' -> Green
        | '2' -> Blue
        | '3' -> CodePeg.Yellow
        | '4' -> Purple
        | '5' -> Orange
        | '6' -> White
        | '7' -> Pink
        | _ -> CodePeg.Red

    let textPrompt =
        TextPrompt("Make a guess:")
            .Validate<string>(fun str ->
                if Regex.IsMatch(str, "^[0-7][0-7][0-7][0-7]$") then
                    ValidationResult.Success()
                else
                    ValidationResult.Error("[red]Enter four numbers between 0 and 7 [/]"))

    let line = AnsiConsole.Prompt<string>(textPrompt)

    line.ToCharArray()
    |> Array.map getCodePeg
    |> fun xs -> (xs.[0], xs.[1], xs.[2], xs.[3])

let printGame game =
    let codePegMarkup peg =
        match peg with
        | CodePeg.Red -> "0"//"[white on red]0[/]"
        | Green -> "1"//"[white on green]1[/]"
        | Blue -> "2"//"[white on blue]2[/]"
        | CodePeg.Yellow -> "3"//"[white on yellow]3[/]"
        | Purple -> "4"//"[white on purple]4[/]"
        | Orange -> "5"//"[white on darkorange]5[/]"
        | White -> "6"//"[black on white]6[/]"
        | Pink -> "7"//"[black on hotpink]7[/]"

    let keyPegMarkup peg =
        match peg with
        | KeyPeg.Red -> "[black on red] [/]"
        | KeyPeg.Yellow -> "[black on yellow] [/]"
        | None -> " "

    let addRow (table : Table) round =
        let guessTable = Table()
        round.Guess
        |> fun (a,b,c,d) -> [|a;b;c;d|]
        |> Array.map (codePegMarkup >> TableColumn)
        |> guessTable.AddColumns
        |> ignore

        let hintTable =  Table()
        round.Hint
        |> fun (a,b,c,d) -> [|a;b;c;d|]
        |> Array.map (keyPegMarkup >> TableColumn)
        |> hintTable.AddColumns
        |> ignore

        table.AddRow(guessTable, hintTable) |> ignore
        ()

    AnsiConsole.Clear()

    let table = Table()
    table.AddColumn("Guesses") |> ignore
    table.AddColumn("Hints") |> ignore

    game.Rounds
    |> List.rev
    |> List.iter (addRow table)

    AnsiConsole.Render(table)

[<EntryPoint>]
let main argv =
    let rec loop game =
        let (game, success) =
            readGuess ()
            |> makeGuess game

        game |> printGame

        match success with
        | true -> 0
        | _ -> game |> loop

    randomSolution ()
    |> createGame
    |> loop