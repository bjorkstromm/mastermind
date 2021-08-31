namespace Mastermind

module Core =
    type CodePeg =
        | Red
        | Green
        | Blue
        | Yellow
        | Purple
        | Orange
        | White
        | Pink

    type KeyPeg =
        | Red
        | Yellow
        | None

    type Guess =
        CodePeg * CodePeg * CodePeg * CodePeg

    type Solution =
        CodePeg * CodePeg * CodePeg * CodePeg

    type Hint =
        KeyPeg * KeyPeg * KeyPeg * KeyPeg

    type Round = {
        Guess : Guess
        Hint : Hint
    }

    type Game = {
        Rounds : Round list
        Code : Solution
    }

    let randomSolution () : Solution =
        let rnd = System.Random()

        let rec getNextDistinct pegs =
            let n = rnd.Next (0, 7)
            if List.contains n pegs then
                getNextDistinct pegs
            else
                n::pegs

        let rec loop pegs =
            if pegs |> List.length = 4 then
                pegs
            else
                pegs |> getNextDistinct |> loop

        let getCodePeg n =
            match n with
            | 0 -> CodePeg.Red
            | 1 -> Green
            | 2 -> Blue
            | 3 -> CodePeg.Yellow
            | 4 -> Purple
            | 5 -> Orange
            | 6 -> White
            | 7 -> Pink
            | _ -> CodePeg.Red

        loop []
        |> List.map getCodePeg
        |> fun xs -> (xs.[0], xs.[1], xs.[2], xs.[3])

    let createGame solution : Game = {
        Rounds = []
        Code = solution
    }

    let makeGuess (game : Game) (guess : Guess) : (Game * bool) =
        let guesses = guess |> fun (a,b,c,d) -> [a;b;c;d]
        let solution = game.Code |> fun (a,b,c,d) -> [a;b;c;d]

        let check i peg =
            if guesses.[i] = peg then
                KeyPeg.Red
            else if guesses |> List.contains peg then
                KeyPeg.Yellow
            else
                KeyPeg.None

        let hint =
            solution
            |> List.mapi check
            |> List.sort
            |> fun h -> Hint(h.[0], h.[1], h.[2], h.[3])

        let round = {
            Hint = hint
            Guess = guess
        }

        (
            { game with Rounds = round::game.Rounds },
            hint = (Red, Red, Red, Red)
        )