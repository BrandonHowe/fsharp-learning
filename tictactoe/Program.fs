// Learn more about F# at http://fsharp.org

open System

module TicTacToeDomain =

    type HorizPosition =
        | Left
        | HCenter
        | Right

    type VertiPosition =
        | Top
        | VCenter
        | Bottom
    // Defining the two components of a cell position

    type CellPosition = HorizPosition * VertiPosition
    // The actual Cell Position type

    type Player =
        | PlayerO
        | PlayerX

    type CellState =
        | Played of Player
        | Empty
    // There are 3 possibilities per cell state

    type Cell =
        { pos: CellPosition
          state: CellState }
    // Each cell has a position and a state

    type PlayerXPos = PlayerXPos of CellPosition

    type PlayerOPos = PlayerOPos of CellPosition
    // Make sure 2 players can't play the same position

    type ValidMovesForPlayerX = PlayerXPos list

    type ValidMovesForPlayerO = PlayerOPos list
    // Make sure a player can only play in valid positions

    type MoveResult =
        | PlayerXToMove of ValidMovesForPlayerX
        | PlayerOToMove of ValidMovesForPlayerO
        | GameWon of Player
        | GameTied
    // We can have one MoveResult for the game status and valid moves for a player

    // A list of Generics
    type NewGame<'GameState> = 'GameState * MoveResult
    // If it's a new game

    type PlayerXMoves<'GameState> = 'GameState -> PlayerXPos -> ('GameState * MoveResult)
    // If player X moves

    type PlayerOMoves<'GameState> = 'GameState -> PlayerOPos -> ('GameState * MoveResult)
    // If player O moves

    type GetCells<'GameState> = 'GameState -> Cell list
    // A helper to get a list of cells

    type TicTacToeAPI<'GameState>  = 
        {
        newGame : NewGame<'GameState>
        playerXMoves : PlayerXMoves<'GameState> 
        playerOMoves : PlayerOMoves<'GameState> 
        getCells : GetCells<'GameState>
        }

module TicTacToeImplementation =
    open TicTacToeDomain

    type GameState =
        { cells: Cell list }
    // Record the game state as a list of cells

    let allHorizPositions = [ Left; HCenter; Right ]
    let allVertiPositions = [ Top; VCenter; Bottom ]
    // List of horizontal and vertical positions

    type Line = Line of CellPosition list
    // A type to store the list o;f cell positions in a line

    let linesToCheck =
        let makeHLine v =
            Line
                [ for h in allHorizPositions do
                    yield (h, v) ]

        let hlines =
            [ for v in allVertiPositions do
                yield makeHLine v ]
        // Horizontal lines

        let makeVLine h =
            Line
                [ for v in allVertiPositions do
                    yield (h, v) ]

        let vlines =
            [ for h in allHorizPositions do
                yield makeVLine h ]
        // Vertical lines

        let diagonalLine1 = Line [Left,Top; HCenter,VCenter; Right,Bottom]
        let diagonalLine2 = Line [Left,Bottom; HCenter,VCenter; Right,Top]
        // The two diagonals

        [ yield! hlines
          yield! vlines
          yield diagonalLine1
          yield diagonalLine2 ]

    let getCells gameState =
        gameState.cells

    let getCell gameState posToFind = 
        gameState.cells 
        |> List.find (fun cell -> cell.pos = posToFind)

    let private updateCell newCell gameState =
        let substituteNewCell oldCell =
            if oldCell.pos = newCell.pos then newCell
            else oldCell
        // Simple helper function

        let newCells = gameState.cells |> List.map substituteNewCell
        // Get a copy of the cells with the new cells swapped in

        { gameState with cells = newCells }
    // Return a new game state with the new cells

    let private isGameWonBy player gameState =
        let cellWasPlayedBy playerToCompare cell =
            match cell.state with
            | Played player -> player = playerToCompare
            | Empty -> false
        // See if a cell was played by a player

        let lineIsAllSamePlayer player (Line cellPosList) =
            cellPosList
            |> List.map (getCell gameState)
            |> List.forall (cellWasPlayedBy player)
        // Helper to see if every cell in a line was played by a plaayer

        linesToCheck |> List.exists (lineIsAllSamePlayer player)

    let private isGameTied gameState = 
        let cellWasPlayed cell = 
            match cell.state with
            | Played _ -> true
            | Empty -> false
        // Helper to see if a cell has been played

        gameState.cells
        |> List.forall cellWasPlayed 

    let private remainingMovesForPlayer playerMove gameState =
        let playableCell cell =
            match cell.state with
            | Played player -> None
            | Empty -> Some(playerMove cell.pos)
        // Helper to see if a cell is playable by a player

        gameState.cells |> List.choose playableCell

    let newGame =
        let allPositions =
            [ for h in allHorizPositions do
                for v in allVertiPositions do
                    yield (h, v) ]
        // Cross product of the positions

        let emptyCells =
            allPositions
            |> List.map (fun pos ->
                {
                pos = pos
                state = Empty
               })
        // All cells are empty at the beginning

        let gameState = { cells = emptyCells }
        // Initial game state

        let validMoves =
            allPositions |> List.map PlayerXPos
        // Initial set of valid moves for player X is all positions

        gameState, PlayerXToMove validMoves
    // Return the game state and the valid moves

    let playerXMoves gameState (PlayerXPos cellPos) =
        let newCell =
            { pos = cellPos
              state = Played PlayerX }

        let newGameState = gameState |> updateCell newCell

        if newGameState |> isGameWonBy PlayerX then
            newGameState, GameWon PlayerX
        elif newGameState |> isGameTied then
            newGameState, GameTied
        else
            let remainingMoves =
                newGameState |> remainingMovesForPlayer PlayerOPos
            newGameState, PlayerOToMove remainingMoves

    let playerOMoves gameState (PlayerOPos cellPos) =
        let newCell =
            { pos = cellPos
              state = Played PlayerO }

        let newGameState = gameState |> updateCell newCell

        if newGameState |> isGameWonBy PlayerO then
            newGameState, GameWon PlayerO
        elif newGameState |> isGameTied then
            newGameState, GameTied
        else
            let remainingMoves =
                newGameState |> remainingMovesForPlayer PlayerXPos
            newGameState, PlayerXToMove remainingMoves

    let api =
        { newGame = newGame
          playerOMoves = playerOMoves
          playerXMoves = playerXMoves
          getCells = getCells }

module ConsoleUI =
    open TicTacToeDomain

    type UserAction<'a> =
        | ContinuePlay of 'a
        | ExitGame
    // Track UI state

    let displayAvailableMoves moves =
        moves
        |> List.iteri (fun i move ->
            printfn "%i) %A" i move )
    // Print every available move

    let getMove moveIndex moves =
        if moveIndex < List.length moves then
            let move = List.nth moves moveIndex
            Some move
        else 
            None
    // Get move by index
    
    let processMoveIndex inputStr gameState availableMoves makeMove processInputAgain =
        let tryParseInt s = 
            try 
                s |> int |> Some
            with :? FormatException -> 
                None

        match tryParseInt inputStr with     
        | Some inputIndex ->
            match getMove inputIndex availableMoves with
            | Some move ->
                // A move was found, so make it
                let moveResult = makeMove gameState move
                ContinuePlay moveResult
            | None ->
                printfn "...No move found for input index %i. Try again" inputIndex
                processInputAgain()
        | None ->
            printfn "...Please input an int corresponding to a displayed move."
            processInputAgain()

    let rec processInput gameState availableMoves makeMove =
        let processInputAgain() =
            processInput gameState availableMoves makeMove
        // Helper function that calls this function again with the same parameters
        
        printfn "Enter an int corresponding to a displayed move, or enter q to quit:"
        let inputStr = Console.ReadLine()
        if inputStr = "q" then
            ExitGame
        else
            processMoveIndex inputStr gameState availableMoves makeMove processInputAgain

    let displayCells cells =
        let cellToStr cell =
            match cell.state with
            | Empty -> "-"
            | Played player ->
                match player with
                | PlayerX -> "X"
                | PlayerO -> "O"
            
        let printCells cells =
            cells
            |> List.map cellToStr
            |> List.reduce (fun s1 s2 -> s1 + "|" + s2)
            |> printfn ("|%s|")
        
        let topCells =
            cells |> List.filter (fun cell -> snd cell.pos = Top)
        let middleCells =
            cells |> List.filter (fun cell -> snd cell.pos = VCenter)
        let bottomCells =
            cells |> List.filter (fun cell -> snd cell.pos = Bottom)

        printCells topCells
        printCells middleCells
        printCells bottomCells
        printfn ""

    let rec askToPlayAgain api =
        printfn "Would you like to play again? (y/n)"
        match Console.ReadLine() with
        | "y" ->
            ContinuePlay api.newGame
        | "n" ->
            ExitGame
        | _ ->
            askToPlayAgain api
    // Ask to play again after each game

    let rec gameLoop api userAction =
        printfn "\n-------------------------------------------\n"

        match userAction with
        | ExitGame ->
            printfn "Exiting game."
        | ContinuePlay (state, moveResult) ->
            state |> api.getCells |> displayCells
            // Update display
            match moveResult with
            | GameTied ->
                printfn "Game over - Tie"
                printfn ""
                let nextUserAction = askToPlayAgain api
                gameLoop api nextUserAction
            | GameWon player ->
                printfn "Game won by %A" player
                printfn ""
                let nextUserAction = askToPlayAgain api
                gameLoop api nextUserAction
            | PlayerOToMove availableMoves ->
                printfn "Player O to move"
                displayAvailableMoves availableMoves
                let newResult = processInput state availableMoves api.playerOMoves
                gameLoop api newResult
            | PlayerXToMove availableMoves ->
                printfn "Player X to move"
                displayAvailableMoves availableMoves
                let newResult = processInput state availableMoves api.playerXMoves
                gameLoop api newResult

    let startGame api =
        let userAction = ContinuePlay api.newGame
        gameLoop api userAction

module ConsoleApplication =
    let startGame() =
        let api = TicTacToeImplementation.api
        ConsoleUI.startGame api

open ConsoleApplication
[<EntryPoint>]
let main argv =
    startGame()
    0 // return an integer exit code
