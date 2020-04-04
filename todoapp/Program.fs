// Learn more about F# at http://fsharp.org

open System

module TodoDomain =
    type TodoStatus =
        | Complete
        | Deleted
        | Incomplete

    type Todo =
        { message: string
          status: TodoStatus }

module TodoImplementation =
    open TodoDomain

    let updateTodo (todos: Todo list, newTodo: Todo) =
        let substituteNewTodo oldTodo =
            if newTodo.message = oldTodo.message then newTodo
            else oldTodo

        todos |> List.map substituteNewTodo

module ConsoleUI =
    open TodoDomain
    open TodoImplementation

    type UserAction<'a> =
        | Continue of 'a
        | Exit

    let tryParseInt s =
        try
            s
            |> int
            |> Some
        with :? FormatException ->
            None

    let displayTodos todos =
        printfn "\nTODO LIST ------------------"
        todos
        |> List.filter (fun todo -> todo.status <> Deleted)
        |> List.iteri (fun i todo ->
            printfn "%i: [%s] %s" i
                (if todo.status = Complete then "X"
                 else " ") todo.message)
        printfn ""

    let askForAdd() =
        printfn "What do you want to add?"
        Console.ReadLine()

    let rec askForComplete todos =
        let completeTodos todoIdx =
            if todoIdx < List.length todos then
                let todo = List.nth todos todoIdx

                let updatedTodo =
                    { message = todo.message
                      status = Complete }
                updateTodo (todos, updatedTodo)
            else
                printfn "That's not valid!"
                askForComplete todos

        displayTodos todos
        printfn "Which do you want to complete?"
        let rawInput = Console.ReadLine()
        match rawInput |> tryParseInt with
        | Some input ->
            if input < List.length todos then
                completeTodos input
            else
                printfn "That number's too big!"
                askForComplete todos
        | None ->
            printfn "That's not an integer!"
            askForComplete todos

    let rec askForDelete todos =
        let deleteTodo todoIdx =
            if todoIdx < List.length todos then
                let todo = List.nth todos todoIdx

                let updatedTodo =
                    { message = todo.message
                      status = Deleted }
                updateTodo (todos, updatedTodo)
            else
                printfn "That's not valid!"
                askForDelete todos

        displayTodos todos
        printfn "Which do you want to delete?"
        let rawInput = Console.ReadLine()
        match rawInput |> tryParseInt with
        | Some input ->
            if input < List.length todos then
                deleteTodo input
            else
                printfn "That number's too big!"
                askForDelete todos
        | None ->
            printfn "That's not an integer!"
            askForDelete todos

    let rec askForAction todos =
        printfn "What do you want to do now? (add/complete/delete/exit)"
        match Console.ReadLine() with
        | "add" ->
            let newTodoMsg = askForAdd()

            let newTodo: Todo =
                { message = newTodoMsg
                  status = Incomplete }
            Some(newTodo :: todos)
        | "complete" ->
            Some(askForComplete todos)
        | "delete" ->
            Some(askForDelete todos)
        | "exit" ->
            None
        | _ ->
            askForAction todos

    let rec todoLoop todos action =
        match action with
        | Exit ->
            printfn "Exiting"
        | Continue(state) ->
            let askRes = askForAction todos
            match askRes with
            | Some newTodos ->
                let userAction = Continue todos
                newTodos |> displayTodos
                todoLoop newTodos userAction
            | None ->
                todoLoop todos Exit

    let startTodo todos =
        let userAction = Continue todos
        todoLoop todos userAction

module ConsoleApplication =
    let startTodo() =
        let todos = []
        ConsoleUI.startTodo todos

open ConsoleApplication

[<EntryPoint>]



let main argv =
    printfn "Hello World from F#!"
    startTodo()
    0 // return an integer exit code
