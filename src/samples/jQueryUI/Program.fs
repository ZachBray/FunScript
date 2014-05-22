[<ReflectedDefinition>] // Placing this attribute on the module allows all functions to be translated to JS. It's just an alias for the ReflectedDefinition F# attribute.
module Program

open FunScript
open FunScript.TypeScript

// loop through the provided array of tasks and create a div for each
let addTasksToElement (elementSelector:string) tasks =
    let tasks = tasks 
                |> Array.mapi (fun index task -> "<div class='ui-widget-content draggable'>" + task + "</div>" |> box)
    Globals.Dollar.Invoke(elementSelector).append(tasks) |> ignore

// Create the initial array of To Do tasks and tasks that are already complete.
let populateTasks () =
    let tasksToDo = 
        [| "Persist the tasks to a data store."
           "Add new tasks."
           "Remove a task." |]
    let tasksDone = 
        [| "Allow tasks to be moved to done."
           "Add dynamic population of tasks." |]
    addTasksToElement ".tasksNotStarted" tasksToDo
    addTasksToElement ".tasksDone" tasksDone

let initDragAndDrop () = 
    // Setup the draggable elements with the desired draggable settings
    let draggable = createEmpty<JQueryUI.Draggable>()
    draggable.revert <- "invalid"
    draggable.cursor <- "move"
    draggable.helper <- "clone"
    Globals.Dollar.Invoke(".draggable").draggable(draggable) |> ignore

    // Setup the drop zone elements
    let dropSettings = createEmpty<JQueryUI.Droppable>()
    dropSettings.hoverClass <- "ui-state-active"
    dropSettings.accept <- ".draggable"
    dropSettings.drop <- fun e ui -> ui.draggable.appendTo(e.target :?> Element) |> ignore
    // The Drop related settings are now tied to the element(s) that have a class named "droppable". 
    Globals.Dollar.Invoke(".droppable").droppable(dropSettings) |> ignore

let main() =
    populateTasks()
    initDragAndDrop()

do FunScript.Runtime.Run()

