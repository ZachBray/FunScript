[<FunScript.JS>] // Placing this attribute on the module allows all functions to be translated to JS. It's just an alias for the ReflectedDefinition F# attribute.
module Page

open System.IO
open System.Reflection
open Microsoft.FSharp.Quotations
open FunScript
open FunScript.TypeScript
open FunScript.FunctionHelpers

// Import the TypeScript definition file for jQuery, jQuery UI, and the JavaScript DOM
type ts = FunScript.TypeScript.Api<
               @"../Typings/jquery.d.ts 
                 ../Typings/jqueryui.d.ts 
                 ../Typings/lib.d.ts" > 

// loop through the provided array of tasks and create a div for each
let addTasksToElement (elementSelector:string) tasks =
    let tasks = tasks 
                |> Array.mapi (fun index task -> "<div class='ui-widget-content draggable'>" + task + "</div>" |> box)
    ts.jQuery.Invoke(elementSelector).append tasks |> ignore 

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
    let dragSettings = ts.Draggable(revert = "invalid", cursor = "move", helper = "clone" )
    ts.jQuery.Invoke(".draggable").draggable(dragSettings) |> ignore

    // Setup the drop zone elements
    let dropSettings = ts.Droppable(hoverClass = "ui-state-active", accept = ".draggable")
    // A few helpers from the FunScript.FunctionHelpers module make it a little easier to setup the Drop callback function. We use one of the overloaded TupledDelegate
    // types to specify the expected signature of the callback function. A standard F# function is then use to allow the element that is being dragged to be appended to the
    // target element. The TupledDelegate type is then turned into an immediate function so that when the event fires the TupledDelegate function executes immediately. 
    dropSettings.drop <- immediateFn 
                         <| TupledDelegate<ts.Event, ts.DroppableEventUIParam, ts.JQuery>(fun e ui -> ui.draggable.appendTo(e.target))
    // The Drop related settings are now tied to the element(s) that have a class named "droppable". 
    ts.jQuery.Invoke(".droppable").droppable(dropSettings) |> ignore

let main() =
    populateTasks()
    initDragAndDrop()

do FunScript.Runtime.Run(components=Interop.Components.all)

