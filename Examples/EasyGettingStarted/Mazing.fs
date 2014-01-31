(*
    Mazing: a simple maze mobile application based on jquerymobile and Cordova/phonegap (TBD).
    The Project.fs creates the index file with the HTML contents and opens it using the default brower.
    Wrapping externally with Cordova (once I get to the Cordova API).
*)

[<ReflectedDefinition>]
module Mazing

open FunScript
open FSharp.Data
open System.IO

// the record for the player's sight direction
type Direction =
    | UP = 0
    | RIGHT = 1
    | DOWN = 2
    | LEFT = 3

// the maze's building blocks
type BuildingBlock =
    | WALL = 0
    | SPACE = 1
    | ENTRANCE = 2
    | EXIT = 3

type SightLane =
    | LEFT = 0
    | MIDDLE = 1
    | RIGHT = 2

// how far the player can see forward (maximum as the maze may end)
let LINE_OF_SIGHT = 5        
// how far the player can see to the sides. here we support simple mazes only.                              
let WIDTH_OF_SIGHT = 3

// User actions
let TurnLeftStr = "Turn-Left"
let TurnRightStr = "Turn-Right"
let GoForwardStr = "Go-Forward"

// static maze structure
let getMazeStruct = [
    [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 2; 1; 1; 1; 1; 1; 1; 1; 0];
    [0; 0; 1; 0; 0; 1; 0; 0; 1; 0];
    [0; 0; 1; 0; 1; 1; 0; 1; 1; 0];
    [0; 1; 1; 1; 1; 0; 0; 1; 0; 0];
    [0; 0; 0; 1; 0; 0; 0; 1; 0; 0];
    [0; 0; 0; 1; 0; 0; 1; 1; 0; 0];
    [0; 0; 0; 1; 0; 0; 1; 0; 0; 0];
    [0; 0; 0; 1; 1; 1; 1; 3; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]]

// the entrance and exit locations. the location position is (0..n-1, 0..n-1)
// Todo: currently stab, find from maze ...
let getEntrancePos = (1, 1)
let getEntranceDir = Direction.RIGHT
let getExitPos = (8, 9)

// Game's state ... MUTABLE and DANGEROUS
let mutable playerPos = getEntrancePos
let mutable playerDirection = getEntranceDir

// since this version of Funcscript does not support compilation of "enum<TYPE> value" to JS, we define it for 2 types
let intToBuildingBlock = function
    | 0 -> BuildingBlock.WALL
    | 1 -> BuildingBlock.SPACE
    | 2 -> BuildingBlock.ENTRANCE
    | 3 -> BuildingBlock.EXIT
    | _ -> BuildingBlock.WALL

let intToDirection = function
    | 0 -> Direction.UP
    | 1 -> Direction.RIGHT
    | 2 -> Direction.DOWN
    | 3 -> Direction.LEFT
    | _ -> Direction.UP

// rotate clock or counter clock wise from a given direction
let rotateDirection (dir: Direction) (clock: bool) =
    let x = int dir
    if clock then
        intToDirection ((x + 1) % 4)
    else
        intToDirection ((x + 3) % 4)

// per direction how to traverse the maze (x,y delta or steps)
let xyDelta (dir: Direction) =
    match dir with
    | Direction.LEFT -> (0, -1)
    | Direction.RIGHT -> (0, 1)
    | Direction.DOWN -> (1, 0)
    | Direction.UP -> (-1, 0)
    | _ -> (0, 0)

// Size of canavas
let getCanvasSize = 300.

// calculates the players view according to the position and direction of sight, before drawing it on the canvas
let getPlayerView pos dir =
(* test purposes:
    [[BuildingBlock.WALL; BuildingBlock.WALL; BuildingBlock.WALL; BuildingBlock.WALL; BuildingBlock.WALL]; 
     [BuildingBlock.ENTRANCE; BuildingBlock.SPACE; BuildingBlock.SPACE; BuildingBlock.SPACE; BuildingBlock.SPACE]; 
     [BuildingBlock.WALL; BuildingBlock.SPACE; BuildingBlock.WALL; BuildingBlock.WALL; BuildingBlock.SPACE]]
*)
    let x = fst pos
    let y = snd pos
    let delta = xyDelta dir
    let ldelta = xyDelta (rotateDirection dir false)
    let rdelta = xyDelta (rotateDirection dir true)
    //let ldelta = (-1, 0)
    //let rdelta = (1, 0)
    let maze = getMazeStruct
    let (v: BuildingBlock list list) = 
                // left lane
               [[for i=1 to LINE_OF_SIGHT do
                    let lx = x + (fst ldelta) + ((fst delta) * (i - 1))
                    let ly = y + (snd ldelta) + ((snd delta) * (i - 1))
                    if lx < maze.Length && lx >= 0 && ly >= 0 && ly < maze.[0].Length then
                        yield (intToBuildingBlock (maze.[lx].[ly]))
                    else 
                        yield BuildingBlock.WALL
                ];
                // middle lane
                [for i=1 to LINE_OF_SIGHT do
                    let lx = x + ((fst delta) * (i - 1))
                    let ly = y + ((snd delta) * (i - 1))
                    if lx < maze.Length && lx >= 0 && ly >= 0 && ly < maze.[0].Length then
                        yield (intToBuildingBlock (maze.[lx].[ly]))
                    else 
                        yield BuildingBlock.WALL
                ];
                // right lane
                [for i=1 to LINE_OF_SIGHT do
                    let lx = x + (fst rdelta) + ((fst delta) * (i - 1))
                    let ly = y + (snd rdelta) + ((snd delta) * (i - 1))
                    if lx < maze.Length && lx >= 0 && ly >= 0 && ly < maze.[0].Length then
                        yield (intToBuildingBlock (maze.[lx].[ly]))
                    else 
                        yield BuildingBlock.WALL
                ]]
    v

// Paints the Maze on the canvas
let paintMaze (view: BuildingBlock list list) =
    // get the canvas element from the document
    let canvas = Globals.document.getElementsByTagName_canvas().[0]
    // NOTICE: resetting the size of the canvas is like refreshing / clearing it!
    // for some reason on android devices (at least 4.1 or 4.2 I think) with Cordova
    // the clearRect below did not work properly (though work perfectly on PC Chrome),
    // I suspect that e.g. gradients leftovers (?) made some coloring and drawing problems
    canvas.width <- getCanvasSize
    canvas.height <- getCanvasSize

    let fgWidth = getCanvasSize
    let bgWidth = fgWidth / 5.
    let wallsWidth = (fgWidth - bgWidth) / 2.
    let wallsWidthStep = wallsWidth / (float LINE_OF_SIGHT)
    let wallColor = "0, 128, 128"                           // the rgb
    
    canvas.setAttribute("style", "border:1px solid #000000;")
    let ctx = canvas.getContext_2d()
    // Also clearing the rect ... but this alone will not work for android (see above). 
    // And after resizing the canvas above, just kept this too for now
    ctx.clearRect(0., 0., fgWidth-1., fgWidth-1.)
    
    let grd1=ctx.createLinearGradient(0.,0., wallsWidth + 10.,0.);
    grd1.addColorStop(0.,"rgb(" + wallColor + ")");
    grd1.addColorStop(1.,"white");
    let grd2=ctx.createLinearGradient(fgWidth - 1. - wallsWidth - 10., 0., fgWidth - 1.,0.);
    grd2.addColorStop(0., "white");
    grd2.addColorStop(1., "rgb(" + wallColor + ")");

    let mutable keepGoing = true
    let mutable exit = 0
    for i = 1 to LINE_OF_SIGHT do
        if keepGoing then
            // as long as we do not bump into a wall infront
            if view.[(int SightLane.MIDDLE)].[i-1] <> BuildingBlock.WALL then
                // if the exit is in front of us
                if view.[(int SightLane.MIDDLE)].[i-1] = BuildingBlock.EXIT then 
                    exit <- i
                // draw left side
                ctx.fillStyle <- grd1;
                if view.[(int SightLane.LEFT)].[i-1] = BuildingBlock.WALL then
                    ctx.beginPath();
                    ctx.moveTo((float (i-1)) * wallsWidthStep, (float (i-1)) * wallsWidthStep);
                    ctx.lineTo((float i) * wallsWidthStep, (float i) * wallsWidthStep);
                    ctx.lineTo((float i) * wallsWidthStep, fgWidth - 1. - ((float i) * wallsWidthStep));
                    ctx.lineTo((float (i-1)) * wallsWidthStep, fgWidth - 1. - ((float (i-1)) * wallsWidthStep));
                    ctx.closePath();
                    ctx.fill();
                    ctx.stroke();
                else ctx.fillRect((float (i-1)) * wallsWidthStep, (float i) * wallsWidthStep, wallsWidthStep, fgWidth - 1. - ((float i) * wallsWidthStep) * 2.)
                // draw right side
                ctx.fillStyle <- grd2;
                if view.[(int SightLane.RIGHT)].[i-1] = BuildingBlock.WALL then
                    ctx.beginPath();
                    ctx.moveTo(fgWidth - 1. - ((float (i-1)) * wallsWidthStep), (float (i-1)) * wallsWidthStep);
                    ctx.lineTo(fgWidth - 1. - ((float i) * wallsWidthStep), (float i) * wallsWidthStep);
                    ctx.lineTo(fgWidth - 1. - ((float i) * wallsWidthStep), fgWidth - 1. - ((float i) * wallsWidthStep));
                    ctx.lineTo(fgWidth - 1. - ((float (i-1)) * wallsWidthStep), fgWidth - 1. - ((float (i-1)) * wallsWidthStep));
                    ctx.closePath();
                    ctx.fill();
                    ctx.stroke();
                else
                    ctx.fillRect(fgWidth - 1. - ((float (i-1)) * wallsWidthStep), (float i) * wallsWidthStep, 0. - wallsWidthStep, fgWidth - 1. - ((float i) * wallsWidthStep) * 2.)
            // when bumping into a wall in front
            else
                let opacity = (float (LINE_OF_SIGHT - i + 1)) / (float LINE_OF_SIGHT)
                ctx.fillStyle <- "rgba(" + wallColor + ", " + opacity.ToString() + ")"
                ctx.fillRect((float (i-1)) * wallsWidthStep, (float (i-1)) * wallsWidthStep, fgWidth - 1. - ((float (i-1)) * wallsWidthStep) * 2., fgWidth - 1. - ((float (i-1)) * wallsWidthStep) * 2.)
                keepGoing <- false

    // if we spotted the exit then draw our goat image
    if exit = 1 then
        // we got to the exit
        // open congrats window and exit
        Globals.window.alert("Well Done! May the Goat be with you ...")
    if exit > 1 then
        let goat = Globals.document.getElementsByTagName_img().[0]
        ctx.drawImage(goat, (float (exit-1)) * wallsWidthStep, (float (exit-1)) * wallsWidthStep, fgWidth - 1. - ((float (exit-1)) * wallsWidthStep) * 2., fgWidth - 1. - ((float (exit-1)) * wallsWidthStep) * 2.)

// Go Forward
let onUserClick (e: MouseEvent) action =
    // to get to the button itself use: Globals.Dollar.Invoke("#" + action).
    // To change its text specifically (which is inside 2 additional <span> tags), 
    //  add " .ui-btn-text" to its id: Globals.Dollar.Invoke("#" + action + " .ui-btn-text").text("some text") 
    // Not relevant here but nice to know.
    // TBD: Was trying to lose the selection on the buttons after the click (refresh, blure ...), 
    //  but the solution for jquery is diff
    if action = TurnLeftStr then playerDirection <- rotateDirection playerDirection false
    elif action = TurnRightStr then playerDirection <- rotateDirection playerDirection true
    elif action = GoForwardStr then
        let step = xyDelta playerDirection
        let maze = getMazeStruct
        let nx = (fst playerPos) + (fst step)
        let ny = (snd playerPos) + (snd step)
        if nx < maze.Length && nx >= 0 && ny >= 0 && ny < maze.[0].Length then
            if maze.[nx].[ny] <> (int BuildingBlock.WALL) then
                playerPos <- (nx, ny)
    
    // Now paint the maze again
    paintMaze (getPlayerView playerPos playerDirection)

    // TODO ... TBD: refresh - or at least try to ... not sure!
    Globals.Dollar.Invoke("#mainpage").trigger("pagecreate") |> ignore
    Globals.Dollar.Invoke("#mainpage").listview("refresh")

// adds a (footer ... in the case below) navbar item button
let addNavbarItem ref icon text =
    let document = Globals.document
    let item = document.createElement_li()
    let btn = document.createElement_a()
    btn.setAttribute ("href", ref)
    btn.setAttribute ("data-role", "button")
    btn.setAttribute ("data-iconpos", "notext")
    //btn.setAttribute ("style", "font-size:8px")
    btn.setAttribute ("data-icon", icon)
    btn.setAttribute ("id", text)
    btn.onclick <- (fun e -> onUserClick e text |> ignore; box false)
    btn.innerText <- text
    item.appendChild(btn) |> ignore
    item

// the player view (of sight) is an array of arrays, whereas the maze structure is list of lists
// we initialize the view to be empty (all false values)
let initMazeDiscovered =
    let retFalse = fun (a: int) -> false
    Array.map (fun a -> List.toArray a |> Array.map retFalse) (List.toArray getMazeStruct)

// with a new player position or view direction we update the player's discovered portion
// non functional-programming oriented function. should have been a private method. changing variables value.
// Todo: stab
let updateMazeDiscovered (md: bool [] []) pos dir =
    md.[0].[0] <- true

// Create a function that will be compiled into JavaScript. 
// Leveraging DOM, js, jquerymobile, cordova.
let mazingTS()=
    let document = Globals.document
    let body = document.body

    // initializing the game data (mutable & dangerous)
    //let mutable playerPos = getEntrancePos
    //let mutable playerDirection = getEntranceDir
    let mazeDiscovered = initMazeDiscovered                 // The portion of the view that the player discovered, initially nothing
    
    // update player's discovered maze portion based on the initial location
    updateMazeDiscovered mazeDiscovered playerPos playerDirection

    // add the goat jpeg to the document
    let goat = Globals.document.createElement_img()
    goat.setAttribute("id", "goat")
    goat.setAttribute("src", "goat.jpg")
    body.appendChild(goat) |> ignore

    // add jquerymobile div page element
    let divPage = document.createElement_div()
    divPage.setAttribute ("data-role", "page")
    divPage.setAttribute ("id", "mainpage")

    // add jquerymobile page header
    let divHeader = document.createElement_div()
    divHeader.setAttribute ("data-role", "header")
    divHeader.setAttribute ("data-position", "fixed")
    let divHeaderH = document.createElement_h2()
    divHeaderH.innerHTML <- "Goat Mazing"
    divHeader.appendChild(divHeaderH) |> ignore

    // add jquerymobile page content
    let divContent = document.createElement_div()
    divContent.setAttribute ("data-role", "content")
    divContent.setAttribute ("id", "content")
    divContent.setAttribute ("style", "overflow-x: visible")
    // Paint canvas
    let canvas = Globals.document.createElement_canvas()
    //canvas.width <- getCanvasSize
    //canvas.height <- getCanvasSize
    divContent.appendChild(canvas) |> ignore

    // add jquerymobile page footer
    let divFooter = document.createElement_div()
    divFooter.setAttribute ("data-role", "footer")
    divFooter.setAttribute ("data-position", "fixed")
    // add NavBar to footer
    let divFooterNavBar = document.createElement_div()
    divFooterNavBar.setAttribute ("data-role", "navbar")
    let ulFooterNavBar = document.createElement_ul()
    // add the footer navbar items
    ulFooterNavBar.appendChild(addNavbarItem "#" "back" TurnLeftStr) |> ignore
    ulFooterNavBar.appendChild(addNavbarItem "#" "arrow-u" GoForwardStr) |> ignore
    ulFooterNavBar.appendChild(addNavbarItem "#" "forward" TurnRightStr) |> ignore
    divFooterNavBar.appendChild(ulFooterNavBar) |> ignore
    divFooter.appendChild(divFooterNavBar) |> ignore

    // the 3 parts of the jquerymobile main page
    divPage.appendChild(divHeader) |> ignore
    divPage.appendChild(divContent) |> ignore
    divPage.appendChild(divFooter) |> ignore
    // add the jquerymobile page to the html body
    body.appendChild(divPage) |> ignore

    // Now that the HTML structure is complete we can paint (otherwise e.g. will not find the canvas ...)
    paintMaze (getPlayerView playerPos playerDirection)

let mazingCode =
    try
        Compiler.Compiler.Compile(
            // This argument is the quotation to compile
            <@ mazingTS() @>, 
            // This argument tells the compiler not to wrap 
            // the result in a return statement
            noReturn=true)
    with
    // if we fail to complie you can open the source of the html page and see the exception description
    | ex -> (ex.ToString())

// Here we define the page we'll create...
let mazingPage = 
    sprintf """<!doctype html>
<html>
<head>
    <meta charset="UTF-8">
    <title>GoatMazing</title>
    <meta name="viewport" id="viewport" 
        content="width=device-width, height=device-height, initial-scale=1.0, maximum-scale=1.0, user-scalable=no;" />
    <link rel="stylesheet" href="http://code.jquery.com/mobile/1.3.2/jquery.mobile-1.3.2.min.css">
    <script src="http://code.jquery.com/jquery-1.8.3.min.js"></script>
    <script src="http://code.jquery.com/mobile/1.3.2/jquery.mobile-1.3.2.min.js"></script>
</head>
<body>
<script>
%s
</script>
</body>
</html>"""  mazingCode
