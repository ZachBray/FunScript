// ---
// header: Mandelbrot
// tagline: Using HTML5 canvas
// ---

[<ReflectedDefinition>]
module Program

open FunScript

type Complex = { r : double; i : double }
type Color = { r : int; g : int; b : int; a : int }

[<JSEmit("return document.getElementById({0});")>]
let getElement (x:string) : 'a = failwith "never"

[<JSEmit("return {0}.getContext(\"2d\");")>]
let getContext (o : obj) : 'a = failwith "never"

[<JSEmit("return {0}.createImageData({1},{2});")>]
let createImageData (ctx : obj, width : int, height : int) : 'a = failwith "never"

[<JSEmit("{0}.putImageData({1},0,0);")>]
let putImageData (ctx : obj, data : obj) : unit = failwith "never"

[<JSEmit("{0}.data[{1}]={2};")>]
let setPixel (img : obj, index : int, value : int) : unit = failwith "never"

let maxIter = 512

let height = 800
let width = 1000

let minX = -2.1 
let maxX = 0.5
let minY = -1.4
let maxY = 1.4

let inBound (p : Complex) : bool =
    p.r*p.r + p.i*p.i < 4.0

let iteratePoint (s : Complex) (p : Complex) : Complex =
    { r = s.r + p.r*p.r - p.i*p.i; i = s.i + 2.0 * p.i * p.r }

let getIterationCount (p : Complex) =
    let mutable z = p
    let mutable i = 0
    while i < maxIter && inBound z do
      z <- iteratePoint p z
      i <- i + 1
    i
    
let iterCountToColor (i : int) : Color =
    let i = maxIter - i
    { r = 0; g = i % 256; b = 100 * (i / 256); a = 255 }

let getCoordColor (x : int, y : int) : Color =
    let p = { r = float x * (maxX - minX) / float width + minX
            ; i = float y * (maxY - minY) / float height + minY }
    let i = getIterationCount p
    iterCountToColor i

let showSet() =
    let ctx = getElement("canvas") |> getContext
    let img = createImageData(ctx, width, height)
    for y = 0 to height-1 do
        for x = 0 to width-1 do
            let index = (x + y * width) * 4
            let color = getCoordColor (x, y)
            setPixel(img, index+0, color.r)
            setPixel(img, index+1, color.g)
            setPixel(img, index+2, color.b)
            setPixel(img, index+3, color.a)
    putImageData(ctx, img)

let main() =
    showSet()
    
do Runtime.Run(directory="Web")