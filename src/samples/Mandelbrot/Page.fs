// ---
// header: Mandelbrot
// tagline: Using HTML5 canvas
// ---

[<ReflectedDefinition>]
module Program

open FunScript
open FunScript.TypeScript

type Complex = { r : double; i : double }
type Color = { r : int; g : int; b : int; a : int }

let maxIter = 768

let height = 800
let width = 1000

let minX = -2.2
let maxX = 1.
let minY = -1.4
let maxY = 1.4

let iteratePoint (s : Complex) (p : Complex) : Complex =
    { r = s.r + p.r*p.r - p.i*p.i; 
      i = s.i + 2.0 * p.i * p.r }

let getColorIndex (p : Complex) =
    let mutable z = p
    let mutable i = 0
    while i < maxIter && (z.r*z.r + z.i*z.i < 2.0) do
        z <- iteratePoint p z
        i <- i + 1
    if i >= maxIter then
        0
    else
        let mo = z.r*z.r + z.i*z.i
        let pi = log(log(mo) / log(4.)) / log(2.)
        let ci = int(float(i) - pi * 768.)
        ci       

let getColor (i : int) : Color = 
    if (i < 0 || i > 767) then
      { r = 0; g = 0; b = 0; a = 255 }
    elif i >= 512 then
      { r = i - 512; g = 768 - i; b = 0; a = 255 } 
    elif i >= 256 then
      { r = 0; g = i - 256; b = 512 - i; a = 255 } 
    else 
      { r = 0; g = 0; b = i; a = 255 }

//   [0..768] |> List.map iterCountToColor |> List.mapi (fun i c -> printf "%d (%d %d %d) \n" i c.r c.g c.b) |> ignore
    
let getCoordColor (x : int, y : int) : Color =
    let p = { r = float x * (maxX - minX) / float width  + minX ; 
              i = float y * (maxY - minY) / float height + minY }
    
    getColorIndex p |> getColor

let showSet() =
    let ctx = Globals.document.getElementsByTagName_canvas().[0].getContext_2d()
    
    let img = ctx.createImageData(float width, float height)
    for y = 0 to height-1 do
        for x = 0 to width-1 do
            let index = (x + y * width) * 4
            let color = getCoordColor (x, y)
            img.data.[index+0] <- float color.r
            img.data.[index+1] <- float color.g
            img.data.[index+2] <- float color.b
            img.data.[index+3] <- float color.a
    ctx.putImageData(img, 0., 0.)

let main() =
    showSet()
    
do Runtime.Run(directory="Web")