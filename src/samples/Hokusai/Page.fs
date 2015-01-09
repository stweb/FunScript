﻿// ---
// header: Hokusai
// tagline: Using HTML5 canvas
// ---
[<ReflectedDefinition>]
module Program

open System
open FunScript
open FunScript.TypeScript

(**
Complex numbers
---------------------------------------------------------------------

Simple implementation of complex numbers for JavaScript.
*)

//type Complex = 
//    | Complex of float * float
type Complex (r: float, i: float) =    
    member x.R = r
    member x.I = i
    member x.Abs() = 
        let num = Math.Abs(r)
        let num2 = Math.Abs(i)
        if (num > num2) then 
            let num3 = num2 / num
            num * Math.Sqrt(1.0 + num3 * num3)
        elif num2 = 0.0 then num
        else 
            let num4 = num / num2
            num2 * Math.Sqrt(1.0 + num4 * num4)
    
    static member (+) (a: Complex, b: Complex) = Complex(a.R + b.R, a.I + b.I)

    static member Pow(c: Complex, power: float) = 
        let num = c.Abs()
        let num2 = Math.Atan2(c.I, c.R)
        let num3 = power * num2
        let num4 = Math.Pow(num, power)
        Complex(num4 * Math.Cos(num3), num4 * Math.Sin(num3))

(**
Calculating the Julia set
---------------------------------------------------------------------
*)

/// Constant that generates nice fractal
let c = Complex(-0.70176, -0.3842)

/// Generates sequence for given coordinates
let iterate x y = 
    let rec loop current = 
        seq { 
            yield current
            yield! loop (Complex.Pow(current, 2.0) + c)
        }
    loop (Complex(x, y))

let countIterations max x y = 
    iterate x y
    |> Seq.take (max - 1)
    |> Seq.takeWhile (fun v -> v.Abs() < 2.0)
    |> Seq.length

(**
Generating the color palette
---------------------------------------------------------------------
*)

// Transition between colors in 'count' steps
let (--) clr count = clr, count
//
//let (-->) ((r1, g1, b1), count) (r2, g2, b2) =     
//    [ for c in 0..count - 1 -> 
//          let k = float c / float count
//          let mid v1 v2 = (float v1 + ((float v2) - (float v1)) * k)
//          (mid r1 r2, mid g1 g2, mid b1 b2) ]

let (-->) ((r1, g1, b1), count) (r2, g2, b2) =     
    Array.init count (fun c -> 
          let k = float c / float count
          let mid v1 v2 = (float v1 + ((float v2) - (float v1)) * k)
          (mid r1 r2, mid g1 g2, mid b1 b2) )

// Palette with colors used by Hokusai
let palette = 
    [| // 3x sky color & transition to light blue
       yield! (245, 219, 184) -- 3 --> (245, 219, 184)
       yield! (245, 219, 184) -- 4 --> (138, 173, 179)
       // to dark blue and then medium dark blue
       yield! (138, 173, 179) -- 4 --> (2, 12, 74)
       yield! (2, 12, 74) -- 4 --> (61, 102, 130)
       // to wave color, then light blue & back to wave
       yield! (61, 102, 130) -- 8 --> (249, 243, 221)
       yield! (249, 243, 221) -- 32 --> (138, 173, 179)
       yield! (138, 173, 179) -- 32 --> (61, 102, 130) |]

(**
Drawing the fractal
---------------------------------------------------------------------
*)

// Specifies what range of the set to draw
let w = -0.4, 0.4
let h = -0.95, -0.35
// Create bitmap that matches the size of the canvas
let width = 400.0
let height = 300.0

/// Set pixel value in ImageData to a given color
let setPixel (img : ImageData) x y width (r, g, b) = 
    let index = (x + y * int width) * 4
    img.data.[index + 0] <- r
    img.data.[index + 1] <- g
    img.data.[index + 2] <- b
    img.data.[index + 3] <- 255.0

/// Dynamic operator that returns HTML element by ID
let (?) (doc : Document) name : 'R = doc.getElementById (name) :?> 'R

/// Render fractal asynchronously with sleep after every line
let render() = 
    async { 
        // Get <canvas> element & create image for drawing
        let canv : HTMLCanvasElement = Globals.document?canvas
        let ctx = canv.getContext_2d()
        let img = ctx.createImageData (float width, float height)

        Globals.console.time("render");

        // For each pixel, transform to the specified range
        // and get color using countInterations and palette
        //for x in 0..int width - 1 do
        //    for y in 0..int height - 1 do
        for x = 0 to int width - 1 do
            for y = 0 to int height - 1 do
                let x' = (float x / width * (snd w - fst w)) + fst w
                let y' = (float y / height * (snd h - fst h)) + fst h
                let it = countIterations palette.Length x' y'
                setPixel img x y width palette.[it]
            
            // Insert non-blocking waiting & update the fractal
            // do! Async.Sleep(1)
            ctx.putImageData (img, 0.0, 0.0)

        Globals.console.timeEnd("render");
        // render: 16565.630ms
    }

/// Setup button event handler to start the rendering
let main() = 
    let go : HTMLButtonElement = Globals.document?go
    go.addEventListener_click (fun _ -> 
        render() |> Async.StartImmediate
        null)

(**
Start local FunScript server
---------------------------------------------------------------------
*)

do Runtime.Run(directory = "Web", port = (new System.Random()).Next(10000) + 8000)
