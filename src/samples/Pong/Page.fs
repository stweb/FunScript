// ---
// header: Pong
// tagline: Pong Clock is a program that represents the current system time through an epic game two dueling paddles.
// popup-style: width:580px
//
// Copyright (C) 2006       Ben Peck (original java version)
// Copyright (C) 2007-2014  Stefan Weber (C# .net port)
// Copyright (C) 2014       Stefan Weber (F# .net port) 

[<ReflectedDefinition>]
module Program

open System
open FunScript
open FunScript.TypeScript

/// Basic functionality for creating and rendering window using HTML canvas
/// (the functions here fill the window, set position of image and create image)
module Window =
  let canvas  = lazy Globals.document.getElementsByTagName_canvas().[0]
  let context = lazy canvas.Value.getContext_2d()
  let ($) s n = s + n.ToString()
  let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"
  let black = (rgb 0 0 0)
  let silver = (rgb 160 160 160)
  let white = (rgb 255 255 255)

  let filled color rect =
      let ctx = context.Value  
      ctx.fillStyle <- color
      ctx.fillRect rect    

// bitmap for the numbers
let num = [ [ (1,1,1);  (1,0,1);  (1,0,1);  (1,0,1);  (1,0,1);  (1,1,1); ];
            [ (0,0,1);  (0,0,1);  (0,0,1);  (0,0,1);  (0,0,1);  (0,0,1); ];
            [ (1,1,1);  (0,0,1);  (1,1,1);  (1,0,0);  (1,0,0);  (1,1,1); ];
            [ (1,1,1);  (0,0,1);  (1,1,1);  (0,0,1);  (0,0,1);  (1,1,1); ];
            [ (1,0,1);  (1,0,1);  (1,1,1);  (0,0,1);  (0,0,1);  (0,0,1); ];
            [ (1,1,1);  (1,0,0);  (1,1,1);  (0,0,1);  (0,0,1);  (1,1,1); ];
            [ (1,1,1);  (1,0,0);  (1,1,1);  (1,0,1);  (1,0,1);  (1,1,1); ];
            [ (1,1,1);  (0,0,1);  (0,0,1);  (0,0,1);  (0,0,1);  (0,0,1); ];
            [ (1,1,1);  (1,0,1);  (1,1,1);  (1,0,1);  (1,0,1);  (1,1,1); ];
            [ (1,1,1);  (1,0,1);  (1,1,1);  (0,0,1);  (0,0,1);  (0,0,1); ] ];

// true when a pixel is on
let pix1 (c, _, _) = (c = 1)
let pix2 (_, c, _) = (c = 1)
let pix3 (_, _, c) = (c = 1)

// The code that is produced by writing (c:char) = 'x' does not work correctly
// in Jint, but works fine in web browsers. This function allows us to write
// charToInt c = charToInt 'x' in the unit tests (which works in Jint too.)
[<FunScript.JS; FunScript.JSEmit("return {0}.charCodeAt(0);")>]
let charToInt (c:char) : int = int c 
 
// ----------------------------------------------------------------------------
// The rest of the code contains the commented web site content
// ----------------------------------------------------------------------------

open Window

let mutable width   = 400.
let mutable height  = 300.
let mutable block   = 10.

type Paddle = {x:float; y:float; vy:float; isleft:bool }

let paddlerect p = p.x, p.y - block*3., block, block*6.

type Ball = {x:float; y:float; vx:float; vy:float }

let ballrect b = b.x, b.y, block*1., block*1.

type State = {p1:Paddle; p2:Paddle; ball:Ball}

type Rect = float * float * float * float

// function intersectRect(r1, r2) {  return !(r2.left > r1.right || r2.right < r1.left || r2.top > r1.bottom || r2.bottom < r1.top); }
let intersect (r1x, r1y, r1w, r1h) (r2x, r2y, r2w, r2h) = 
    not (r2x > r1x + r1w || r2x + r2w < r1x || r2y > r1y + r1w || r2y + r2w < r1y)

//let intersects x y w h = true
let hit (b:Ball) (p:Paddle) =  
    intersect (ballrect b) (paddlerect p)    

// move ball
let move b:Ball = {b with x = b.x + b.vx; y = b.y + b.vy }

// score
let score (state:State) = (state.ball.x > width) || (state.ball.x < -block) 

// deflect ball
let bounce (state:State) (b:Ball) = 
  if b.y < 0. || b.y > height - block then
    {b with vy = -b.vy }    
  elif hit b state.p1 || hit b state.p2 then
    {b with vx = -b.vx }    
  else
    b

let next (off:bool) (b:Ball) =
    if off then    
        {b with x = 250.; y = 150. ; vx = -b.vx }      
    else
    b
   
let moveto (b:Ball) (p:Paddle) =
  {p with vy = if (p.y < b.y) then block else -block ;
          y = if (p.isleft && b.x < 200. || (not (p.isleft)) && b.x > 300.) then p.y + p.vy else p.y}

let drawRect x y =
    let rect = x, y, block+1., block+1.
    rect |> filled silver
    ()

let drawDigit x y (n:char) =
    let ix = charToInt(n) - 48
    num.[ix] |> List.iteri (fun i line ->
        let yy = y + (block * float i)
        if pix1 line then drawRect x yy
        if pix2 line then drawRect (x+block) yy
        if pix3 line then drawRect (x+block+block) yy
    )

let drawNumber x y (num:string) =
    if num.Length = 2 then // TODO leading 0 
        drawDigit (block*4. + x) y num.[1] |> ignore
    if num.Length = 1 then // TODO leading 0 
        drawDigit x y num.[0] |> ignore
    ()

/// Render mario on canvas 
let render state =
    (0., 0., width, height) |> filled black // background
    (width/2.0 - block/4., 0., block/2., height) |> filled white // line
    paddlerect state.p1 |> filled white
    paddlerect state.p2 |> filled white
    ballrect state.ball |> filled white

    let now = DateTime.Now
    let hh = sprintf "%02d" now.Hour // TODO leading 0 in sprintf seems broken
    let mm = sprintf "%02d" now.Minute
    drawNumber (width/2. - 12.*block) (block*2.) hh |> ignore
    drawNumber (width/2. + 5.*block) (block*2.) mm |> ignore
    ()

let main() =
  width  <- canvas.Value.width
  height <- canvas.Value.height
  block  <- canvas.Value.width/60.0 // size of a block (super pixel)

  // Recursive function that updates the state & renders it
  let rec update (s:State) () =
      let s = {s with ball = s.ball |> move |> bounce s |> next (score s);
                      p1 = s.p1 |> moveto s.ball;
                      p2 = s.p2 |> moveto s.ball }
      render s   
      Globals.setTimeout(update s, 1000. / 60.) |> ignore
  
  // Initial state
  let paddle1 = { x=0.0; y=height/2.0; vy=0.; isleft=true }
  let paddle2 = { x=width-block; y=height/2.0; vy=0.; isleft=false }
  let ball = { x=width/2.0; y=height/2.0; vx=block/3.; vy= -(block/1.5) }
  let state:State = {p1=paddle1; p2=paddle2; ball=ball }

  // run
  update state ()
   
(*** hide ***)
do FunScript.Runtime.Run(directory="Web")