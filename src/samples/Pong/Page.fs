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

open FunScript
open FunScript.TypeScript

/// Basic functionality for creating and rendering window using HTML canvas
/// (the functions here fill the window, set position of image and create image)
module Window =
  let canvas = lazy Globals.document.getElementsByTagName_canvas().[0]
  let context = lazy canvas.Value.getContext_2d()
  let dimensions () = canvas.Value.width, canvas.Value.height, canvas.Value.width/60.0
  let ($) s n = s + n.ToString()
  let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

  let filled color rect =
      let ctx = context.Value  
      ctx.fillStyle <- color
      ctx.fillRect rect    

//  let position (x,y) (img : HTMLImageElement) =
//      img.style.left <- x.ToString() + "px"
//      img.style.top <- (canvas.Value.offsetTop + y).ToString() + "px"


//  let image (src:string) = 
//      let image = Globals.document.getElementsByTagName_img().[0]
//      if image.src.IndexOf(src) = -1 then image.src <- src
//      image

// ----------------------------------------------------------------------------
// The rest of the code contains the commented web site content
// ----------------------------------------------------------------------------

open Window

type Paddle = {x:float; y:float; vy:float; isleft:bool }

let paddlerect p u = p.x, p.y - u*3., u, u*6.

type Ball = {x:float; y:float; vx:float; vy:float }

let ballrect b u = b.x, b.y, u*1., u*1.

type State = {w:float; h:float; u:float; p1:Paddle; p2:Paddle; ball:Ball}

// function intersectRect(r1, r2) {  return !(r2.left > r1.right || r2.right < r1.left || r2.top > r1.bottom || r2.bottom < r1.top); }
let intersect r1 r2 = not (r2.x > r1.x + 2. || r2.x + 2. < r1.x || r2.y > r1.y + 2. || r2.y + 2. < r1.y)

//let intersects x y w h = true
let hit (b:Ball) (p:Paddle) =  false // TODO intersect (ballrect b) (paddlerect p)

// move ball
let move b:Ball = {b with x = b.x + b.vx; y = b.y + b.vy }

// score
let score (state:State) = (state.ball.x > state.w) || (state.ball.x < -state.u) 

// deflect ball
let bounce (state:State) (b:Ball) = 
  if b.y < 0. || b.y > state.h - state.u then
    {b with vy = -b.vy }    
  elif hit b state.p1 then
    {b with vx = Globals.Math.random() }    
  elif hit b state.p2 then
    {b with x = b.x + b.vx; y = b.y + b.vy }    
  else
    b

let next (off:bool) (b:Ball) =
  if off then
    {b with x = 250.; y = 150. ; vx = -b.vx }    
  else
    b
   
let moveto (b:Ball) (p:Paddle) =
  {p with vy = if (p.y < b.y) then 2. else -2. ;
          y = if (p.isleft && b.x < 200. || (not (p.isleft)) && b.x > 300.) then p.y + p.vy else p.y}


/// Render mario on canvas 
let render state =
    // Render background, line, p1, p2
    (0., 0., state.w, state.h) |> filled (rgb 0 0 0)
    (state.w/2.0 - 2., 0., 4.0, state.h) |> filled (rgb 255 255 255)
    paddlerect state.p1 state.u |> filled (rgb 255 255 255)
    paddlerect state.p2 state.u |> filled (rgb 255 255 255)
    ballrect state.ball state.u |> filled (rgb 255 255 255)
    ()

let main() =
  // Some initialization
  let w,h,unit = dimensions()
  //let rnd = new System.Random()

  // Recursive function that updates the state & renders it
  let rec update (s:State) () =
      let s = {s with ball = s.ball |> move |> bounce s |> next (score s);
                      p1 = s.p1 |> moveto s.ball;
                      p2 = s.p2 |> moveto s.ball }
      render s   
      Globals.setTimeout(update s, 1000. / 60.) |> ignore

  
  // 
  let paddle1 = { x=0.0; y=h/2.0; vy=0.; isleft=true }
  let paddle2 = { x=w-unit; y=h/2.0; vy=0.; isleft=false }
  let ball = { x=w/2.0; y=h/2.0; vx=1.; vy= -2.0 }
  let state:State = {w=w; h=h; u=unit; p1=paddle1; p2=paddle2; ball=ball }
  update state ()
   
(*** hide ***)
do FunScript.Runtime.Run(directory="Web")