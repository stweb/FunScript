(*
    Modifying the BodyElementsExample to work with the JQueryMobile library.
    The Project.fs then creates an index file with the HTML contents and opens it using the default brower.
*)

[<ReflectedDefinition>]
module JQueryMobileExample

open FunScript
open FunScript.TypeScript


// Access standard JavaScript libraries in a type-safe way.
// Generate strongly-typed interfaces from TypeScript 0.9.x 
// definitions files or use any of the 280+ pre-built library
// mappings (hosted on NuGet).

// Just the paint part from the HTMLCanvasExample example
let paintCanvas (canvas: HTMLCanvasElement) =
    canvas.width <- 200.
    canvas.height <- 200.
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- "rgb(200,0,0)"
    ctx.fillRect (10., 80., 100., 50.);
    ctx.fillStyle <- "rgba(0, 0, 200, 0.5)"
    ctx.fillRect (10., 10., 55., 100.)

let jQueryMobileTS()=
    let document = Globals.document
    let body = document.body

    let divPage = document.createElement_div()
    divPage.setAttribute ("data-role", "page")
    divPage.setAttribute ("id", "mainpage")

    let divHeader = document.createElement_div()
    divHeader.setAttribute ("data-role", "header")
    let divHeaderH1 = document.createElement_h1()
    divHeaderH1.innerHTML <- "Header: Using HTML5 Canvas (JQueryMobileExample)"
    divHeader.appendChild(divHeaderH1) |> ignore

    let divContent = document.createElement_div()
    divContent.setAttribute ("data-role", "content")
    let divContentH1 = document.createElement_h1()
    divContentH1.innerHTML <- "Content ..."
    divContent.appendChild(divContentH1) |> ignore
    // Paint canvas
    let canvas = Globals.document.createElement_canvas()
    divContent.appendChild(canvas) |> ignore
    paintCanvas canvas

    let divFooter = document.createElement_div()
    divFooter.setAttribute ("data-role", "footer")
    let divFooterH1 = document.createElement_h1()
    divFooterH1.innerHTML <- "Footer"
    divFooter.appendChild(divFooterH1) |> ignore

    divPage.appendChild(divHeader) |> ignore
    divPage.appendChild(divContent) |> ignore
    divPage.appendChild(divFooter) |> ignore

    body.appendChild(divPage) |> ignore    

let main() =
    jQueryMobileTS()    

    //initialize the new page 
    //$.mobile.initializePage();
    Globals.Dollar.mobile.initializePage()
    Globals.Dollar.mobile.changePage("#mainpage")
    

do FunScript.Runtime.Run()
