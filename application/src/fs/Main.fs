module App.Main
open Fable.Import
open App.CodeMirrorInterface
open App.Renderer
open Fable.Import.Browser





let main () =
    let mutable myCounter = 0
    myCounter <- 1
    printfn "Starting..."
    let editId = getById<Fable.Import.Browser.HTMLTextAreaElement> "code"
    printfn "Creating editor"
    let cmEditor = App.CodeMirrorImports.CodeMirror.fromTextArea(editId, initOptions)
    let content = Browser.document.getElementById "myText"
    let slider = Browser.document.getElementById("mySlide") :?> HTMLInputElement
    //let button = Browser.document.getElementById("myOtherButton") :?> HTMLInputElement
    let incCountPrint () = // Change here to start actual program. 
        myCounter <- myCounter + 1 // Have parser go through input, and then print to
        console.log("Counter incremented") // the content box.
        content.innerHTML <- ("    " + string myCounter + "<br />")
        content.innerHTML <- content.innerHTML + string (cmEditor.getValue())
    slider.addEventListener_change(fun _ -> incCountPrint(); box())
    printfn "Setting editor value"
    cmEditor.setValue " abc def *** //comment"
    printfn "Line tokens: %A" (cmEditor.getLineTokens 3)
    console.log("It's working!")
    printfn "Hallo %A" (cmEditor.getValue())
    //render()
    printfn "Main code finished"
    
main()

