module App.Main
open Fable.Import
open App.CodeMirrorInterface
open App.Renderer
open Fable.Import.Browser





let main () =
    (*let incCountPrint counter content =
        counter <- counter + 1
        content.innerHTML <- string counter *)
    let GreetingWords = "Hello world!"
    let mutable myCounter = 0
    myCounter <- 1
    printfn "Starting..."
    let editId = getById<Fable.Import.Browser.HTMLTextAreaElement> "code"
    printfn "Creating editor"
    let cmEditor = App.CodeMirrorImports.CodeMirror.fromTextArea(editId, initOptions)
    //let myTextArea = getById<Fable.Import.Browser.HTMLTextAreaElement> "panel"
    //let cmEditor2 = App.CodeMirrorImports.CodeMirror.fromTextArea(myTextArea, initOptions)
    //cmEditor2.setValue "hei"
    //cmEditor2.setOption ("ReadOnly", "True")
    let content = Browser.document.getElementById "myText"
    let button = Browser.document.getElementById("mySlide") :?> HTMLInputElement
    
    //myButton.innerHTML <- "New button"
    let incCountPrint () =
        myCounter <- myCounter + 1
        console.log("Counter incremented")
        content.innerHTML <- string myCounter
    content.innerHTML <- (sprintf "<h1>%s</h1>" GreetingWords)
    content.innerHTML <- content.innerHTML + "\nHello"
    content.innerHTML <- content.innerHTML + "\n" + (string (3+2))
    incCountPrint()
    incCountPrint()
    button.addEventListener_change(fun _ -> incCountPrint(); box())
    //button.onclick = incCountPrint()
    printfn "Setting editor value"
    cmEditor.setValue " abc def *** //comment"
    //printfn "Line tokens: %A" (cmEditor.getLineTokens 0)
    console.log("It's working!")
    //let width = getElementById("width") : Fable.Import.Browser.HTMLInputElement
    //render()
    //cmEditor.setValue ""
    printfn "Main code finished"
    
main()

