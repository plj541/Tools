function OnStart() {
 // Launch a file, then exit
 var myFile= app.GetClipboardText()
 if (0< myFile.length) {
  if (myFile.startsWith("/")) {
   app.OpenFile(myFile)
  } else {
   app.OpenUrl(myFile)
  }
  app.SetClipboardText(":*?")  // Tell J I'm Done
 }
 app.Exit()
}