"assist.plot" <-
function(up=0)
{
   parameters <- ""
   envir=parent.frame(up+1)
   assign(".comand", "assist.plot()", envir=.GlobalEnv)
   call <- sys.call(-up)

   cat("Note: For multiple plots on one page, see 'par'.\n") 
   cat("\n")
   input <- .getexpr(prompt="Enter 'x'", envir=envir, call=call)
   if(input$expr != ".")
      parameters <- paste(sep="", input$expr)
   .assist.update("plot", parameters, FINAL=FALSE)
   cat("\n")
   xlab <- input$expr

   input <- .getexpr(prompt="Enter 'y'", envir=envir, call=call, SHOW.VARS=FALSE)
   if(input$expr != ".") parameters <- if(parameters!="")
     paste(sep="", parameters, ", ", input$expr) else
     paste(input$expr)
   .assist.update("plot", parameters, FINAL=FALSE)
   ylab <- input$expr

   choice <- .menu(c("Scatter plot (default)", "Line plot", "Both 1 and 2 (lines segmented)",
                  "Line segments only (from 3)",
                  "Overplotted 1 & 2 (lines complete)",
                  "Histogram (vertical lines)", "Steps (trailing)",
                  "Steps (other way round)", "No plotting"),
                  default="for default", inputs=1:9)
   if(is.na(choice)) choice <- 0
   switch(choice,
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", type=\"p\"") else
      paste(sep="",  "type=\"p\""),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", type=\"l\"") else paste(sep="",  "type=\"l\""),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", type=\"b\"") else
      paste(sep="",  "type=\"b\""),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", type=\"c\"") else
      paste(sep="",  "type=\"c\""),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", type=\"o\"") else
      paste(sep="",  "type=\"o\""),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", type=\"h\"") else
      paste(sep="",  "type=\"h\""),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", type=\"s\"") else
      paste(sep="",  "type=\"s\""),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", type=\"S\"") else
      paste(sep="",  "type=\"S\""),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", type=\"n\"") else
      paste(sep="",  "type=\"n\"")
   )
   if(choice) { .assist.update("plot", parameters, FINAL=FALSE); cat("\n") }

   if(choice %in% c(1, 3, 5)) {
      repeat{
         input <- readline(prompt="Enter point character, or leave blank for default: ")
         if(nchar(input) > 1)
            cat("Only", deparse(substr(input,1,1)), "will apply.\n") else break
         choice <- .choose(choice="to Continue", select=1, not=0,
                           default="to change")
         if(choice) break
      }
      if(input != "") parameters <- if(parameters != "")
         paste(sep="", parameters, ", pch=", deparse(input)) else
         paste(sep="",  "pch=", deparse(input))
      .assist.update("plot", parameters, FINAL=FALSE)

      cat("\n")

      choice <- .menu(c("Default size", "Double size", "Triple size",
                       "5 times size", "10 times size"), inputs=1:5)
      switch(choice,
        parameters <- parameters,
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex=2") else
         paste(sep="",  "lty=2"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex=3") else
         paste(sep="",  "lty=3"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex=5") else
         paste(sep="",  "lty=5"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex=10") else
         paste(sep="",  "lty=6"),
      )
      if(choice) .assist.update("plot", parameters, FINAL=FALSE)
   }

   if(!choice %in% c(1, 9)) {
      cat("\n") 
      choice <- .menu(c("Solid lines (default)", "Broken lines (dashes)",
                       "Broken lines (dots)", "Dot-Dash lines",
                       "Long dashes", "Long-Short dashes"),
                       default="for default", inputs=1:6)
      switch(choice, parameters <- parameters,
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", lty=2") else
         paste(sep="",  "lty=2"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", lty=3") else
         paste(sep="",  "lty=3"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", lty=4") else
         paste(sep="",  "lty=4"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", lty=5") else
         paste(sep="",  "lty=5"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", lty=6") else
         paste(sep="",  "lty=6")
      )
      if(choice) .assist.update("plot", parameters, FINAL=FALSE);
   }

   if(!choice %in% c(1,9)) {
      cat("\n")
      choice <- .menu(c("Default thickness", "Thick lines", "Quite thick lines",
                        "Very thick lines", "Extremely thick lines"),
                         default="for default", inputs=1:5)
      switch(choice, parameters <- parameters,
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", lwd=2") else
         paste(sep="",  "lwd=2"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", lwd=5") else
         paste(sep="",  "lwd=5"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", lwd=10") else
         paste(sep="",  "lwd=10"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", lwd=30") else
         paste(sep="",  "lwd=30")
      )
      if(choice) { .assist.update("plot", parameters, FINAL=FALSE); cat("\n") }
   }

   cat("Please select:\n")
   LABELS <- .choose("to set Label options", select=c("L","l"))
   cat("\n")

   if(LABELS) {
      prompt <- "Enter X-label, '.' for default, or leave blank: "
      input <- readline(prompt=prompt)
      if(input != ".") parameters <- if(parameters != "")
        paste(sep="", parameters, ", xlab=", deparse(input)) else
        paste(sep="",  "xlab=", deparse(input))
      if(input != ".") .assist.update("plot", parameters, FINAL=FALSE)
   
      prompt <- "Enter Y-label, '.' for default, or leave blank: "
      input <- readline(prompt=prompt)
      if(input != ".") parameters <- if(parameters != "")
        paste(sep="", parameters, ", ylab=", deparse(input)) else
        paste(sep="",  "ylab=", deparse(input))
      if(input != ".") .assist.update("plot", parameters, FINAL=FALSE)
   
      prompt <- "Enter Title for plot, '.' for default, or leave blank: "
      input <- readline(prompt=prompt)
      if(input != ".") parameters <- if(parameters != "")
        paste(sep="", parameters, ", main=", deparse(input)) else
        paste(sep="",  "main=", deparse(input))
      .assist.update("plot", parameters, FINAL=FALSE)
   }

   COLORS <- .choose("to set Color options", select=c("C","c"))
   cat("\n")

   if(COLORS) {
      cat("To set a background color, use 'par'.\n") 
      cat("For some available colors, Type: colors()\n")
      cat("Or try: blue, red, green, brown, purple, orange etc.\n")
      cat("\n")

      repeat{
         input <- readline(prompt =
           "Enter a color for plot or leave blank to Skip: ")
         if(substr(input,1,6) == "colors") {
            c <- colors()
            n <- nchar(c)
            cat(sep="\t", c[!substr(c,n,n) %in% as.character(0:9) & n<7])
            cat("\n")
         }
         if(input %in% colors() | input=="")
            break else if(input != "colors()")
               cat(deparse(input), "not an available color.\n")
      }
      if(input != "") parameters <- if(parameters != "")
        paste(sep="", parameters, ", col=", deparse(input)) else
        paste(sep="",  "col=", deparse(input))
      if(input != "") {
         .assist.update("plot", parameters, FINAL=FALSE)
         cat("\n")
      }

      repeat{
         input <- readline(prompt =
           "Enter a color for axes or leave blank for default: ")
         if(substr(input,1,6) == "colors") {
            c <- colors()
            n <- nchar(c)
            cat(sep="\t", c[!substr(c,n,n) %in% as.character(0:9) & n<7])
            cat("\n")
         }
         if(input %in% colors() | input=="") break else if(input != "colors()")
            cat(deparse(input), "not an available color.\n")
      }
      if(input != "") parameters <- if(parameters != "")
        paste(sep="", parameters, ", col.axis=", deparse(input)) else
        paste(sep="",  "col.axis=", deparse(input))
      if(input != "") {
         .assist.update("plot", parameters, FINAL=FALSE)
         cat("\n")
      }

      repeat{
         input <- readline(prompt =
           "Enter a color for axis labels or leave blank for default: ")
         if(substr(input,1,6) == "colors") {
            c <- colors()
            n <- nchar(c)
            cat(sep="\t", c[!substr(c,n,n) %in% as.character(0:9) & n<7])
            cat("\n")
         }
         if(input %in% colors() | input=="") break else if(input != "colors()")
            cat(deparse(input), "not an available color.\n")
      }
      if(input != "") parameters <- if(parameters != "")
        paste(sep="", parameters, ", col.lab=", deparse(input)) else
        paste(sep="",  "col.lab=", deparse(input))
      if(input != "") {
         .assist.update("plot", parameters, FINAL=FALSE)
         cat("\n")
      }

      repeat{
         input <- readline(prompt =
           "Enter a color for main title or leave blank for default: ")
         if(substr(input,1,6) == "colors") {
            c <- colors()
            n <- nchar(c)
            cat(sep="\t", c[!substr(c,n,n) %in% as.character(0:9) & n<7])
            cat("\n")
         }
         if(input %in% colors() | input=="") break else if(input != "colors()")
            cat(deparse(input), "not an available color.\n")
      }
      if(input != "") parameters <- if(parameters != "")
        paste(sep="", parameters, ", col.main=", deparse(input)) else
        paste(sep="",  "col.main=", deparse(input))
      if(input != "") {
         .assist.update("plot", parameters, FINAL=FALSE)
         cat("\n")
      }
   }

   cat("The command you require is:\n")
   .assist.update("plot", parameters, FINAL=TRUE)
   return(invisible(NULL))
}

