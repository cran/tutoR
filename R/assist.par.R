"assist.par" <-
function(up=0)
{
   warnings <- options("warn"); options("warn"=-1)
   assign(".comand", "assist.par()", envir=.GlobalEnv)

   parameters <- input <- ""

   MFROW <- FALSE
   nrows <- .get.posint(prompt="Enter number of graphs down the page")
   if(nrows == 0) nrows <- 1 else MFROW <- TRUE
   ncols <- .get.posint(prompt="Enter number of graphs across the page")
   if(ncols == 0) ncols <- 1 else MFROW <- TRUE
   if(!is.na(nrows) & !is.na(ncols)) {
      if(is.na(nrows)) nrows <- 1
      if(is.na(ncols)) ncols <- 1
      mfrow <- c(nrows,ncols)
      if(MFROW) if(parameters != "")
         parameters <- paste(sep="", parameters, ", mfrow=", deparse(mfrow)) else
         parameters <- paste(sep="", "mfrow=", deparse(mfrow))
   }
   if(parameters != "") .assist.update("par", parameters, FINAL=FALSE)

   TEXT <- .choose("For Text Options", c("T","t"))
   if(TEXT) {
      cat("\n\n")
      choice <- .menu(c("Halve size of all text on graphs",
                        "Double size of all text",
                        "Triple size of all text"))
      switch(choice,
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex=0.5") else
         paste(sep="",  "cex.lab=0.5"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex=2") else
         paste(sep="",  "cex.lab=2"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex=3") else
         paste(sep="",  "cex.lab=3"),
      )
      if(choice != 0) .assist.update("par", parameters, FINAL=FALSE)

      cat("\n")
      choice <- .menu(c("Halve size of Axis Label",
                        "Double size Axis Label",
                        "Triple size Axis Label"))
      switch(choice,
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex.lab=0.5") else
         paste(sep="",  "cex.lab=0.5"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex.lab=2") else
         paste(sep="",  "cex.lab=2"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex.lab=3") else
         paste(sep="",  "cex.lab=3"),
      )
      if(parameters != "") .assist.update("par", parameters, FINAL=FALSE)

      cat("\n")
      choice <- .menu(c("Halve size of Axis Tick labels",
                        "Double size Tick labels",
                        "Triple size Tick labels"))
      switch(choice,
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex.axis=0.5") else
         paste(sep="",  "cex.axis=0.5"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex.axis=2") else
         paste(sep="",  "cex.axis=2"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex.axis=3") else
         paste(sep="",  "cex.axis=3"),
      )
      if(parameters != "") .assist.update("par", parameters, FINAL=FALSE)

      cat("\n")
      choice <- .menu(c("Halve size of Title",
                        "Double size Title",
                        "Triple size Title"))
      switch(choice,
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex.main=0.5") else
         paste(sep="",  "cex.main=0.5"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex.main=2") else
         paste(sep="",  "cex.main=2"),
        parameters <- if(parameters != "")
         paste(sep="", parameters, ", cex.main=3") else
         paste(sep="",  "cex.main=3"),
      )
      if(parameters != "") .assist.update("par", parameters, FINAL=FALSE)

      cat("Justify text:")
      choice <- .menu(c("Left-justified text", "Centered text", "Right-justified text"))
      if(choice !=0) if(parameters != "")
         parameters <- paste(sep="", parameters, ", adj=", 0.5*(choice-1)) else
         parameters <- paste(sep="", "adj=", 0.5*(choice-1))
      if(parameters != "") .assist.update("par", parameters, FINAL=FALSE)
      cat("\n")
   } # END TEXT

   COLORS <- .choose("For Color Options", c("C","c"))
   if(COLORS) {
      cat("\n")
      cat("For some available colors, Type: colors()\n")
      cat("Or try: blue, red, green, brown, purple, gold, turquoise etc.\n")

      repeat{
         input <- readline(prompt =
           "Enter a background color or leave blank to keep default: ")
         if(input == "colors()") {
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
        paste(sep="", parameters, ", bg=", deparse(input)) else
        paste(sep="",  "bg=", deparse(input))
      if(input != "") .assist.update("par", parameters, FINAL=FALSE)

      repeat{
         input <- readline(prompt =
           "Enter Axis color or leave blank to keep default: ")
         if(input == "colors()") {
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
        paste(sep="", parameters, ", col.axis=", deparse(input)) else
        paste(sep="",  "col.axis=", deparse(input))
      if(input != "") .assist.update("par", parameters, FINAL=FALSE)

      repeat{
         input <- readline(prompt =
           "Enter Axis Label color or leave blank to keep default: ")
         if(input == "colors()") {
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
        paste(sep="", parameters, ", col.lab=", deparse(input)) else
        paste(sep="",  "col.lab=", deparse(input))
      if(input != "") .assist.update("par", parameters, FINAL=FALSE)

      repeat{
         input <- readline(prompt =
           "Enter Title color or leave blank to keep default: ")
         if(input == "colors()") {
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
        paste(sep="", parameters, ", col.main=", deparse(input)) else
        paste(sep="",  "col.main=", deparse(input))
      if(input != "") .assist.update("par", parameters, FINAL=FALSE)
   } # END COLORS

   cat("\n")
   choice <- .menu(c("Always Parallel to axis (default)", "Always Horizontal",
                    "Always Perpendicular to axis", "Always Vertical"),
                   prompt="Please select Axis Label orientation",
                   inputs=1:4)
   switch(choice,
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", las=0") else
      paste(sep="",  "las=0"),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", las=1") else
      paste(sep="",  "las=1"),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", las=2") else
      paste(sep="",  "las=2"),
     parameters <- if(parameters != "")
      paste(sep="", parameters, ", las=3") else
      paste(sep="",  "las=3"),
   )
   if(parameters != "") .assist.update("par", parameters, FINAL=FALSE)

   choice <- .menu(c("to Prompt before new plots",
                     "Don't prompt (default)"), inputs=1:0)
   if(choice) if(parameters != "")
      parameters <- paste(sep="", parameters, ", ask=", 2-choice) else
      parameters <- paste(sep="", "ask=", 2-choice)

   cat("\n")
   if(parameters != "") {
      cat("The command you require is:\n")
      .assist.update("par", parameters, FINAL=TRUE)
   }

   options(warnings)

   return(invisible(NULL))
}

