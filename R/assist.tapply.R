"assist.tapply" <-
function(up=0)
{
   .clearScreen()
   assign(".comand", "assist.tapply()", envir=.GlobalEnv)
   envir=parent.frame(up+1)
   call=sys.call(-up)
   cat("The goal in 'tapply' is to apply a function to groups of\n")
   cat("entries in Vector 1, when grouped by repeating values that\n")
   cat("occur in Vector 2. You then choose what function to apply\n")
   cat("to each group, such as 'mean' or a function of your own.\n")
   cat("\n")

   if(up > 1) {
      choice <- .choose(choice="to Proceed", select="", default="to Return", not=".")
      if(!choice) {
         .clearScreen()
         eval(expression(RETURN <- TRUE), envir=parent.frame(up+1))
         return()
      } else eval(expression(RETURN <- FALSE), envir=parent.frame(up+1))
   }

   l <- as.character(eval(expression(ls()), envir=parent.frame(up+1)))
   if(length(l) == 0) cat("Local variables: No local valiables found.\n") else
      .match.varname("a", envir=parent.frame(up+1))
   input <- list(expr=".")
   input <- .getexpr("tapply", prompt="Enter Vector 1:", envir=envir,
                       call=call, default="To Quit", SHOW.VARS=FALSE)
   if(input$expr == ".") { cat("\n"); return() }
   parameters <- input$expr
   len1 <- length(input$value)
   parameters <- input$expr
   .assist.update("tapply", parameters, FINAL=FALSE)
   cat("tapply: Arranging", len1, "entries into groups as given by,\n\n")

   input <- list(expr=".")
   repeat{
      choice <- TRUE
      input <- .getexpr("tapply", prompt="Enter Vector 2:", envir=envir,
                          call=call, default="To Quit", SHOW.VARS=FALSE)
      if(input$expr == ".") { cat("\n"); return() }
      if(input$expr == ".") { cat("\n"); return() }
      if(length(input$value) != len1) {
         cat("Caution: Vectors not of equal length.\n")
         choice <- .choose(choice="to continue anyway", select=1,
                       not=0, default="to try again")
      }
      if(choice & input$expr != ".") break else cat("\n")
   }
   parameters <- paste(sep="", parameters, ", ", input$expr)
   .assist.update("tapply", parameters, FINAL=FALSE)

   repeat{
      input <- readline(prompt=
                "Enter function to be applied (or '.' to Quit): ")
      if(input == ".") { cat("\n"); return() }
      FUNCTION <- FALSE
      withCallingHandlers(tryCatch(
         FUNCTION <- is.function(get(input, envir=parent.frame(up+1))),
         error=function(e) .error(e)), warning=function(w) .warning(w)
      )
      if(FUNCTION) break else cat(sep="", "assist.sapply: \"",
                 input, "\" not a valid function.\n")
   }
   parameters <- paste(sep="", parameters, ", ", input)
   .assist.update("sapply", parameters, FINAL=FALSE)

   cat("The command you require is:\n")
   .assist.update("tapply", parameters, FINAL=TRUE)
}

