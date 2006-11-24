"assist.lapply" <-
function(up=0)
{
   envir=parent.frame(up+1)
   assign(".comand", "assist.lapply()", envir=.GlobalEnv)
   call <- sys.call(-up)
   .clearScreen()
   cat("The goal in 'lapply' is to take a list of objects\n")
   cat("and apply a function to each element in the list,\n")
   cat("such as 'mean' or a function of your own.\n")

   cat("\n")

   if(up > 1) {
      choice <- .choose(choice="to Proceed", select="", default="to Return", not=".")
      if(!choice) {
         .clearScreen()
         eval(expression(RETURN <- TRUE), envir=sys.parent())
         return()
      } else eval(expression(RETURN <- FALSE), envir=sys.parent())
   }

   l <- as.character(eval(expression(ls()), envir=sys.parent(up+1)))
   valid <- rep(FALSE, length(l))
   if(length(l) != 0) for(i in 1:length(l))
      if(!is.na(l[1])) valid[i] <- is.list(get(l[i], envir=sys.parent(up+1)))
   l <- l[valid]

   if(length(l) > 0) cat("Local lists found:") else
                     cat("Local lists: No local lists found.")
       
   repeat{
      input <- list(expr=".")
      input <- .getexpr(prompt="Enter a list:", default="To Quit",
                          envir=envir, call=call, l=l, SHOW.VARS=TRUE)
      if(input$expr == ".") { cat("\n"); return() }
      if(is.list(input$value)) break else
         cat(sep="", "\nlapply: \"", input$expr, "\" is not a list object\n")
   }
   m <- input$value
   if(input$expr == ".") { cat("\n"); return() }

   parameters <- input$expr
   .assist.update("lapply", parameters, FINAL=FALSE)

   repeat{
      input <- readline(prompt=
                "Enter function to be applied (or '.' to Quit): ")
      if(input == ".") { cat("\n"); return() }
      FUNCTION <- FALSE
      withCallingHandlers(tryCatch(
         FUNCTION <- is.function(get(input, envir=sys.parent(up+1))),
         error=function(e) .error(e)), warning=function(w) .warning(w)
      )
      if(FUNCTION) break else cat(sep="", "assist.lapply: \"",
                 input, "\" not a valid function.\n")
   }
   parameters <- paste(sep="", parameters, ", FUN=", input)

   cat("\n")

   cat("The command you require is:\n")
   .assist.update("lapply", parameters, FINAL=TRUE)
}

