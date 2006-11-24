"assist.sapply" <-
function(up=0)
{
   .clearScreen()
   assign(".comand", "assist.sapply()", envir=.GlobalEnv)
   envir=parent.frame(up+1)

   cat("The goal in 'sapply' is to take a list of objects\n")
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
      input <- .getexpr(prompt="Enter a list", default="To Quit",
                          envir=envir, l=l, SHOW.VARS=TRUE)
      if(is.list(input$value)) break else
         cat(sep="", "\nlapply: \"", input$expr, "\" is not a list object\n")
      if(input$expr == ".") { cat("\n"); return() }
      if(is.list(input$value)) break
   }
   m <- input$value
   if(input$expr == ".") { cat("\n"); return() }

   parameters <- input$expr
   .assist.update("sapply", parameters, FINAL=FALSE)

   repeat{
      input <- readline(prompt=
                "Enter function to be applied (or '.' to Quit): ")
      if(input == ".") { cat("\n"); return() }
      FUNCTION <- FALSE
      withCallingHandlers(tryCatch(
         FUNCTION <- is.function(get(input, envir=sys.parent(up+1))),
         error=function(e) .error(e)), warning=function(w) .warning(w)
      )
      if(FUNCTION) break else cat(sep="", "assist.sapply: \"",
                 input, "\" not a valid function.\n")
   }
   parameters <- paste(sep="", parameters, ", FUN=", input)

   cat("The command you require is:\n")
   .assist.update("sapply", parameters, FINAL=TRUE)
}

