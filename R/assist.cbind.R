"assist.cbind" <-
function(up = 0)
{
   envir=parent.frame(up+1)
   call <- sys.call(-up)
   .clearScreen()
   cat("Typically, cbind is used to bind together a number\n")
   cat("of equal* length columns to form a matrix.\n")
   cat("For viewing, each column is given a name.\n")
   cat("Each entry is given the same type (number, string etc.)\n")
   cat("\n")
   cat("Suppose we have: d <- cbind(x=1:3, y=runif(3))\n")
   cat("Then you may treat d like a matrix: d[,2]\n")
   cat("\n")
   cat("Take for example features of 3 cars:\n")
   cat("  cars <- function()\n")
   cat("  {\n")
   cat("     v <- \"car\"\n")
   cat("     mk <- c(\"Mazda\", \"Fiat\", \"Citroen\")\n")
   cat("     md <- c(\"121\", \"Punto\", \"Berlingo\")\n")
   cat("     p <- c(11490, 19990, 17990)\n")
   cat("     return(cbind(vehicle=v, make=mk, model=md, price=p))\n")
   cat("  }\n")
   cat("*So here: vehicle=c(\"car\", \"car\", \"car\").\n")

   cat("\n")

   if(up > 1) {
      choice <- .choose(choice="to Proceed", select="", default="to Return", not=".")
      if(!choice) {
         .clearScreen()
         eval(expression(RETURN <- TRUE), envir=parent.frame(up+1))
         return()
      } else eval(expression(RETURN <- FALSE), envir=parent.frame(up+1))
   }

   parameters <- ""
   assign(".comand", "assist.cbind()", envir=.GlobalEnv)
   index <- 0
   input <- list(expr=".")
   .match.varname("a", envir=parent.frame(up+1))
   nrows <- -Inf

   repeat{ index <- index+1
      input <- .getexpr(prompt=paste(sep="", "Enter Object #", index, ":"), 
                     envir=envir, call=call, SHOW.VARS=FALSE, default="if finished")
      if(input$expr == ".") { cat("\n"); break() }
      name <- readline(prompt="Enter a name for this or leave blank: ")
      if(name != "") name <- paste(sep="", name, "=")
      nrowsi <- length(attr(as.data.frame(input$value), "row.names"))
      if(nrowsi > nrows) {
         cat("\nColumns with", nrowsi, "rows: ")
         if(as.integer(nrowsi/nrows) != nrowsi/nrows) cat("\nCAUTION",
            nrowsi, "not a multiple of", nrows, "rows.")
         nrows <- nrowsi 
      } else if(as.integer(nrows/nrowsi) != nrows/nrowsi) cat("\nCAUTION",
            nrows, "not a multiple of", nrowsi, "rows.")

      if(parameters == "")
         parameters <- paste(sep="", name, input$expr) else
         parameters <- paste(sep="", parameters, ", ", name, input$expr)
      .assist.update("cbind", parameters, FINAL=FALSE)
   }

   if(parameters != "") {
      cat("\n")
      cat("The command you require is:\n")
      .assist.update("cbind", parameters, FINAL=TRUE)
   }
   return(invisible(NULL))
}

