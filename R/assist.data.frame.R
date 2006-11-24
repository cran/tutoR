"assist.data.frame" <-
function(up = 0)
{
   envir=parent.frame(up+1)
   assign(".comand", "assist.data.frame()", envir=.GlobalEnv)
   call <- sys.call(-up)
   .clearScreen()
   cat("Typically, a data.frame is used to bind together a number\n")
   cat("of equal* length columns into a list/matrix-like structure.\n")
   cat("Each column may have a different type (numbers, strings etc.)\n")
   cat("\n")
   cat("Suppose we have: d <- data.frame(x=1:3, y=runif(3))\n")
   cat("You may treat d as a matrix: d[,2]\n")
   cat("Or as a list (equivalently): d$y\n")
   cat("\n")
   cat("Take for example a data.frame for car features:\n")
   cat("  cars <- function()\n")
   cat("  {\n")
   cat("     v <- \"car\"\n")
   cat("     mk <- c(\"Mazda\", \"Fiat\", \"Citroen\")\n")
   cat("     md <- c(\"121\", \"Punto\", \"Berlingo\")\n")
   cat("     p <- c(11490, 19990, 17990)\n")
   cat("     return(data.frame(vehicle=v, make=mk, model=md, price=p))\n")
   cat("  }\n")
   cat("*It is assumed: vehicle=c(\"car\", \"car\", \"car\").\n")

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
   index <- 0
   input <- list(expr=".")
   .match.varname("a", envir=parent.frame(up+1))
   nrows <- -Inf

   repeat{ index <- index+1
      input <- .getexpr(prompt=paste(sep="", "Object #", index, " Enter:"), 
                     envir=envir, SHOW.VARS=FALSE, default="if finished")
      if(input$expr == ".") { cat("\n"); break() }
      name <- readline(prompt="Enter a name for this or leave blank: ")
      if(name != "") name <- paste(sep="", name, "=")
      nrowsi <- length(attr(as.data.frame(input$value), "row.names"))
      if(nrowsi > nrows) {
         cat("\nData frame with", nrowsi, "rows: ")
         if(as.integer(nrowsi/nrows) != nrowsi/nrows) cat("\nCaution:",
            nrowsi, "not a multiple of", nrows, "rows.")
         nrows <- nrowsi 
      } else if(as.integer(nrows/nrowsi) != nrows/nrowsi) cat("\nCaution:",
            nrows, "not a multiple of", nrowsi, "rows.")

      if(parameters == "")
         parameters <- paste(sep="", name, input$expr) else
         parameters <- paste(sep="", parameters, ", ", name, input$expr)
      .assist.update("data.frame", parameters, FINAL=FALSE)
   }

   if(parameters != "") {
      choice <- .menu(c("Enter a vector of row names", 
                        "Enter each row names separately"),
                        not=".", default="to Skip")
   
      switch(choice,
      {
         input <- .getexpr(prompt=paste(nrows, "Row names:"), envir=envir)
         if(input$expr != ".")
            parameters <- paste(sep="", parameters, ", row.names=", input$expr)
      },
      {
         row.names <- rep("", nrows)
         parameters <- paste(sep="", parameters, ", row.names=",
                       deparse(row.names))
      })

      if(parameters != "") {
         cat("\n")
         cat("The command you require is:\n")
         .assist.update("data.frame", parameters, FINAL=TRUE)
      }
   }
   return(invisible(NULL))
}

