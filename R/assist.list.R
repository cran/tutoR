"assist.list" <-
function(up = 0)
{
   envir=parent.frame(up+1)
   assign(".comand", "assist.list()", envir=.GlobalEnv)
   call <- sys.call(-up)
   .clearScreen()
   cat("A list is used to bind together a number of different\n")
   cat("types of objects into one structure.\n")
   cat("Each object may have a different type (numbers, strings etc.)\n")
   cat("\n")
   cat("Take for example a list of car features:\n")
   cat("  cars <- function()\n")
   cat("  {\n")
   cat("     v <- \"car\"\n")
   cat("     mk <- c(\"Mazda\", \"Fiat\", \"Citroen\")\n")
   cat("     md <- c(\"121\", \"Punto\", \"Berlingo\")\n")
   cat("     p <- c(11490, 19990, 17990)\n")
   cat("     return(list(vehicle=v, make=mk, model=md, price=p))\n")
   cat("  }\n")
   cat("With: c <- cars()\n")
   cat("Then 'price' is given by: c$price\n")
   cat("Or (not like a matrix): c[[4]]\n")

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

   repeat{ index <- index+1
      input <- .getexpr(prompt=paste(sep="", "Object #", index, " Enter:"), 
                     envir=envir, call=call, SHOW.VARS=FALSE, default="if finished")
      if(input$expr == ".") { cat("\n"); break() }
      name <- readline(prompt="Enter a name for this or leave blank: ")
      if(name != "") name <- paste(sep="", name, "=")
      if(parameters == "")
         parameters <- paste(sep="", name, input$expr) else
         parameters <- paste(sep="", parameters, ", ", name, input$expr)
      .assist.update("data.frame", parameters, FINAL=FALSE)
   }

   if(parameters != "") {
      cat("\n")
      cat("The command you require is:\n")
      .assist.update("list", parameters, FINAL=TRUE)
   }
   return(invisible(NULL))
}

