"deskcheck" <-
function(.function)
{
#  .setcall(sys.call())
   .funcname <- as.character(substitute(.function))
   if(substr(.funcname,1,1) == "\"") {
      .funcname <- .function
      withCallingHandlers(tryCatch( .function <- get(.funcname, sys.parent()),
         error=function(e) .error(e)), warning=function(w) .warning(w)
      )
   }
   .IN.ERROR <- FALSE

### GET PARENT DIRECTORY: VALID FUNCTIONS
   .l <- eval(expression(ls()), sys.parent())
   .valid <- rep(FALSE, length(.l))

   if(missing(.function)) {
      cat("deskcheck: Function not received.\n")
      .IN.ERROR <- TRUE
   }

### PICK OUT VALID USER FUNCTIONS
   if(length(.l)>0) { for(.i in 1:length(.l)) 
      .valid[.i] <- is.function( get(.l[.i], envir=sys.parent()) )
   } else { .l <- NULL
         .valid <- FALSE }

   .FUNCTION <- FALSE
   withCallingHandlers(tryCatch( .FUNCTION <- is.function(.function),
      error=function(e) .error(e)), warning=function(w) .warning(w)
   )

   if(!.FUNCTION) {
      cat("User-defined functions are:\n");
      cat(sep="\t", .l[.valid])
      if(.valid[1]) cat("\n")
   }
 
### CATCH: NO FUNCTIONS DEFINED
   if(length(.l[.valid]) == 0 &!.FUNCTION) {
      cat("No user-defined functions found.\n")
   } else if(!.funcname %in% .l[.valid] & !missing(.function)) {
      cat(sep="", "deskcheck: \"", .funcname, "\" is not a .valid function.\n")
   }


   if(!.FUNCTION) {
      cat("For examples, Type: eg(deskcheck)\n")
   } else {
      .contents <- deparse(.function)

      .clearScreen()

      cat("Function definition:\n\n")
      cat("\t"); cat(sep="\n\t", .contents, "")

      .pars <- names(formals(.function))
      .inputs <- ""
      if(length(.pars) > 0) {
         cat("Please enter values for required arguments:\n")
         cat("\n")
         .tmp <- "()"
         try(.tmp <- deparse(args(.function))[1], silent=TRUE)
         cat(sep="", " ", .funcname, substr(.tmp,10,nchar(.tmp)), "\n")
         cat("\n")

         .SHOW.VARS <- TRUE
         for(.i in 1: length(.pars)) if(.pars[.i] != "...") {
            .input <- list(expr=".")
            .input <- .getexpr(prompt=paste(sep="", "Enter ", .pars[.i], ":"),
                           envir=parent.frame(), call=sys.call(), SHOW.VARS=.SHOW.VARS)
            .SHOW.VARS <- FALSE
            if(.input$expr != ".") if(.inputs != "")
                  .inputs <- paste(sep="", .inputs, ", ", .pars[.i], "=", .input$expr) else
                  .inputs <- paste(sep="", .pars[.i], "=", .input$expr)
         } else {
            .dots <- readline(prompt=
                "Enter any inputs for '...', or leave blank: ")
            if(.dots != "") if(.inputs != "")
               .inputs <- paste(sep="", .inputs, ", ", .dots) else
               .inputs <- paste(sep="", .dots)
         }
      }

      .fn.call <- paste(sep="", .funcname, "(", .inputs, ")")

      cat("\n")
      cat("During debugging, press [ENTER] to continue.\n")
      cat("Or you may enter commands to display or assign\n")
      cat(" the value of any variable (or type Q to Quit)\n")
      cat("\n")
      .input <- readline(prompt=
       paste("Press [ENTER] to confirm syntax or Q to Quit:", .fn.call, ""))
      if(.input == "Q") return(invisible(NULL))

      .deskcheck <- function(e)
      {
         msg <- paste(e)
         n <- nchar(msg)
         i <- 1
         repeat{i <- i+1
                if(substr(msg,i,i) == ":") break }
         msg <- paste(sep="", "Error in ", .fn.call, " ", substr(msg,i,n))
         cat(msg)
         undebug(.function)
         stop()
      }

      .out <- NULL
      debug(.function)
      tryCatch(.out <- eval.string(paste(sep="", ".function(",
                 paste(sep=", ", .inputs), ")"), envir=sys.frame()),
                 error=function(e) .deskcheck(e))
      undebug(.function)
      if(!is.null(.out)) .out
   }
}

