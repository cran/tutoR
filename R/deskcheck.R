"deskcheck" <-
function(func, ...)
{
   funcname <- deparse(substitute(func))
   IN.ERROR <- FALSE

### GET PARENT DIRECTORY: VALID FUNCTIONS
   l <- eval(expression(ls()), sys.parent(1))
   valid <- rep(FALSE, length(l))

   if(missing(func)) {
      cat("deskcheck: Function not received.\n")
      IN.ERROR <- TRUE
   }

### PICK OUT VALID FUNCTIONS
   if(length(l)>0) { for(i in 1:length(l)) 
      valid[i] <- is.function( get(l[i], envir=sys.parent(1)) )
   } else { l <- NULL
         valid <- FALSE }

### CATCH: NO FUNCTIONS DEFINED
   if(length(l[valid]) == 0) {
      cat("No user-defined functions found.\n")
      IN.ERROR <- TRUE
   } else if(!funcname %in% l[valid] & !missing(func)) {
      cat(sep="", "\"", funcname, "\" not a user-defined function.\n")
      IN.ERROR <- TRUE
   }


   if(IN.ERROR) {
      cat("User-define functions are:\n");
      cat(sep="\t", l[valid], "\n")
      cat("For examples, Type: eg(deskcheck)\n")
   } else { ### PROCEED IF NOT IN ERROR
      deskcheck <- func 
      cat("\n")
      cat("Debugging will now commence line-by-line.  Please first verify,\n")
      cat("\n")
      cat(sep="", "Testing: ", deparse(substitute(func)), "(")
      args <- list(...)
      if(length(args) > 0) {
         for(i in 1:length(args)) cat(args[[i]], "\b, "); cat("\b\b)\n") 
      } else cat(")\n")
      repeat{
         expr <- parse(prompt="1:  Inputs OK, continue; Or  0:  To exit\nSelection: ")
         choice <- 0
         choice <- try(eval(expr))
         if(choice == 1) { 
            .Primitive("debug")(deskcheck)
            output <- eval(expression(deskcheck(...)))
            if(!is.null(output)) print(output)
            .Primitive("undebug")(deskcheck)
            break
         }
         if(choice == 0) { cat("For examples, Type: eg(deskcheck)\n")
                           break }
      }
   } 
}

