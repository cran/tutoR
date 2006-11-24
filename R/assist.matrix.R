"assist.matrix" <-
function(up=0)
{
   envir=parent.frame(up+1)
   call <- sys.call(-up)
   assign(".comand", "assist.matrix()", envir=.GlobalEnv)
   byrow <- FALSE
   parameters <- ""
   choice <- .menu(c("Fill entries by row", "Fill by column (default)"),
                     inputs=c("r", "c"))
   if(choice == 1) byrow <- TRUE
   if(choice != 0) {
      parameters <- paste(sep="", parameters, "byrow=", byrow)
      .assist.update("matrix", parameters, FINAL=FALSE)
   }

   if(byrow) {
      ncol <- .get.posint(prompt= "Enter Number of Columns")
      if(!is.na(ncol)) { if(parameters != "")
         parameters <- paste(sep="", parameters, ", ncol=", ncol) else
         parameters <- paste(sep="", parameters, "ncol=", ncol)
         .assist.update("matrix", parameters, FINAL=FALSE)
         cat("\n")
      }
   } else {
      nrow <- .get.posint(prompt= "Enter Number of Rows")
      if(!is.na(nrow)) { if(parameters != "")
         parameters <- paste(sep="", parameters, ", nrow=", nrow) else
         parameters <- paste(sep="", parameters, "nrow=", nrow)
         .assist.update("matrix", parameters, FINAL=FALSE)
      }
   }

   cat("\n")
   choice <- .menu(c("Variable or Expression for data",
                     "Keyboard entry of data"), prompt="For data entry")
   switch(choice,
      { input <- .getexpr(envir=envir, call=call)
        if(input$expr != ".") {
          if(parameters != "")
            parameters <- paste(sep="", as.character(input$expr),
                           ", ", parameters) else
            parameters <- paste(sep="", as.character(input$expr))
        }
      },
      if(parameters != "")
          parameters <- paste(sep="", "scan(), ", parameters) else
          parameters <- paste(sep="", "scan()")
   )

   cat("\n")
   if(parameters != "") {
      cat("The command you require is:\n")
      .assist.update("matrix", parameters, FINAL=TRUE)
   }
   return(invisible(NULL))
}

