"assist.apply" <-
function(up=0)
{RETURN <- FALSE; repeat{
   parameters <- ""
   choice <- .menu(c("apply a function to rows or columns",
                     "sapply a function to each element of a list",
                     "tapply a function, grouping by a 2nd vector",
                     "lapply, like sapply but to return a list"),
                     inputs=c("a","s","t","l"),
                     prompt="Please confirm the appropriate apply",
                     default="to Exit")

   if(choice == 0) { cat("\n"); return() }

   CONTINUE <- FALSE
   switch(choice,
   {
      CONTINUE <- TRUE
   },
   {
      if(up == 1)
         eval(expression(funcname <- "sapply"), parent.frame(up+1))
      assist.sapply(up=up+2)
   },
   {
      if(up == 1)
         eval(expression(funcname <- "tapply"), parent.frame(up+1))
      assist.tapply(up=up+2)
   },
   {
      if(up == 1)
         eval(expression(funcname <- "lapply"), parent.frame(up+1))
      assist.lapply(up=up+2)
   }
   )
   if(CONTINUE) {
      .clearScreen()
      assign(".comand", "assist.apply()", envir=.GlobalEnv)
      envir <- parent.frame(up+1)
      call <- sys.call(-up)
      cat("The goal in 'apply' is to take a matrix of numbers and\n")
      cat("apply a function, such as 'mean' or a function your own.\n")
      cat("You choose if the function is applied to rows or columns.\n")
      cat("\n")
      choice <- .choose(choice="to Proceed", select="", default="to Return", not=".")
      if(!choice) {
         .clearScreen()
         RETURN <- TRUE
      } else {

      .match.varname("a", envir=parent.frame(up+1), SHOW.EMPTY=FALSE)
      input <- list(expr=".")
      input <- .getexpr(prompt="Enter a matrix", default="To Quit",
                          envir=envir, call=call, SHOW.VARS=FALSE)
      if(input$expr == ".") { cat("\n"); return() }
      m <- input$value
      if(input$expr == ".") { cat("\n"); return() }

      if(!is.null(attr(m,"dim"))) {
         cat(sep="", "'", input$expr, "' is a ", .dim(m), " array.\n")
         parameters <- input$expr
      } else if(!is.data.frame(m)) {
         cat(sep="", "Updating to: as.matrix(", input$expr, ")\n")
         choice <- .choose(not="0", default="to Quit")
         if(!choice) return()
         parameters <- paste(sep="", "as.matrix(", input$expr, ")")
      } else parameters <- input$expr

      .assist.update("apply", parameters, FINAL=FALSE)

      MARGIN <- 2
      MARGIN <- MARGIN - .choose(choice="to apply function to rows",
                                select="r", not="c", default="for columns")
      parameters <- paste(sep="", parameters, ", MARGIN=", MARGIN)
      .assist.update("apply", parameters, FINAL=FALSE)

      input <- ""

      repeat{
         input <- readline(prompt=
                   "Enter function to be applied (or '.' to Quit): ")
         if(input == ".") { cat("\n"); return() }
         FUNCTION <- FALSE
         withCallingHandlers(tryCatch(
            FUNCTION <- is.function(get(input, envir=parent.frame(up+1))),
            error=function(e) .error(e)),
            warning=function(w) .warning(w))
         if(FUNCTION) break else cat(sep="", "assist.apply: \"",
                 input, "\" not a valid function.\n")
      }

      parameters <- paste(sep="", parameters, ", ", input)
      cat("The command you require is:\n")
      .assist.update("apply", parameters, FINAL=TRUE)
   }}
if(!RETURN) break
}}

