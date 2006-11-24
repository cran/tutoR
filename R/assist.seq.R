"assist.seq" <-
function(up=0)
{
   l <- eval(expression(ls()), envir=parent.frame(up+1))
   assign(".comand", "assist.seq()", envir=.GlobalEnv)
   call <- sys.call(-up)
   envir=parent.frame(up+1)
   from <- to <- by <- length.out <- 0
   from <- .getexpr(prompt="Enter start value:", envir=envir,
                     call=call, SHOW.VARS=FALSE, default="to Exit")$expr
   if(from == ".") { cat("\n"); return() }
   parameters <- paste(sep="", from)
   .assist.update("seq", paste(sep="", "from=", parameters), FINAL=FALSE)

   if(from == "1") cat("'from=1' is the default and can be omitted.\n")
   choice <- .menu(c("A sequence not exceeding a final value",
                     "A sequence by given increments",
                     "A sequence of a given length",
                     "Take the same length as another variable"
                    ), not="", default="to Exit")
   if(choice == 0) { cat("\n"); return() }
   switch(choice,
   {
      to <- .getexpr(prompt="Enter final value:", envir=envir, call=call, default="to Exit")$expr
      if(to == ".") return()

   cat("\n")

      parameters <- paste(sep="", parameters, ", ", to)
      .assist.update("seq", parameters, FINAL=FALSE)
      choice <- .menu(c("A sequence by given increments",
                        "A sequence of a given length",
                        "Take the same length of another variable"
                       ), not="", default="to Exit")
      switch(choice,
      {
         by <- .getexpr(prompt="Enter increment value:", envir=envir,
                 call=call, SHOW.VARS=FALSE, default="to Exit")$expr
         if(by == ".") return()
         parameters <- paste(sep="", parameters, ", by=", by)
         .assist.update("seq", parameters, FINAL=FALSE)
      },
      {
         out <- .getexpr(prompt="Sequence Length value:", envir=envir,
                     call=call, default="to Exit")$expr
         if(out == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", length.out=", out)
         .assist.update("seq", parameters, FINAL=FALSE)
      },
      {
         with <- .getexpr(prompt="Use length of:", envir=envir,
                     call=call, SHOW.VARS=TRUE, default="to Exit")$expr
         if(with == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", along.with=", with)
         .assist.update("seq", parameters, FINAL=FALSE)
      })
   },
   {
         by <- .getexpr(prompt="Enter increment value:", envir=envir,
                     call=call, SHOW.VARS=FALSE,
                     default="to Exit")$expr
      if(by == ".") { cat("\n"); return() }

   cat("\n")

      parameters <- paste(sep="", "from=", parameters, ", by=", by)
      .assist.update("seq", parameters, FINAL=FALSE)
      choice <- .menu(c("A sequence not exceeding a final value",
                       "A sequence of a given length",
                       "Take the same length of another variable"
                       ), not="", default="to Exit")
      if(choice == 0) return()
      switch(choice,
      {
         to <- .getexpr(prompt="Enter final value:", envir=envir,
                     call=call, default="to Exit")$expr
         if(to == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", to=", to)
         .assist.update("seq", parameters, FINAL=FALSE)
      },
      {
         out <- .getexpr(prompt="Sequence Length value:", envir=envir,
                     call=call, default="to Exit")$expr
         if(out == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", length.out=", out)
         .assist.update("seq", parameters, FINAL=FALSE)
      },
      {
         with <- .getexpr(prompt="Use length of:", envir=envir,
                     call=call, SHOW.VARS=TRUE, default="to Exit")$expr
         if(with == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", along.with=", with)
      })
   },
   {
      out <- .getexpr(prompt="Sequence Length value:", envir=envir,
                     call=call, default="to Exit")$expr
      if(out == ".") { cat("\n"); return() }

   cat("\n")

      parameters <- paste(sep="", "from=", parameters, ", length.out=", out)
      .assist.update("seq", parameters, FINAL=FALSE)
      choice <- .menu(c("A sequence not exceeding a final value",
                       "A sequence by given increments"
                       ), not="", default="to Exit")
      if(choice == 0) { cat("\n"); return() }
      switch(choice,
      {
         to <- .getexpr(prompt="Enter final value:", envir=envir,
                     call=call, default="to Exit")$expr
         if(to == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", to=", to)
         .assist.update("seq", parameters, FINAL=FALSE)
      },
      {
         by <- .getexpr(prompt="Enter increment value:", envir=envir, SHOW.VARS=FALSE,
                     call=call, default="to Exit")$expr
         if(by == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", by=", by)
         if(by == "1") cat("'by=1' is the default and can be omitted.\n")
      },
      {
         with <- .getexpr(prompt="Use length of:", envir=envir,
                     call=call, SHOW.VARS=TRUE, default="to Exit")$expr
         if(with == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", along.with=", with)
         .assist.update("seq", parameters, FINAL=FALSE)
      })
   },
   {
      with <- .getexpr(prompt="Use length of:", envir=envir,
                     call=call, SHOW.VARS=TRUE, default="to Exit")$expr
      if(with == ".") { cat("\n"); return() }

   cat("\n")

      parameters <- paste(sep="", "from=", parameters, ", along.with=", with)
      .assist.update("seq", parameters, FINAL=FALSE)
      choice <- .menu(c("A sequence not exceeding a final value",
                       "A sequence by given increments"
                       ), not="", default="to Exit")
      if(choice == 0) { cat("\n"); return() }
      switch(choice,
      {
         to <- .getexpr(prompt="Enter final value:", envir=envir,
                     call=call, default="to Exit")$expr
         if(to == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", to=", to)
      },
      {
         by <- .getexpr(prompt="Enter increment value:", envir=envir, SHOW.VARS=FALSE,
                     call=call, default="to Exit")$expr
         if(by == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", by=", by)
         if(by == "1") cat("'by=1' is the default and can be omitted.\n")
      },
      {
         out <- .getexpr(prompt="Sequence Length value:", envir=envir,
                     call=call, default="to Exit")$expr
         if(out == ".") { cat("\n"); return() }
         parameters <- paste(sep="", parameters, ", length.out=", out)
      })
   })

   cat("\n")

   cat("The command you require is:\n")
   .assist.update("seq", parameters, FINAL=TRUE)
   return(invisible(NULL))
}

