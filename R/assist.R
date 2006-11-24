"assist" <-
function(func)
{
   assign(".comand", paste(sep="", "assist(", deparse(substitute(func)), ")"),
              envir=.GlobalEnv)
   if(!missing(func)){
      funcname <- as.character(substitute(func))

      if(!funcname %in% c("combine")) try({
         cat("\n\n")
         cat("We are given the following syntax:\n")
         cat("\n")
         inputs <- deparse(args(funcname))[1]
         cat(sep="", " ", funcname, substr(inputs,10,nchar(inputs)), "\n\n")
      })

      if(funcname %in% c("apply", "sapply", "tapply", "lapply"))
         assist.apply(up=1)
      if(funcname %in% c("combine", "data.frame", "list", "cbind", "rbind"))
         assist.combine(up=1)
      if(funcname == "matrix") assist.matrix(up=1)
      if(funcname == "par") assist.par(up=1)
      if(funcname == "plot") assist.plot(up=1)
      if(funcname == "seq") assist.seq(up=1)

      if(funcname %in% .Reserved())
         funcname <- paste(sep="", "\"", funcname, "\"")
      cat(sep="", "For Examples, Type: eg(", funcname, ")\n\n")
   } else .assist.help(up=1)

   return(invisible(NULL))
}

