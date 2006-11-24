"eval.string" <-
function (expr, envir = parent.frame(), enclos = if (is.list(envir) ||
    is.pairlist(envir)) parent.frame() else baseenv())
{
   if(missing(enclos)) enclose <- NULL
   if(!is.null(envir)) envir=sys.parent()
      assign(".comand", "eval.string(expr)", envir=.GlobalEnv)
   
   global <- ls(all.names=TRUE, envir=.GlobalEnv)
   local <- as.character(eval(expression(ls(all.names=TRUE)), envir=envir))

   global <- global[global != "enclos" & global != "envir"
                  & global != "global" & global != "expr"]
   for(i in 1:length(global))
      assign(global[i], get(global[i], envir=.GlobalEnv))
   for(i in 1:length(local))
      assign(local[i], get(local[i], envir=envir), envir=.GlobalEnv)

   output <- list(visible=FALSE)
   writeOK <- FALSE
   try({ tmp <- file(".eval.string.tempfile", "w+"); writeOK <- TRUE })
   if(writeOK) {
      cat(sep="", expr, "\n", file=tmp)
      output <- source(".eval.string.tempfile")
      unlink(".eval.string.tempfile")
   }

   variable.used <- .get.varnames(expr)
   for(i in 1:length(global)) if(!global[i] %in% variable.used)
      assign(global[i], get(global[i]), envir=.GlobalEnv)

   new <- ls(all.names=TRUE, envir=.GlobalEnv)
   duplicates <- ""
   for(i in 1:length(new)) if(!new[i] %in% global)
     if(substr(new[i],1,1) != ".")
       duplicates <- paste(sep="\t", duplicates, new[i])
   if(duplicates != "")
      cat("eval.string: Created the new objects,\n", duplicates, "\n")

   if(length(output) > 0) if(output$visible) return(output$value)
   invisible(NULL)
}

