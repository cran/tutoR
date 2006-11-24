"retrieve" <-
function(x)
{
   funcname <- deparse(substitute(x))
   if(substr(funcname,1,1) == "\"") funcname=x


   if(funcname %in% .Control()) funcname <- "Control"
   if(funcname %in% .Log()) funcname <- "Log"
   if(funcname %in% .Trig()) funcname <- "Trig"
   if(funcname == "tutoR") funcname <- "tutoR.package"

   if(.function.exists(funcname) & length(.find.owner(funcname)) > 0) {
      pkg <- .find.owner(funcname)
      if(length(pkg) > 1) {
         cat(sep="", "Note: '", funcname, "' may differ with: ")
         cat(sep="", "retrieve(", funcname, ")[1]; retrieve(", 
                                  funcname, ")[2] and so on.\n")
      }
   } else {
      pkg <- .find.owner(funcname)
      if(length(pkg) > 1) pkg <- pkg[1]
      if(!is.null(pkg)) {
         choice <- .choose(paste(sep="", "to Load the required '",
                     pkg, "' package"), not=".", select=c("L", "l"),
                     default="to Skip")
         if(choice) library(pkg, character.only = TRUE)
      }
   }

    x <- as.character(substitute(x))
    objs <- list()
    where <- character(0)
    visible <- logical(0)
    if (length(pos <- find(x, numeric = TRUE))) {
        objs <- lapply(pos, function(pos, x) get(x, pos = pos),
            x = x)
        where <- names(pos)
        visible <- rep.int(TRUE, length(pos))
    }
    if (length(grep(".", x, fixed = TRUE))) {
        np <- length(parts <- strsplit(x, ".", fixed = TRUE)[[1]])
        for (i in 2:np) {
            gen <- paste(parts[1:(i - 1)], collapse = ".")
            cl <- paste(parts[i:np], collapse = ".")
            if (gen == "" || cl == "")
                next
            if (!is.null(f <- getS3method(gen, cl, TRUE))) {
                ev <- topenv(environment(f), baseenv())
                nmev <- if (isNamespace(ev))
                  getNamespaceName(ev)
                else NULL
                objs <- c(objs, f)
                msg <- paste("registered S3 method for", gen)
                if (!is.null(nmev))
                  msg <- paste(msg, "from namespace", nmev)
                where <- c(where, msg)
                visible <- c(visible, FALSE)
            }
        }
    }
    ln <- length(objs)
    dups <- rep.int(FALSE, ln)
    objs2 <- lapply(objs, function(x) {
        if (is.function(x))
            environment(x) <- baseenv()
        x
    })
    if (ln > 1)
        for (i in 2:ln) for (j in 1:(i - 1)) if (identical(objs2[[i]],
            objs2[[j]])) {
            dups[i] <- TRUE
            break
        }
    res <- list(name = x, objs = objs, where = where, visible = visible,
        dups = dups)
    class(res) <- "getAnywhere"
    res
}

