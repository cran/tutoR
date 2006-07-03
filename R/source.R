"source" <-
function (file, local = FALSE, echo = verbose, print.eval = echo, 
    verbose = getOption("verbose"), prompt.echo = getOption("prompt"), 
    max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding")) 
{
    eval.with.vis <- function(expr, envir = parent.frame(), enclos = if (is.list(envir) || 
        is.pairlist(envir)) 
        parent.frame()
    else baseenv()) .Internal(eval.with.vis(expr, envir, enclos))
    envir <- if (local) 
        parent.frame()
    else .GlobalEnv
    if (!missing(echo)) {
        if (!is.logical(echo)) 
            stop("'echo' must be logical")
        if (!echo && verbose) {
            warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
            echo <- TRUE
        }
    }
    if (verbose) {
        cat("'envir' chosen:")
        print(envir)
    }
    ofile <- file
    from_file <- FALSE
    if (is.character(file)) {
        if (capabilities("iconv")) {
            if (identical(encoding, "unknown")) {
                enc <- utils::localeToCharset()
                encoding <- enc[length(enc)]
            }
            else enc <- encoding
            if (length(enc) > 1) {
                encoding <- NA
                owarn <- options("warn")
                options(warn = 2)
                for (e in enc) {
                  if (is.na(e)) 
                    next
                  zz <- file(file, encoding = e)
                  res <- try(readLines(zz), silent = TRUE)
                  close(zz)
                  if (!inherits(res, "try-error")) {
                    encoding <- e
                    break
                  }
                }
                options(owarn)
            }
            if (is.na(encoding)) 
                stop("unable to find a plausible encoding")
            if (verbose) 
                cat("encoding =", dQuote(encoding), "chosen\n")
        }
        if (file == "") 
            file <- stdin()
        else {
            file <- file(file, "r", encoding = encoding)
            on.exit(close(file))
            from_file <- TRUE
        }
    }
    Ne <- length(exprs <- .Internal(parse(file, n = -1, NULL, 
        "?")))
    if (from_file) {
        close(file)
        on.exit()
    }
    if (verbose) 
        cat("--> parsed", Ne, "expressions; now eval(.)ing them:\n")
    if (Ne == 0) 
        return(invisible())
    if (chdir) {
        if (is.character(ofile)) {
            isURL <- length(grep("^(ftp|http|file)://", ofile)) > 
                0
            if (isURL) 
                warning("'chdir = TRUE' makes no sense for a URL")
            if (!isURL && (path <- dirname(ofile)) != ".") {
                owd <- getwd()
                on.exit(setwd(owd), add = TRUE)
                setwd(path)
            }
        }
        else {
            warning("'chdir = TRUE' makes no sense for a connection")
        }
    }
    if (echo) {
        sd <- "\""
        nos <- "[^\"]*"
        oddsd <- paste("^", nos, sd, "(", nos, sd, nos, sd, ")*", 
            nos, "$", sep = "")
    }
    for (i in 1:Ne) {
        if (verbose) 
            cat("\n>>>> eval(expression_nr.", i, ")\n\t\t =================\n")
        ei <- exprs[i]
        if (echo) {
            dep <- substr(paste(deparse(ei, control = c("showAttributes", 
                "useSource")), collapse = "\n"), 12, 1e+06)
            nd <- nchar(dep, "chars") - 1
            do.trunc <- nd > max.deparse.length
            dep <- substr(dep, 1, if (do.trunc) 
                max.deparse.length
            else nd)
            cat("\n", prompt.echo, dep, if (do.trunc) 
                paste(if (length(grep(sd, dep)) && length(grep(oddsd, 
                  dep))) 
                  " ...\" ..."
                else " ....", "[TRUNCATED] "), "\n", sep = "")
        }
        yy <- eval.with.vis(ei, envir)
        i.symbol <- mode(ei[[1]]) == "name"
        if (!i.symbol) {
            curr.fun <- ei[[1]][[1]]
            if (verbose) {
                cat("curr.fun:")
                str(curr.fun)
            }
        }
        if (verbose >= 2) {
            cat(".... mode(ei[[1]])=", mode(ei[[1]]), "; paste(curr.fun)=")
            str(paste(curr.fun))
        }
        if (print.eval && yy$visible) 
            print(yy$value)
        if (verbose) 
            cat(" .. after ", sQuote(deparse(ei, control = c("showAttributes", 
                "useSource"))), "\n", sep = "")
    }
    invisible(yy)
}
