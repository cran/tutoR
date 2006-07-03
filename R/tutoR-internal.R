".abs" <-
function(x)
{
.Primitive("abs")(x)
}

".clean.html" <-
function(str)
{
   j <- 0
   out <- ""
   repeat{
      j <- j+1
      ch <- substr(str,j,j)
      if(ch == "") break
      if(ch == ";") ch <- ";\n"

      if(ch == "<") {
         if(substr(str,j+1,j+5) == "code>") {
            out <- paste(sep="", out, "'")
            j <- j+5
         } else
         if(substr(str,j+1,j+6) == "/code>") {
            out <- paste(sep="", out, "'")
            j <- j+6
         } else
         if(substr(str,j+1,j+3) == "tr ") {
            repeat{ j <- j+1; if(substr(str,j,j) == ">") break }
            out <- paste(sep="", out, "\n")
         } else
         if(substr(str,j+1,j+3) == "li>") {
            out <- paste(sep="", out, "\t*  ")
            j <- j+3
         } else
         if(substr(str,j+1,j+3) == "dd>") {
            out <- paste(sep="", out, ": ")
            j <- j+3
         } else
            repeat{ j <- j+1; if(substr(str,j,j) == ">") break }
      } else if(ch == "&") {
         if(substr(str,j+1,j+3) == "lt;") {
            out <- paste(sep="", out, "<")
            j <- j+3
         } else
         if(substr(str,j+1,j+3) == "gt;") {
            out <- paste(sep="", out, ">")
            j <- j+3
         } else
         if(substr(str,j+1,j+3) == "pi;") {
            out <- paste(sep="", out, "pi")
            j <- j+3
         } else
         if(substr(str,j+2,j+6) == "dquo;") {
            out <- paste(sep="", out, "\"")
            j <- j+6
         } else
         {}
      } else out <- paste(sep="", out, ch)
   }
   return(out)
}

".clearScreen" <-
function()
{
   for(i in 1:33) cat("\n")
}

".Control" <-
function()
{
   return(c(
      "Control", "for", "if", "repeat", "while"
   ))
}

".eg.html" <-
function(filename, tempfile, max.indent=7)
{
   txt <- readLines(filename)

   examples <- function(str) {
      FLAG <- FALSE; j <- 0
      repeat{ j<-j+1
         if(substr(str,j,j)=="E")
         if(substr(str,j+1,j+1)=="x")
         if(substr(str,j+2,j+7)=="amples") FLAG <- TRUE
         if(j > max.indent | FLAG 
            | substring(str,j,j)=="") break
      }
      return(FLAG)
   }

### EXTRACT Examples
   START <- STOP <- FALSE
   for(i in 1:(length(txt)-1)) 
   {
      old <- txt[i]
      if(examples(old)) { old <- txt[i <- i+1]; START <- TRUE }

      if(START & !STOP) {
         if(substr(old,1,6) == "</pre>") STOP <- TRUE else
         if(substr(old,1,5) != "<pre>") 
            cat(sep="", .clean.html(old), "\n", file=tempfile)
      } # END if(START & ..
   } # END for

   cat("\n", file=tempfile)
   cat("\n", file=tempfile)

### EXTRACT Description, Usage, Arguments etc.
   for(i in 8:(length(txt)-2))
   {
      old <- txt[i]
      new <- .clean.html(old)
      if(examples(new)) break

      n <- length(old)
      if(substr(old,1,4) == "<tr ") 
         cat(sep="", new, ": ", file=tempfile) else 
      if(old != "<td>" & old != "")
         cat(sep="", new, "\n", file=tempfile)
      if(substr(new,1,11) == "Description") 
         cat("============", file=tempfile) 
      if(substr(new,1,5) == "Usage") 
         cat("======", file=tempfile) 
      if(substr(new,1,9) == "Arguments") 
         cat("==========", file=tempfile) 
      if(substr(new,1,7) == "Details") 
         cat("========", file=tempfile) 
      if(substr(new,1,10) == "References") 
         cat("===========", file=tempfile) 
      if(substr(new,1,8) == "See Also")
         cat("=========\n", file=tempfile)
   }
   cat("\n", file=tempfile)
}

".exp" <-
function(x)
{
.Primitive("exp")(x)
}

".extra.examples" <-
function(funcname, outfile)
{
   if(funcname %in% c("Trig"))
      cat("\n\#\# Converting angles to be in degrees.",
          "\nasin(90*pi/180)",
          "\nasin(1)*180/pi", 
          file=outfile)
   if(funcname %in% c("Control"))
      cat("\nx <- rbinom(1, 100, 0.5)",
          "\nif(x < 40 || x >= 60) paste(x, \"is pretty extreme.\") else",
          "\nif(x > 45 && x <= 55) {",
          "\n   paste(x, \"is about what you'd expect.\")",
          "\n} else paste(x, \"is nothing special.\")",
          file=outfile)
   if(funcname %in% c("function"))
      cat("\n\nsqr <- function(x) return(x^2)", 
          file=outfile)
   if(funcname %in% c("plot"))  # IN CASE MISSING OR UNCLEAR
      cat("\nx <- seq(0, 10, by=0.5)",
          "\ny <- exp(-x)",
          "\nplot(x, y, xlab=\"My X\", ylab=\"My Y\",",
          "main=\"My line graph\", type=\"l\")",
          "\n",
          "\n\#\# 'cars' data loaded: data(cars)",
          file=outfile)
   if(funcname %in% c("write"))
      cat("\nm <- matrix(1:9, byrow=T, ncol=3)",
          "\nwrite(t(m), ncolumns=3, \"mymat\")",
          "\nunlink(\"mymat\") \# tidy up",
          "\nmymat <- matrix(scan(\"mymat\"), byrow=T, ncol=3)",
          "\n",
          file=outfile)
   if(funcname %in% c("matrix"))
      cat("\nm <- matrix(1:9, byrow=T, ncol=3)",
          "\nn <- matrix(rep(2,9), ncol=3)",
          "\nprint(m * n)             \# Elementwise multiplication",
          "\nprint(m %*% n)           \# Matrix multiplication", 
          "\n",
          "\nvec <- 1:3",
          "\nprint(vec)               \# Warning: 'vec' is a column",
          "\nv1 <- as.matrix(vec)",
          "\nprint(v1)                \# So 'v1' is a column matrix",
          "\nv2 <- as.matrix(t(vec))",
          "\nprint(v2)                \# But 'v2' is a row matrix",
          file=outfile)
   if(funcname %in% c("write", "source"))
      cat("\nmyfunc <-",
          "\nfunction(x, y) {",
          "\n\tif(y < x) {",
          "\n\t\ttemp <- x",
          "\n\t\tx <- y; y <- temp \n\t}",
          "\n\tANSWER <- 1",
          "\n\tfor(i in 0:(y-x)) ANSWER <- ANSWER * (y-i)",
          "\n\treturn(ANSWER)",
          "\n}",
          "\n\#\# Write function copy to file",
          "\nwrite(\"mycopy <-\", \"mycopy.R\")",
          "\nwrite(deparse(myfunc), append=T, \"mycopy.R\")",
          "\nunlink(\"mycopy\") \# tidy up",
          "\n\#\# Read function copy from file",
          "\nsource(\"mycopy.R\")",
          "\nmycopy",
          file=outfile)
}

".find.owner" <-
function(funcname)
{
   pkg <- NULL

   packages <- list.files(paste(sep="", R.home(), "/library"))

   for(i in 1:length(packages)) 
   {
      if(file.exists(paste(sep="", R.home(), "/library/", 
                  packages[i], "/help/", funcname)))
         pkg <- c(pkg, packages[i]) else
      if(file.exists(paste(sep="", R.home(), "/library/", 
                  packages[i], "/html/", funcname, ".html")))
         pkg <- c(pkg, packages[i])
   }

   return(pkg)
}

".function.exists" <-
function(funcname)
{
   pos <- 1
   REPEAT <- FOUND <- EXHAUSTED <- FALSE
   if(funcname %in% c(.Control(), .Trig(), .Log())) 
    FOUND <- TRUE else repeat{
      pos <- pos + 1
      if(EXHAUSTED) { if(REPEAT) break else REPEAT <- TRUE }
      if(length(ls(pos)) == 0) EXHAUSTED <- TRUE
      if(funcname %in% ls(pos)) FOUND <- TRUE
      if(funcname %in% ls(pos)) FOUND <- TRUE
      if(funcname %in% ls(pos)) FOUND <- TRUE
      if(FOUND) break
    }

   return(FOUND)
}

".getexpr" <-
function(x=NULL, up=0)
{
   input <- NULL
   repeat{
      OK <- TRUE
      expr <- parse(stdin(), 
        prompt="Enter new expression, variable or '.' to leave unchanged: ")
      input <- expr[[1]]
      if(!.is.num.expr(deparse(input))) {
         OK <- try(.var.exists("stdin", input, deparse(input), up=up+1))
      }
      if(OK) break
   }
   if(deparse(input) == ".") value <- x else {
      value <- try(eval(input, sys.parent(up+1)))
      lab <- try(deparse(input))
   }
   return(value)
}

".get.varnames" <-
function(expr)
{
   if(expr==".") return(".")

   math.term <- function(varname)
   {
      terms <- c("TRUE", "FALSE", "T", "F", "NULL", "NA")
      return(varname %in% terms)
   }

   str <- expr
   out <- ""
   index <- 1
   varfound <- FALSE
   n <- 0

   letter <- c(letters, LETTERS, "_")
   digit <- c("1","2","3","4","5","6","7","8","9","0", ".")
   symbol <- c(":","*","/","+","-","^","%", "(",")", 
                ",", "~", " ")

   repeat{ n <- n+1
           if(substr(str,n,n) == "") break

           if(substr(str,n,n) == "." & substr(str,n+1,n+1) %in% letter) {
              out[index] <- "."
              n <- n+1
           }

           if(substr(str,n,n) %in% letter) {
              repeat{  
                 out[index] <- 
                 paste(sep="", out[index], substr(str,n,n))
                 n <- n+1
                 if(!(substr(str,n,n) %in% c(letter, digit))) break
              }
              if(substr(str,n,n) != "(") 
              { varfound <- TRUE
                out <- c(out, "")
                index <- index+1
              } else out[index] <- ""
           } else
              if(!(substr(str,n,n) %in%
                      c(digit, symbol))) n <- n+1
   }

   index <- 0
   repeat{ index <- index+1
      if(index > length(out)) break
      if(out[index]=="") break
      if(math.term(out[index])) { out <- out[-index]; index <- index-1 }
   }

   return(out)
}

".is.num.expr" <-
function(str)
{
   MATH <- TRUE
   n <- 0

   letter <- c(letters, LETTERS, "_")
   digit <- c("1","2","3","4","5","6","7","8","9","0", ".")
   symbol <- c(":","*","/","+","-","^","%", "(",")", 
                ",", "[", "]", " ")

   repeat{ n <- n+1
           if(substr(str,n,n) == "") break

           if(substr(str,n,n) %in% letter) {
              repeat{ 
                      n <- n+1
                      if(!(substr(str,n,n) %in% letter)) break
              }
              if(substr(str,n,n) != "(") 
                 MATH <- FALSE
           } else if(!(substr(str,n,n) %in% c(digit, symbol)))  
                     MATH <- FALSE
   }

   return(MATH)
}

".Log.checkX" <-
function(fun, x, xexpr, up=1)
{
### VALIDATE x INPUT: ls() FOR PARENT DIRECTORY
   l <- as.character(eval(expression(ls()), sys.parent(up+1)))
   DONE <- FALSE
   HELP <- FALSE

   if(missing(x)) {
      .match.varname("a", up=up+1)
      cat(sep="", "For examples, Type: eg(", fun, ")\n")
      DONE <- TRUE
   } else {
      xvarnames <- .get.varnames(xexpr)
      for(i in 1:length(xvarnames))
      {
        if(xvarnames[i] != "") if( sum(l==xvarnames[i]) == 0 )
        { ### VARIABLE NOT FOUND
          cat(sep="","A variable \"", xvarnames[i], "\" not found.\n")
          .match.varname(xvarnames[i], up=up+1)
          HELP <- TRUE
          DONE <- TRUE
        } # END if
      } # END for
   }

   if(!DONE) {
      cat(sep="", fun, "(", xexpr, "): input taken as radians")
      if(sum(x == as.integer(x)) > 0 | sum(abs(x) > 5) > 0)
         cat(sep="", ", for degrees use sin(", xexpr, "*pi/180)")
      cat(".\n")
   }

   return(data.frame(cbind(DONE, HELP)))
}

".Log" <-
function()
{
   return(c(
      "Log", "log", "logb", "log1p", "log10", "log2", "exp", "expm1"
   ))
}

".match.varname" <-
function(varname, up=0)
{
  likeness <- function(str1, str2)
  {
    n <- 1; repeat{if(substr(str1,n,n)=="") break; n <- n+1}
    m <- 1; repeat{if(substr(str2,m,m)=="") break; m <- m+1}
    return( 3*(substr(str1,1,1)==substr(str2,1,1)) +
            (substr(str1,1,1)==substr(str2,1,1)) - abs(n-m) )
  }

  eval(expression(.temp<-0), sys.parent(up+1))
  l <- as.character(eval(expression(ls()), sys.parent(up+1)))

     keep <- valid <- rep(FALSE, length(l))

     is.numbers <- function(i) {
       eval(expression(.temp<-0), sys.parent(up+2))
       for(loop in 1:i)
         eval(expression(.temp<-.temp+1), 
                                   sys.parent(2+up))
       variable <- eval(expression(get(ls()[.temp])), 
                                   sys.parent(up+2))
       valid <- is.numeric(variable)
       return(valid==1)
     }

     if(length(l) > 0) {
        for(i in 1:length(l))  
           keep[i] <- (is.numbers(i) & l[i]!=".temp")
        eval(expression(rm(".temp")), sys.parent(1+up))

        l <- l[keep]
        nmatch <- NA * (1:length(l))
     }

     if(length(l)>0) {
         for(i in 1:length(l))
         nmatch[i] <- likeness(l[i], varname)

         cat("Local variables:\n\n")
         cat(sep="\t", c(l[nmatch >= 2], l[nmatch == 1], l[nmatch == 0],
                 l[nmatch < 0] ), "\n")
         cat("\n")
     }
}

".Math" <-
function()
{
   return(c(
      "Math", "sqrt", "abs"
   ))
}

".plot.checkXY" <-
function(x, y, xexpr, yexpr)
{
  up <- 1
  l <- as.character(eval(expression(ls()), sys.parent(up+1)))
  DONE <- FALSE

  if(missing(x) & missing(y)) {
    cat("No variables received in plot.\n")
    cat(sep="", "For examples, Type: eg(plot)\n")
    .match.varname("a", up=up+1)
    DONE <- TRUE
  } else { 

     if(substr(xexpr,1,1)=="\"") { #"
        cat("WARNING:", yexpr, "received where X-variable expected\n")
        DONE <- TRUE
     }
    if(.is.num.expr(xexpr)) {
      if(!missing(y)) cat("X-variable:", xexpr,"\n")
    } else { 

      if(!missing(y)) {
        if(!is.null(attr(x, "class"))) {
         cat("Plot of",xexpr,"\n")
         } else if(is.function(x)) {
            cat("Plot of", paste(sep="", xexpr, "(x)"), "vs x\n")
         } else cat("X-variable:",xexpr,"\n")
      }
      DONE <- !.var.exists("plot", x, xexpr, up=up+1)
    }

if(xexpr == "xavies") DONE <- TRUE

    if(!DONE) {
      if(missing(y)) {              

        if(!is.null(attr(x, "class"))) {
          if(is.data.frame(x))
             cat(sep="", "Plot for data.frame ", xexpr, "\n") else
          cat("Plot of", paste(xexpr), "\n")
        } else if(!is.function(x)) {
          if(!is.null(attr(x, "dim"))) {
             cat(sep="", "Plot for multidimensional ", xexpr, "\n")
          } else {
            cat("X-variable: Index\n")
            cat("Y-variable:", paste(xexpr), "\n")
          }
        } else 
        if(is.function(x)) {
          cat("Plot of", paste(sep="", xexpr, "(x)"), "vs x\n")
        }
      }

      if(!missing(y)) {

        if(!is.null(attr(x, "class")))
          cat(sep="", "CAUTION: ",paste(xexpr), 
                " not simple X-variable.\nAs a result, \"", yexpr, 
                "\" may be misinterpretted in plot.\n")

        if(substr(yexpr,1,1)=="\"") {
           cat("WARNING:", yexpr, "received where Y-variable expected\n")
           DONE <- TRUE
        }

        if(!DONE) {
          if(is.null(attr(x, "class")) & !is.function(x)) {
            cat("Y-variable:", yexpr,"\n") 
            DONE <- !.var.exists("plot", y, yexpr, up=up+1)
          }

          if(!is.null(attr(y, "class"))) {
            cat("CAUTION. Did you mean: plot(")
            cat(sep="", paste(yexpr), ")\n")
          }

          if(is.function(y)) {
            cat("CAUTION. Did you mean: plot(")
            cat(sep="", yexpr, ")\n")
          }
        }
      }
    }
  }

  return(!DONE)
}

".source" <-
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

".sqrt" <-
function(x)
{
.Primitive("sqrt")(x)
}

".Trig.checkX" <-
function(fun, x, xexpr, up=1)
{
### VALIDATE x INPUT: ls() FOR PARENT DIRECTORY
   l <- as.character(eval(expression(ls()), sys.parent(up+1)))
   DONE <- FALSE
   HELP <- FALSE

   if(missing(x)) {
      .match.varname("a", up=up+1)
      cat(sep="", "For examples, Type: eg(", fun, ")\n")
      DONE <- TRUE
   } else {
      xvarnames <- .get.varnames(xexpr)
      for(i in 1:length(xvarnames))
      {
        if(xvarnames[i] != "") if( sum(l==xvarnames[i]) == 0 )
        { ### VARIABLE NOT FOUND
          cat(sep="","A variable \"", xvarnames[i], "\" not found.\n")
          .match.varname(xvarnames[i], up=up+1)
          HELP <- TRUE
          DONE <- TRUE
        } # END if
      } # END for
   }

   if(!DONE) {
      cat(sep="", fun, "(", xexpr, "): input taken as radians")
      if(sum(x == as.integer(x)) > 0 | sum(abs(x) > 5) > 0)
         cat(sep="", ", for degrees use sin(", xexpr, "*pi/180)")
      cat(".\n")
   }

   return(data.frame(cbind(DONE, HELP)))
}

".Trig" <-
function()
{
   return(c(
      "Trig", "cos", "sin", "tan", "acos", "asin", "atan", "atan2"
   ))
}

".var.exists" <-
function(func, x, xexpr, up=1)
{
   if(xexpr==".") return(TRUE)

   predefined <- c(
      "sqrt", "pi", .Trig(), .Log()
   )
   l <- as.character(eval(expression(ls(all.names=TRUE)), sys.parent(up+1)))
   OK <- FOUND <- TRUE

   if(missing(x)) {
      cat("\n")
      cat(sep="", func, ": Input missing.\n")
      .match.varname("a", up=up+1)
      cat(sep="", "For examples, Type: eg(", func, ")\n")
      FOUND <- FALSE
      cat("\n")
   } else {
      xvarnames <- .get.varnames(xexpr)
      for(i in 1:length(xvarnames))
      {
        if(xvarnames[i] != "") OK <- exists(xvarnames[i])
        if(xvarnames[i] != "") 
           if(sum(l==xvarnames[i] | xvarnames[i] %in% predefined) == 0
               & !OK)
        {
          cat("\n")
          cat(sep="",func, ": Variable \"", xvarnames[i], "\" not found.\n")
          .match.varname(xvarnames[i], up=up+1)
          FOUND <- FALSE
        }
      }
   }
   return(FOUND)
}

