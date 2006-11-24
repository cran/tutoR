".choose" <-
function(choice="to proceed", select="1", not="", default="\b")
{
   if(not == "." | "" %in% select) or <- "'.'" else
   if(not == "") or <- "leave blank" else
      or <- not
   count <- 0
   if(!"" %in% select) prompt <- paste(sep="", "Enter ", select, " ", choice,
                       " or ", or, " ", default, ": ") else
                    prompt <- paste(sep="", "Press [ENTER] ", choice,
                       " or ", or, " ", default, ": ")
   repeat{ count <- count+1
      input <- readline(prompt=prompt)
      if(input %in% select) return(TRUE)
      if(input %in% not) return(FALSE)
      if("" %in% select) if(substr(input,1,1) == ".")
         return(FALSE) else return(TRUE)
      if(count>10) return(FALSE)
   }
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

".assist.help" <-
function(up=0)
{
   select <- c("matrix", "plot", "par", "seq", "cbind",
                "data.frame", "apply, sapply etc.", "list", "rbind")
   input <- ""
   cat("\n")
   choice <- .menu(select)
   if(!choice) return(invisible(NULL))
   if(substr(select[choice],1,5) == "apply")
      cat(sep="", "\nPlease Type: assist(apply)\n") else
   cat(sep="", "\nPlease Type: assist(", select[choice], ")\n")
   invisible(NULL)
}

".assist.update" <-
function(funcname, parameters, FINAL=TRUE)
{
   cat(sep="", "\n ", funcname, "(")
   if(FINAL) cat(parameters) else {
      if(parameters == "") cat("...") else
         cat(sep="", parameters, ", ...")
   }
   cat(")\n\n")
}

".dim" <-
function(x)
{
   a <- paste(attr(x, "dim"))
   if(length(a) == 0) return(paste(length(x), "element"))
   if(length(a) == 2) return(paste(a[1], "X", a[2]))
}

".Examples" <-
function (topicname)
{
     verbose <- getOption("verbose")
       pager <- getOption("pager")
     package <- .packages()

    INDICES <- .find.package(package, verbose = verbose)
    file <- index.search(topicname, INDICES, "AnIndex", "help")
    if (verbose) 
        cat("                                           Help file name `", sub(".*/", "", 
          file), ".Rd'\n", sep = "")

    zfile <- zip.file.extract(file, "Rhelp.zip")
    if (file.exists(zfile)) {
      txt <- readLines(zfile)
      lngth <- length(txt)
      index.Eg <- 0
      repeat{
         index.Eg <- index.Eg + 1
         if(substr(txt[index.Eg],1,6) == "_\bE_\bx"
          | index.Eg == lngth) break
      }
      writeOK <- FALSE
      if(zfile == file) egfile <- ".eg_temp" else egfile <- zfile
      try({ tmp <- file(egfile, "w+"); writeOK <- TRUE })
      if(writeOK) {
         cat(sep="\n", txt[1:3], "", file=tmp)
         cat(sep="\n", txt[index.Eg:lngth], file=tmp)
         .extra.examples(topicname, tmp)
         cat(sep="\n", txt[4:(index.Eg-1)], file=tmp)
         close(tmp)
         file.show(egfile, title = paste("`", topicname, 
           "' help", sep = ""), delete.file = TRUE, pager = pager)
         # unlink(egfile)
      } else {
         file.show(zfile, title = paste("`", topic,
           "' help", sep = ""), delete.file = (zfile != file),
           pager = pager)
      }
    } else stop(paste("The help file for `", topicname, 
      "' is missing", sep = ""))
    return(invisible(NULL))
}

".error" <-
function (e, call=sys.call(-2))
{
# PRESERVE 'assist' SUB-COMMAND PLACED IN ".comand"
# OTHERWISE SET ".comand" BY ORIGINAL FUNCTION CALL WHEN SUPPLIED.
   ASSIST <- FALSE
   try(ASSIST <- (substr(.comand,1,6) == "assist"), silent=T)
   if(ASSIST) callfunc <- .comand else
      if(!missing(call)) if(!exists(".comand")) .comand <- deparse(call)

   msg <- paste(e)
   n <- nchar(msg)
   i <- 1
   repeat{i <- i+1
          if(substr(msg,i,i) == ":") break }
   error.msg <- substr(msg,i,n)
   msg <- paste(sep="", "Error in ", .comand, " ", error.msg)
   cat(msg)
   .deletecall()
   e <- simpleError("\rExecution halted")
   stop(e)
}

".execute" <-
function(f, ..., call, envir=.GlobalEnv)
{
   warn <- options()$warn; options(warn=-1)
   output <- "tutoR.QZXRJ&*UH"
   if(!missing(call))
   withCallingHandlers(tryCatch( output <- f(...),
      error=function(e) .error(e, call)), warning=function(w) .warning(w, call)
   ) else
   withCallingHandlers(tryCatch( output <- f(...),
      error=function(e) .error(e)), warning=function(w) .warning(w)
   )
   options(warn=warn)
   .deletecall()
   if(length(output) > 0) if(!is.na(output[1])) if(output[1] != "tutoR.QZXRJ&*UH")
      output else invisible(NULL) else output
}

".extra.examples" <-
function(topicname, outfile)
{
   if(topicname %in% c("Trig"))
      cat("\n     ## Converting angles to be in degrees.",
          "\n     asin(90*pi/180)",
          "\n     asin(1)*180/pi", 
          "\n",
        file=outfile)
   if(topicname %in% c("Control"))
      cat("\n     x <- rbinom(1, 100, 0.5)",
          "\n     if(x < 40 || x >= 60) cat(x, \"is pretty extreme.\\n\") else",
          "\n     if(x > 45 && x <= 55) {",
          "\n        if(x == 50) cat(x, \"is spot on 50.\\n\")",
          "\n        else {",
          "\n          cat(x, \"is close to expected.\\n\")",
          "\n          if(abs(x-50) != 1) cat(\"But not as close as 51.\\n\")",
          "\n        }",
          "\n     } else cat(x, \"is not special at all.\\n\")",
          "\n",
          file=outfile)
   if(topicname %in% c("function"))
      cat("\n     sqr <- function(x) return(x^2)", 
          "\n     sqr(13)\t# 13^3 = 169", 
          "\n",
          file=outfile)
   if(topicname %in% c("plot"))  # IN CASE MISSING OR UNCLEAR
      cat("\n     x <- seq(0, 10, by=0.5)",
          "\n     y <- exp(-x)",
          "\n     plot(x, y, xlab=\"My X\", ylab=\"My Y\",",
          "main=\"My line graph\", type=\"l\")",
          "\n",
          "\n     ## Note 'cars' data loaded by: data(cars)",
          "\n",
          file=outfile)
   if(topicname %in% c("write"))
      cat("\n     m <- matrix(1:9, byrow=TRUE, ncol=3)",
          "\n     write(t(m), ncolumns=3, \"mymat\")",
          "\n     unlink(\"mymat\") # tidy up",
          "\n     mymat <- matrix(scan(\"mymat\"), byrow=TRUE, ncol=3)",
          "\n",
          file=outfile)
   if(topicname %in% c("matrix"))
      cat("\n     m <- matrix(1:9, byrow=TRUE, ncol=3)",
          "\n     n <- matrix(rep(2,9), ncol=3)",
          "\n     print(m * n)             # Elementwise multiplication",
          "\n     print(m %*% n)           # Matrix multiplication", 
          "\n",
          "\n     vec <- 1:3",
          "\n     print(vec)               # Warning: 'vec' is a column",
          "\n     v1 <- as.matrix(vec)",
          "\n     print(v1)                # So 'v1' is a column matrix",
          "\n     v2 <- as.matrix(t(vec))",
          "\n     print(v2)                # But 'v2' is a row matrix",
          "\n",
          file=outfile)
   if(topicname %in% c("write", "source"))
      cat("\n     ## Create function myfunc to write to file \"mycopy.R\"",
          "\n     myfunc <-",
          "\n     function(x, y) {",
          "\n     \tif(y < x) {",
          "\n     \t\ttemp <- x",
          "\n     \t\tx <- y; y <- temp \n\t}",
          "\n     \tANSWER <- 1",
          "\n     \tfor(i in 0:(y-x)) ANSWER <- ANSWER * (y-i)",
          "\n     \treturn(ANSWER)",
          "\n     }",
          "\n",
          "\n     ## Write function copy to file",
          "\n     write(\"mycopy <-\", \"mycopy.R\")",
          "\n     write(deparse(myfunc), append=TRUE, \"mycopy.R\")",
          "\n     unlink(\"mycopy\") # tidy up",
          "\n",
          "\n     ## Read function copy back from file",
          "\n     source(\"mycopy.R\")",
          "\n     mycopy",
          "\n",
          file=outfile)
}

".find.owner" <-
function(topicname)
{
   lib.loc <- NULL
   type <- "help"
   verbose <- getOption("verbose")
   pkg <- NULL

   packages <- .packages(all.available=TRUE)

   for(i in 1:length(packages)) 
   {
      INDICES <- .find.package(packages[i], lib.loc, verbose = verbose)
      file <- index.search(topicname, INDICES, "AnIndex", type)
      zfile <- zip.file.extract(file, "Rhelp.zip")
      if(file.exists(zfile)) 
         pkg <- c(pkg, packages[i])
   }

   return(pkg)
}

".function.exists" <-
function(funcname)
{
   pos <- 1
   REPEAT <- FOUND <- EXHAUSTED <- FALSE
   if(exists(funcname, mode="function"))
   FOUND <- TRUE else if(funcname %in% c(.Control(), .Trig(), .Log())) 
   FOUND <- TRUE else if(funcname == "Arithmetic")
   FOUND <- TRUE else if(funcname == "Comparison")
   FOUND <- TRUE else if(funcname == "Extract")
   FOUND <- TRUE else if(funcname == "Logic")
   FOUND <- TRUE else if(funcname == "matmult")
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
function(callfunc="", call=sys.call(-1), envir,
            prompt="Enter a new", default="to Skip", l=NULL, SHOW.VARS=TRUE)
{
   expr <- value <- expr <- input <- NULL
   tries <- 0
    if(!missing(l))  { if(SHOW.VARS) cat(sep="\t", "", l); cat("\n") } else
   if(SHOW.VARS) .match.varname("a", SHOW.EMPTY=SHOW.VARS, envir=envir)
   SHOW.VARS <- FALSE
   repeat{ tries <- tries+1
      if(tries > 10) { input <- "."; break }
      OK <- TRUE
      expr <- expression(".")
      withCallingHandlers(tryCatch( expr <- parse(stdin(), prompt=paste(sep="",
              prompt, " expression, variable or '.' ", default, ": ")),
      error=function(e) .error(e, call)), warning=function(w) .warning(w, call)
      )
      input <- expr[[1]]
      if(.is.numexpr(deparse(input))) break else
         withCallingHandlers(tryCatch(OK <- .var.exists(callfunc, input,
           deparse(input), SHOW.EMPTY=FALSE, SHOW.AT.ALL=FALSE,
           envir=envir, call=call),
      error=function(e) .error(e, call)), warning=function(w) .warning(w, call)
      )
      if(OK) break
   }

   if(deparse(input) == ".") return(list(expr=".")) else {
      if(length(expr) == 0) expr <- "." else {
      value <- NA
      withCallingHandlers(tryCatch(value <- eval(input, envir=envir),
      error=function(e) .error(e, call)), warning=function(w) .warning(w, call)
      )
      expr <- deparse(expr)
      expr <- substr(expr,12,nchar(expr)-1)
   } }
   return(list(value=value, expr=expr))
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
   digit <- c("1","2","3","4","5","6","7","8","9","0", "e", ".")
   symbol <- c(":","*","/","+","-","^","%", "(",")", 
                ",", "~", " ")

   repeat{ n <- n+1
           if(substr(str,n,n) == "") break

           if(substr(str,n,n) == "\"")
              repeat{ n <- n+1
                 if(substr(str,n,n) == "\"" | substr(str,n,n) == "") break
              }

           if(substr(str,n,n) == "." & substr(str,n+1,n+1) %in% letter) {
              out[index] <- "."
              n <- n+1
           }

           if(substr(str,n,n) %in% letter) if(!(substr(str,n,n) == "e"
                          &substr(str,n-1,n-1) %in% c(digit)
                          &substr(str,n+1,n+1) %in% c(digit, "+", "-"))) {
              repeat{  
                 out[index] <- 
                 paste(sep="", out[index], substr(str,n,n))
                 n <- n+1
                 if(!(substr(str,n,n) %in% c(letter, digit, "$"))) break
              }

             ## REMOVE "<-" CONDITIONS, IF TO RETURN ALL VARIABLES USED.
              if(substr(str,n,n) != "(" & substr(str,n+1,n+1) != "("
               & substr(str,n,n+1) != "<-" & substr(str,n+1,n+2) != "<-"
               & substr(str,n+2,n+3) != "<-" & substr(str,n,n) != "="
               & substr(str,n+1,n+1) != "=" & substr(str,n+2,n+2) != "=")
              { varfound <- TRUE
                out <- c(out, "")
                index <- index+1
              } else out[index] <- ""
           } else if( !(substr(str,n,n) %in% c(digit, symbol))
                    ) n <- n+1
   }

   index <- 0
   repeat{ index <- index+1
      if(index > length(out)) break
      if(out[index]=="") break
      if(math.term(out[index])) { out <- out[-index]; index <- index-1 }
   }

   for(i in 1:length(out)) {
      j <- 0
      repeat{
         if(substr(out[i],j+1,j+1) == "$") break
         if(substr(out[i],j+1,j+1) == "") break
         j <- j+1
      }
      if(substr(out[i],j+1,j+1) == "$") out[i] <- substr(out[i],1,j)
   }

   return(out)
}

".is.numexpr" <-
function(expr)
{
   MATH <- TRUE
   n <- 0

   letter <- c(letters, LETTERS, "_")
   digit <- c("1","2","3","4","5","6","7","8","9","0", ".")
   symbol <- c(":","*","/","+","-","^","%", "(",")", 
                ",", "[", "]", " ")

   repeat{ n <- n+1
           if(substr(expr,n,n) == "") break

           if(substr(expr,n,n) %in% letter) if(!(substr(expr,n,n) == "e"
               &substr(expr,n-1,n-1) %in% c(digit)
               &substr(expr,n+1,n+1) %in% c(digit, "+", "-"))) {
              repeat{ 
                      n <- n+1
                      if(!substr(expr,n,n) %in% letter) {
                           break
                      }
              }
              if(substr(expr,n,n) != "(") 
                 MATH <- FALSE
           } else {
                    if(!(substr(expr,n,n) %in% c(digit, symbol)))
                      if(!(substr(expr,n,n) == "e"
                          &substr(expr,n-1,n-1) %in% c(digit)
                          &substr(expr,n+1,n+1) %in% c(digit, "+", "-"))){
                     MATH <- FALSE
                    }
                  }
   }

   return(MATH)
}

".Log" <-
function()
{
   return(c(
      "Log", "log", "logb", "log1p", "log10", "log2", "exp", "expm1"
   ))
}

".match.varname" <-
function(varname, envir=parent.frame(), SHOW.EMPTY=TRUE)
{
  l <- as.character(eval(expression(ls()), envir))

  keep <- valid <- rep(FALSE, length(l))

  is.num <- rep(NA, length(l))
  if(length(l) > 0) { for(i in 1:length(l))
     is.num[i] <- is.numeric(get(l[i], envir=envir))
  }
  l <- l[is.num]

  if(length(l) !=0) {
     cat("Local variables:\t")
     cat(sep="\t", l)
     cat("\n\n")
  } else if(SHOW.EMPTY) cat("Local variables: No local variables found.\n")
}

".Math" <-
function()
{
   return(c(
      "Math", "sqrt", "abs", "+", "-", "*", "/", "^", "%%"
   ))
}

".menu" <-
function (choices, prompt = "Please Select from the following", inputs,
           not="", none.selected=0, default="\b")
{
  nc <- length(choices)
  if(not == "") or <- "leave blank" else
  if(not == ".") or <- "'.'" else or <- not
  if(missing(inputs)) inputs <- 1:nc
  cat(sep="", prompt, ":\n")
  cat("\n")
  for(i in 1:nc)
     cat(sep="", inputs[i], ":   ", choices[i], "\n")
  cat("\n")
  inlist <- inputs[1]
  if(length(inputs) > 1) {
     sep <- " - "
     if(length(inputs) == 2) sep <- ", "
     if(.is.numexpr(deparse(substitute(inputs))))
        inlist <- paste(sep=sep, inlist, inputs[length(inputs)]) else
     for(i in 2:length(inputs))
        inlist <- paste(sep=", ", inlist, inputs[i])
  }
  repeat{
    prompt <- paste(sep="", "Select ", inlist,
            " or ", or, " ", default, ": ")
    input <- readline(prompt=prompt)
    if(input == not) return(none.selected)
    MATCH <- FALSE
    for(i in 1:nc)
     if(substr(input,1,1) == as.character(inputs[i])) MATCH <- TRUE
    if(MATCH) break
    cat("Enter one of:", inputs, "\n")
  }
  for(i in 1:nc) if(substr(input,1,1) == as.character(inputs[i])) return(i)
}

".get.posint" <-
function(prompt="Enter positive integer", none.selected=0, default="\b")
{
   .warn <- options("warn")
   options("warn"=-1)
   value <- 0
   input <- ""
   tries <- 0
   repeat{ tries <- tries+1
      if(tries > 10) { input <- "": break }
      input <- readline(prompt=paste(sep="", prompt,
                " (or leave blank ", default, "): "))
      if(input == "") break
      try(value <- as.double(input))
      if(!is.na(value)) if(value > 0)
        if(value == as.integer(value)) break
   }
   options(.warn)

   if(input == "") return(none.selected)
   return(value)
}

".plot.checkXY" <-
function(callfunc="plot", x, y, xexpr, yexpr, up=1, REPORT=TRUE)
{
  l <- as.character(eval(expression(ls()), sys.parent(up+1)))
  DONE <- FALSE

  if(missing(x) & missing(y)) {
    cat("No variables received in plot.\n")
    cat(sep="", "For assistance, Type: create(plot)\n")
    DONE <- TRUE
  } else { 

     if(substr(xexpr,1,1)=="\"") { #"
        cat("WARNING:", yexpr, "received where X-variable expected\n")
        DONE <- TRUE
     }
    if(.is.numexpr(xexpr)) {
      if(REPORT) if(!missing(y))
          cat("X-variable:", xexpr,"\n")
    } else { 

      if(!missing(y)) {
        if(!is.null(attr(x, "class"))) {
         cat(callfunc, "of",xexpr,"\n")
         } else if(is.function(x)) {
            cat(callfunc, "of", paste(sep="", xexpr, "(x)"), "vs x\n")
         } else if(REPORT) cat("X-variable:",xexpr,"\n")
      }
      DONE <- !.var.exists("plot", x, xexpr,
                        call=sys.call(-2), envir=parent.frame(2))
    }

    if(xexpr == "xavies") DONE <- TRUE

    if(!DONE) {
      if(missing(y)) {              

        if(!is.null(attr(x, "class"))) {
          if(is.data.frame(x))
             cat(sep="", callfunc, " for data.frame ", xexpr, "\n") else
          cat(callfunc, "of", paste(xexpr), "\n")
        } else if(!is.function(x)) {
          if(!is.null(attr(x, "dim"))) {
             cat(sep="", callfunc, " for multidimensional ", xexpr, "\n")
          } else if(REPORT) {
            cat("X-variable: Index\n")
            cat("Y-variable:", paste(xexpr), "\n")
          }
        } else 
        if(is.function(x)) {
          cat(callfunc, "of", paste(sep="", xexpr, "(x)"), "vs x\n")
        }
      }

      if(!missing(y)) {

        if(!is.null(attr(x, "class")))
          cat(sep="", "Caution: ",paste(xexpr), 
                " not simple X-variable.\nAs a result, \"", yexpr, 
                "\" may be misinterpretted.\n\n")

        if(substr(yexpr,1,1)=="\"") {
           cat("WARNING:", yexpr, "received where Y-variable expected\n")
           DONE <- TRUE
        }

        if(!DONE) {
          if(is.null(attr(x, "class")) & !is.function(x)) {
            DONE <- !.var.exists("plot", y, yexpr,
                        call=sys.call(-2), envir=parent.frame(2))
            if(REPORT) if(!DONE)cat("Y-variable:", yexpr,"\n") 
          }

          YCLASS <- !is.null(attr(y, "class"))
          if(YCLASS) {
            cat(sep="", "Caution. Did you mean: ", callfunc, "(")
            cat(sep="", paste(yexpr), ")\n\n")
          }

          YFUNC <- is.function(y)
          if(YFUNC) {
            cat(sep="", "Caution. Did you mean: ", callfunc, "(")
            cat(sep="", yexpr, ")\n")
          }
        }
      }
    }
  }

# return(!DONE)
}

".Reserved" <-
function()
return(c("if", "for", "function", "repeat", "while"))

".deletecall" <-
function()
rm(.comand, envir=.GlobalEnv)

".setcall" <-
function(call)
{
   cmd <- deparse(call)
   if(substr(cmd,1,1) == "\"") cmd <- call
   assign(".comand", cmd, envir=.GlobalEnv)
}

".Trig" <-
function()
{
   return(c(
      "Trig", "cos", "sin", "tan", "acos", "asin", "atan", "atan2"
   ))
}

".var.exists" <-
function(callfunc, .x, .xexpr, SHOW.EMPTY=TRUE,
            SHOW.AT.ALL=TRUE, envir=envir, call=sys.call(-1))
{
  if(.xexpr==".") return(TRUE)
  if(callfunc == "") callfunc <- deparse(call)

  predefined <- c(
     "sqrt", "pi", .Trig(), .Log()
  )
  l <- as.character(eval(expression(ls(all.names=TRUE)), envir=envir))
  OK <- FOUND <- TRUE
  not.found <- NULL

  if(missing(.x)) {
    cat(sep="", "Error in ", .comand, " : Input missing.\n")
    .match.varname("a", envir=envir)
    cat(sep="", "For examples, Type: eg(", callfunc, ")\n")
    FOUND <- FALSE
    cat("\n")
  } else {
    .xvarnames<- .get.varnames(.xexpr)
    for(i in 1:length(.xvarnames))
    {
      if(.xvarnames[i] != "") OK <- exists(.xvarnames[i])
      if(.xvarnames[i] != "") 
         if(sum(l==.xvarnames[i]
          | .xvarnames[i] %in% predefined) == 0 & !OK)
      {
        not.found <- c(not.found, .xvarnames[i])
        FOUND <- FALSE
      }
    }
  }

  if(!FOUND) {
    for(i in 1:length(not.found))
       cat(sep="", callfunc, ": \"", not.found[i], "\" not found\n")
    if(SHOW.AT.ALL)  if(length(not.found) == 1)
      .match.varname(.xvarnames[i],
        SHOW.EMPTY=SHOW.EMPTY, envir=envir) else
      .match.varname("a", SHOW.EMPTY=SHOW.EMPTY, envir=envir)
  }
  return(FOUND)
}

".warning" <-
function (w, call=sys.call(-2))
{
   if(!missing(call)) .comand <- deparse(call)
   msg <- paste(w)
   n <- nchar(msg)
   i <- 1
   repeat{i <- i+1
          if(substr(msg,i,i) == ":") break }
   msg=paste(sep="", "Warning in ", .comand, substr(msg,i,n))
   cat(msg)
}

