.conflicts.OK <- TRUE

.First.lib <- function(...)
{
   cat(sep="", "\n",
        "  _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/\n",
        "  _/      Welcome to the tutoR Package    _/\n",
        "  _/  Type eg() for help getting started  _/\n",
        "  _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/\n",
        "\n")
}

abs <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("abs", x, xexpr, call=sys.call(), envir=parent.frame())

   input <- list(expr=".")
   if(!OK) input <- .getexpr("abs", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "abs(", input$expr, ")")) }

   .execute(.Primitive("abs"), x, call=sys.call(), envir=sys.parent(1))
}

acos <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("sqrt", x, xexpr, call=sys.call(), envir=parent.frame())
   input <- list(expr=".")

   if(OK) cat("acos: Angles given in radians.\n") else
      input <- .getexpr("acos", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "acos(", input$expr, ")")) }

   .execute(.Primitive("acos"), x, call=sys.call(), envir=sys.parent(1))
}

asin <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("sqrt", x, xexpr, call=sys.call(), envir=parent.frame())
   input <- list(expr=".")

   if(OK) cat("asin: Angles given in radians.\n") else
      input <- .getexpr("asin", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "asin(", input$expr, ")")) }

   .execute(.Primitive("asin"), x, call=sys.call(), envir=parent.frame())
}

as.matrix <-
function (x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("matrix", x, xexpr, call=sys.call(), envir=parent.frame())
   if(!missing(x)) if(OK) if(is.null(attr(x,"dim"))) if(length(x) > 1)
      cat(sep="", "as.matrix: Column matrix produced.\n")
   UseMethod("as.matrix")
}

atan <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("sqrt", x, xexpr, call=sys.call(), envir=parent.frame())
   input <- list(expr=".")

   if(OK) cat("atan: Angles given in radians.\n") else
      input <- .getexpr("atan", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "atan(", input$expr, ")")) }

   .execute(.Primitive("atan"), x, call=sys.call(), envir=parent.frame())
}

cos <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("cos", x, xexpr, envir=parent.frame(), call=sys.call())
   input <- list(expr=".")

   if(OK){
      cat("cos: Input taken as radians.\n")
      if(!is.na(x)) if(is.numeric(x))
        if(sum(x == as.integer(x)) > 0 | sum(abs(x) > 2*pi) > 0)
         cat(sep="", "If input degrees use: cos(", xexpr, " * pi/180)\n")
   } else input <- .getexpr("cos", call=sys.call(), parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "cos(", input$expr, ")")) }

   .execute(.Primitive("cos"), x, envir=sys.parent(1))
}

exp <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("exp", x, xexpr, envir=parent.frame(), call=sys.call())
   input <- list(expr=".")

   if(!OK) input <- .getexpr("exp", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "exp(", input$expr, ")")) }

   .execute(.Primitive("exp"), x, envir=sys.parent(1), call=sys.call())
}

format <-
function(x, digits=0, width=0, justify="right", ...)
{
   nms <- names(list(...))
   if("nsmall" %in% nms | "trim" %in% nms | "na.encode" %in% nms
    | "scientific" %in% nms | "na.encode" %in% nms
    | "big.mark" %in% nms | "small.mark" %in% nms
    | "big.interval" %in% nms | "small.interval" %in% nms
    | "decimal.mark" %in% nms) UseMethod("format")
    
   num=""
   withCallingHandlers(tryCatch(num <- as.character(round(x*10^digits)/10^digits),
      error=function(e) .error(e, sys.call())),
      warning=function(w) .warning(w, sys.call())
   )

   
   for(i in 1:length(num)) {
      n <- nchar(num[i])
      if(substr(num[i],n-digits,n-digits) != ".")
         try(num[i] <- format.default(as.numeric(x[i]), nsmall=digits), silent=TRUE)
      n <- nchar(num[i])
      if(n < width) {
         if(justify == "right") for(k in 1:(width-n))
            num[i] <- paste("", num[i])
         if(justify == "left") for(k in 1:(width-n))
            num[i] <- paste(num[i], "")
         if(justify == "centre") { for(k in 1:((width-n)/2))
            num[i] <- paste("", num[i], "")
            if((width-n)/2 != as.integer((width-n)/2))
               num[i] <- paste("", num[i])
         }
      }
   }
   return(num)
}

log <-
function(x, base=exp(1))
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("log", x, xexpr, envir=parent.frame(), call=sys.call())
   input <- list(expr=".")

   if(!OK) input <- .getexpr("log", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   cat("log: Using log base e.\n")
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "log(", input$expr, ")")) }
   if(is.numeric(x)) if(sum(x<0) > 0) cat("     NaNs produced by negative input\n")

   .execute(.Primitive("log"), x, envir=sys.parent(1))
}

log10 <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("log10", x, xexpr, envir=parent.frame(), call=sys.call())
   input <- list(expr=".")

   if(!OK) input <- .getexpr("log10", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "log10(", input$expr, ")")) }
   if(is.numeric(x)) if(sum(x<0) > 0) cat("log10: NaNs produced by negative input\n")

   .execute(.Primitive("log"), x, base=10, envir=sys.parent(1))
}

log2 <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("log2", x, xexpr, envir=parent.frame(), call=sys.call())
   input <- list(expr=".")

   if(!OK) input <- .getexpr("log2", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "log2(", input$expr, ")")) }
   if(is.numeric(x)) if(sum(x<0) > 0) cat("log2: NaNs produced by negative input\n")

   .execute(.Primitive("log"), x, base=2, envir=sys.parent(1))
}

matrix <-
function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
{
   dexpr <- deparse(substitute(data))
   OK <- .var.exists("matrix", data, dexpr, call=sys.call(), envir=parent.frame())
   if(OK) {
      if(!missing(data)) if(is.null(attr(data,"dim")))
       if(length(data) > 1 &missing(byrow) &missing(ncol) &missing(nrow)) 
         cat(sep="", "matrix: Column matrix produced.\n") else
      if(missing(byrow) & !(ncol==1 & nrow==1))
         cat("matrix: Filling by columns (byrow=FALSE).\n")
   }

   input <- list(expr=".")
   if(!OK) try(input <- .getexpr("matrix", prompt="Enter", call=sys.call(),
                                  envir=parent.frame(), SHOW.VARS=FALSE))
   if(input$expr == ".") .setcall(sys.call()) else {
      data <- input$value
      if(is.null(attr(data,"dim")))
       if(length(data) > 1) if(missing(byrow)) if(ncol*nrow==1)
         cat(sep="", "\nmatrix: Column matrix produced.\n")
      if(missing(byrow) & !(ncol==1 & nrow==1))
         cat("\nmatrix: Filling by columns (byrow=FALSE).\n")
   }

   withCallingHandlers(tryCatch( data <- as.vector(data),
      error=function(e) .error(e)),
      warning=function(w) .warning(w)
   )
   if (missing(nrow))
       try(nrow <- ceiling(length(data)/ncol), silent=TRUE)
   else if (missing(ncol))
       try(ncol <- ceiling(length(data)/nrow), silent=TRUE)
   OK <- FALSE
   try({ x <- .Internal(matrix(data, nrow, ncol, byrow))
         OK <- TRUE }, silent=TRUE)
   try(dimnames(x) <- dimnames, silent=TRUE)
   if(OK) x
}

sin <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("sin", x, xexpr, envir=parent.frame(), call=sys.call())
   input <- list(expr=".")

   if(OK){
      cat("sin: Input taken as radians.\n")
      if(!is.na(x)) if(is.numeric(x))
         if(sum(x == as.integer(x)) > 0 | sum(abs(x) > 2*pi) > 0)
         cat(sep="", "If input degrees use: sin(", xexpr, " * pi/180)\n")
   } else input <- .getexpr("sin", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "sin(", input$expr, ")")) }

   .execute(.Primitive("sin"), x, envir=sys.parent(1))
}

sqrt <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("sqrt", x, xexpr, envir=parent.frame(), call=sys.call())
   input <- list(expr=".")

   if(OK){
   } else input <- .getexpr("sqrt", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "sqrt(", input$expr, ")")) }
   if(is.numeric(x)) if(sum(x<0) > 0) cat("sqrt: NaNs produced by negative input\n")

   .execute(.Primitive("sqrt"), x, envir=sys.parent(1))
}

tan <-
function(x)
{
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("tan", x, xexpr, envir=parent.frame(), call=sys.call())
   input <- list(expr=".")

   if(OK){
      cat("tan: Input taken as radians.\n")
      if(!is.na(x)) if(is.numeric(x))
        if(sum(x == as.integer(x)) > 0 | sum(abs(x) > 2*pi) > 0)
         cat(sep="", "If input degrees use: tan(", xexpr, " * pi/180)\n")
   } else input <- .getexpr("tan", call=sys.call(), envir=parent.frame(), SHOW.VARS=FALSE)
   if(input$expr == ".") .setcall(sys.call()) else {
      x <- input$value
      .setcall(paste(sep="", "tan(", input$expr, ")")) }

   .execute(.Primitive("tan"), x, envir=sys.parent(1))
}

