
.First.lib <- function(libname, pkgname) {
cat("################################################\n")
cat("##       Welcome to the R assist Package      ##\n")
cat("## For examples to help start off, Type: eg() ##\n")
cat("################################################\n")
}
abs <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("abs", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }

   return(.abs(x))
}

sin <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("sin", x, xexpr, up=1)
   if(!OK) x <- { x <- NA
                  x <- try(.getexpr(x, up=1)) }
   if(OK){
      cat(sep="", "sin: input taken as radians")
      if(sum(x == as.integer(x)) > 0 | sum(abs(x) > 2*pi) > 0)
         cat(sep="", ", for degrees use: sin(", xexpr, "*pi/180)")
      cat("\n")
   }

   return(.Primitive("sin")(x))
}

cos <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("cos", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- .getexpr(x, up=1) }
   if(OK){
      cat(sep="", "cos(", xexpr, "): input taken as radians")
      if(sum(x == as.integer(x)) > 0 | sum(abs(x) > 2*pi) > 0)
         cat(sep="", ", for degrees use: cos(", xexpr, "*pi/180)")
      cat(".\n")
   }

   return(.Primitive("cos")(x))
}

tan <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("cos", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }
   if(OK){
      cat(sep="", "tan(", xexpr, "): input taken as radians")
      if(sum(x == as.integer(x)) > 0 | sum(abs(x) > 2*pi) > 0)
         cat(sep="", ", for degrees use: tan(", xexpr, "*pi/180)")
      cat(".\n")
   }

   return(.Primitive("tan")(x))
}

acos <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("acos", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }
   if(OK) cat("acos: Angles given in radians.\n")

   .Primitive("acos")(x)
}

asin <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("asin", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }
   if(OK) cat("asin: Angles given in radians.\n")

   .Primitive("asin")(x)
}

atan <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("atan", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }
   if(OK) cat("atan: Angles given in radians.\n")

   .Primitive("atan")(x)
}

exp <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("exp", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }

   return(.exp(x))
}

log <-
function(x, base=exp(1))
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("log", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }
   ans <- 0*x
   if(missing(base)) {
      ans[x<0] <- NaN
      ans[x>=0] <- .Internal(log(x[x>=0]))
   } else {
      ans[x<0] <- NaN
      ans[x>=0] <- .Internal(log(x[x>=0], base))
   }
   cat("log: Using log base e.\n")
   return(ans)
}

log2 <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("log2", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }

   ans <- 0*x
   ans[x<0] <- NaN
   ans[x>=0] <- .Internal(log(x[x>=0], 2)) 
   return(ans)
}

log10 <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("log10", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }
   ans <- 0*x
   ans[x<0] <- NaN
   ans[x>=0] <- .Internal(log(x[x>=0], 10)) 

   return(ans)
}

sqrt <-
function(x)
{
### Validate input, defaulting to NA.
   xexpr <- deparse(substitute(x))
   OK <- .var.exists("sqrt", x, xexpr, up=1)
   if(!OK) { x <- NA
             x <- try(.getexpr(x, up=1)) }

   return(.sqrt(x))
}

source <-
function (file, local = FALSE, echo = verbose, print.eval = echo,
    verbose = getOption("verbose"), prompt.echo = getOption("prompt"),
    max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"))
{ cat("source: MASK UNDER CONSTRUCTION.\n")
    dl <- dir()
    rl <- eval(expression(ls()), sys.parent(1))

    if(missing(file)) {
       cat("source: No file received.\n")
       cat("Present Working Directory:\n")
       print(dl)
       cat("\n")
       cat("For examples, Type: eg(source)\n")
       return(invisible(NULL))
    }

    if(!file.exists(file)) {
       cat(sep="", "source: File ", deparse(substitute(file)), " not found.\n")
       cat("Present Working Directory:\n")
       print(dl)
       cat("\n")
       cat("For examples, Type: eg(source)\n")
       return(invisible(NULL))
    }

    try(.source(file, local, echo, print.eval, verbose, prompt.echo,
                 max.deparse.length, chdir, encoding))
}

