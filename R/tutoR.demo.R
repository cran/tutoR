"tutoR.demo" <-
function()
{
   .clearScreen()

   assign(".comand", "tutoR.demo()", envir=.GlobalEnv)
   cat("* The tutoR Package provides useful assistance using 'assist'.\n")
   cat("  Suppose a new user is unsure of the correct syntax to create a\n")
   cat("  plot or a matrix, then 'assist(plot)' or 'assist(matrix)' can\n")
   cat("  offer step-by-step assistance.\n")
   cat("\n")
   cat("* Suppose (for a given 'n') we wish to construct a sequence to\n")
   cat("  act as an index for:\n")
   cat("\n")
   cat("   x <- runif(n)\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for assist(seq), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      .clearScreen()
      n <- 57
      x <- runif(n)
      cat("* Suppose (for a given 'n') we wish to construct a sequence to\n")
      cat("  act as an index for:\n")
      cat("\n")
      cat("   x <- runif(n)\n")
      try(assist(seq))
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* The tutoR Package also provides help with 'eg()'.\n")
   cat("  For faster help, 'eg(topic)' first shows Examples.\n")
   cat("  Examples are followed by Description, Usage, Arguments etc.\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for eg(plot), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) { eg(plot)
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* Suppose a new user wants to obtain the data 'acme' without knowing\n")
   cat("  where it is.  With the 'boot' Package not currently loaded, the\n")
   cat("  command, getAnywhere(acme), may not be helpful.  A more helpful\n")
   cat("  alternative may be offered by, retrieve(acme).\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for retrieve(acme), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      cat("\n")
      boot.ATTACHED <- .function.exists("smooth.f")
      if(boot.ATTACHED) try(detach(package:boot))
      try(print(retrieve(acme)))
      if(!boot.ATTACHED) if(.function.exists("smooth.f"))
         try(detach(package:boot))
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* When a help topic is not available, eg(topic) will\n")
   cat("  search all packages for help documentation.\n")
   cat("  The user is then told how to load a required package.\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for eg(unit), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      cat("\n")
      grid.ATTACHED <- .function.exists("grid.set")
      if(grid.ATTACHED) try(detach(package:grid))
      try(eg(unit))
      if(grid.ATTACHED) library(grid)
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* Suppose we desire examples of setting 'cex', by typing: eg(cex)\n")
   cat("  Having no such help topic, 'help.search' is automatically launched.\n")
   cat("  Additional instructions direct us toward trying 'eg(par)'.\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for eg(cex), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      try(eg(cex))
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* The tutoR Package offers user-friendly function masks.\n")
   cat("  Consider a typo made in 'plot', given the following variables:\n")
   cat("\n")
   cat("  > n <- 57\n")
   cat("  > x <- 1:20\n")
   x <- 1:20
   cat("  > y <- x^5\n")
   y <- x^5
   cat("  > xavier <- cbind(x, x^2)\n")
   xavier <- cbind(x, x^2)
   cat("  > kavies <- cbind(x, x^3)\n")
   kavies <- cbind(x, (x/10)^3)
   cat("  > xy <- cbind(x, y)\n")
   xy <- cbind(x, y)
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for plot(xavies), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      try(plot(xavies))
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* Function masks also help to clarify errors that may occur.\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for plot(x, density(x)), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      try(plot(x, density(x)))
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* When the input looks OK, feedback is given to help clarify.\n")
   cat("  Take for example a plot of sin(theta) vs theta, where\n")
   cat("  > theta <- seq(-2*pi, 2*pi, by=pi/10)\n")
   theta <- seq(-2*pi, 2*pi, by=pi/10.01)
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for plot(theta, sin(theta), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      try(plot(theta, sin(theta), type="l"))
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
      try(graphics.off())
   }

   .clearScreen()

   cat("* Masks for 'matrix' and 'as.matrix' remind when filling by entries\n")
   cat("  by columns and when a column matrix is produced by default.\n")
   cat("\n")
   cat("  > x <- 1:9\n")
   x <- 1:9
   cat("  > x\n")
   show(x)
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for some matrix operations, 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      .clearScreen()
      cat("  > x\n")
      show(x)
      cat("\n")
      cat("  > m <- matrix(x, ncol=3)\n")
      m <- matrix(x, ncol=3)
      cat("\n")
      cat("  > x <- as.matrix(x)\n")
      x <- as.matrix(x)
   }
   cat("\n\n")
   cat("* Common mathematical functions are also made more robust.\n")
   cat("  With 'x', 'xavier', 'kavies', 'theta' etc. as already defined,\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for exp(xavies), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      cat("\n\n")
      try(print(exp(xavies)))
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* For log(), the user is reminded that log base e is used.\n")
   cat("\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for log(x), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      cat("\n")
      ans <- NA
      ans <- try(log(x))
      print(ans)
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* Trignometric functions remind that angles are taken in radians.\n")
   cat("\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for sin(x/100), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      cat("\n")
      x <- x + 0.0001
      ans <- NA
      ans <- try(sin(x/10))
      x <- x - 0.0001
      print(ans)
      cat("\n")
      ans <- NA
      ans <- try(asin(0.7))
      print(ans)
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* If the input looks to be in degrees, a conversion is suggested.\n")
   cat("\n")
   try(choice <- as.integer(readline(prompt=
          "Enter 1 for sin(y), 2 to Skip, or 0 to Exit: ")))
   if(!is.na(choice)) if(choice == 0) return(invisible(NULL))
   if(!is.na(choice)) if(choice == 1) {
      cat("\n")
      ans <- NA
      ans <- try(sin(y))
      print(ans)
      cat("\n")
      choice <- .choose(choice="to continue Demo", default="to Exit",
                         select=1, not=0)
      if(!choice) return(invisible(NULL))
   }

   .clearScreen()

   cat("* Now lets try to locate where exactly an error has occurred in\n")
   cat("  a new function. This could be done by setting a 'debug' flag,\n")
   cat("  or simply by using 'deskcheck'.\n")
   cat("\n")
   cat("  'deskcheck' will switch on (and then switch off) the debug flag\n")
   cat("  and start execution, while interactively requesting values of any\n")
   cat("  inputs required.  Lets try deskcheck for a new function in error.\n")
   cat("\n")
   choice <- .choose(choice="to try 'deskcheck'",
                     default="for demo Complete", select=1, not=0)
   if(!choice) return(invisible(NULL))
   if(choice) {
      cat("\n")
      cat("  hanoi <- function(n)\n")
      cat("  { move <- function(pegs, p1, p2) {\n")
      cat("       n <- length(pegs[1,])\n")
      cat("       plot(3/n*(0:(n+1)), 0:(n+1), type=\"n\", axes=F, xlab=\"\", ylab=\"\")\n")
      cat("       abline(v=c(1,2,3), h=0.5, lwd=3)\n")
      cat("       d1 <- sum(pegs[p1,]!=0); d2 <- sum(pegs[p2,]!=0)+1\n")
      cat("       pegs[p2, d2] <- pegs[p1, d1]; pegs[p1, d1] <- 0\n")
      cat("        . . .\n")
      cat("    from <- 3; to=1; move(p, from, to)\n")
      cat("    from <- 3; to=2; move(p, from, to)\n")
      cat("  }\n")
      cat("\n")
      cat("* 'hanoi' has an unlocated error: \"subscript out of bounds\"\n")
      cat("  We wish to know where that error was generated.\n")
      cat("\n")
   choice <- .choose(choice="for deskcheck(hanoi)", default="and demo Complete",
                         select=1, not=0)
   if(choice == 0) return(invisible(NULL))
    hanoi <- function(n)
    { move <- function(pegs, p1, p2) {
         n <- length(pegs[1,])
         plot(3/n*(0:(n+1)), 0:(n+1), type="n", axes=F, xlab="", ylab="")
         abline(v=c(1,2,3), h=0.5, lwd=3)
         d1 <- sum(pegs[p1,]!=0); d2 <- sum(pegs[p2,]!=0)+1
         pegs[p2, d2] <- pegs[p1, d1]; pegs[p1, d1] <- 0
         for(i in 1:3) for(j in 1:n) {
           disc <- ""
           if(pegs[i,j] != 0) for(size in 1:p[i,j])
             disc <- paste(sep="", disc, "#")
           text(i, j, disc, adj=0.5, cex=2.0) }
         return(pegs) }
      n <- 5; p <- matrix(c(n:1,rep(0,2*n)), 3, byrow=T)
      from <- 1; to=2; move(p, from, to)
      from <- 1; to=3; move(p, from, to)
      from <- 1; to=3; move(p, from, to)
      from <- 1; to=2; move(p, from, to)
      from <- 3; to=1; move(p, from, to)
      from <- 3; to=2; move(p, from, to)
    }
    if(choice) {
         try(undebug(hanoi), silent=TRUE)
         try(deskcheck(hanoi))
      }
   }
   
   cat("\n")
   readline("Completed tutoR demo. Press [ENTER]: ")
}

