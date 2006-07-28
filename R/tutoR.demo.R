"tutoR.demo" <-
function(...)
{
   .clearScreen()

   cat("The tutoR Package firstly offers more rapid help using eg().\n")
   cat("TO UPDATE: eg() extracts Examples text from (html) help files.\n")
   cat("\n")
   cat("* 'eg()' gives a help menu for common tasks, such as 'if',\n")
   cat("  'for', or examples creating 'function's.\n")
   cat("\n")
   cat("* For fast help, 'eg(func)' first shows Examples using 'func()'.\n")
   cat("  Additional examples may be included to help new users.\n")
   cat("  Examples are followed by the Description, Usage & Arguments.\n")
   cat("\n")
   readline("Press [ENTER] for: eg(plot) ")
   try(eg(plot))

   .clearScreen()

   cat("* Suppose a new user wants to obtain the data 'acme' without knowing\n")
   cat("  where it is.  With the 'boot' Package not currently loaded, the\n")
   cat("  command, getAnywhere(acme), may create some confusion.  A simpler\n")
   cat("  alternative may be offered by, retrieve(acme), as follows.\n")
   cat("\n")
   readline("Press [ENTER] for: retrieve(acme) ")
   cat("\n")
   if(.function.exists("smooth.f")) try(detach(package:boot))
   try(print(retrieve(acme)))
   if(.function.exists("smooth.f")) try(detach(package:boot))
   cat("\n")
   readline("Press [ENTER] to continue: ")

   .clearScreen()

   cat("* Suppose a new user wants examples of setting 'cex': eg(cex).\n")
   cat("  Having no such function, help.search is automatically launched.\n")
   cat("  The new user is further told how to use the results shown.\n")
   cat("\n")
   readline("Press [ENTER] for: eg(cex) ")
   cat("\n")
   try(eg(cex))

   .clearScreen()

   cat("* When a function is not currently available,\n")
   cat("  eg(func) will search all packages for help on 'func'.\n")
   cat("  The user is told how to load the required package.\n")
   cat("\n")
   readline("Press [ENTER] for: eg(unit) ")
   cat("\n")
   if(.function.exists("layout.torture")) try(detach(package:grid))
   try(eg(unit))
   cat("\n")
   readline("Press [ENTER] to continue: ")

   .clearScreen()

   cat("* The tutoR Package also offers more robust functions.\n")
   cat("  Function masks report an error rather than generate one.\n")
   cat("  We see later, deskcheck(myfunc) can help pin-point errors.\n")
   cat("\n")
   cat("* Consider a typo made in plot(), given the following variables:\n")
   cat("\n")
   cat("  > x <- 1:20\n")
   x <- 1:20
   cat("  > y <- x^5\n")
   y <- x^5
   cat("  > xavier <- cbind(x, y)\n")
   xavier <- cbind(x, y)
   cat("  > kavies <- cbind(x, x)\n")
   kavies <- cbind(x, x)
   cat("  > xy <- cbind(y, y)\n")
   xy <- cbind(y, y)
   cat("\n")
   readline("Press [ENTER] for: plot(xavies) ")
   cat("\n")
   try(plot(xavies))
   readline("Press [ENTER] to continue: ")

   .clearScreen()

   cat("* Additional warning messages help clarify errors.\n")
   cat("\n")
   cat("\n")
   readline("Press [ENTER] for: plot(x, density(x)) ")
   cat("\n")
   try(plot(x, density(x)))
   cat("\n")
   readline("Press [ENTER] to continue: ")

   .clearScreen()

   cat("* When the input looks OK, feedback is given to help clarify.\n")
   cat("  Take for example a plot of sin(theta) vs theta, where\n")
   cat("  > theta <- seq(-2*pi, 2*pi, by=pi/10)\n")
   theta <- seq(-2*pi, 2*pi, by=pi/10.01)
   cat("\n")
   readline("Press [ENTER] for: plot(theta, sin(theta), type=\"l\") ")
   cat("\n")
   try(plot(theta, sin(theta), type="l"))
   cat("\n")
   readline("Press [ENTER] to continue: ")
   graphics.off()

   .clearScreen()

   cat("* Common mathematical functions are also made more robust.\n")
   cat("  With 'x', 'xavier', 'kavies', 'xy' & 'theta' as already defined,\n")
   cat("\n")
   readline("Press [ENTER] for: exp(xavies) ")
   cat("\n")
   try(print(exp(xavies)))
   cat("\n")
   readline("Press [ENTER] to continue: ")

   .clearScreen()

   cat("* For log(), the user is reminded that log base e is used.\n")
   cat("\n")
   cat("\n")
   readline("Press [ENTER] for: log(x) ")
   cat("\n")
   ans <- NA
   ans <- try(log(x))
   print(ans)
   cat("\n")
   readline("Press [ENTER] to continue: ")

   .clearScreen()

   cat("* Trignometric functions remind that angles are taken in radians.\n")
   cat("\n")
   cat("\n")
   readline("Press [ENTER] for: sin(x/100); asin(0.7) ")
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
   readline("Press [ENTER] to continue: ")

   .clearScreen()

   cat("* If the input looks to be in degrees, a conversion is suggested.\n")
   cat("\n")
   cat("\n")
   readline("Press [ENTER] for: sin(y) ")
   cat("\n")
   ans <- NA
   ans <- try(sin(y))
   print(ans)
   cat("\n")
   readline("Press [ENTER] to continue: ")

   .clearScreen()

   cat("* UNDER CONSTRUCTION: A source() mask validates file input.\n")
   cat("  If a problem then the Present Working Directory is listed.\n")
   cat("\n")
   readline("Press [ENTER] for: source(\"some non-existent file\") ")
   source("some non-existent file")
   cat("\n")
   readline("Press [ENTER] to continue: ")

   .clearScreen()

   cat("* To deskckeck a user-defined function, consider:\n")
   cat("  my func <-\n")
   myfunc <-
   function(x, y)
   {
      if(y < x) {
         temp <- x
         x <- y; y <- temp
      }
      ANSWER <- 1
      for(i in 0:(y-x)) ANSWER <- ANSWER * (y-i)
   
      return(ANSWER)
   }
   cat("+ "); cat(sep="\n+ ", deparse(myfunc), "\n")
   readline("Press [ENTER] for: deskcheck(myfunc, 10, 5) ")
   try(deskcheck(myfunc, 10, 5))

   cat("\n")
   readline("Press [ENTER] to leave tutoR demo: ")
}

