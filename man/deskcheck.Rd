\name{debug}
\title{Deskcheck a function line-by-line}
\usage{
deskcheck(func, ...)
}
\alias{deskcheck}
\arguments{
\item{func}{any user-defined \R function.}
\item{...}{The function inputs required by 'func': input1, input2, ....}
}
\description{
  Utilizes the 'debug' flag and starts execution of a function, line-by-line.
}
\details{
  debug: When a function flagged for debugging is entered, normal execution
  is suspended and the body of function is executed one statement at a time.
  A new browser context is initiated for each step (and the previous one
  destroyed).
  You take the next step by typing carriage return, \code{n} or \code{next}.
  You can see the values of variables by typing their names.
  Typing \code{c} or \code{cont} causes the debugger to continue to the
  end of the function (or loop if within a loop).
  You can \code{debug} new functions before you step in to them from inside
  the debugger.
  Typing \code{Q} quits the current execution and returns you to the
  top--level prompt.
  Typing \code{where} causes the debugger to print out the current
  stack trace (all functions that are active).
  If you have variables with names that are identical to the controls
  (eg. \code{c} or \code{n} ) then you need to use \code{print(c)} and
  \code{print(n)} to evaluate them.

  In order to debug S4 methods (see \code{\link[methods]{Methods}}), you
  need to use \code{\link{trace}}, typically calling \code{\link{browser}},
  e.g., as \cr
  \code{trace("plot", browser, exit=browser, signature = c("track", "missing"))}
}
\seealso{
  \code{\link{debug}}
}
\examples{
   myfunc <- function(x, y)
   { 
      ANSWER <- 1
      for(i in 0:(y-x)) ANSWER <- ANSWER * (y-i)
      return(ANSWER) 
   }

   # Then type: deskcheck(myfunc, 10, 5)
}
\keyword{programming}
\keyword{environment}
