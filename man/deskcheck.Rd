\name{deskcheck}
\title{Deskcheck a function line-by-line}
\usage{
deskcheck(.function)
}
\alias{deskcheck}
\arguments{
\item{.function}{any user-defined \R function.}
}
\description{
  Utilizes the 'debug' flag in line-by-line execution of a function.
}
\details{
  \code{deskcheck} activates the \code{debug} flag before initiating
  line-by-line execution of the function, \code{.function}.
  Values are first requested for required arguments to \code{.function}.

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
## Sample function to locate a bug
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

\dontrun{

deskcheck(hanoi)

}
}
\keyword{programming}
\keyword{environment}
