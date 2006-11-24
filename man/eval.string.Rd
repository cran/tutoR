\name{eval.string}
\alias{eval.string}
\title{Evaluate an (Unevaluated) String Expression}
\description{
  Evaluate an \R string expression.
}
\usage{
eval.string(expr, envir = parent.frame(),
    	   enclos = if(is.list(envir) || is.pairlist(envir))
                       parent.frame() else baseenv())
}
\arguments{
  \item{expr}{string of expression to be evaluated.}
  \item{envir}{From \code{eval}, made redundant in \code{eval.string}.}
  \item{enclos}{From \code{eval}, made redundant in \code{eval.string}.}
}
\details{
  \code{eval} evaluates the expression \code{expr} argument only in
  the local environment.

  \code{eval.string} evaluates \code{expr} in the .GlobalEnv environment,
  duplicating local objects, but restoring any former values.
}
\seealso{
  \code{\link{eval}}
}
\examples{

eval.string("x <- 1")
eval.string("cat(\"x =\", x, \"\\n\")")
eval.string("2 ^ 2 ^ 3")

x <- 1:10
cmd <- "plot(x, x^5)"
eval.string(cmd)

}
\keyword{data}
\keyword{programming}
