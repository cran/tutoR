\name{retrieve}
\alias{retrieve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
retrieve(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x)
{
   funcname <- deparse(substitute(x))
   if(substr(funcname,1,1) == "\"") funcname=x

   if(funcname \%in\% .Control()) funcname <- "Control"
   if(funcname \%in\% .Log()) funcname <- "Log"
   if(funcname \%in\% .Trig()) funcname <- "Trig"
   if(funcname == "tutoR") funcname <- "tutoR.package"

   if(.function.exists(funcname) & length(.find.owner(funcname))>0) {
      pkg <- .find.owner(funcname)
      if(length(pkg) > 1) {
         cat(sep="", "Note: ", "'", funcname, "' may differ with: library(",
                  paste(sep=", library(", pkg[-1]), ")\n")
         pkg <- pkg[1]
      }
      cat(funcname, "is in", pkg, "\n")
   } else {}
  }
}
\keyword{data}
