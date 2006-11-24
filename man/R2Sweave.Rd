\name{R2Sweave}
\alias{R2Sweave}
\alias{Sweave}
\title{Sweave document creation}
\description{Translate R code into a Sweave (then Latex) document.}
\usage{
R2Sweave(func = NULL, infile = NULL)
}
\arguments{
  \item{func}{Any R function to form the basis of the document.}
  \item{infile}{Alternatively, a file containing all R-code to be included.}
}
\details{
Separate blocks of code are identified by 2 or more blank lines between them.
A line of text starting with \code{###} is thereafter included as purely Latex.
Otherwise, a single or double \code{#}, or text, is taken to be R-code.
}
\author{Mark Fielding, Andrew Robinson <A.Robinson@ms.unimelb.edu.au>}
\note{A suitable input file should be, \code{file.R}, or \code{file.R}.
  An outfile will be chosen and suitably indexed, \code{Sw--.Rnw} or \code{Sw--.rnw}.
  A sdingle figure will be included, to which appropriate code
  may later be placed, or reproduced for further figures.
}
\seealso{ \code{\link{Sweave}}, \code{\link{RweaveLatex}} }
\examples{
\dontrun{

myfunc <- function(x, y) {
	if(y < x) {temp <- x; x <- y; y <- temp }
	ANSWER <- 1
	for(i in 0:(y-x)) ANSWER <- ANSWER * (y-i)
	return(ANSWER)
}

R2Sweave(myfunc)

write("myfunc <-", "myfunc.R")
write(deparse(myfunc), append=TRUE, "myfunc.R")
unlink(\"myfunc\")

R2Sweave("myfunc.R")

}
}
\keyword{utilities}
