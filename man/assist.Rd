\name{assist}
\alias{assist}
\alias{assist.matrix}
\alias{assist.options}
\alias{assist.par}
\alias{assist.plot}
\alias{assist.seq}
\alias{assist.apply}
\alias{assist.sapply}
\alias{assist.tapply}
\alias{assist.lapply}
\alias{assist.combine}
\alias{assist.data.frame}
\alias{assist.list}
\alias{assist.cbind}
\alias{assist.rbind}
\title{Step-by-step assistance to call a function}
\description{
  \code{assist} displays how a given function is to be called, with a syntax
  as given by \code{args}.  For a number of chosen functions, the function
  call is constructed step-by-step, prompting for User-specifications.
}
\usage{
assist(func)
}
\arguments{
  \item{func}{Any R function, such as \code{plot}, \code{matrix}, \code{par}
  or \code{seq}.  'func' may be quoted ("func") or unquoted. }
}
\details{
  A collection of functions are called upon.  \code{assist(matrix)}, for example,
  will call upon \code{assist.matrix}.  Similarly for \code{assist(plot)}
  and \code{assist(seq)}.
}
\seealso{
  \code{\link{args}}, \code{\link{eg}}, \code{\link{help}}.
}
\examples{

assist()	## Gives menu of available functions to assist

x <- 1:9
assist(matrix)	## To construct say: matrix(x^2, byrow=TRUE, ncol=3)
assist(plot)	## To construct say: plot(x, x^5, type="o")
assist(par)     ## To set options in 'par'
assist(seq)     ## To generate a given sequence
assist(apply)	## Incorporating 'sapply' and 'tapply'
assist(list)	## Or assist(combine). Incorprating, assist(data.frame),
		## as well as assist(list), assist(cbind), assist(rbind)

}
\keyword{programming}

