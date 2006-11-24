\name{format}
\alias{format}
\description{
  Format a number for pretty printing - tutoR mask.
}
\title{Display a number with a given number of decimal places}
\usage{
format(x, digits=0, width=0, justify="right", ...)
}
\arguments{
  \item{x}{any numeric value.}
  \item{digits}{number of digits to appear to the right of the decimal point.}
  \item{width}{number of character spaces the number is to be aligned in,
  where a \code{width} of 0 procuces a number necessarily left justified.}
  \item{justify}{should the final \emph{character} vector be "right"-justified (the
    default), "left"-justified or "centre" aligned.}
  \item{...}{Additional inputs to format.default, called on appropriately.}
}
\details{
  The tutoR function, \code{format}, masks \code{format} from base,
  with \code{format.default} preserved.
}
\value{
  A string with the number in the desired format.
}
\seealso{
  \code{\link{format.default}}, \code{\link{formatC}}, \code{\link{paste}},
  \code{\link{as.character}}, \code{\link{sprintf}}, \code{\link{print}},
  \code{\link{toString}}, \code{\link{encodeString}}.
}
\examples{
format(1:10, 2)

## use of nsmall
format(13.7)
format.default(13.7, nsmall = 3)
format(c(6.0, 13.1), digits = 2)
format.default(c(6.0, 13.1), digits = 2, nsmall = 1)

## use of scientific
format(2^31-1)
format.default(2^31-1, sci = TRUE)

## a list
z <- list(a=letters[1:3], b=(-pi+0i)^((-2:2)/2), c=c(1,10,100,1000),
          d=c("a", "longer", "character", "string"))
format.default(z, digits = 2)
format.default(z, digits = 2, justify = "left", trim = FALSE)

}

\keyword{character}
\keyword{print}

