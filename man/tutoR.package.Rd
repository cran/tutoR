\name{tutoR-package}
\alias{tutoR-package}
\alias{tutoR}
\docType{package}
\title{
User-friendly R
}
\description{
User-friendly package that masks common functions.
Help available giving examples first off.
Deskcheck for testing user-defined defined functions.
}
\details{
\tabular{ll}{
Package: \tab tutoR\cr
Type: \tab Package\cr
Version: \tab 0.3.2\cr
Date: \tab 2006-11-24\cr
License: \tab  GPL version 2.0\cr
}
Package to mask common functions so that inputs in error are
explained and able to be corrected, prior to execution.
'assist' offers step-by-step assistance to correctly call
a function such as 'par'.  'eg' picks out Examples first and
foremost in help.  With 'deskcheck', execution is initiated
and the 'debug' flag utilized, to help deskcheck a function.
}
\author{
Mark Fielding <M.Fielding@ms.unimelb.edu.au>,
Dept. Mathematics and Statistics,
The University of Melbourne
}
\examples{
eg()		## Gives Help Menu to help get started.
eg(plot)  	## Picks out 'Examples', 'Description', 'Usage', 
		## followed by 'Arguments' and 'See Also'.

sin(45)		## suggests a conversion, degrees to radians

assist()        ## Will request which function to assist with

x <- 1:9
assist(matrix)  ## To construct say: matrix(x^2, byrow=TRUE, ncol=3)

myfunc <- function(x, y)
{
  ANSWER <- 1
  for(i in 0:(y-x)) { ANSWER <- ANSWER * (y-i) }
  return(ANSWER)
}

deskcheck(myfunc) 	# sets debug() flag and initiates execution
}
\keyword{package}
