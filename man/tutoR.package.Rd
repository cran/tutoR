\name{student-package}
\alias{student-package}
\alias{student}
\docType{package}
\title{
Student-friendly R
}
\description{
User-friendly package that masks common functions.
Help available giving examples first off.
Deskcheck for testing user-defined defined functions.
}
\details{
\tabular{ll}{
Package: \tab student\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2006-06-06\cr
License: \tab  GPL version 2.0\cr
}
Package to mask common functions so that bad inputs are picked
up in advance.  Errors are explained prior to or instead of
function execution.  eg() picks out "Examples" first and foremost.  
In deskcheck() the debug() flag is set and
execution initiated for a function to be tested.
}
\author{
Mark Fielding <M.Fielding@ms.unimelb.edu.au>,
Dept. Mathematics and Statistics,
The University of Melbourne
}
\examples{
eg()			# Gives Help Menu to help get started.
eg(plot)  		# Picks out 'Examples', 'Description', 'Usage', 
			# followed by 'Arguments' and 'See Also'.
myfunc <- function(x, y)
{
  ANSWER <- 1
  for(i in 0:(y-x)) { ANSWER <- ANSWER * (y-i) }
  return(ANSWER)
}
deskcheck(myfunc) 	# sets debug() flag and initiates execution

sin(45)			# suggests a conversion, degrees to radians
}
\keyword{package}
