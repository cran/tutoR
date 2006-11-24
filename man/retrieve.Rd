\name{retrieve}
\alias{retrieve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mask of getAnywhere().}
\description{
After validation of input received, functionality
of 'getAnywhere' is performed.
When a required package is not already loaded,
the user is prompted and the package loaded.
}
\usage{
retrieve(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character string or name.}
}
\details{
A search is performed among all loaded packages.
Upon the requested object not being found, a search
is performed among all available packages.
The User is then prompted for the relevant package to
be loaded, before proceeding as with 'getAnywhere'.

}
\value{
     An object of class '"getAnywhere"'.  This is a list with
     components

    \item{name}{the name searched for.}

    \item{objs}{a list of objects found}

   \item{where}{a character vector explaining where the object(s) were found}

 \item{visible}{logical: is the object visible}

    \item{dups}{logical: is the object identical to one earlier in the list.}


     Normally the structure will be hidden by the 'print' method. There
     is a '[' method to extract one or more of the objects found.
}
\seealso{ \code{\link{getAnywhere}} }
\examples{
data1 <- retrieve(cars)

## If 'boot' Package not already loaded, getAnywhere could be unclear
getAnywhere(acme)
## But retrieve will locate and load the required package.
data2 <- retrieve(acme)
}
\keyword{data}
