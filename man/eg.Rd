\name{eg}
\alias{eg}
\alias{print.hsearch}
\title{help with Examples first}
\description{
Places \code{Examples} first and foremost in a help file.
}
\usage{
eg(topic)
}
\arguments{
\item{}{With 'topic' missing, eg() provides a menu for examples to help start off.}
\item{topic}{Locates help file for 'topic' (with all packages searched).
            'topic' may be quoted ("topic") or unquoted.
            With "if", for example, syntax requires (double) quotes.
            Upon 'topic' not being located (and taking keyword 'topic'),
            help.search("keyword") is automatically activated.
            Complete documentation, and further options,
            are available by using the \code{help} command.  }
}
\examples{

eg()		## Display Menu for help getting started.

eg(plot)	## plot help: 'Examples', 'Desciption', 'Usage', etc.

eg(bootstrap)	## Upon not finding a function 'bootstrap',
		## related functions and packages are listed.

eg(acme)	## Instructs how to load the boot Package if not loaded.

}
\seealso{
\code{\link{help}},
\code{\link{help.search}}
}
\keyword{documentation}
