\name{eg}
\alias{eg}
\title{help with examples first}
\description{
Scans help files to pick out first Sections: Examples, Description, and so on.
}
\usage{
eg(func)
}
\arguments{
\item{}{With 'func' missing, eg() gives menu for examples to help start off.}
\item{func}{Locates help file for 'func' (with all packages searched).
            'func' may be quoted ("func") or unquoted.
            "if", for example, syntax requires in (double) quotes.
            More complete documentation, and options,
            are available by using the 'help' command.
            Upon 'func' not being located (then taking keyword='func'),
            help.search("keyword") is automatically activated.}
}
\examples{
eg(plot)                # Picks out 'Examples', then 'Desciption', 'Usage', etc.

eg(bootstrap)		# Upon not finding a function 'bootstrap',
			# related functions and packages are listed.

eg(acme)		# Will instruct how to load the boot Package.
}
\seealso{
\code{\link{help}},
\code{\link{help.search}}
}
\keyword{aplot}
