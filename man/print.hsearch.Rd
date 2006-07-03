\name{print.hsearch}
\alias{print.hsearch}
\title{Search the Help System}
\description{
  Allows for searching the help system for documentation matching a
  given character string in the (file) name, alias, title, concept or
  keyword entries (or any combination thereof), using either
  \link{fuzzy matching} or \link{regular expression} matching.  Names
  and titles of the matched help entries are displayed nicely.
}
\usage{
print.hsearch(x, ...)
}
\arguments{
  \item{x}{When a help file is not found by \code{eg(func)},
    then func is taken a character string (pattern) to be
    matched using \code{help.search}.
    print.hsearch is called by \code{help.search}, also appropriately
    updated to give reference back to using \code{eg}. }
  \item{...}{Fields passed from \code{help.search} as required.},
}
\details{
  Upon installation of a package, a contents data base which contains
  the information on name, title, aliases and keywords and, concepts
  starting with \R 1.8.0, is computed from the Rd files in the package and
  serialized as \file{Rd.rds} in the \file{Meta} subdirectory of the
  top-level package installation directory (or, prior to \R 1.7.0, as
  \file{CONTENTS} in Debian Control Format with aliases and keywords
  collapsed to character strings in the top-level package installation
  directory).  This, or a pre-built help.search index serialized as
  \file{hsearch.rds} in the \file{Meta} directory, is the data base
  searched by \code{help.search()}.

  Note that currently, the aliases in the matching help files are not
  displayed.
}
\value{
  The results are returned in an object of class \code{"hsearch"}, which
  has a print method for nicely displaying the results of the query.
  This mechanism is experimental, and may change in future versions of
  R.
}
\seealso{
  \code{\link{help}};
  \code{\link{eg}};
  \code{\link{help.start}} for starting the hypertext (currently HTML)
  version of \R's online documentation, which offers a similar search
  mechanism.

  \code{\link{RSiteSearch}} to access an on-line search of \R resources.
}
\examples{
eg("linear models")    # In case you forgot how to fit linear
                                # models
eg("non-existent topic")
\dontrun{
eg("print")            # All help pages with topics or title
                                # matching 'print'
help.search(apropos = "print")  # The same

help.search(keyword = "hplot")  # All help pages documenting high-level
                                # plots.
file.show(file.path(R.home(), "doc", "KEYWORDS"))  # show all keywords

## Help pages with documented topics starting with 'try'.
help.search("\\\\btry", fields = "alias")
## Do not use '^' or '$' when matching aliases or keywords
## (unless all packages were installed using R 1.7 or newer).
}
}
\keyword{documentation}
