"print.hsearch" <-
function(x, ...)
{
    fields <- paste(x$fields, collapse = " or ")
    type <- switch(x$type, fuzzy = "fuzzy", "regular expression")
    db <- x$matches
    if(NROW(db) > 0) {
	outFile <- tempfile()
	outConn <- file(outFile, open = "w")
	writeLines(c(strwrap(paste(sep="", "Help files with reference to ", 
                     sQuote(x$pattern), ":")), "\n\n"),
		   outConn)
	dbnam <- paste(db[ , "topic"], "(",
		       db[, "Package"], ")",
		       sep = "")
	dbtit <- paste(db[ , "title"], sep = "")
	writeLines(formatDL(dbnam, dbtit), outConn)
        writeLines(c("\n\n", strwrap(paste("For examples using 'funcname(package)',",
                       "Type: eg(funcname)"))), 
                    outConn)
	close(outConn)
	file.show(outFile, delete.file = TRUE)
    } else {
	writeLines(strwrap(paste("No help files found with", fields,
                                 "matching", sQuote(x$pattern),
                                 "using", type, "matching.")))
    }
}

