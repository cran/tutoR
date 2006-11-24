"matrix" <-
function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) 
{
    data <- as.vector(data)
    if (missing(nrow)) 
        nrow <- ceiling(length(data)/ncol)
    else if (missing(ncol)) 
        ncol <- ceiling(length(data)/nrow)
    x <- .Internal(matrix(data, nrow, ncol, byrow))
    dimnames(x) <- dimnames
    x
}
