"plot" <-
function (x, y, ...)
{
  xexpr <- deparse(substitute(x))
  yexpr <- deparse(substitute(y))
  OK <- TRUE
  OK <- .plot.checkXY(x, y, xexpr, yexpr)

  if(OK) {
    if (is.null(attr(x, "class")) && is.function(x)) {
        nms <- names(list(...))
        if (missing(y))
            y <- {
                if (!"from" %in% nms)
                  0
                else if (!"to" %in% nms)
                  1
                else if (!"xlim" %in% nms)
                  NULL
            }
        if ("ylab" %in% nms)
            curve(x, y, ...)
        else curve(x, y, ylab = paste(deparse(substitute(x)),
            "(x)"), ...)
    }
    else UseMethod("plot")
  }
}

