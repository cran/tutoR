"lines" <-
function (x, y, ...)
{
  xexpr <- deparse(substitute(x))
  yexpr <- deparse(substitute(y))
  .plot.checkXY("lines", x, y, xexpr, yexpr, REPORT=FALSE)
  UseMethod("lines")
}

