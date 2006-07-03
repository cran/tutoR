"logb" <-
function (x, base = exp(1)) 
if (missing(base)) .Internal(log(x)) else .Internal(log(x, base))
