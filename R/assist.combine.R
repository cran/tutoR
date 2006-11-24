"assist.combine" <-
function(up=0)
{ RETURN <- FALSE; repeat{
   assign(".comand", "assist.combine", envir=.GlobalEnv)
   choice <- .menu(c(
       paste("data.frame to bind together variables of equal length\n",
             "   - in a list with a matrix-like structure."),
       paste("list to bind together varied objects\n",
             "   - as a single list."),
       paste("cbind to combine columns to form a matrix\n",
             "   - where you may assign column names."),
       paste("rbind to combine columns as the rows of a matrix\n",
             "   - and you may assign row names.")),
          prompt="Please confirm the appropriate function",
          inputs=c("d","l","c","r")
                  )
   if(choice == 0) { cat("\n"); return() }


   switch(choice,
   {
      if(up == 1)
         eval(expression(funcname <- "data.frame"), sys.parent(1))
      assist.data.frame(up=up+2)
   },
   {
      if(up == 1)
         eval(expression(funcname <- "list"), sys.parent(1))
      assist.list(up=up+2)
   },
   {
      if(up == 1)
         eval(expression(funcname <- "cbind"), sys.parent(1))
      assist.cbind(up=up+2)
   },
   {
      if(up == 1)
         eval(expression(funcname <- "rbind"), sys.parent(1))
      assist.rbind(up=up+2)
   }
   )
if(!RETURN) break
}}

