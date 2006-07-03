"eg" <-
function(func)
{  max.Examples.indent <- 7
   HELP <- TRUE
   reserved <- c("if", "for", "function", "repeat", "tutoR", "while")

### eg() HELP MENU
   if(missing(func)) {
      repeat{

         .clearScreen()

         cat("Please select menu item for help.\n")
         cat(" 1) A demo: tutoR Package\n",
              "2) Control: eg(\"if\"), eg(\"for\")\n",
              "3) Examples using eg(funcname)\n",
              "4) Functions: eg(\"function\")\n",
              "5) Functions: Deskcheck\n",
              "6) Matrices: Working with,\n",
              "7) Plots: About,\n",
              "8) Plots: A demo\n",
              "9) Exit eg()\n")

         expr <- parse(prompt="Selection: ")
         choice <- 0
         choice <- try(eval(expr))

         if(is.numeric(choice))
         if(choice == 1) tutoR.demo() else
         if(choice == 2) eg(Control) else
         if(choice == 3) eg(eg) else
         if(choice == 4) eg("function") else
         if(choice == 5) eg(deskcheck) else
         if(choice == 6) eg(matrix) else
         if(choice == 7) eg(plot) else
         if(choice == 8) { demo(graphics); graphics.off() } else
         if(choice == 9) return(invisible(NULL))
      }
   } else { ### eg(FUNCNAME)
      funcname <- deparse(substitute(func))
      if(substr(funcname,1,1) == "\"") funcname=func

      if(funcname %in% .Control()) funcname <- "Control"
      if(funcname %in% .Log()) funcname <- "Log"
      if(funcname %in% .Trig()) funcname <- "Trig"
      if(funcname == "tutoR") funcname <- "tutoR.package"

      if(.function.exists(funcname) & length(.find.owner(funcname))>0) {
        pkg <- .find.owner(funcname)
        if(length(pkg) > 1) {
           cat(sep="", "Note: ", "'", funcname, "' may differ with: library(", 
                    paste(sep=", library(", pkg[-1]), ")\n")
           pkg <- pkg[1]
        }
        path <- paste(sep="", .find.package(pkg),
                             "/html/", funcname, ".html")

################### FIND ALTERNATIVE EXTRACTING FROM  HELP #############
# helpfile <- readLines(paste(sep="",.find.package(pkg),"/help/", funcname))
# show(paste(sep="\n", helpfile))
########################################################################

### OPEN tempfile FOR READING/WRITING
        tempfile <- file(".studefile", "w+")
        cat(sep="", "Examples \t\t", funcname, ": library(", pkg, ")\n", 
                         file=tempfile)
        cat("=========\n", file=tempfile)
        .extra.examples(funcname, tempfile)
        .eg.html(path, tempfile, max.Examples.indent)
        close(tempfile)

        tmp <- file(".temp", "w+")
        txt <- readLines(".studefile")
        for(i in 1:length(txt)) cat(sep="", txt[i], "\n", file=tmp)
        close(tmp)
        file.show(".temp")
        unlink(".temp")

        unlink(".studefile")
      } else { # funcname NOT LOCATED
        pkg <- .find.owner(funcname)
        if(length(pkg) > 1) pkg <- pkg[1]
        if(is.null(pkg)) {
           show(help.search(funcname)) 
           HELP <- FALSE
        } else {
           cat(sep="", "'", funcname, 
                "' available with the ", pkg, " Package.\n")
           cat(sep="", "To load ", pkg[1], 
                  ", Type: library(", pkg[1], ")\n")
           HELP <- FALSE
        } 
      }
   }

   if(HELP) {
      if(funcname %in% reserved) 
         funcname <- paste(sep="", "\"", funcname, "\"")
      cat(sep="", "For complete documentation, Type: help(", funcname, 
                                           ")  OR  ?", funcname, "\n")
   }
   invisible(NULL)
}

