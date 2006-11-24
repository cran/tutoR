"eg" <-
function(topic)
{
   HELP <- TRUE
   reserved <- c(.Reserved(), "tutoR")

### eg() HELP MENU
   if(missing(topic)) {
      repeat{

         .clearScreen()

         cat(sep="",
              "  _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/\n",
              "  _/  Please select item or 0 to exit Menu  _/\n",
              "  _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/\n",
              "  _/ 1) A demo of the tutoR Package         _/\n",
              "  _/ 2) Examples using eg(topic)            _/\n",
              "  _/ 3) eg(assist): Assist plot, matrix etc _/\n",
              "  _/ 4) eg(\"function\"): Create a function   _/\n",
              "  _/ 5) eg(deskcheck): Deskcheck a function _/\n",
              "  _/ 6) If & Loops: eg(\"if\"), eg(\"for\")     _/\n",
              "  _/ 7) plot demonstration: demo(graphics)  _/\n",
              "  _/ 0) Exit Menu                           _/\n",
              "  _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/\n")
         cat("\n")
         cat("\n")
         choice <- 0
         tries <- 0
         repeat{ tries <- tries+1
            warnings <- options("warn"); options(warn=-1)
            input <- readline(prompt=" Your Selection: ")
            try(choice <- as.double(input))
            if(choice %in% 0:9) break
            if(tries > 10) { choice <- 0; break }
            cat(" Please select from: 1 - 9 or 0 to Exit.\n")
            options(warnings)
         }

         if(choice == 1) tutoR.demo() else
         if(choice == 2) print(eg(eg)) else
         if(choice == 3) print(eg(assist)) else
         if(choice == 4) print(eg("function")) else
         if(choice == 5) print(eg(deskcheck)) else
         if(choice == 6) print(eg(Control)) else
         if(choice == 7) { demo(graphics); graphics.off() } else
         if(choice == 0) return(invisible(NULL))
      }
   } else { ### eg(FUNCNAME)
      if(substr(deparse(substitute(topic)),1,1) == "\"")
        topicname <- topic else
        topic <- deparse(substitute(topic))

      topicname <- deparse(substitute(topic))
      if(substr(topicname,1,1) == "\"") topicname=topic

      if(topicname %in% .Control()) topicname <- "Control"
      if(topicname %in% .Log()) topicname <- "Log"
      if(topicname %in% .Math()) topicname <- "Math"
      if(topicname %in% .Trig()) topicname <- "Trig"
      if(topicname == "tutoR") topicname <- "tutoR.package"
      if (!is.na(match(topic, c("+", "-", "*", "/", "^", "%%"))))
          topicname <- "Arithmetic" else
      if (!is.na(match(topic, c("<", ">", "<=", ">=", "==", "!="))))
          topicname <- "Comparison" else
      if (!is.na(match(topic, c("[", "[[", "$"))))
          topicname <- "Extract" else
      if (!is.na(match(topic, c("&", "&&", "|", "||", "!"))))
          topicname <- "Logic" else if (!is.na(match(topic, c("%*%"))))
          topicname <- "matmult"


      if(.function.exists(topicname) & length(.find.owner(topicname))>0) {
        pkg <- .find.owner(topicname)
        if(length(pkg[pkg!="tutoR"]) > 1) {
           cat(sep="", "Note: '", topicname, "' may differ with: library(")
           cat(sep="); library(", pkg); cat(")\n")
           pkg <- pkg[1]
        }
### EXTRACT 'Examples' FIRST FROM help()
        return({ .Examples(topicname); invisible(NULL) })

      } else { # topicname NOT LOCATED
        pkg <- .find.owner(topicname)
        if(is.null(pkg)) {
           show(help.search(topicname)) 
           HELP <- FALSE
        } else {
           cat(sep="", "'", topicname, 
                "' available with the ", pkg, " Package.\n")
           cat(sep="", "To load ", pkg[1], 
                  ", Type: library(", pkg[1], ")\n")
           HELP <- FALSE
        } 
        if(length(pkg) > 1) pkg <- pkg[1]
      }
   }

   if(HELP) {
      if(topicname %in% reserved) 
         topicname <- paste(sep="", "\"", topicname, "\"")
      cat(sep="", "For complete documentation, Type: help(", topicname, 
                                           ")  OR  ?", topicname, "\n")
   }

   invisible(NULL)
}

