"R2Sweave" <-
function(func=NULL, infile=NULL)
{
   funcname <- deparse(substitute(func))
   if(substr(funcname,1,1) == "\"" & is.null(infile)) {
      
      infile <- func
      func <- NULL
   }

   one.only <- function()
   {
      cat(sep="", "Expecting only one of:\n\tR2Sweave(infile = ",
           infile, ")\n\tR2Sweave(func = ", funcname, ")\n")
   }

   code <- NULL
   if(!is.null(func)) {
      if(!is.null(infile)) one.only() else {
         try(code <- deparse(func))
         if(is.null(code)) cat(sep="", "R2Sweave: Error viewing function ",
                                deparse(funcname)) else {
            choice <- .choose(choice= paste(sep="", "to use the function, ",
                          funcname, ","), select="", default="to Exit")
            if(!choice) return(invisible(NULL))
            code[1] <- paste(funcname, "<-", code[1])
         }
      }
   } else if(!is.null(infile)) {
      if(!is.null(func)) one.only() else {
         try(code <- readLines(infile))
         if(is.null(code)) cat(sep="", "R2Sweave: Error opening file \"",
              infile, "\"\n") else {
           choice <- .choose(choice= paste(sep="", "to use infile, ",
                         infile, ","), select="", default="to Exit")
           if(!choice) return(invisible(NULL))
         }
      }
   }

   suffix <- ".Rnw"
   if(!is.null(infile)) {
      i <- 0
      repeat{ i <- i+1
         if(substr(infile,i,i) == "") break
         if(substr(infile,i,i+1) == ".r" & substr(infile,i+2,i+2) == "") {
            suffix <- ".rnw"
            break
         }
      }
   }
   
   Sw <- SweaveFile <- NULL
   index <- 0
   repeat{
      index <- index + 1
      if(index < 10)
         SweaveFile <- paste(sep="", "Sw0", index, suffix) else
      if(index < 100)
         SweaveFile <- paste(sep="", "Sw", index, suffix) else break
         
      if(!file.exists(SweaveFile)) {
         try(Sw <- file(SweaveFile, "w"))
         if(!is.null(Sw)) break
      }
   }

   if(is.null(Sw)) {
      if(index == 100) cat("100 file limit. ")
      cat(sep="", "Error creating file \"", SweaveFile, "\"\n")
   } else {
      cat("\\documentclass[12pt]{article}\n", file=Sw)
      cat("\n", file=Sw)
      cat("\\author{R2Sweave()\\thanks{R tutoR Package}}\n", file=Sw)
      cat("\n", file=Sw)
      cat("\\begin{document}\n", file=Sw)
      cat("\n", file=Sw)
      cat("\\title{R2Sweave Document.}\n", file=Sw)
      cat("\n", file=Sw)
      cat("\\maketitle\n", file=Sw)
      cat("\n", file=Sw)

      block <- 1
      start <- 1

      repeat{ if(code[start] != "") break
        start <- start+1 }

      INCODE <- (substr(code[start],1,3) != "###")

      if(INCODE) {
         cat(sep="", "<<label=block", block,
                     ", include=TRUE>>=\n", file=Sw)
      }

      for(i in start:(length(code)-1))
      {
         if(code[i] == "" & code[i+1] == "") {
            if(INCODE) cat("@\n\n", file=Sw)
            block <- block+1
            INCODE <- FALSE
            repeat{
               if(code[i] != "" | i == length(code)) break
               i <- i+1
            }
         } else if(code[i] == "") {
         } else if(substr(code[i],1,3) == "###") {
            if(INCODE) {
               cat("@\n\n", file=Sw)
               INCODE <- FALSE
            }
               cat(sep="\n", substr(code[i],4,nchar(code[i])), file=Sw)
         } else {
                  if(!INCODE) {
                     block <- block+1
                     cat(sep="", "<<label=block", block,
                                 ", include=TRUE>>=\n", file=Sw)
                     INCODE <- TRUE
                  }
                  cat(sep="", code[i], "\n", file=Sw)
         }
      }# FOR i upto length(code)

      if(INCODE) cat("@\n", file=Sw)

      cat("\n", file=Sw)

      cat("\\begin{figure}[!ht]\n", file=Sw)
      cat("\\centering\n", file=Sw)
      cat("<<fig=TRUE, echo=FALSE, eval=TRUE, width=7, height=7, label=plot1>>=\n",
            file=Sw)
      cat("## Place here any relevant code to produce graphics\n", file=Sw)
      cat("@\n", file=Sw)
      cat("\\caption{R2Sweave: Sample figure.}\n", file=Sw)
      cat("\\label{plot1}\n", file=Sw)
      cat("\\end{figure}\n", file=Sw)
      cat("\\end{document}\n", file=Sw)

      cat(sep="", "Sweave document created: \"", SweaveFile, "\"\n")
      expr <- parse(prompt= "Select: 1) View file; Or 0) To Continue: ")
      choice <- 0
      choice <- try(eval(expr))
      if(choice==1)
         file.show(SweaveFile, title = SweaveFile, pager = getOption("pager"))
      close(Sw)
      cat("\n")
      cat(sep="", "Please edit and rename the file appropriately.\n")
      cat(sep="", "Or Type: Sweave(\"", SweaveFile, "\")\n")
   }
}

