write("", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex",append=FALSE)
resultDirectory<-"experimentalTEST1/NSGAIIStudy2/data"
latexHeader <- function() {
  write("\\documentclass{article}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\title{StandardStudy}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\usepackage{amssymb}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\author{A.J.Nebro}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\begin{document}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\maketitle", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\section{Tables}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
}

latexTableHeader <- function(problem, tabularString, latexTableFirstLine) {
  write("\\begin{table}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\caption{", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write(problem, "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write(".EP.}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)

  write("\\label{Table:", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write(problem, "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write(".EP.}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)

  write("\\centering", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\begin{scriptsize}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\begin{tabular}{", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write(tabularString, "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write(latexTableFirstLine, "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\hline ", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
}

printTableLine <- function(indicator, algorithm1, algorithm2, i, j, problem) { 
  file1<-paste(resultDirectory, algorithm1, sep="/")
  file1<-paste(file1, problem, sep="/")
  file1<-paste(file1, indicator, sep="/")
  data1<-scan(file1)
  file2<-paste(resultDirectory, algorithm2, sep="/")
  file2<-paste(file2, problem, sep="/")
  file2<-paste(file2, indicator, sep="/")
  data2<-scan(file2)
  if (i == j) {
    write("-- ", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  }
  else if (i < j) {
    if (is.finite(wilcox.test(data1, data2)$p.value) & wilcox.test(data1, data2)$p.value <= 0.05) {
      if (median(data1) <= median(data2)) {
        write("$\\blacktriangle$", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
      }
      else {
        write("$\\triangledown$", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE) 
      }
    }
    else {
      write("--", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE) 
    }
  }
  else {
    write(" ", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  }
}

latexTableTail <- function() { 
  write("\\hline", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\end{tabular}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\end{scriptsize}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
  write("\\end{table}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
}

latexTail <- function() { 
  write("\\end{document}", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
}

### START OF SCRIPT 
# Constants
problemList <-c("ZDT1", "ZDT2", "ZDT3", "ZDT4", "ZDT6") 
algorithmList <-c("NSGAIIa", "NSGAIIb", "NSGAIIc", "NSGAIId") 
tabularString <-c("lccc") 
latexTableFirstLine <-c("\\hline  & NSGAIIb & NSGAIIc & NSGAIId\\\\ ") 
indicator<-"EP"

 # Step 1.  Writes the latex header
latexHeader()
tabularString <-c("| l | p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm } | p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm } | p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm }p{0.15cm } | ") 

latexTableFirstLine <-c("\\hline \\multicolumn{1}{|c|}{} & \\multicolumn{5}{c|}{NSGAIIb} & \\multicolumn{5}{c|}{NSGAIIc} & \\multicolumn{5}{c|}{NSGAIId} \\\\") 

# Step 3. Problem loop 
latexTableHeader("ZDT1 ZDT2 ZDT3 ZDT4 ZDT6 ", tabularString, latexTableFirstLine)

indx = 0
for (i in algorithmList) {
  if (i != "NSGAIId") {
    write(i , "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
    write(" & ", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)

    jndx = 0
    for (j in algorithmList) {
      for (problem in problemList) {
        if (jndx != 0) {
          if (i != j) {
            printTableLine(indicator, i, j, indx, jndx, problem)
          }
          else {
            write("  ", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
          } 
          if (problem == "ZDT6") {
            if (j == "NSGAIId") {
              write(" \\\\ ", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
            } 
            else {
              write(" & ", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
            }
          }
     else {
    write("&", "experimentalTEST1/NSGAIIStudy2/R/EP.Wilcoxon.tex", append=TRUE)
     }
        }
      }
      jndx = jndx + 1
    }
    indx = indx + 1
  }
} # for algorithm

  latexTableTail()

#Step 3. Writes the end of latex file 
latexTail()

