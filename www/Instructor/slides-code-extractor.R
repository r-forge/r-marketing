# get R code from .Rpres file and make a .R file
#

wd <- "/Users/cchapman/Documents/Chris Documents/papers/repos/RIntroBook/Instructor/"

filelist <- c(
  "Chapter2/Chapter2-ChapmanFeit",
  "Chapter3/Chapter3-ChapmanFeit",
  "Chapter4/Chapter4-ChapmanFeit",
  "Chapter5/Chapter5-ChapmanFeit",
  "Chapter6/Chapter6-ChapmanFeit",
  "Chapter7/Chapter7-ChapmanFeit",
  "Chapter9/Chapter9-ChapmanFeit",
  "Chapter12/Chapter12-ChapmanFeit",
  "Chapter13/Chapter13-ChapmanFeit"
)

for (filename in filelist) {
  # open files to read and write
  infile <- file(paste0(wd, filename, ".Rpres"), "rt")
  outfile <- file(paste0(wd, filename, "-slides.R"), "wt")

  # read file in
  linesin <- readLines(infile)
  
  # iterate over lines and write code chunks to outfile
  cat(paste0("# R code snippets from slides for Chapman & Feit 2015\n",
             "# Slide file: ", filename, "\n\n",
             "# All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/",
             "\n\n# ==========\n\n"), file=outfile)

  chunkon <- FALSE    # are we in an R chunk?
  slideon <- TRUE     # are we at the top of a slide?

  for (i in seq_along(linesin)) {
    if (chunkon & grepl('```', linesin[i], fixed=TRUE)) {   # code block ends
      chunkon <- FALSE
      cat("\n", file=outfile)
    } else if (grepl('====', linesin[i], fixed=TRUE)) {    # slide divider
      slideon <- TRUE
      if (i > 1) {
        slidetitle <- linesin[i-1]
      }
    } else if (chunkon) {                                   # code to output
      cat(linesin[i], "\n", file=outfile, sep="")
      slideon <- FALSE
    } else if (grepl('```{r', linesin[i], fixed=TRUE)) {    # code block starts
      if (slideon)  {                                       # write slide title
          cat("\n# ", slidetitle, "\n", file=outfile, sep="")   # slide title
          cat("# ==========\n", file=outfile)
      }
      slideon <- FALSE
      chunkon <- TRUE
    }
  }
  
  # close all
  close(outfile)
  close(infile)
    
}

