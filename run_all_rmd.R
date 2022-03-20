run_all_rmd <- function(filename) {
  tempR <- tempfile(fileext = ".R")
  knitr::purl(filename, output=tempR)
  source(tempR , echo = TRUE)
  unlink(tempR)
}