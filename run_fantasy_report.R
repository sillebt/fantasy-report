library(rmarkdown)

portfolio_name <- "Sleeper"
output_file    <- "sleeper.pdf"

rmarkdown::render(
  input         = "report.Rmd",
  output_format = "pdf_document",
  output_file   = output_file,
  output_dir = "report/",
  clean = TRUE
  )



