# =============================================================================
# Fantasy Report Runner
# =============================================================================
# This script generates all fantasy report outputs and renders the PDF report.
#
# Usage:
#   1. Generate tables only:  source("R/main.R"); generate_all_reports()
#   2. Generate full report:  source("run_fantasy_report.R")
# =============================================================================

# -----------------------------------------------------------------------------
# Step 1: Generate all tables and visualizations
# -----------------------------------------------------------------------------

message("Step 1: Generating all tables and visualizations...")
source("R/main.R")
data <- generate_all_reports()

# -----------------------------------------------------------------------------
# Step 2: Render the PDF report
# -----------------------------------------------------------------------------

message("\nStep 2: Rendering PDF report...")
library(rmarkdown)

# Create report output directory if it doesn't exist
if (!dir.exists("report")) {
  dir.create("report")
}

rmarkdown::render(
  input         = "report.Rmd",
  output_format = "pdf_document",
  output_file   = "sleeper.pdf",
  output_dir    = "report/",
  clean         = TRUE
)

message("\nComplete! Report saved to: report/sleeper.pdf")
