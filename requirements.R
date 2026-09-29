required_packages <- c(
  "shiny",
  "data.table",
  "dplyr",
  "ggplot2",
  "DT",
  "plotly",
  "scales",
  "htmltools"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

message("All required R packages are installed.")
