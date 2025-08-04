# CRAN packages used in the app
cran_packages <- c(
  "shiny", "shinydashboard", "shinythemes", "shinyjs", "shinyBS", "slickR",
  "data.table", "DT", "dplyr", "ggplot2", "viridis", "plotly",
  "reshape2", "tidyr", "tidyverse", "BH", "base64enc",
  "shinycssloaders", "shinyWidgets", "dendextend", "magrittr",
  "stringr", "Hmisc"
)

# Install missing CRAN packages
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Install BiocManager if needed
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# Bioconductor packages used in the app
bioc_packages <- c("ComplexHeatmap", "InteractiveComplexHeatmap")

# Install missing Bioconductor packages
for (pkg in bioc_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    BiocManager::install(pkg)
  }
}
