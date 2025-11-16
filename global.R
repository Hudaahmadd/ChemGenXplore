# Load necessary libraries
# Core Shiny libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(slickR)
library(data.table)

# Data manipulation and visualisation
library(DT)
library(dplyr)
library(ggplot2)
library(viridis)
library(plotly)
library(reshape2)
library(tidyr)
library(tidyverse)
library(BH)

# Advanced plotting
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)

# Additional utilities
library(base64enc)
library(shinycssloaders)
library(shinyWidgets)
library(dendextend)
library(magrittr)
library(stringr)
library(data.table)
library(Hmisc)
library(readr)

# Encode images
encoded_image <- base64encode("www/overview_figure.png")
encoded_logo <- base64encode("www/logo.png")
encoded_example_dataset <- base64encode("www/example_dataset.png")

# Increase maximum file upload size
options(shiny.maxRequestSize = 1000 * 1024^2)

# Define shared variables or functions
default_theme <- shinythemes::shinytheme("united")

# Enable detailed error tracing for debugging
options(shiny.error = function() {
  traceback(3)
  cat("An error occurred! Check logs for details.\n")
})





