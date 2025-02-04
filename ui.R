  # The UI is structured using a dashboard layout with three primary components:
  # 1. Header
  # 2. Sidebar
  # 3. Body 
  
  ui <- dashboardPage(
    # Dashboard Header: Displays the application title
    dashboardHeader(
        title = span("By Banzhaf & Moradigaravand Labs", style = "font-weight: bold; color: #FFFFFF; font-size: 15px;"), # Title with custom styling
        titleWidth = 300 # Fixed width for the header
    ),
    
    # Dashboard Sidebar: Contains the navigation menu
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("About", tabName = "about", icon = icon("info-circle")),
        menuItem("User Guide", tabName = "user_guide", icon = icon("book")),
        menuItem("E. coli", tabName = "ecoli", icon = icon("bacteria")),
        menuItem("Upload Your Dataset", tabName = "upload_your_dataset", icon = icon("upload"))
      )
    ),
  
    # Dashboard Body: Includes custom styling and dynamic content placeholders
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # Your custom CSS
        tags$link(
          href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap",
          rel = "stylesheet"
        ), # Google Fonts link
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1") # Responsive meta tag
      ),
      
      
    tabItems(
      # Home tab content
      tabItem(
        tabName = "home",
        fluidRow(
          # Row for the application logo
          column(
            width = 12, # Full width on large screens
            div(
              style = "display: flex; justify-content: center; align-items: center;", 
              class = "centered", # Flexbox for centering content
              tags$img(
                src = paste0("data:image/png;base64,", encoded_logo),
                alt = "ChemGenXplore Logo", # Accessible text for the logo
                style = "max-width: 450px; width: 65%; min-width: 220px; height: auto; border-radius: 10px;" 
              )
            )
          )
        ),
        
        # Center the overview image below the title
        fluidRow(
          column(
            width = 12,
            tags$img(
              src = paste0("data:image/png;base64,", encoded_image),
              style = "max-width: 100%; height: auto; display: block; margin: 0 auto; border-radius: 10px;"
            )
          )
        ),       
        # Add space between the image and features
        br(),

        
        # Key Features Header
        fluidRow(
          column(
            width = 12,
            h2(
              "Key Features",
              style = "color: #00509E; font-weight: bold; font-size: 24px; text-align: left; margin: 15px 15px 20px 15px;"
            )
          )
        ),
        fluidRow(
          # Phenotype Visualization
          column(
            width = 6,
            div(
              style = "background-color: #D6EAF8; color: black; border: 2px solid #00509E; border-radius: 10px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);",
              h3(
                icon("chart-bar", style = "font-size: 20px; margin-right: 10px; color: #00509E;"),
                "Phenotype Visualisation",
                style = "margin: 0; font-weight: bold; font-size: 18px;"
              ),
              tags$p(
                "Visualise gene and condition phenotypes with interactive plots and tables, filtered by FDR thresholds.",
                style = "font-size: 14px; color: black; margin-top: 10px;"
              )
            )
          ),
          # Correlation Visualization
          column(
            width = 6,
            div(
              style = "background-color: #85C1E9; color: black; border: 2px solid #00509E; border-radius: 10px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);",
              h3(
                icon("project-diagram", style = "font-size: 20px; margin-right: 10px; color: #00509E;"),
                "Correlation Visualisation",
                style = "margin: 0; font-weight: bold; font-size: 18px;"
              ),
              tags$p(
                "Visualise gene and condition correlations with interactive plots and tables, filtered by FDR thresholds.",
                style = "font-size: 14px; color: black; margin-top: 10px;"
              )
            )
          )
        ),
        fluidRow(
          # Enrichment Analysis
          column(
            width = 6,
            div(
              style = "background-color: #D5F5E3; color: black; border: 2px solid #117A65; border-radius: 10px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);",
              h3(
                icon("flask", style = "font-size: 20px; margin-right: 10px; color: #117A65;"),
                "Enrichment Analysis",
                style = "margin: 0; font-weight: bold; font-size: 18px;"
              ),
              tags$p(
                "Perform GO and KEGG enrichment analysis to identify functional pathways and gene sets, with interactive plots and downloadable results.",
                style = "font-size: 14px; color: black; margin-top: 10px;"
              )
            )
          ),
          column(
            # Interactive Heatmaps
            width = 6,
            div(
              style = "background-color: #82E0AA; color: black; border: 2px solid #117A65; border-radius: 10px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);",
              h3(
                icon("th", style = "font-size: 20px; margin-right: 10px; color: #117A65;"),
                "Interactive Heatmaps",
                style = "margin: 0; font-weight: bold; font-size: 18px;"
              ),
              tags$p(
                "Visualise gene-condition fitness data with interactive heatmaps and dendrograms, offering clustering options and downloadable visualisations.",
                style = "font-size: 14px; color: black; margin-top: 10px;"
              )
            )
          )
        )
      ),
      
      # About Tab Content
      tabItem(
        tabName = "about",
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: #F9F9F9; border-radius: 10px; padding: 20px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
              
              # Objectives Section
              h3(
                tags$span(style = "color: #1ABC9C; margin-right: 10px;", tags$i(class = "fas fa-lightbulb")), 
                "Objectives", 
                style = "color: #34495E; font-weight: bold; margin-bottom: 15px; font-size: 18px;"
              ),
              p(
                "Chemical genomic screens are a valuable resource for uncovering gene functions and mapping biological pathways. However, their large-scale nature makes these datasets complex and challenging to interpret. ChemGenXplore addresses this challenge by simplifying access to these resources and providing researchers with an intuitive platform to explore and understand genotype-phenotype relationships across various species.",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 20px;"
              ),
              tags$hr(style = "border-color: #1ABC9C; margin: 20px 0;"),
              
              # Data Sources Section
              h3(
                tags$span(style = "color: #1ABC9C; margin-right: 10px;", tags$i(class = "fas fa-folder-open")), 
                "Data Sources", 
                style = "color: #34495E; font-weight: bold; margin-bottom: 15px; font-size: 18px;"
              ),
              p(
                "ChemGenXplore includes three pre-integrated, publicly available ",
                tags$i("Escherichia coli"),
                " datasets from chemical genomic screens. These datasets provide fitness scores across a wide range of conditions and serve as the foundation for the analyses performed in ChemGenXplore:",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 15px;"
              ),
              tags$ul(
                style = "font-size: 14px; color: #2C3E50; padding-left: 20px;",
                tags$li(
                  "Nichols et al., 2011: ",
                  tags$a(
                    href = "https://doi.org/10.1016/j.cell.2010.11.052",
                    "https://doi.org/10.1016/j.cell.2010.11.052", 
                    target = "_blank", 
                    style = "color: #0047AB; font-weight: normal; text-decoration: none;"
                  )
                ),
                tags$li(
                  "Shiver et al., 2016: ",
                  tags$a(
                    href = "https://doi.org/10.1371/journal.pgen.1006124",
                    "https://doi.org/10.1371/journal.pgen.1006124", 
                    target = "_blank", 
                    style = "color: #0047AB; font-weight: normal; text-decoration: none;"
                  )
                ),
                tags$li(
                  "Price et al., 2018: ",
                  tags$a(
                    href = "https://doi.org/10.1038/s41586-018-0124-0",
                    "https://doi.org/10.1038/s41586-018-0124-0", 
                    target = "_blank", 
                    style = "color: #0047AB; font-weight: normal; text-decoration: none;"
                  )
                )
              ),
              p(
                "Users also have the flexibility to upload their own datasets for analysis.",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 20px;"
              ),
              tags$hr(style = "border-color: #1ABC9C; margin: 20px 0;"),
              
              # Acknowledgment Section
              h3(
                tags$span(style = "color: #1ABC9C; margin-right: 10px;", tags$i(class = "fas fa-hands-helping")), 
                "Acknowledgment", 
                style = "color: #34495E; font-weight: bold; margin-bottom: 15px; font-size: 18px;"
              ),
              p(
                "ChemGenXplore was developed with support from the Banzhaf Lab and the Moradigaravand Lab.",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 10px;"
              ),
              p(
                "Institute of Microbiology and Infection, University of Birmingham, Birmingham, UK",
                style = "font-size: 14px; color: #2C3E50; margin-bottom: 5px;"
              ),
              p(
                "Biosciences Institute, Faculty of Medical Sciences, Newcastle University, Newcastle Upon Tyne, UK",
                style = "font-size: 14px; color: #2C3E50; margin-bottom: 5px;"
              ),
              p(
                "KAUST Computational Bioscience Research Centre, King Abdullah University of Science and Technology, Thuwal, Makkah, Saudi Arabia",
                style = "font-size: 14px; color: #2C3E50; margin-bottom: 20px;"
              ),
              p(
                "Financial support was provided by the Darwin Trust of Edinburgh.",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 20px;"
              ),
              tags$hr(style = "border-color: #1ABC9C; margin: 20px 0;"),
              
              # Contact Section
              h3(
                tags$span(style = "color: #1ABC9C; margin-right: 10px;", tags$i(class = "fas fa-envelope")), 
                "Contact Information", 
                style = "color: #34495E; font-weight: bold; margin-bottom: 15px; font-size: 18px;"
              ),
              p(
                "For questions or suggestions for additional features, please contact:",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 10px;"
              ),
              p(
                "Huda Ahmad: ",
                tags$a(href = "mailto:hxa105@student.bham.ac.uk", "hxa105@student.bham.ac.uk", 
                       style = "color: #0047AB; text-decoration: none; font-weight: normal;"),
                style = "font-size: 14px; color: #2C3E50; margin-bottom: 10px;"
              ),
              p(
                "Manuel Banzhaf: ",
                tags$a(href = "mailto:manuel.banzhaf@newcastle.ac.uk", "manuel.banzhaf@newcastle.ac.uk", 
                       style = "color: #0047AB; text-decoration: none; font-weight: normal;"),
                style = "font-size: 14px; color: #2C3E50; margin-bottom: 10px;"
              ),
              p(
                "Danesh Moradigaravand: ",
                tags$a(href = "mailto:danesh.moradigaravand@kaust.edu.sa", "danesh.moradigaravand@kaust.edu.sa", 
                       style = "color: #0047AB; text-decoration: none; font-weight: normal;"),
                style = "font-size: 14px; color: #2C3E50;"
              )
            )
          )
        )
      ),
      # User Guide tab content
      tabItem(
        tabName = "user_guide",
        fluidRow(
          column(
            width = 12,
            h2(
              "User Guide",
              style = "color: #1ABC9C; font-size: 30px; font-weight: bold; margin-left: 15px;"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$div(
              style = "margin: 15px; font-size: 16px; line-height: 1.6;",
              bsCollapse(
                id = "userGuideAccordion",
                open = "Navigation",  # Default open section
                
                # Navigation Section
                bsCollapsePanel(
                  title = "Navigation",
                  style = "primary",
                  tags$p("ChemGenXplore interface includes the following sections:"),
                  tags$ul(
                    tags$li("Home: Overview of ChemGenXplore and its key features."),
                    tags$li("About: Objectives, data sources, contributors, and contact information"),
                    tags$li("User Guide: Detailed instructions on using each component of the app."),
                    tags$li("E. coli: Analyse preloaded Escherichia coli datasets."),
                    tags$li("Upload Your Dataset: Upload a custom dataset for analysis.")
                  )
                ),
                
                
                # E. coli Section
                bsCollapsePanel(
                  title = "E. coli",
                  style = "success",
                  tags$p(
                    "This section provides an overview of the analyses available for the preloaded E. coli datasets. All analysis types allow users to adjust False Discovery Rate (FDR) thresholds to refine results and access interactive plots and tables for download."
                  ),
                  
                  # Phenotypes Visualization Section
                  bsCollapsePanel(
                    title = "Phenotypes Visualisation",
                    style = "info",
                    tags$p(
                      "This section focuses on visualising the number of phenotypes associated with selected genes or conditions across preloaded datasets. Users can adjust False Discovery Rate (FDR) thresholds to identify statistically significant phenotypes. Results are displayed as interactive bar plots and tables, with the option to download both for further analysis. An additional table is available, listing all phenotypes without applying the FDR threshold for a comprehensive overview."
                    ),
                    tags$h4(
                      "Gene-Level",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li("Navigate to 'E. coli' > 'Phenotypes' > 'Gene-Level'."),
                      tags$li("Select one or more genes of interest using the dropdown menu."),
                      tags$li("Adjust the FDR threshold slider to filter significant phenotypes."),
                      tags$li("View results as interactive bar plots and data tables."),
                      tags$li("Download plots and tables using the provided buttons.")
                    ),
                    tags$hr(style = "border-color: #D3D3D3; margin: 20px 0;"),
                    tags$h4(
                      "Condition-Level",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li("Navigate to 'E. coli' > 'Phenotypes' > 'Condition-Level'."),
                      tags$li("Select one or more conditions of interest using the dropdown menu."),
                      tags$li("Adjust the FDR threshold slider to filter significant phenotypes."),
                      tags$li("View results as interactive bar plots and data tables."),
                      tags$li("Download plots and tables using the provided buttons.")
                    )
                  ),
                  
                  # Correlation Analysis Section
                  bsCollapsePanel(
                    title = "Correlation Analysis",
                    style = "info",
                    tags$p(
                      "This section focuses on exploring correlations between genes or conditions based on their fitness profiles across preloaded datasets. Users can adjust False Discovery Rate (FDR) thresholds to identify statistically significant correlations. Results are displayed as interactive bar plots and tables, with the option to download both for further analysis. An additional table is available, listing all correlations without applying the FDR threshold for a comprehensive overview."
                    ),
                    tags$h4(
                      "Gene-Level",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li("Navigate to 'E. coli' > 'Correlation Analysis' > 'Gene-Level'."),
                      tags$li("Select one or more genes using the dropdown menu."),
                      tags$li("Adjust the FDR threshold slider to filter significant correlations."),
                      tags$li("View correlation results as bar plots and data tables."),
                      tags$li("Download plots and tables using the provided buttons.")
                    ),
                    tags$hr(style = "border-color: #D3D3D3; margin: 20px 0;"),
                    tags$h4(
                      "Condition-Level",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li("Navigate to 'E. coli' > 'Correlation Analysis' > 'Condition-Level'."),
                      tags$li("Select one or more conditions of interest using the dropdown menu."),
                      tags$li("Adjust the FDR threshold slider to filter significant correlations."),
                      tags$li("View correlation results as bar plots and data tables."),
                      tags$li("Download plots and tables using the provided buttons.")
                    )
                  ),
                  
                  # Enrichment Analysis Section
                  bsCollapsePanel(
                    title = "Enrichment Analysis",
                    style = "info",
                    tags$p(
                      "This section focuses on identifying biological processes or pathways associated with a set of genes through enrichment analysis. Both Gene Ontology (GO) and KEGG pathway enrichment analysis are supported. Users can adjust False Discovery Rate (FDR) thresholds to refine results. Results are displayed as interactive bar plots and tables, with the option to download both for further analysis. An additional table is available, listing all enrichment results without applying the FDR threshold for a comprehensive overview."
                    ),
                    tags$h4(
                      "GO Enrichment",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li("Navigate to 'E. coli' > 'Enrichment Analysis' > 'GO Enrichment'."),
                      tags$li("Select one or more genes using the dropdown menu."),
                      tags$li("Adjust the FDR threshold slider to filter significant results."),
                      tags$li("View results as bar plots and data tables."),
                      tags$li("Download plots and tables using the provided buttons.")
                    ),
                    tags$hr(style = "border-color: #D3D3D3; margin: 20px 0;"),
                    tags$h4(
                      "KEGG Enrichment",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li("Navigate to 'E. coli' > 'Enrichment Analysis' > 'KEGG Enrichment'."),
                      tags$li("Repeat the steps outlined for GO Enrichment.")
                    )
                  ),
                  
                  # Heatmaps Section
                  bsCollapsePanel(
                    title = "Heatmaps",
                    style = "info",
                    tags$p(
                      "This section focuses on visualising fitness data across genes and conditions through interactive heatmaps. Users can customise clustering options and distance metrics to refine the visualisation. Results are displayed as interactive heatmaps, with the option to download both the heatmap images and the associated data tables for further analysis."
                    ),
                    tags$ul(
                      tags$li("Navigate to 'E. coli' > 'Heatmaps'."),
                      tags$li("Select datasets, genes, and conditions to generate heatmaps."),
                      tags$li("Options include clustering rows or columns, choosing distance metrics, and downloading heatmap images and data tables.")
                    )
                  )
                ),
                
                # Upload Your Dataset Section
                bsCollapsePanel(
                  title = "Uploading a Dataset",
                  style = "warning",
                  tags$p(
                    "This section allows users to upload custom datasets for analysis. Uploaded datasets must be correctly formatted for compatibility, with rows representing genes or mutants and columns representing conditions. Once uploaded, users can explore phenotypes, correlations, enrichment analysis, and heatmap generation using the same tools and workflows available in the E. coli tab."
                  ),
                  tags$ul(
                    tags$li("Navigate to 'Upload Your Dataset'."),
                    tags$li("Upload a CSV file where rows represent genes/mutants and columns represent conditions."),
                    tags$li("Follow the same steps for phenotypes, correlation analysis, enrichment analysis, and heatmap generation as the E. coli tab.")
                  )
                ),
                
                # Tips and Troubleshooting Section
                bsCollapsePanel(
                  title = "Tips and Troubleshooting",
                  style = "danger",
                  tags$p(
                    "This section provides guidance for effectively using ChemGenXplore and resolving common issues encountered during data analysis. Following these tips ensures a smoother experience and more reliable results."
                  ),
                  tags$ul(
                    tags$li("Ensure the uploaded dataset contains numeric values for analysis."),
                    tags$li("Check column and row names for invalid characters or formatting issues."),
                    tags$li("If no data appears, verify that your selections align with the available data in the app.")
                  )
                )
              )
            )
          )
        )
      ),
      
      # E.coli tab content
      tabItem(
        tabName = "ecoli",
        fluidRow(
          box(
            width = 12,
            tabsetPanel(
              type = "tabs",
              
              # Phenotypes Parent Tab
              tabPanel(
                title = tagList(icon("dna"), "Phenotypes"),
                tabsetPanel(
                  type = "tabs",
                  
                  # Gene-Level Tab
                  tabPanel(
                    "Gene-Level",
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput(
                          "Gene1", 
                          "Select or Type Genes:", 
                          choices = NULL,  # Populate with all available genes
                          multiple = TRUE
                        ),
                        
                        sliderInput(
                          "FDRq", 
                          "% FDR for phenotypes (qvals)", 
                          min = 0, 
                          max = 100, 
                          step = 0.1, 
                          value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Phenotypes",
                            shinycssloaders::withSpinner(plotlyOutput("barplotps")),
                            downloadButton('download', "Download Table"),
                            downloadButton('downloadPlot', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("genes_scores_sig_filetable"))
                          ),
                          tabPanel(
                            "All Phenotypes",
                            shinycssloaders::withSpinner(DT::dataTableOutput("genes_scores_filetable")),
                            downloadButton('downloadAllGenesPhenotypes', "Download Table")  # Add this download button
                          )
                          
                        )
                      )
                    )
                  ),
                  
                  # Condition-Level Tab
                  tabPanel(
                    "Condition-Level",
                    sidebarLayout(
                      sidebarPanel(
                        
                        selectizeInput(
                          "cond1", 
                          "Select or Type Conditions:", 
                          choices = NULL, 
                          multiple = TRUE
                        ),
                        
                        sliderInput(
                          "FDRqc", 
                          "% FDR for phenotypes (qvals)", 
                          min = 0, 
                          max = 100, 
                          step = 0.1, 
                          value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Phenotypes",
                            shinycssloaders::withSpinner(plotlyOutput("barplotpsc")),
                            downloadButton('downloadc', "Download Data"),
                            downloadButton('downloadPlotc', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("conditions_scores_sig_filetable"))
                          ),
                          tabPanel(
                            "All Phenotypes",
                            shinycssloaders::withSpinner(DT::dataTableOutput("conditions_scores_filetable")),
                            downloadButton('downloadAllConditionsPhenotypes', "Download Table")  
                            
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              # Correlation Analysis Parent Tab
              tabPanel(
                title = tagList(icon("project-diagram"), "Correlation Analysis"),
                tabsetPanel(
                  type = "tabs",
                  # Gene-Level Tab
                  tabPanel("Gene-Level",
                           sidebarLayout(
                             sidebarPanel(
                               selectizeInput(
                                 "Gene2", 
                                 "Select or Type Genes:",
                                 choices = NULL, 
                                 multiple = TRUE
                               ),
                               sliderInput("FDRq2", "% FDR for genes correlations (qvals)", min = 0, max = 100, step = 0.1, value = 5),
                               width = 3
                             ),
                             mainPanel(
                               tabsetPanel(
                                 type = "tabs",
                                 tabPanel("Significant Correlations",
                                          shinycssloaders::withSpinner(plotlyOutput("barplot")),
                                          downloadButton('download_gene_cor', "Download Data"),
                                          downloadButton('downloadcorplot', "Download Plot"),
                                          shinycssloaders::withSpinner(DT::dataTableOutput("genes_correlations_sig_filetable"))
                                          
                                 ),
                                 tabPanel(
                                   "All Correlations",
                                   shinycssloaders::withSpinner(DT::dataTableOutput("genes_correlations_filetable")),
                                   downloadButton('downloadAllCorrelations', "Download Table")  # Add this download button
                                 )
                               )
                             )
                           )
                  ),
                  
                  # Condition-Level Tab
                  tabPanel("Condition-Level",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(
                                 "cond2", 
                                 "Select or Type Conditions:", 
                                 choices = NULL,
                                 multiple = TRUE),
                               sliderInput("FDRq3", "% FDR for conditions correlations (qvals)", min = 0, max = 100, step = 0.1, value = 5),
                               width = 3
                             ),
                             mainPanel(
                               tabsetPanel(
                                 type = "tabs",
                                 tabPanel("Significant Correlations",
                                          shinycssloaders::withSpinner(plotlyOutput("barplot2")),
                                          downloadButton('download_cond_cor', "Download Data"),
                                          downloadButton('downloadcorplot2', "Download Plot"),
                                          shinycssloaders::withSpinner(DT::dataTableOutput("conditions_correlations_sig_filetable"))
                                 ),
                                 tabPanel(
                                   "All Correlations",
                                   shinycssloaders::withSpinner(DT::dataTableOutput("conditions_correlations_filetable")),
                                   downloadButton('downloadAllCondCorrelations', "Download Table")  # Add this download button
                                 )))
                           )
                  )
                )
              ),
              
              # Enrichment Analysis Parent Tab
              tabPanel(
                title = tagList(icon("chart-bar"), "Enrichment Analysis"),
                tabsetPanel(
                  type = "tabs",
                  # GO Enrichment tab
                  tabPanel("GO Enrichment",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("Gene3", "Select or Type Genes:", choices = NULL, multiple = TRUE),
                               sliderInput("FDRq4", "FDR for GO enrichment (qvals)", min = 0, max = 100, step = 0.1, value = 100),
                               width = 3
                             ),
                             mainPanel(
                               tabsetPanel(
                                 type = "tabs",
                                 tabPanel("Significant GO Enrichment",
                                          shinycssloaders::withSpinner(plotlyOutput("barplotGO")),
                                          downloadButton('downloadgos', "Download Data"),
                                          downloadButton('downloadgoplot', "Download Plot"),
                                          shinycssloaders::withSpinner(DT::dataTableOutput("filetableGOs"))
                                 ),
                                 tabPanel(
                                   "All GO Enrichment",
                                   shinycssloaders::withSpinner(DT::dataTableOutput("filetableGO")),
                                   downloadButton('downloadgo', "Download Table")  # 
                                 ))))
                  ),
                  
                  # KEGG Enrichment tab
                  tabPanel("KEGG Enrichment",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("Gene4", "Select or Type Genes:", choices = NULL, multiple = TRUE),
                               sliderInput("FDRq5", "FDR for KEGG enrichment (qvals)", min = 0, max = 100, step = 0.1, value = 100),
                               width = 3
                             ),
                             mainPanel(
                               tabsetPanel(
                                 type = "tabs",
                                 tabPanel("Significant KEGG Enrichment",
                                          shinycssloaders::withSpinner(plotlyOutput("barplotKEGG")),
                                          downloadButton('downloadkeggs', "Download Data"),
                                          downloadButton('downloadkeggplot', "Download Plot"),
                                          shinycssloaders::withSpinner(DT::dataTableOutput("filetableKEGGs"))
                                 ),
                                 tabPanel(
                                   "All KEGG Enrichment",
                                   shinycssloaders::withSpinner(DT::dataTableOutput("filetableKEGG")),
                                   downloadButton('downloadkegg', "Download Table") 
                                 )))
                           )
                  )
                )
              ),
              
              # Heatmaps Tab
              tabPanel(
                title = tagList(icon("th"), "Heatmaps"),
                sidebarLayout(
                  sidebarPanel(
                    # Add dataset selector
                    uiOutput("dataset_selector"),  # Dropdown to select the dataset
                    
                    # Existing selectors for genes and conditions
                    uiOutput("gene_selector"),
                    uiOutput("condition_selector"),
                    
                    # Clustering options
                    checkboxInput("cluster_rows", "Cluster Genes", value = TRUE),
                    checkboxInput("cluster_columns", "Cluster Conditions", value = FALSE),
                    
                    # Clustering and distance method options
                    selectInput(
                      "clustering_method", "Clustering Method", 
                      choices = c("complete", "single", "average", "ward.D")
                    ),
                    selectInput(
                      "distance_metric", "Distance Metric", 
                      choices = c("euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski")
                    ),
                    # Add download button for heatmap data
                    downloadButton("downloadHeatmapData", "Download Heatmap Data"),
                    downloadButton("downloadInteractiveHeatmapPlot", "Download Interactive Heatmap Plot"),
                    downloadButton("downloadDendrogramPlot", "Download Dendrogram Plot")
                    
                  ),
                  
                  mainPanel(
                    # Add tabs for heatmap visualizations
                    tabsetPanel(
                      type = "tabs",
                      tabPanel(
                        "Interactive Heatmap",
                        shinycssloaders::withSpinner(
                          plotlyOutput("heatmap_interactive", height = "800px", width = "100%")
                        )
                      ),
                      tabPanel(
                        "Dendrogram Heatmap",
                        shinycssloaders::withSpinner(
                          plotOutput("heatmap_dendrogram", height = "800px", width = "100%")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # Upload Your Dataset Tab
      tabItem(
        tabName = "upload_your_dataset",
        fluidRow(
          box(
            title = "Upload Your Dataset",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            fileInput(
              inputId = "file_upload",
              label = "Upload your dataset (.csv format):",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".rds"),
              width = "25%"
            ),
            
            tags$p("Please upload a CSV file where rows represent genes/mutants, and columns represent conditions. Example Dataset Below:"),
            tags$img(
              src = paste0("data:image/png;base64,", encoded_example_dataset),
              alt = "Example dataset format",
              style = "max-width: 27%; height: auto; border: 1px solid #ddd; padding: 10px;"
            ),
            tabsetPanel(
              type = "tabs",
              
              # Phenotypes Parent Tab
              tabPanel(
                title = tagList(icon("dna"), "Phenotypes"),
                tabsetPanel(
                  type = "tabs",
                  
                  # Gene-Level Tab
                  tabPanel(
                    "Gene-Level",
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput(
                          "Gene5", 
                          "Select or Type Genes:", 
                          choices = NULL,  # Populate with all available genes
                          multiple = TRUE
                        ),
                        sliderInput(
                          "FDRq_5", 
                          "% FDR for phenotypes (qvals)", 
                          min = 0, 
                          max = 100, 
                          step = 0.1, 
                          value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Phenotypes",
                            shinycssloaders::withSpinner(plotlyOutput("barplotgenesig")),
                            downloadButton('downloadGeneSignificant', "Download Table"),
                            downloadButton('downloadGenePlot', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("filetablegenesig"))
                          ),
                          tabPanel(
                            "All Phenotypes",
                            shinycssloaders::withSpinner(DT::dataTableOutput("filetablegene")),
                            downloadButton('downloadAllGenesPhenotypes_2', "Download Table")
                          )
                        )
                      )
                    )
                  ),
                  
                  # Condition-Level Tab
                  tabPanel(
                    "Condition-Level",
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput(
                          "Condition1", 
                          "Select or Type Conditions:", 
                          choices = NULL,  # Populate with all available conditions
                          multiple = TRUE
                        ),
                        sliderInput(
                          "FDRq1", 
                          "% FDR for conditions (qvals)", 
                          min = 0, 
                          max = 100, 
                          step = 0.1, 
                          value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Phenotypes",
                            shinycssloaders::withSpinner(plotlyOutput("barplotconditionsig")),
                            downloadButton('downloadConditionSignificant', "Download Table"),
                            downloadButton('downloadConditionPlot', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("filetableconditionsig"))
                          ),
                          tabPanel(
                            "All Phenotypes",
                            shinycssloaders::withSpinner(DT::dataTableOutput("filetablecondition")),
                            downloadButton('downloadAllConditionsPhenotypes_2', "Download Table")
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              # Correlation Parent Tab
              tabPanel(
                title = tagList(icon("project-diagram"), "Correlations"),
                tabsetPanel(
                  type = "tabs",
                  
                  # Gene-Level Tab
                  tabPanel(
                    "Gene-Level",
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput("run_correlation_ui"),  # Action button 
                        selectizeInput(
                          "Gene6",
                          "Select or Type Genes:",
                          choices = NULL,  # Dynamically populated
                          multiple = TRUE
                        ),
                        sliderInput(
                          "FDRq6",
                          "% FDR for gene correlations (qvals)",
                          min = 0,
                          max = 100,
                          step = 0.1,
                          value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Correlations",
                            shinycssloaders::withSpinner(plotlyOutput("barplotgenecorsig")),
                            downloadButton('downloadGeneCorSignificant', "Download Table"),
                            downloadButton('downloadGeneCorPlot', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("filetablegenecorsig"))
                          ),
                          tabPanel(
                            "All Correlations",
                            shinycssloaders::withSpinner(DT::dataTableOutput("filetablegenecor")),
                            downloadButton('downloadAllGenesCorrelations', "Download Table")
                          )
                        )
                      )
                    )
                  ),
                  
                  # Condition-Level Tab
                  tabPanel(
                    "Condition-Level",
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput(
                          "Condition2",
                          "Select or Type Conditions:",
                          choices = NULL,  # Dynamically populated
                          multiple = TRUE
                        ),
                        sliderInput(
                          "FDRq3",
                          "% FDR for condition correlations (qvals)",
                          min = 0,
                          max = 100,
                          step = 0.1,
                          value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Correlations",
                            shinycssloaders::withSpinner(plotlyOutput("barplotconditioncorsig")),
                            downloadButton('downloadConditionCorSignificant', "Download Table"),
                            downloadButton('downloadConditionCorPlot', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("filetableconditioncorsig"))
                          ),
                          tabPanel(
                            "All Correlations",
                            shinycssloaders::withSpinner(DT::dataTableOutput("filetableconditioncor")),
                            downloadButton('downloadAllConditionsCorrelations', "Download Table")
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              # Heatmaps Tab
              tabPanel(
                title = tagList(icon("th"), "Heatmaps"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Existing selectors for genes and conditions
                    uiOutput("gene_selector2"),
                    uiOutput("condition_selector2"),
                    
                    # Clustering options
                    checkboxInput("cluster_rows2", "Cluster Genes", value = TRUE),
                    checkboxInput("cluster_columns2", "Cluster Conditions", value = FALSE),
                    
                    # Clustering and distance method options
                    selectInput(
                      "clustering_method2", "Clustering Method", 
                      choices = c("complete", "single", "average", "ward.D")
                    ),
                    selectInput(
                      "distance_metric2", "Distance Metric", 
                      choices = c("euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski")
                    ),
                    # Add download button for heatmap data
                    downloadButton("downloadHeatmapData2", "Download Heatmap Data"),
                    downloadButton("downloadInteractiveHeatmapPlot2", "Download Interactive Heatmap Plot"),
                    downloadButton("downloadDendrogramPlot2", "Download Dendrogram Plot")
                    ),
                  
                  mainPanel(
                    # Add tabs for heatmap visualizations
                    tabsetPanel(
                      type = "tabs",
                      tabPanel(
                        "Interactive Heatmap",
                        shinycssloaders::withSpinner(
                          plotlyOutput("heatmap_interactive2", height = "800px", width = "100%")
                        )
                      ),
                      tabPanel(
                        "Dendrogram Heatmap",
                        shinycssloaders::withSpinner(
                          plotOutput("heatmap_dendrogram2", height = "800px", width = "100%")
                        )
                      )
                    )
                  )
                )
              )
              
            )
          )
        )
      )
    ),
    # Footer Section
    tags$div(
      style = "text-align: center; margin-top: 20px;",
      tags$p(
        "© 2025 ChemGenXplore | Developed by Huda Ahmad",
        style = "font-size: 14px; color: #2C3E50;"
      )
    )
  )
)


