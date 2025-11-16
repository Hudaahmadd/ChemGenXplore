# The UI is structured using a dashboard layout with three primary components:
# 1. Header
# 2. Sidebar
# 3. Body 

# helper 
cgx_selectize <- function(inputId, label, choices = NULL, multiple = TRUE) {
  selectizeInput(
    inputId, label,
    choices = choices, multiple = multiple,
    options = list(
      onPaste = I("window.CGX_onPaste"),
      plugins = NULL,
      create  = TRUE,
      persist = FALSE,
      render = I("{
        item: function(item, escape) {
          var lbl = item.label || item.text || item.value || '';
          return '<div class=\"item-no-x\">' + escape(lbl) + '</div>';
        }
      }")
    )
  )
}

ui <- dashboardPage(
  title = "ChemGenXplore",  # browser tab title
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
      menuItem("S. cerevisiae", tabName = "yeast", icon = icon("bread-slice")),
      menuItem("Upload Your Dataset", tabName = "upload_your_dataset", icon = icon("upload")),
      menuItem("Omics Integration", tabName = "omics", icon = icon("layer-group"))
      
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
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"), # Responsive meta tag
        
        # ⬇️ ADD THIS JAVASCRIPT SNIPPET BELOW
        tags$script(HTML(
          "window.CGX_onPaste = function(text){
   var s = this; // selectize instance
   var items = text.split(/\\r?\\n|,|;|\\t|\\s+/)
                   .map(function(x){return x.trim();})
                   .filter(Boolean);

   var toAdd = [];
   var optionKeys = Object.keys(s.options || {});
   items.forEach(function(v){
     if (s.options[v] !== undefined) { toAdd.push(v); return; }
     // case-insensitive fallback
     var found = optionKeys.find(function(k){ return k.toLowerCase() === v.toLowerCase(); });
     if (found !== undefined) { toAdd.push(found); return; }
     // allow new values if create=TRUE
     if (s.settings.create) {
       s.addOption({value:v, text:v});
       toAdd.push(v);
     }
   });

   var current = s.getValue();
   if (!Array.isArray(current)) current = current ? [current] : [];
   var merged = Array.from(new Set(current.concat(toAdd)));
   s.setValue(merged, true);
   return false;
};")
        )
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
            ,
            
            # Upload Your Dataset 
            column(
              width = 6,
              div(
                style = "background-color: #EAF4FC; color: black; border: 2px solid #00509E; border-radius: 10px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);",
                h3(
                  icon("upload", style = "font-size: 20px; margin-right: 10px; color: #00509E;"),
                  "Upload Your Dataset",
                  style = "margin: 0; font-weight: bold; font-size: 18px;"
                ),
                tags$p(
                  "Upload your scored dataset to run FDR analysis and visualise phenotypes, correlations, and heatmaps.",
                  style = "font-size: 14px; color: black; margin-top: 10px;"
                )
              )
            ),
            
            # Omics Integration 
            column(
              width = 6,
              div(
                style = "background-color: #B7DBF2; color: black; border: 2px solid #00509E; border-radius: 10px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);",
                h3(
                  icon("layer-group", style = "font-size: 20px; margin-right: 10px; color: #00509E;"),
                  "Omics Integration",
                  style = "margin: 0; font-weight: bold; font-size: 18px;"
                ),
                tags$p(
                  "Integrate chemical genomic data with omics to compare hits, quantify overlap, and visualise shared patterns.",
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
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 20px; text-align: justify;"
              ),
              tags$hr(style = "border-color: #1ABC9C; margin: 20px 0;"),
              
              
              # Data Sources Section
              h3(
                tags$span(style = "color: #1ABC9C; margin-right: 10px;", tags$i(class = "fas fa-folder-open")), 
                "Data Sources", 
                style = "color: #34495E; font-weight: bold; margin-bottom: 15px; font-size: 18px;"
              ),
              p(
                "ChemGenXplore includes pre-integrated, publicly available chemical genomic datasets from ",
                tags$i("Escherichia coli"),
                " and ",
                tags$i("Saccharomyces cerevisiae"),
                ". These datasets provide fitness scores across a wide range of conditions and serve as the foundation for the analyses performed in ChemGenXplore:",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 15px; text-align: justify;"
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
                  "Shiver et al., 2017: ",
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
                ),
                tags$li(
                  "Viéitez et al., 2022: ",
                  tags$a(
                    href = "https://doi.org/10.1038/s41587-021-01051-x",
                    "https://doi.org/10.1038/s41587-021-01051-x", 
                    target = "_blank", 
                    style = "color: #0047AB; font-weight: normal; text-decoration: none;"
                  )
                )
              ),
              p(
                "Users also have the flexibility to upload their own datasets for analysis.",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 20px; text-align: justify;"
              ),
              tags$hr(style = "border-color: #1ABC9C; margin: 20px 0;"),
              
              # Acknowledgements Section
              h3(
                tags$span(style = "color: #1ABC9C; margin-right: 10px;", tags$i(class = "fas fa-hands-helping")), 
                "Acknowledgements", 
                style = "color: #34495E; font-weight: bold; margin-bottom: 15px; font-size: 18px;"
              ),
              p(
                "ChemGenXplore was developed with support from the Banzhaf Lab and the Moradigaravand Lab.",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 10px; text-align: justify;"
              ),
              p(
                "H.A. was funded by the Darwin Trust of Edinburgh.",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 15px; text-align: justify;"
              ),
              p(
                "Affiliated institutions include:",
                style = "font-size: 14px; color: #2C3E50; margin-bottom: 8px; text-align: justify;"
              ),
              tags$ul(
                style = "font-size: 14px; color: #2C3E50; padding-left: 20px; margin-bottom: 20px;",
                tags$li("Institute of Microbiology and Infection, University of Birmingham, Birmingham, UK"),
                tags$li("Biosciences Institute, Faculty of Medical Sciences, Newcastle University, Newcastle Upon Tyne, UK"),
                tags$li("Laboratory of Infectious Disease Epidemiology, KAUST Center of Excellence for Smart Health and Biological and Environmental Science and Engineering (BESE) Division, King Abdullah University of Science and Technology (KAUST), Thuwal, Saudi Arabia"),
                
              ),
              tags$hr(style = "border-color: #1ABC9C; margin: 20px 0;"),
              
              
              # Contact Section
              h3(
                tags$span(style = "color: #1ABC9C; margin-right: 10px;", tags$i(class = "fas fa-envelope")), 
                "Contact", 
                style = "color: #34495E; font-weight: bold; margin-bottom: 15px; font-size: 18px;"
              ),
              p(
                "For questions or suggestions for additional features, please contact:",
                style = "font-size: 16px; color: #2C3E50; margin-bottom: 12px; text-align: justify;"
              ),
              tags$ul(
                style = "font-size: 14px; color: #2C3E50; padding-left: 20px;",
                tags$li(
                  "Huda Ahmad: ",
                  tags$a(href = "mailto:hxa105@student.bham.ac.uk", "hxa105@student.bham.ac.uk", 
                         style = "color: #0047AB; text-decoration: none; font-weight: normal;")
                ),
                tags$li(
                  "Manuel Banzhaf: ",
                  tags$a(href = "mailto:manuel.banzhaf@newcastle.ac.uk", "manuel.banzhaf@newcastle.ac.uk", 
                         style = "color: #0047AB; text-decoration: none; font-weight: normal;")
                ),
                tags$li(
                  "Danesh Moradigaravand: ",
                  tags$a(href = "mailto:danesh.moradigaravand@kaust.edu.sa", "danesh.moradigaravand@kaust.edu.sa", 
                         style = "color: #0047AB; text-decoration: none; font-weight: normal;")
                )
              ) )))),
              
      
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
                
                # Pre-loaded Datasets (E. coli & S. cerevisiae) Section
                    bsCollapsePanel(
                      title = HTML(
                        "Pre-loaded Datasets (<span class='taxon'>E. coli</span> & <span class='taxon'>S. cerevisiae</span>)"
                      ),
                      style = "success",
                      tags$p(HTML(
                        "This section provides an overview of the analyses available for the pre-loaded datasets. 
     All analysis types apply to both <em>E. coli</em> and <em>S. cerevisiae</em> and allow users to adjust 
     False Discovery Rate (FDR) thresholds to refine results. Each analysis provides interactive 
     plots and tables with the option to download outputs for further use."
                      )),
                  
                  # Phenotypes Visualisation Section
                  bsCollapsePanel(
                    title = "Phenotypes Visualisation",
                    style = "info",
                    tags$p(HTML(
                    "This section focuses on visualising the number of phenotypes associated with selected genes or conditions across preloaded datasets. Users can adjust False Discovery Rate (FDR) thresholds to identify statistically significant phenotypes. Results are displayed as interactive bar plots and tables, with the option to download both for further analysis. An additional table is available, listing all phenotypes without applying the FDR threshold for a comprehensive overview."
                    )),
                    tags$h4(
                      "Gene-Level",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li(HTML("Go to <i>E. coli</i> or <i>S. cerevisiae</i> ▸ Phenotypes ▸ Gene-Level.")),
                      tags$li("Select one or more genes from the dropdown."),
                      tags$li("Adjust the FDR slider to filter significant phenotypes."),
                      tags$li("Review interactive bar plots and data tables."),
                      tags$li("Download plots and tables using the buttons provided.")
                    ),
                    tags$hr(style = "border-color: #D3D3D3; margin: 20px 0;"),
                    tags$h4(
                      "Condition-Level",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li(HTML("Go to <i>E. coli</i> or <i>S. cerevisiae</i> ▸ Phenotypes ▸ Condition-Level.")),
                      tags$li("Select one or more conditions from the dropdown."),
                      tags$li("Adjust the FDR slider to filter significant phenotypes."),
                      tags$li("Review interactive bar plots and data tables."),
                      tags$li("Download plots and tables using the buttons provided.")
                    )
                  ),
                  

                  # Correlation Analysis Section
                  bsCollapsePanel(
                    title = "Correlation Analysis",
                    style = "info",
                    tags$p(HTML(
                      "This section focuses on visualising correlations between selected genes or conditions across pre-loaded <i>E. coli</i> and <i>S. cerevisiae</i> datasets. Users can adjust False Discovery Rate (FDR) thresholds to identify statistically significant correlations. Results are displayed as interactive bar plots and tables, with the option to download both for further analysis. An additional table is available, listing all correlations without applying the FDR threshold for a comprehensive overview."
                    )),
                    tags$h4(
                      "Gene-Level",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li(HTML("Go to <i>E. coli</i> or <i>S. cerevisiae</i> ▸ Correlation Analysis ▸ Gene-Level.")),
                      tags$li("Select one or more genes from the dropdown."),
                      tags$li("Adjust the FDR slider to filter significant correlations."),
                      tags$li("Review interactive bar plots and data tables."),
                      tags$li("Download plots and tables using the buttons provided.")
                    ),
                    tags$hr(style = "border-color: #D3D3D3; margin: 20px 0;"),
                    tags$h4(
                      "Condition-Level",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li(HTML("Go to <i>E. coli</i> or <i>S. cerevisiae</i> ▸ Correlation Analysis ▸ Condition-Level.")),
                      tags$li("Select one or more conditions from the dropdown."),
                      tags$li("Adjust the FDR slider to filter significant correlations."),
                      tags$li("Review interactive bar plots and data tables."),
                      tags$li("Download plots and tables using the buttons provided.")
                    )
                  ),
                  
                  # Enrichment Analysis Section
                  bsCollapsePanel(
                    title = "Enrichment Analysis",
                    style = "info",
                    tags$p(HTML(
                      "This section focuses on identifying enriched biological processes and pathways associated with selected genes across pre-loaded <i>E. coli</i> and <i>S. cerevisiae</i> datasets. Users can adjust False Discovery Rate (FDR) thresholds to identify statistically significant terms. Results are displayed as interactive bar plots and tables, with the option to download both for further analysis. An additional table is available, listing all enrichment results without applying the FDR threshold for a comprehensive overview."
                    )),
                    tags$h4(
                      "GO Enrichment",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li(HTML("Go to <i>E. coli</i> or <i>S. cerevisiae</i> ▸ Enrichment Analysis ▸ GO Enrichment.")),
                      tags$li("Select one or more genes from the dropdown."),
                      tags$li("Adjust the FDR slider to filter significant terms."),
                      tags$li("Review interactive bar plots and data tables."),
                      tags$li("Download plots and tables using the buttons provided.")
                    ),
                    tags$hr(style = "border-color: #D3D3D3; margin: 20px 0;"),
                    tags$h4(
                      "KEGG Enrichment",
                      style = "color: #104E8B; font-weight: bold; margin-top: 20px;"
                    ),
                    tags$ul(
                      tags$li(HTML("Go to <i>E. coli</i> or <i>S. cerevisiae</i> ▸ Enrichment Analysis ▸ KEGG Enrichment.")),
                      tags$li("Repeat the same steps as for GO Enrichment."),
                      tags$li("Adjust the FDR slider to filter significant pathways."),
                      tags$li("Review interactive bar plots and data tables."),
                      tags$li("Download plots and tables using the buttons provided.")
                    )
                  ),
                  
                  
                  # Heatmaps Section
                  bsCollapsePanel(
                    title = "Heatmaps",
                    style = "info",
                    tags$p(HTML(
                      "This section focuses on visualising fitness profiles across genes and conditions for pre-loaded <i>E. coli</i> and <i>S. cerevisiae</i> datasets using interactive heatmaps. Users can customise clustering options and distance metrics to refine the visualisation. Results are displayed as interactive heatmaps and tables, with the option to download both for further analysis. An additional table is available, listing the selected data used to generate the heatmap for a comprehensive overview."
                    )),
                    tags$ul(
                      tags$li(HTML("Go to <i>E. coli</i> or <i>S. cerevisiae</i> ▸ Heatmaps.")),
                      tags$li("Select datasets, genes, and conditions to generate heatmaps."),
                      tags$li("Optionally cluster rows and/or columns and choose a distance metric."),
                      tags$li("Download images and data tables using the buttons provided.")
                    )
                  )
                    ),
                  
                
                # Upload Your Dataset Section
                bsCollapsePanel(
                  title = "Uploading a Dataset",
                  style = "warning",
                  tags$p(HTML(
                    "This section allows users to upload custom datasets for analysis. Uploaded datasets must be formatted with genes/strains as rows and conditions as columns. Once uploaded, users can explore phenotypes, correlations, enrichment analysis, and heatmap generation using the same tools and workflows available for the pre-loaded datasets."
                  )),
                  tags$ul(
                    tags$li(HTML("Go to <b>Upload Your Dataset</b>.")),
                    tags$li("Upload a CSV file where the first column contains unique gene/strain identifiers and all remaining columns contain numeric values."),
                    tags$li("Explore Phenotypes, Correlation Analysis, Enrichment Analysis, and Heatmaps using the same steps as described above.")
                  )
                ),
                
                # Omics Integration Section (accordion-style sub-sections)
                bsCollapsePanel(
                  title = "Omics Integration",
                  style = "info",
                  
                  tags$p(HTML(
                    "This section focuses on integrating chemical genomic data with an omics dataset (e.g., transcriptomics or proteomics) to identify shared gene-level signals across datasets. Users can align by gene IDs, apply thresholds to define hits, and explore concordance through scatter plots, paired heatmaps, and overlap statistics. Results include interactive visualisations, summary tables, and downloadable outputs for further analysis."
                  )),
                  
                  # Nested accordion for sub-sections
                  bsCollapse(
                    id = "omicsAccordion",
                    open = NULL,  # or "Scatter Plot" if you want one open by default
                    
                    bsCollapsePanel(
                      title = "Scatter Plot",
                      style = "info",
                      tags$ul(
                        tags$li(HTML("Go to <i>Omics Integration</i> ▸ Scatter plot.")),
                        tags$li("Upload one chemical-genomics CSV and one omics CSV (first column must contain Gene IDs)."),
                        tags$li("Select one column from each dataset for comparison."),
                        tags$li("Adjust the |score| thresholds to classify significant hits."),
                        tags$li("Review interactive scatter plots with quadrant classification and optional gene labels."),
                        tags$li("Download the scatter (PDF) and hit list (CSV) using the buttons provided.")
                      )
                    ),
                    
                    bsCollapsePanel(
                      title = "Heatmaps",
                      style = "info",
                      tags$ul(
                        tags$li(HTML("Go to <i>Omics Integration</i> ▸ Heatmaps.")),
                        tags$li("Optionally select a subset of genes and columns from each dataset."),
                        tags$li("View paired heatmaps aligned by gene with symmetric colour scales centred at zero."),
                        tags$li("Download the heatmap data (CSV) using the button provided.")
                      )
                    ),
                    
                    bsCollapsePanel(
                      title = "Overlap Summary",
                      style = "info",
                      tags$ul(
                        tags$li(HTML("Go to <i>Omics Integration</i> ▸ Overlap Summary.")),
                        tags$li("Set thresholds for each dataset to define significant hits."),
                        tags$li("Review counts for Chemical Genomics only, Omics only, both significant, and neither."),
                        tags$li("Inspect Fisher’s exact test statistics (Odds Ratio, 95% CI, p-value) to assess enrichment."),
                        tags$li("Download the overlap summary (CSV) using the button provided.")
                      )
                    )
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
                          cgx_selectize("Gene1", "Select or Type Genes:"),
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
                        cgx_selectize(
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
                               cgx_selectize(
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
                               cgx_selectize(
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
                               cgx_selectize("Gene3", "Select or Type Genes:", choices = NULL, multiple = TRUE),
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
                                   downloadButton('downloadgo', "Download Table")  
                                 ))))
                  ),
                  
                  # KEGG Enrichment tab
                  tabPanel("KEGG Enrichment",
                           sidebarLayout(
                             sidebarPanel(
                               cgx_selectize("Gene4", "Select or Type Genes:", choices = NULL, multiple = TRUE),
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
                      choices = c(
                        "Euclidean"      = "euclidean",
                        "Manhattan"      = "manhattan",
                        "Pearson"     = "pearson",
                        "Spearman" = "spearman",
                        "Cosine" = "cosine"
                      ),
                      selected = "pearson"
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
     
      # Yeast (S. cerevisiae) Tab 
      tabItem(
        tabName = "yeast",
        fluidRow(
          box(
            width = 12,
            tabsetPanel(
              type = "tabs",
              
              # Phenotypes Parent tab
              tabPanel(
                title = tagList(icon("dna"), "Phenotypes"),
                tabsetPanel(
                  type = "tabs",
                  
                  # --- Gene-Level ---
                  tabPanel(
                    "Gene-Level",
                    sidebarLayout(
                      sidebarPanel(
                        cgx_selectize(
                          "sc_Gene1", "Select or Type Genes:",
                          choices = NULL, multiple = TRUE
                        ),
                        sliderInput(
                          "sc_FDRq", "% FDR for phenotypes (qvals)",
                          min = 0, max = 100, step = 0.1, value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Phenotypes",
                            shinycssloaders::withSpinner(plotlyOutput("sc_barplotps")),
                            downloadButton('sc_download', "Download Table"),
                            downloadButton('sc_downloadPlot', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_genes_scores_sig_filetable"))
                          ),
                          tabPanel(
                            "All Phenotypes",
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_genes_scores_filetable")),
                            downloadButton('sc_downloadAllGenesPhenotypes', "Download Table")
                          )
                        )
                      )
                    )
                  ),
                  
                  # Condition-Level tab
                  tabPanel(
                    "Condition-Level",
                    sidebarLayout(
                      sidebarPanel(
                        cgx_selectize(
                          "sc_cond1", "Select or Type Conditions:",
                          choices = NULL, multiple = TRUE
                        ),
                        sliderInput(
                          "sc_FDRqc", "% FDR for phenotypes (qvals)",
                          min = 0, max = 100, step = 0.1, value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Phenotypes",
                            shinycssloaders::withSpinner(plotlyOutput("sc_barplotpsc")),
                            downloadButton('sc_downloadc', "Download Data"),
                            downloadButton('sc_downloadPlotc', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_conditions_scores_sig_filetable"))
                          ),
                          tabPanel(
                            "All Phenotypes",
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_conditions_scores_filetable")),
                            downloadButton('sc_downloadAllConditionsPhenotypes', "Download Table")
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
                  tabPanel(
                    "Gene-Level",
                    sidebarLayout(
                      sidebarPanel(
                        cgx_selectize(
                          "sc_Gene2", "Select or Type Genes:",
                          choices = NULL, multiple = TRUE
                        ),
                        sliderInput(
                          "sc_FDRq2", "% FDR for genes correlations (qvals)",
                          min = 0, max = 100, step = 0.1, value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Correlations",
                            shinycssloaders::withSpinner(plotlyOutput("sc_barplot")),
                            downloadButton('sc_download_gene_cor', "Download Data"),
                            downloadButton('sc_downloadcorplot', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_genes_correlations_sig_filetable"))
                          ),
                          tabPanel(
                            "All Correlations",
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_genes_correlations_filetable")),
                            downloadButton('sc_downloadAllCorrelations', "Download Table")
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
                        cgx_selectize(
                          "sc_cond2", "Select or Type Conditions:",
                          choices = NULL, multiple = TRUE
                        ),
                        sliderInput(
                          "sc_FDRq3", "% FDR for conditions correlations (qvals)",
                          min = 0, max = 100, step = 0.1, value = 5
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant Correlations",
                            shinycssloaders::withSpinner(plotlyOutput("sc_barplot2")),
                            downloadButton('sc_download_cond_cor', "Download Data"),
                            downloadButton('sc_downloadcorplot2', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_conditions_correlations_sig_filetable"))
                          ),
                          tabPanel(
                            "All Correlations",
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_conditions_correlations_filetable")),
                            downloadButton('sc_downloadAllCondCorrelations', "Download Table")
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              # Enrichment Analysis Parent Tab
              tabPanel(
                title = tagList(icon("chart-bar"), "Enrichment Analysis"),
                tabsetPanel(
                  type = "tabs",
                  
                  # GO Enrichment Tab
                  tabPanel(
                    "GO Enrichment",
                    sidebarLayout(
                      sidebarPanel(
                        cgx_selectize("sc_Gene3", "Select or Type Genes:", choices = NULL, multiple = TRUE),
                        sliderInput("sc_FDRq4", "FDR for GO enrichment (qvals)", min = 0, max = 100, step = 0.1, value = 50),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant GO Enrichment",
                            shinycssloaders::withSpinner(plotlyOutput("sc_barplotGO")),
                            downloadButton('sc_downloadgos', "Download Data"),
                            downloadButton('sc_downloadgoplot', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_filetableGOs"))
                          ),
                          tabPanel(
                            "All GO Enrichment",
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_filetableGO")),
                            downloadButton('sc_downloadgo', "Download Table")
                          )
                        )
                      )
                    )
                  ),
                  
                  # --- KEGG Enrichment ---
                  tabPanel(
                    "KEGG Enrichment",
                    sidebarLayout(
                      sidebarPanel(
                        cgx_selectize("sc_Gene4", "Select or Type Genes:", choices = NULL, multiple = TRUE),
                        sliderInput("sc_FDRq5", "FDR for KEGG enrichment (qvals)", min = 0, max = 100, step = 0.1, value = 100),
                        width = 3
                      ),
                      mainPanel(
                        tabsetPanel(
                          type = "tabs",
                          tabPanel(
                            "Significant KEGG Enrichment",
                            shinycssloaders::withSpinner(plotlyOutput("sc_barplotKEGG")),
                            downloadButton('sc_downloadkeggs', "Download Data"),
                            downloadButton('sc_downloadkeggplot', "Download Plot"),
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_filetableKEGGs"))
                          ),
                          tabPanel(
                            "All KEGG Enrichment",
                            shinycssloaders::withSpinner(DT::dataTableOutput("sc_filetableKEGG")),
                            downloadButton('sc_downloadkegg', "Download Table")
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
                    uiOutput("sc_gene_selector"),
                    uiOutput("sc_condition_selector"),
                    checkboxInput("sc_cluster_rows", "Cluster Genes", value = TRUE),
                    checkboxInput("sc_cluster_columns", "Cluster Conditions", value = FALSE),
                    selectInput("sc_clustering_method", "Clustering Method",
                                choices = c("complete", "single", "average", "ward.D")),
                    selectInput("sc_distance_metric", "Distance Metric",
                                choices = c("Euclidean"="euclidean","Manhattan"="manhattan",
                                            "Pearson"="pearson","Spearman"="spearman","Cosine"="cosine"),
                                selected = "pearson"),
                    downloadButton("sc_downloadHeatmapData", "Download Heatmap Data"),
                    downloadButton("sc_downloadInteractiveHeatmapPlot", "Download Interactive Heatmap Plot"),
                    downloadButton("sc_downloadDendrogramPlot", "Download Dendrogram Plot")
                  ),
                  mainPanel(
                    tabsetPanel(
                      type = "tabs",
                      tabPanel(
                        "Interactive Heatmap",
                        shinycssloaders::withSpinner(plotlyOutput("sc_heatmap_interactive", height = "800px", width = "100%"))
                      ),
                      tabPanel(
                        "Dendrogram Heatmap",
                        shinycssloaders::withSpinner(plotOutput("sc_heatmap_dendrogram", height = "800px", width = "100%"))
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
            
            tags$p("Upload a CSV file where rows contain unique identifiers (e.g., genes, samples, strains) and columns contain experimental measurements (e.g., fitness scores, gene expression levels, growth rates). Example Dataset Below:"),
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
                        cgx_selectize(
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
                        cgx_selectize(
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
                        cgx_selectize(
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
                        cgx_selectize(
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
                      choices = c(
                        "Euclidean"      = "euclidean",
                        "Manhattan"      = "manhattan",
                        "Pearson"     = "pearson",
                        "Spearman" = "spearman",
                        "Cosine" = "cosine"
                      ),
                      selected = "pearson"
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
      ), 
      
      # Omics Tab
      
      tabItem(
        tabName = "omics",
        # small, tab-local styles
        tags$head(tags$style(HTML("
    .panel-like { border:1px solid #e5e7eb; border-radius:10px; padding:14px; margin-bottom:16px; }
    .panel-like h4 { margin-top:0; color:#2C3E50; }
  "))),
        fluidRow(
          column(
            12,
            h3("Omics Integration"),
            p("Upload a chemical genomics matrix and an omics matrix, align by gene IDs, and generate figures and tables.")
          )
        ),
        # TOP: uploads
        fluidRow(
          column(
            6,
            div(class = "panel-like",
                h4("Chemical Genomics Upload"),
                fileInput("chem_file", NULL, buttonLabel = "Browse…",
                          placeholder = "Upload Chemical Genomics CSV",
                          accept = ".csv"),
                helpText("Format: first column = Gene IDs; columns = conditions; values = numeric scores."),
                shinycssloaders::withSpinner(DT::DTOutput("chem_preview"), type = 4)
            )
          ),
          column(
            6,
            div(class = "panel-like",
                h4("Omics Upload"),
                fileInput("omics_file", NULL, buttonLabel = "Browse…",
                          placeholder = "Upload Omics CSV",
                          accept = ".csv"),
                helpText("Format: first column = Gene IDs; columns = samples/conditions; values = numeric (e.g., log2FC)."),
                shinycssloaders::withSpinner(DT::DTOutput("omics_preview"), type = 4)
            )
          )
        ),
        # LEFT controls + RIGHT tabs
        fluidRow(
          column(
            4,
            div(class = "panel-like",
                h4("Alignment"),
                verbatimTextOutput("align_stats"),
                tags$hr(),
                uiOutput("left_controls")
            )
          ),
          column(
            8,
            tabsetPanel(
              id = "inner", type = "tabs",
              tabPanel("Scatter plot",
                       shinycssloaders::withSpinner(plotlyOutput("scatter_plot", height = "520px"), type = 4)),
              tabPanel("Heatmaps",
                       shinycssloaders::withSpinner(plotlyOutput("heatmaps", height = "720px"), type = 4)),
              tabPanel("Overlap Summary",
                       shinycssloaders::withSpinner(plotOutput("overlap_plot", height = "320px"), type = 4),
                       shinycssloaders::withSpinner(tableOutput("overlap_table"), type = 4))
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
    ) # close dashboardBody
    )   # close dashboardPage
  

