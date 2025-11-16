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
                  selectizeInput(
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
                  selectizeInput(
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
                  selectizeInput(
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
                  selectizeInput(
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
                  selectizeInput("sc_Gene3", "Select or Type Genes:", choices = NULL, multiple = TRUE),
                  sliderInput("sc_FDRq4", "FDR for GO enrichment (qvals)", min = 0, max = 100, step = 0.1, value = 100),
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
                  selectizeInput("sc_Gene4", "Select or Type Genes:", choices = NULL, multiple = TRUE),
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
