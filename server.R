# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  ######## Server logic for the "E.coli" tab ########
  
  # Load key datasets required for the application:
  scores_file <- readRDS("3_Score_plus_FDR.rds")  # Fitness scores and False Discovery Rate (FDR) values
  gene_cor_file <- readRDS("3_gene_cor.rds")      # Gene correlation data
  cond_cor_file <- readRDS("3_cond_cor.rds")      # Condition correlation data
  go_file <- readRDS("3_go_enrich.rds")          # Gene Ontology (GO) enrichment data
  kegg_file <- readRDS("3_kegg_enrich.rds")      # KEGG pathway enrichment data
  

  # Load heatmap matrices for datasets from three published studies:
  heatmap_datasets <- list(
    "Nichols, R. et al." = readRDS("nichols_heatmap.rds"),
    "Shiver, A. et al." = readRDS("shiver_heatmap.rds"),
    "Price, MN. et al." = readRDS("price_heatmap.rds")
  )
  

  # Observe block to ensure datasets are loaded before proceeding
  observe({
    req(scores_file, gene_cor_file, cond_cor_file, go_file, kegg_file)
    
    # Extract unique gene names from the fitness scores file
    unique_genes <- unique(scores_file$Gene)
    
    # Dynamically update the Gene1 dropdown menu in the UI
    updateSelectizeInput(
      session,
      "Gene1",  # The input ID for the Gene1 dropdown
      choices = unique_genes,  # Populate dropdown with unique gene names
      selected = if (length(unique_genes) >= 2) unique_genes[1:6] else unique_genes,  # Pre-select up to the first 6 genes if available
      options = list(
        placeholder = "Type to search...",  # Add a placeholder guiding users to search for genes
        maxOptions = length(unique_genes)  # Limit displayed options to the total number of unique genes
      ),
      server = TRUE  # Enable server-side processing for better performance with large datasets
    )
    
    # Dynamically update the cond1 dropdown menu in the UI as above
    updateSelectizeInput(
      session,
      "cond1",  
      choices = unique(scores_file$Condition),  
      selected = c("GLUCOSE", "EGTA 1 MM", "NALIDIXIC 0.006.MG.ML"),  
      options = list(
        placeholder = "Type to search...",  
        maxOptions = length(unique(scores_file$Condition))  
      ),
      server = TRUE 
    )
    
    
    # Extract unique gene names from the gene correlation dataset (either Gene_1 or Gene_2)
    unique_genes_cor <- unique(c(gene_cor_file$Gene_1, gene_cor_file$Gene_2))
    
    # Dynamically update the Gene2 dropdown menu in the UI
    updateSelectizeInput(
      session,
      "Gene2",  
      choices = unique_genes_cor,  
      selected = if (length(unique_genes_cor) >= 2) unique_genes_cor[1:2] else unique_genes_cor,  
      options = list(
        placeholder = "Type to search...",  
        maxOptions = length(unique_genes_cor)  
      ),
      server = TRUE  
    )
    
    # Dynamically update the cond2 dropdown menu in the UI
    updateSelectizeInput(
      session,
      "cond2", 
      choices = unique(cond_cor_file$Condition_1),  
      selected = c("TETRACYCLINE.0.75", "EGTA 1 MM", "NITRITE.20.MM"),  
      options = list(
        placeholder = "Type to search...",  
        maxOptions = length(unique(cond_cor_file$Condition_1)) 
      ),
      server = TRUE  
    )
    
    # Extract unique genes from the Gene column in the GO file
    unique_genes_go <- unique(go_file$Gene)  
    
    # Dynamically update the Gene3 dropdown menu in the UI
    updateSelectizeInput(
      session,
      "Gene3",  
      choices = unique_genes_go,  
      selected = if (length(unique_genes_go) >= 2) unique_genes_go[1:2] else unique_genes_go,  
      options = list(
        placeholder = "Type to search...",  
        maxOptions = length(unique_genes_go)  
      ),
      server = TRUE  
    )
  
    # Extract unique genes from the Gene column in the KEGG file
    unique_genes_kegg <- unique(kegg_file$Gene)  
    
    # Update the Gene4 dropdown in the UI dynamically
    updateSelectizeInput(
      session,
      "Gene4",  
      choices = unique_genes_kegg,  
      selected = if (length(unique_genes_kegg) >= 2) unique_genes_kegg[1:2] else unique_genes_kegg,  
      options = list(
        placeholder = "Type to search...",  
        maxOptions = length(unique_genes_kegg)  
      ),
      server = TRUE 
    )
    
  })

  # Create a reactive function to return the selected heatmap dataset
  selected_heatmap_data <- reactive({
    req(input$selected_dataset)  # Ensure the user has selected a dataset from the dropdown or input field
    heatmap_datasets[[input$selected_dataset]]  # Retrieve and return the selected dataset from the heatmap_datasets list
  })
  
  # Dynamically render the gene selector input based on the selected heatmap dataset
  output$gene_selector <- renderUI({
    req(selected_heatmap_data())  
    
    # Extract gene names from the rownames of the selected dataset
    genes <- rownames(selected_heatmap_data())
    
    # Render a selectInput UI element for gene selection
    selectInput(
      inputId = "genes",  
      label = "Select Genes",  # Label displayed above the dropdown
      choices = c("All", genes),  # Add an "All" option at the top of the list
      multiple = TRUE,  # Allow multiple gene selections
      selected = genes[1:7]  # Default selection: the first 7 genes
    )
  })
  
  # Dynamically render the condition selector input based on the selected heatmap dataset
  output$condition_selector <- renderUI({
    req(selected_heatmap_data())  
    
    # Extract condition names from the column names of the selected dataset
    conditions <- colnames(selected_heatmap_data())
    
    # Render a selectInput UI element for condition selection
    selectInput(
      inputId = "conditions",  
      label = "Select Conditions",  
      choices = c("All", conditions),  
      multiple = TRUE,  
      selected = conditions[1:12]  
    )
  })
  
  
  #### Server logic for Phenotypes of Selected Genes ####
  
  Genes_phenotypes <- reactive({
    req(input$Gene1)  # Ensure that the user has selected at least one gene (Gene1 input must not be NULL)
    
    # Filter the scores file to include only rows where the Gene matches the selected input (Gene1)
    dplyr::filter(scores_file, Gene %in% input$Gene1) %>%
      # Add a new column 'Fitness' based on the value of Score
      mutate(Fitness = case_when(
        Score > 0 ~ 'Increased',  # If score is greater than 0, label as 'Increased'
        Score < 0 ~ 'Decreased'   # If score is less than 0, label as 'Decreased'
      ))
  })
  
  # Render "All Phenotypes" table
  output$genes_scores_filetable <- DT::renderDataTable({
    # Create a DataTable using the Genes_phenotypes reactive dataset
    DT::datatable(
      Genes_phenotypes(),  # Use the data from the Genes_phenotypes reactive function
      options = list(
        pageLength = 10,  # Display 10 rows per page by default
        scrollX = TRUE    # Enable horizontal scrolling for wide tables
      ),
      rownames = FALSE  # Do not display row names in the table
    )
  })
  
  # Download handler for the "All Phenotypes" table
  output$downloadAllGenesPhenotypes <- downloadHandler(
    # Define the filename dynamically with the current date
    filename = function() { 
      paste("All_Phenotypes_", Sys.Date(), ".csv", sep = "")  # Format: All_Phenotypes_YYYY-MM-DD.csv
    },
    
    # Define the content of the downloaded file
    content = function(file) {
      write.csv(
        Genes_phenotypes(),  # Use the data from the Genes_phenotypes reactive function
        file,                # File path provided by the user
        row.names = FALSE    # Exclude row names from the CSV file for a cleaner output
      )
    }
  )

  # Reactive function for filtering significant phenotypes
  Genes_phenotypes_sig <- reactive({
    Genes_phenotypes() %>%  # Start with the data from the Genes_phenotypes reactive function
      dplyr::filter(
        as.numeric(FDR) <= (input$FDRq / 100)  # Filter rows where the FDR is less than or equal to the user-specified threshold
      )
  })
  
  # Render a table for significant gene scores
  output$genes_scores_sig_filetable <- DT::renderDataTable({
    # Create a DataTable using the filtered significant phenotypes dataset
    DT::datatable(
      Genes_phenotypes_sig(),  
      options = list(
        pageLength = 5,  
        scrollX = TRUE  
      ),
      rownames = FALSE  
    )
  })
  
  # Reactive function to summarize significant phenotypes data for the bar plot
  Genes_phenotypes_sig_with_totals <- reactive({
    Genes_phenotypes_sig() %>%  # Start with the filtered significant phenotypes dataset
      group_by(Dataset, Fitness) %>%  # Group data by Dataset and Fitness
      summarise(
        Count = n(),       # Count the number of occurrences in each group
        .groups = "drop"   # Remove grouping to return a flat data frame
      ) %>%
      mutate(
        Fitness = factor(Fitness, levels = c("Decreased", "Increased"))  # Ensure Fitness is a factor with a defined order
      )
  })
  
  # Reactive function to generate the bar plot for significant phenotypes
  plotInput <- reactive({
    ggplot(
      Genes_phenotypes_sig_with_totals(),  # Use the summarized data for significant phenotypes
      aes(x = Count, y = Dataset, fill = Fitness)  # Map Count to x-axis, Dataset to y-axis, and Fitness to fill
    ) +
      geom_bar(stat = "identity") +  # Create a bar plot with pre-summarized data
      scale_fill_manual(
        values = c("Decreased" = "#d0f0f0", "Increased" = "#238b8e")  # Define custom colors for Fitness levels
      ) +
      theme_classic() +  # Apply a clean, classic theme
      xlab("Count of Phenotypes") +  # Set x-axis label
      ylab(NULL) +  # Remove y-axis label for a cleaner look
      theme(
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),  
        plot.title = element_text(size = 16, face = "bold"),  
        legend.text = element_text(size = 10),  
        legend.title = element_text(size = 11)  
      )
  })
  
  # Download handler for the Significant Phenotypes Genes table
  output$download <- downloadHandler(
    # Generate the filename dynamically with the current date
    filename = function() {
      paste("Significant_Phenotypes_Genes_", Sys.Date(), ".csv", sep = "")  
    },
    
    # Define the content of the file to be downloaded
    content = function(fname) {
      write.csv(
        Genes_phenotypes_sig(),  
        fname,                   
        row.names = FALSE        
      )
    }
  )
  
  # Render the interactive bar plot for significant phenotypes
  output$barplotps <- renderPlotly({
    ggplotly(plotInput())  # Convert the ggplot object created by plotInput() to an interactive plotly object
  })
  
  # Download handler for the Significant Phenotypes bar plot
  output$downloadPlot <- downloadHandler(
    # Generate the filename dynamically based on the selected gene and current date
    filename = function() {
      paste(input$Gene1, 'Significant_Phenotypes.pdf', sep = '_')  # Format: SelectedGene_Significant_Phenotypes.pdf
    },
    
    # Define the content of the file to be downloaded
    content = function(file) {
      ggsave(
        file,             
        plotInput(),      
        width = 8,       
        height = 5        
      )
    }
  )

  #### Server logic for Phenotypes Conditions #####
  
  # Condition-Level Tab: Filter by selected condition
  Conditions_phenotypes <- reactive({
    req(input$cond1)  
    
    # Filter the dataset based on the selected condition(s)
    dplyr::filter(scores_file, Condition %in% input$cond1) %>%
      # Add a new column 'Fitness' based on the value of Score
      mutate(Fitness = case_when(
        Score > 0 ~ 'Increased',  
        Score < 0 ~ 'Decreased'   
      ))
  })
  
  # Render the data table for all phenotypes based on selected conditions
  output$conditions_scores_filetable <- DT::renderDataTable({
    # Create a DataTable using the filtered conditions phenotypes dataset
    DT::datatable(
      Conditions_phenotypes(),  
      options = list(
        pageLength = 10,  
        scrollX = TRUE    
      ),
      rownames = FALSE  
    )
  })
  
  # Download handler for the "All Phenotypes" table at the condition level
  output$downloadAllConditionsPhenotypes <- downloadHandler(
    # Generate the filename dynamically with the current date
    filename = function() {
      paste("All_Phenotypes_", Sys.Date(), ".csv", sep = "")  # Format: All_Phenotypes_YYYY-MM-DD.csv
    },
    
    # Define the content of the file to be downloaded
    content = function(file) {
      write.csv(
        Conditions_phenotypes(),  
        file,                     
        row.names = FALSE         
      )
    }
  )
  
  # Reactive function for filtering significant phenotypes at the condition level based on the selected FDR threshold
  Conditions_phenotypes_sig <- reactive({
    Conditions_phenotypes() %>%  
      dplyr::filter(
        as.numeric(FDR) <= (input$FDRqc / 100)  
      )
  })
  
  # Render the data table for significant phenotypes at the condition level
  output$conditions_scores_sig_filetable <- DT::renderDataTable({
    # Create a DataTable using the filtered significant phenotypes dataset
    DT::datatable(
      Conditions_phenotypes_sig(),  
      options = list(
        pageLength = 10,  
        scrollX = TRUE    
      ),
      rownames = FALSE  
    )
  })
  
  # Summarize data for the bar plot of significant phenotypes
  Conditions_phenotypes_sig_with_totals <- reactive({
    # Filter the data for the selected conditions
    filtered_data <- Conditions_phenotypes_sig() %>%
      dplyr::filter(Condition %in% input$cond1)  
    
    # Summarize the filtered data
    summarized_data <- filtered_data %>%
      group_by(Dataset, Fitness) %>%  
      summarise(
        Count = n(),  
        .groups = "drop"  
      ) %>%
      mutate(
        Fitness = factor(Fitness, levels = c("Decreased", "Increased"))  
      )
    
    summarized_data  
  })
  
  # Reactive function to generate and render the bar plot for significant phenotypes
  plotInputc <- reactive({
    ggplot(
      Conditions_phenotypes_sig_with_totals(),  
      aes(x = Count, y = Dataset, fill = Fitness)  
    ) +
      geom_bar(stat = "identity") +  
      scale_fill_manual(
        values = c("Decreased" = "#f6e4d9", "Increased" = "#f4a582")  
      ) +
      theme_classic() +  
      xlab("Count of Phenotypes") + 
      ylab(NULL) +  
      theme(
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),  
        plot.title = element_text(size = 16, face = "bold"),  
        legend.text = element_text(size = 10),  
        legend.title = element_text(size = 11)  
      )
  })
  
  # Render the bar plot as an interactive image
  output$barplotpsc <- renderPlotly({
    ggplotly(plotInputc())  
  })
  
  # Download handler for significant phenotypes condition data
  output$downloadc <- downloadHandler(
    # Generate the filename dynamically with the current date
    filename = function() { 
      paste("Significant_Phenotypes_Condition_", Sys.Date(), ".csv", sep = "")  
    },
    
    # Define the content of the file to be downloaded
    content = function(file) {
      write.csv(
        Conditions_phenotypes_sig(),  
        file,                         
        row.names = FALSE             
      )
    }
  )
  
  # Download handler for significant phenotypes condition plot
  output$downloadPlotc <- downloadHandler(
    # Generate the filename dynamically based on the selected condition(s) and current date
    filename = function() { 
      paste(input$cond1, 'Significant_Phenotypes_Condition.pdf', sep = '_')  
    },
    
    # Define the content of the file to be downloaded
    content = function(file) {
      ggsave(
        file,              
        plotInputc(),      
        width = 8,         
        height = 5         
      )
    }
  )
  
  # Server logic for Genes Correlations
  
  # Reactive function for all gene correlations
  all_gene_cor <- reactive({
    req(input$Gene2)  
    
    # Filter the gene correlation dataset
    dplyr::filter(
      gene_cor_file, 
      Gene_1 %in% input$Gene2 
    )
  })
  
  # Render the "All Correlations" table
  output$genes_correlations_filetable <- DT::renderDataTable({
    # Create a DataTable using the filtered gene correlations dataset
    DT::datatable(
      all_gene_cor(),  
      options = list(
        pageLength = 10,  
        scrollX = TRUE    
      ),
      rownames = FALSE  
    )
  })
  
  # Download handler for the "All Correlations" table
  output$downloadAllCorrelations <- downloadHandler(
    # Generate the filename dynamically with the current date
    filename = function() { 
      paste("All_Correlations_", Sys.Date(), ".csv", sep = "")  
    },
    
    # Define the content of the file to be downloaded
    content = function(file) {
      write.csv(
        all_gene_cor(),       
        file,            
        row.names = FALSE 
      )
    }
  )
  
  # Reactive function to filter gene correlations based on FDR threshold
  sig_gene_cor <- reactive({
    all_gene_cor() %>%  
      dplyr::filter(
        as.numeric(qval) <= (input$FDRq2 / 100)  
      )
  })
  
  # Render the data table of significant gene correlations
  output$genes_correlations_sig_filetable <- DT::renderDataTable({
    # Create a DataTable using the filtered significant correlations dataset
    DT::datatable(
      sig_gene_cor(),  
      options = list(
        pageLength = 5,  
        scrollX = TRUE   
      ),
      rownames = FALSE  
    )
  })
  
  # Download handler for significant correlations data
  output$download_gene_cor <- downloadHandler(
    # Define the filename for the downloaded file
    filename = function() { 
      paste("Genes_Correlation_", Sys.Date(), ".csv", sep = "")
    }
    ,
    
    # Define the content of the file to be downloaded
    content = function(fname) {
      write.csv(
        sig_gene_cor(),  
        fname,           
        row.names = FALSE  
      )
    }
  )
  
  # Reactive function to plot significant gene correlations
  plotInputcor <- reactive({
    ggplot(
      sig_gene_cor(),  
      aes(y = Dataset, fill = Correlation)  
    ) +
      geom_bar(stat = "count") +  # Create a bar plot where bar heights represent counts of Pearson r values
      scale_fill_manual(
        values = c("Positive" = "#08589e", "Negative" = "#edf8fb")  
      ) +
      ylab("") +  # Remove the y-axis label for a cleaner appearance
      xlab("Count of Pearson r values > 0.4 or < -0.4") +  # Set a descriptive label for the x-axis
      theme_classic() +  
      theme(
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),  
        legend.text = element_text(size = 10),  
        legend.title = element_text(size = 11), 
        axis.title.x = element_text(size = 11),  
        plot.title = element_text(size = 16, face = "bold")  
      )
  })
  
  # Render the interactive plot of significant gene correlations
output$barplot <- renderPlotly({
  ggplotly(plotInputcor())  
})

# Download handler for the gene correlations plot
output$downloadcorplot <- downloadHandler(
  # Define the filename dynamically based on the selected gene(s)
  filename = function() { 
    paste(input$Gene2, 'Genes_Correlation.pdf', sep = '_')  
  },
  
  # Define the content of the file to be downloaded
  content = function(file) {
    ggsave(
      file,               
      plotInputcor(),    
      width = 8,          
      height = 5          
    )
  }
)

# Reactive function for all condition correlations
all_cond_cor <- reactive({
  # Filter the condition correlation dataset to include rows where Condition_1 matches the selected condition(s)
  dplyr::filter(
    cond_cor_file,  
    Condition_1 %in% input$cond2  
  )
})

# Render the "All Correlations" table for conditions
output$conditions_correlations_filetable <- DT::renderDataTable({
  # Create a DataTable using the filtered condition correlations dataset
  DT::datatable(
    all_cond_cor(),  
    options = list(
      pageLength = 10,  
      scrollX = TRUE    
    ),
    rownames = FALSE  
  )
})
  
# Download handler for the "All Correlations" table for conditions
output$downloadAllCondCorrelations <- downloadHandler(
  # Define the filename dynamically with the current date
  filename = function() {
    paste("All_Correlations_", Sys.Date(), ".csv", sep = "")  
  },
  
  # Define the content of the file to be downloaded
  content = function(file) {
    write.csv(
      all_cond_cor(),  
      file,            
      row.names = FALSE  
    )
  }
)
  
# Reactive function to filter condition correlations based on the FDR threshold
condsig <- reactive({
  all_cond_cor() %>%  
    dplyr::filter(
      as.numeric(qval) <= (input$FDRq3 / 100)  
    )
})
  
# Render the data table of significant condition correlations
output$conditions_correlations_sig_filetable <- DT::renderDataTable({
  # Create a DataTable using the filtered significant correlations dataset
  DT::datatable(
    condsig(),  
    options = list(
      pageLength = 5, 
      scrollX = TRUE   
    ),
    rownames = FALSE  
  )
})

# Download handler for significant condition correlations data
output$download_cond_cor <- downloadHandler(
  # Define the filename for the downloaded file
  filename = function() {
    paste("Conditions_Correlation_", Sys.Date(), ".csv", sep = "")
  },
  
  # Define the content of the file to be downloaded
  content = function(fname) {
    write.csv(
      condsig(),    
      fname,         
      row.names = FALSE  
    )
  }
)
  
# Reactive function to plot significant condition correlations
plotInputcor_cond <- reactive({
  ggplot(
    condsig(),  
    aes(y = Dataset, fill = Correlation)  
  ) +
    geom_bar(stat = "count") +  
    scale_fill_manual(
      values = c("Positive" = "#756bb1", "Negative" = "#dadaeb")  
    ) +
    ylab("") +  # Remove the y-axis label for a cleaner appearance
    xlab("Count of Pearson r values > 0.4 or < -0.4") + 
    theme_classic() +  
    theme(
      axis.text.x = element_text(size = 10),  
      axis.text.y = element_text(size = 10),  
      legend.text = element_text(size = 10),  
      legend.title = element_text(size = 11),  
      axis.title.x = element_text(size = 11),  
      plot.title = element_text(size = 16, face = "bold")  
    )
})

# Render the plot of significant condition correlations as an interactive plot
output$barplot2 <- renderPlotly({
  ggplotly(plotInputcor_cond())  
})

  
# Download handler for the condition correlations plot
output$downloadcorplot2 <- downloadHandler(
  # Define the filename dynamically based on the selected condition(s)
  filename = function() { 
    paste(input$cond2, 'Conditions_Correlation.pdf', sep = '_')  
  },
  
  # Define the content of the file to be downloaded
  content = function(file) {
    ggsave(
      file,               
      plotInputcor_cond(), 
      width = 8,          
      height = 5         
    )
  }
)

#### Server logic for GO Enrichment ####

# Reactive function to filter GO enrichment data based on selected genes
all_go <- reactive({
  # Filter the GO enrichment dataset to include rows where the Gene matches the selected gene(s)
  dplyr::filter(
    go_file,  
    Gene %in% input$Gene3  
  )
})

# Render the "All GO Enrichment" table
output$filetableGO <- DT::renderDataTable({
  # Create a DataTable using the filtered GO enrichment dataset
  DT::datatable(
    all_go(),  
    options = list(
      pageLength = 10,  
      scrollX = TRUE    
    ),
    rownames = FALSE  
  )
})

# Download handler for the "All GO Enrichment" table
output$downloadgo <- downloadHandler(
  # Define the filename dynamically with the current date
  filename = function() { 
    paste("All_GO_Enrichment_", Sys.Date(), ".csv", sep = "")  
  },
  
  # Define the content of the file to be downloaded
  content = function(file) {
    write.csv(
      all_go(),       
      file,           
      row.names = FALSE  
    )
  }
)

# Reactive function to filter significant GO terms based on FDR threshold
sig_go <- reactive({
  dplyr::filter(
    all_go(),  
    qvalue <= (input$FDRq4 / 100)  
  )
})
  
# Render the data table of significant GO enrichment
output$filetableGOs <- DT::renderDataTable({
  # Create a DataTable using the filtered significant GO enrichment dataset
  DT::datatable(
    sig_go(),  
    options = list(
      pageLength = 5,  
      scrollX = TRUE   
    ),
    rownames = FALSE  
  )
})
  
# Download handler for significant GO enrichment data
output$downloadgos <- downloadHandler(
  # Define the filename for the downloaded file
  filename = function() { 
    "GO_enrichment.csv" 
  },
  
  # Define the content of the file to be downloaded
  content = function(fname) {
    write.csv(
      sig_go(),       
      fname,         
      row.names = FALSE  
    )
  }
)

# Reactive function to generate a plot for significant GO enrichment
plotInputGO <- reactive({
  sig_go() %>%  
    group_by(Description) %>%  # Group by the GO term description
    summarise(
      Count = sum(Count),  # Summarize by summing the counts for each GO term
      .groups = "drop"  # Remove grouping after summarisation
    ) %>%
    mutate(
      # Extract the part of the Description before the first comma
      Description = str_extract(Description, "^[^,]+"),
      # Reorder Description by Count for better plotting
      Description = fct_reorder(Description, Count)
    ) %>%
    ggplot(aes(x = Description, y = Count, fill = Description)) +  
    geom_bar(stat = "identity", width = 0.8) +  # Create a horizontal bar plot
    coord_flip() +  # Flip the axes for better readability of GO term names
    ylab("Count of Genes") +  # Set the y-axis label
    xlab("") +  # Leave the x-axis label blank
    scale_fill_manual(
      values = colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(n_distinct(sig_go()$Description))  # Dynamic colors for each GO term
    ) +
    theme_classic() +  
    theme(
      axis.text.x = element_text(size = 10), 
      axis.text.y = element_text(size = 10), 
      axis.title.y = element_text(size = 11), 
      plot.title = element_blank(),  
      legend.position = "none"  
    )
})
  
# Render the interactive plot for GO enrichment
output$barplotGO <- renderPlotly({
  ggplotly(plotInputGO())  
})

# Download handler for GO enrichment plot
output$downloadgoplot <- downloadHandler(
  # Define the filename dynamically based on the selected gene(s)
  filename = function() { 
    paste(input$Gene3, 'GO_enrichment.pdf', sep = '_')  
  },
  
  # Define the content of the file to be downloaded
  content = function(file) {
    ggsave(
      file,               
      plotInputGO(),      
      width = 12,         
      height = 5         
    )
  }
)

#### Server logic for KEGG Enrichment ####
  
# Reactive function to filter KEGG enrichment data based on selected genes
all_kegg <- reactive({
  # Filter the KEGG enrichment dataset to include rows where the Gene matches the selected gene(s)
  dplyr::filter(
    kegg_file,  
    Gene %in% input$Gene4  
  )
})

# Render the "All KEGG Enrichment" table
output$filetableKEGG <- DT::renderDataTable({
  # Create a DataTable using the filtered KEGG enrichment dataset
  DT::datatable(
    all_kegg(),  
    options = list(
      pageLength = 10,  
      scrollX = TRUE    
    ),
    rownames = FALSE  
  )
})

# Download handler for the "All KEGG Enrichment" table
output$downloadkegg <- downloadHandler(
  # Define the filename dynamically with the current date
  filename = function() { 
    paste("All_KEGG_Enrichment_", Sys.Date(), ".csv", sep = "") 
  },
  
  # Define the content of the file to be downloaded
  content = function(file) {
    write.csv(
      all_kegg(),       
      file,            
      row.names = FALSE  
    )
  }
)

# Reactive function to filter significant KEGG terms based on the FDR threshold
sig_kegg <- reactive({
  dplyr::filter(
    all_kegg(), 
    qvalue <= (input$FDRq5 / 100) 
  )
})
  
# Render the data table of significant KEGG enrichment
output$filetableKEGGs <- DT::renderDataTable({
  # Create a DataTable using the filtered significant KEGG enrichment dataset
  DT::datatable(
    sig_kegg(),  
    options = list(
      pageLength = 5,  
      scrollX = TRUE   
    ),
    rownames = FALSE  
  )
})
  
# Download handler for significant KEGG enrichment data
output$downloadkeggs <- downloadHandler(
  # Define the filename for the downloaded file
  filename = function() {
    paste("KEGG_enrichment_", Sys.Date(), ".csv", sep = "")
  },
  
  # Define the content of the file to be downloaded
  content = function(fname) {
    write.csv(
      sig_kegg(),       
      fname,            
      row.names = FALSE  
    )
  }
)
  
# Reactive function to generate a plot for significant KEGG enrichment
plotInputKEGG <- reactive({
  sig_kegg() %>%  
    group_by(Description) %>%  
    summarise(
      Count = sum(Count),  
      .groups = "drop"  
    ) %>%
    mutate(
      Description = fct_reorder(Description, Count)  
    ) %>%
    ggplot(aes(x = Description, y = Count, fill = Description)) +  
    geom_bar(stat = "identity", width = 0.8) +  
    coord_flip() +  
    ylab("Count of Genes") + 
    xlab("") +  
    scale_fill_manual(
      values = colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(nrow(sig_kegg()))  
    ) +
    theme_classic() +  
    theme(
      axis.text.x = element_text(size = 10),  
      axis.text.y = element_text(size = 10),  
      axis.title.y = element_text(size = 11),  
      plot.title = element_blank(),  
      legend.position = "none"  
    )
})

# Render the interactive plot for KEGG enrichment
output$barplotKEGG <- renderPlotly({
  ggplotly(plotInputKEGG())  
})

# Download handler for KEGG enrichment plot
output$downloadkeggplot <- downloadHandler(
  # Define the filename dynamically based on the selected gene(s)
  filename = function() { 
    paste(input$Gene4, 'KEGG_enrichment.pdf', sep = '_')  
  },
  
  # Define the content of the file to be downloaded
  content = function(file) {
    ggsave(
      file,              
      plotInputKEGG(),   
      width = 12,        
      height = 5          
    )
  }
)
  
  #### Server logic for Heatmaps ####
  
# Reactive function to retrieve the selected heatmap dataset
selected_heatmap_data <- reactive({
  req(input$selected_dataset)  
  heatmap_datasets[[input$selected_dataset]]  # Retrieve the dataset corresponding to the selected option
})
  
# Render the UI for selecting genes based on the selected heatmap dataset
output$gene_selector <- renderUI({
  req(selected_heatmap_data()) 
  genes <- rownames(selected_heatmap_data())  

  # Generate a multi-select dropdown for selecting genes
  selectInput(
    inputId = "genes",  
    label = "Select Genes",  
    choices = c("All", genes),  # Include an "All" option followed by the list of genes
    multiple = TRUE,  
    selected = genes[1:7]  
  )
})
  
# Render the UI for selecting conditions based on the selected heatmap dataset
output$condition_selector <- renderUI({
  req(selected_heatmap_data())  
  conditions <- colnames(selected_heatmap_data())  
  
  # Generate a multi-select dropdown for selecting conditions
  selectInput(
    inputId = "conditions",  
    label = "Select Conditions",  
    choices = c("All", conditions),  
    multiple = TRUE,  
    selected = conditions[1:15]  
  )
})

heatmap_data <- reactive({
  # Reactive function to dynamically generate heatmap data based on user selections.
  req(selected_heatmap_data(), input$genes, input$conditions)
  selected_genes <- if ("All" %in% input$genes) rownames(selected_heatmap_data()) else input$genes # If the user selects "All" for genes, include all row names from the dataset.
  selected_conditions <- if ("All" %in% input$conditions) colnames(selected_heatmap_data()) else input$conditions # If the user selects "All" for conditions, include all column names from the dataset.

    # Extract selected data
  data <- selected_heatmap_data()[selected_genes, selected_conditions, drop = FALSE]
  
  # Apply clustering if needed
  if (input$cluster_rows) {
    # If the user has selected to cluster rows:
    row_dist <- dist(data, method = input$distance_metric) # Computes the distance matrix for rows using the user-specified distance metric.
    row_clust <- hclust(row_dist, method = input$clustering_method)  # Performs hierarchical clustering on the row distance matrix using the user-specified clustering method.
    data <- data[order.dendrogram(as.dendrogram(row_clust)), ] # Reorders the rows of the data based on the clustering results.
  }
  
  if (input$cluster_columns) {
    # If the user has selected to cluster columns:
    col_dist <- dist(t(data), method = input$distance_metric)
    col_clust <- hclust(col_dist, method = input$clustering_method)
    data <- data[, order.dendrogram(as.dendrogram(col_clust))]
  }
  
  data
  # Returns the processed data, either clustered or in its original order, based on user inputs.
})

# Render an interactive heatmap using Plotly
output$heatmap_interactive <- renderPlotly({ # Defines a Plotly-based heatmap output that dynamically updates based on reactive heatmap data.
    req(heatmap_data())
  # Convert heatmap data to long format for ggplot
  melted_data <- heatmap_data() %>% # Uses the reactive heatmap data as the input for conversion.
    as.data.frame() %>% # Converts the heatmap data into a data frame to facilitate transformations.
    rownames_to_column(var = "Gene") %>% 
    reshape2::melt( # Reshapes the data from wide format (conditions as columns) to long format (one row per gene-condition pair).
      id.vars = "Gene", # Specifies "Gene" as the identifier column to remain unchanged during reshaping.
      variable.name = "Condition",  # Renames the wide-format columns (conditions) as "Condition" in the long-format output.
      value.name = "Fitness" # Stores the values (fitness scores) in a new column named "Fitness" in the reshaped data.
    )
  
  # Create the heatmap plot using ggplot2
  p <- ggplot(melted_data, aes(x = Condition, y = Gene, fill = Fitness)) +  # Initializes the ggplot object with the melted data.
    geom_tile() + # Uses tiles to create the heatmap cells, where each cell represents a gene-condition pair.
    scale_fill_viridis() + # Applies the Viridis color scale for the "Fitness" variable
    theme_minimal() +                  
    labs(                              
      x = NULL,                      
      y = NULL,                      
      fill = "Fitness Score"   # Sets the legend title for the color scale.
    ) +
    
    theme(                             
      axis.text.x = element_text(    
        angle = 90,                # Rotates the x-axis labels 90 degrees.
        hjust = 1                  # Aligns the labels horizontally for clarity.
      )
    )
  
  # Convert the static ggplot heatmap into an interactive Plotly heatmap
  ggplotly(p) %>% # Enables interactive features such as zooming, panning, and tooltips.
    layout(xaxis = list(tickangle = 90)) 
})

# Render a heatmap with dendrograms for clustering
output$heatmap_dendrogram <- renderPlot({
  req(heatmap_data())
  # Generate the heatmap using the ComplexHeatmap package
  Heatmap(
    heatmap_data(), # The matrix or data frame to be visualized in the heatmap.
    name = "Fitness Score", # Title for the color scale (legend) indicating the fitness scores.
    # Clustering options
    cluster_rows = input$cluster_rows, # Enables or disables clustering of rows based on user input.
    cluster_columns = input$cluster_columns, # Enables or disables clustering of columns based on user input.
    clustering_distance_rows = input$distance_metric, # Specifies the distance metric (e.g., Euclidean, Manhattan) for row clustering.
    clustering_distance_columns = input$distance_metric, # Specifies the distance metric for column clustering.
    clustering_method_rows = input$clustering_method, # Specifies the clustering method (e.g., complete, average, single) for rows.
    clustering_method_columns = input$clustering_method, # Specifies the clustering method for columns.

    # Legend customization
    heatmap_legend_param = list(
      title_gp = gpar(fontsize = 14), # Sets the font size for the legend title.
      labels_gp = gpar(fontsize = 13),# the font size for the legend labels.
      legend_height = unit(4, "cm"), # the height of the legend box.
      legend_width = unit(3, "cm") #  the width of the legend box.
    )
  )
})

  
# Render a UI element for selecting a dataset
output$dataset_selector <- renderUI({ # Dynamically generates a dropdown menu for dataset selection in the UI.
  selectInput(
    inputId = "selected_dataset", # Unique identifier for the dropdown menu. This ID will be used to access the user's selection in the server logic.
    label = "Select Dataset", # Label displayed above the dropdown menu to guide the user.
    choices = names(heatmap_datasets), # Populates the dropdown menu with the names of available datasets.
    selected = "Nichols, R. et al." # Pre-selects a default dataset ("Nichols, R. et al.") in the dropdown when the UI is first rendered.
  )
})

# Create a download handler for exporting heatmap data
output$downloadHeatmapData <- downloadHandler( 
  filename = function() {
    paste("Heatmap_Data_", Sys.Date(), ".csv", sep = "")
  },
  
  # Define the content of the downloaded file
  content = function(file) {
    write.csv(
      as.data.frame(heatmap_data()), 
      file,                          
      row.names = TRUE               
    )
  }
)

# Add a Download Handler for the Dendrogram Plot
output$downloadDendrogramPlot <- downloadHandler(
  filename = function() {
    paste("Dendrogram_Plot_", Sys.Date(), ".pdf", sep = "")
  },
  
  # Define the content of the downloaded file
  content = function(file) {
    pdf(file, width = 10, height = 8)
    draw(
      Heatmap(
        heatmap_data(),                   
        name = "Fitness Score",           
        cluster_rows = input$cluster_rows,       
        cluster_columns = input$cluster_columns, 
        clustering_distance_rows = input$distance_metric, 
        clustering_distance_columns = input$distance_metric, 
        clustering_method_rows = input$clustering_method,   
        clustering_method_columns = input$clustering_method 
      )
    )
    dev.off()
  }
)

  # Interactive heatmap plot download
  output$downloadInteractiveHeatmapPlot <- downloadHandler(
    filename = function() {
      paste("Interactive_Heatmap_Plot_", Sys.Date(), ".pdf", sep = "") 
    },
    
    # Define the content of the downloaded file
    content = function(file) {
      # Convert heatmap data to long format for ggplot
      melted_data <- heatmap_data() %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "Gene") %>% 
        reshape2::melt(                     
          id.vars = "Gene",               
          variable.name = "Condition",   
          value.name = "Fitness"        
        )
      
      # Create the static ggplot heatmap
      p <- ggplot(melted_data, aes(x = Condition, y = Gene, fill = Fitness)) + 
        geom_tile() +                      
        scale_fill_viridis() +             
        theme_minimal() +                  
        labs(                              
          x = NULL,                      
          y = NULL,                     
          fill = "Fitness Score"         
        ) +
        theme(                             
          axis.text.x = element_text(    
            angle = 90,               
            hjust = 1                 
          )
        )
      # Save the plot to file as a PDF
      ggsave(
        file,                             
        plot = p,                          
        width = 10,                        
        height = 8,                        
        dpi = 300                          
      )
    }
  )
  
  ######## Server logic for the "Upload Your Dataset" tab ########
  
  # Reactive function to read the uploaded dataset
  dataset <- reactive({
    req(input$file_upload)  # Ensure a file has been uploaded
    file <- input$file_upload$datapath
    
    tryCatch({
      # Check the file extension
      if (tools::file_ext(file) == "rds") {
        # Load the .rds file
        data <- readRDS(file)
      } else if (tools::file_ext(file) == "csv") {
        # Load the .csv file
        data <- fread(file)
        rownames(data) <- data[[1]]  # Set the first column as row names
        data <- data[ , -1, with = FALSE]  # Remove the first column
      } else {
        stop("Unsupported file type. Please upload a .rds or .csv file.")
      }
      
      # Ensure all columns are numeric
      if (!all(sapply(data, is.numeric))) {
        stop("The uploaded dataset must contain only numeric values.")
      }
      
      # Replace missing values
      data[is.na(data)] <- median(as.numeric(unlist(data)), na.rm = TRUE)
      
      # Return the processed data
      as.data.frame(data)
      
    }, error = function(e) {
      showNotification(paste("Error processing file:", e$message), type = "error")
      NULL
    })
  })
  
  
  # Function to compute Median Absolute Deviation (MAD)
  compute_mad <- function(data, c = 0.6745) {
    data <- na.omit(data) # Remove NA values from the input data
    median_val <- median(data) # Calculate the median of the data
    
    # Compute the Median Absolute Deviation (MAD)
    # MAD = median(|data - median(data)|) / c
    return(median(abs(data - median_val)) / c)
  }
  
  # Compute FDR results when a dataset is uploaded
  fdr_results <- reactive({
    req(dataset())
    Scored_Dataset <- dataset() # Retrieve the uploaded dataset
    
    # Calculate FDR-adjusted p-values for each column in the dataset
    Scored_Dataset %>%
      mutate(  # Apply transformations to all columns
        across(everything(), ~ {  # Iterate over all columns in the dataset
          z_scores <- (.- median(., na.rm = TRUE)) / compute_mad(.)  # Compute Z-scores
          p_values <- 2 * pnorm(-abs(z_scores))                      # Convert Z-scores to two-tailed p-values
          p.adjust(p_values, method = "fdr")                        # Adjust p-values using the False Discovery Rate (FDR) method
        })
      ) %>%
      as_tibble(rownames = "Gene")  # Convert the result to a tibble with row names as a new column "Gene"
  })
  
  # Combine S Scores with FDR values
  Score_plus_FDR <- reactive({
    req(fdr_results())
    # Transform the dataset into a long format with rounded scores
    Scored_Dataset_fdr <- dataset() %>% 
      rownames_to_column("Gene") %>% 
      pivot_longer(               
        -Gene,                    
        names_to = "Condition",   
        values_to = "Score"       
      ) %>% 
      mutate(Score = round(Score, 4)) # Round the "Score" values to 4 decimal points

        # Transform FDR results into a long format with rounded FDR values
    fdr_long <- fdr_results() %>% 
      pivot_longer(                
        -Gene,                    
        names_to = "Condition",   
        values_to = "FDR"         
      ) %>% 
      mutate(FDR = round(FDR, 4))  
    
    # Combine the dataset with FDR results
    combined_data <- Scored_Dataset_fdr %>% 
      left_join(fdr_long, by = c("Gene", "Condition")) # Merge datasets by "Gene" and "Condition"

    # Return the combined dataset with scores and FDR values
    return(combined_data)
  })
    observe({
    req(Score_plus_FDR())
    unique_genes2 <- unique(Score_plus_FDR()$Gene)       
    unique_conditions <- unique(Score_plus_FDR()$Condition) 
    
    # Update the gene dropdown input dynamically
    updateSelectizeInput(
      session,                                 
      "Gene5",                                 
      choices = unique_genes2,                
      selected = if (length(unique_genes2) >= 2) 
        unique_genes2[1:4] else unique_genes2, 
      options = list(                         
        placeholder = "Type to search...",  
        maxOptions = length(unique_genes2)  
      ),
      server = TRUE                           
    )
    
    # Update the condition dropdown input dynamically
    updateSelectizeInput(
      session,                                 
      "Condition1",                            
      choices = unique_conditions,           
      selected = if (length(unique_conditions) >= 2) 
        unique_conditions[1:4] else unique_conditions, 
      options = list(                         
        placeholder = "Type to search...",  
        maxOptions = length(unique_conditions) 
      ),
      server = TRUE                          
    )
  })
  
  # Generate filtered phenotype data for selected genes at the gene level
  Genes_phenotypes_2 <- reactive({
    req(input$Gene5, Score_plus_FDR())
        dplyr::filter(Score_plus_FDR(), Gene %in% input$Gene5) %>%
      mutate(
        Fitness = case_when(
          Score > 0 ~ 'Increased', 
          Score < 0 ~ 'Decreased',  
          Score == 0 ~ 'Neutral'   
        )
      )
  })
  
  # Generate filtered phenotype data for selected conditions at the condition level
  Conditions_phenotypes_2 <- reactive({
    req(input$Condition1, Score_plus_FDR())
        dplyr::filter(Score_plus_FDR(), Condition %in% input$Condition1) %>%
      mutate(
        Fitness = case_when(
          Score > 0 ~ 'Increased',  
          Score < 0 ~ 'Decreased',  
          Score == 0 ~ 'Neutral'   
        )
      )
  })
  
  # Render the "All Phenotypes" table for Gene-Level
  output$filetablegene <- DT::renderDataTable({
    req(Genes_phenotypes_2())
    # Create a DataTable for displaying gene-level phenotype data
    DT::datatable(
      Genes_phenotypes_2(),                 
      options = list(                      
        pageLength = 10,                  
        scrollX = TRUE                     
      ),
      rownames = FALSE                      
    )
  })
  
  # Render the "All Phenotypes" table for Condition-Level
  output$filetablecondition <- DT::renderDataTable({
    req(Conditions_phenotypes_2())
    # Create a DataTable for displaying condition-level phenotype data
    DT::datatable(
      Conditions_phenotypes_2(),             
      options = list(                        
        pageLength = 10,                  
        scrollX = TRUE                  
      ),
      rownames = FALSE                       
    )
  })
  
  # Generate significant phenotypes for Gene-Level
  Genes_phenotypes_sig_2 <- reactive({
    req(Genes_phenotypes_2())
    Genes_phenotypes_2() %>%
      dplyr::filter(                          
        as.numeric(FDR) <= (input$FDRq_5 / 100) 
      )
  })
  
  # Generate significant phenotypes for Condition-Level
  Conditions_phenotypes_sig_2 <- reactive({
    req(Conditions_phenotypes_2())
    Conditions_phenotypes_2() %>%
      dplyr::filter(as.numeric(FDR) <= (input$FDRq1 / 100))
  })
  
  # Render significant phenotypes table for Gene-Level
  output$filetablegenesig <- DT::renderDataTable({
    req(Genes_phenotypes_sig_2())
    DT::datatable(
      Genes_phenotypes_sig_2(), 
      options = list(
        pageLength = 5,  
        scrollX = TRUE   
      ),
      rownames = FALSE
    )
  })
  
  # Render significant phenotypes table for Condition-Level
  output$filetableconditionsig <- DT::renderDataTable({
    req(Conditions_phenotypes_sig_2())
    DT::datatable(
      Conditions_phenotypes_sig_2(), 
      options = list(
        pageLength = 5,  
        scrollX = TRUE   
      ),
      rownames = FALSE
    )
  })

  # Render bar plot for significant phenotypes for Gene-Level
  output$barplotgenesig <- renderPlotly({
    req(Genes_phenotypes_sig_2())
    plot <- Genes_phenotypes_sig_2() %>%
      group_by(Gene, Fitness) %>%
      summarise(Count = n(), .groups = "drop") %>%
      ggplot(aes(x = Count, y = Gene, fill = Fitness)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Decreased" = "#d0f0f0", "Increased" = "#238b8e", "Neutral" = "#808080")) +
      theme_classic() +
      xlab("Count of Phenotypes") +
      ylab(NULL) +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11)
      )
    ggplotly(plot)
  })
  
  # Render bar plot for significant phenotypes for Condition-Level
  output$barplotconditionsig <- renderPlotly({
    req(Conditions_phenotypes_sig_2())
    plot <- Conditions_phenotypes_sig_2() %>%
      group_by(Condition, Fitness) %>%
      summarise(Count = n(), .groups = "drop") %>%
      ggplot(aes(x = Count, y = Condition, fill = Fitness)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Decreased" = "#f6e4d9", "Increased" = "#f4a582", "Neutral" = "#808080")) +
      theme_classic() +
      xlab("Count of Phenotypes") +
      ylab(NULL) +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11)
      )
    ggplotly(plot)
  })
  
  # Download handler for all phenotypes in Gene-Level
  output$downloadAllGenesPhenotypes_2 <- downloadHandler(
    filename = function() {
      paste("Gene_Level_All_Phenotypes_", Sys.Date(), ".csv", sep = "")
    },
      content = function(file) {
      req(Genes_phenotypes_2())
      write.csv(Genes_phenotypes_2(), file, row.names = FALSE)
    }
  )
  
  # Download handler for significant phenotypes in Gene-Level
  output$downloadGeneSignificant <- downloadHandler(
    filename = function() {
      paste("Gene_Level_Significant_Phenotypes_", Sys.Date(), ".csv", sep = "")
    },
    
    # Function to write the content to the specified file
    content = function(file) {
      req(Genes_phenotypes_sig_2())
      write.csv(Genes_phenotypes_sig_2(), file, row.names = FALSE)
    }
  )
  
  # Download handler for the significant phenotype bar plot in Gene-Level
  output$downloadGenePlot <- downloadHandler(
    filename = function() {
      paste("Gene_Level_Significant_Phenotypes_Plot_", Sys.Date(), ".pdf", sep = "")
    },
    
    # Function to create and save the plot to the specified file
    content = function(file) {
      # Generate the plot using ggplot2
      plot <- Genes_phenotypes_sig_2() %>%
        group_by(Gene, Fitness) %>%
        summarise(Count = n(), .groups = "drop") %>%
        ggplot(aes(x = Count, y = Gene, fill = Fitness)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c(
          "Decreased" = "#d0f0f0",  
          "Increased" = "#238b8e",  
          "Neutral" = "#808080"     
        )) +
        theme_classic() +
        xlab("Count of Phenotypes") +
        theme(
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)
        )
      ggsave(file, plot = plot, width = 8, height = 6, dpi = 300)
    }
  )
  
  # Download handler for all phenotypes in Condition-Level
  output$downloadAllConditionsPhenotypes <- downloadHandler(
    filename = function() {
      paste("Condition_Level_All_Phenotypes_", Sys.Date(), ".csv", sep = "")
    },
      content = function(file) {
      req(Conditions_phenotypes_2())
      write.csv(Conditions_phenotypes_2(), file, row.names = FALSE)
    }
  )
  
  # Download handler for significant phenotypes in Condition-Level
  output$downloadConditionSignificant <- downloadHandler(
    filename = function() {
      paste("Condition_Level_Significant_Phenotypes_", Sys.Date(), ".csv", sep = "")
    },
    
    # Function to write the content to the specified file
    content = function(file) {
      req(Conditions_phenotypes_sig_2())
      write.csv(Conditions_phenotypes_sig_2(), file, row.names = FALSE)
    }
  )
  
  # Download handler for generating and saving the significant phenotype bar plot at the Condition level
  output$downloadConditionPlot <- downloadHandler(
    filename = function() { 
      paste("Condition_Level_Significant_Phenotypes_Plot_", Sys.Date(), ".pdf", sep = "") 
    },
    content = function(file) {
      ggsave(
        file, 
        
        # Create the bar plot using the processed significant phenotype data
        plot = Conditions_phenotypes_sig_2() %>%
          group_by(Condition, Fitness) %>%                
          summarise(Count = n(), .groups = "drop") %>%    
          ggplot(aes(x = Count, y = Condition, fill = Fitness)) +  
          geom_bar(stat = "identity") +                 
          scale_fill_manual(                             
            values = c(
              "Decreased" = "#f6e4d9", 
              "Increased" = "#f4a582", 
              "Neutral" = "#808080"
            )
          ) +
          theme_classic() +                             
          xlab("Count of Phenotypes") +                  
          theme(                                         
            axis.text.x = element_text(size = 10), 
            axis.text.y = element_text(size = 10)
          ),
        # Define the dimensions and resolution of the saved plot
        width = 8, height = 6, dpi = 300
      )
    }
  )
  
  # Gene Correlation Analysis 
  
  #### Reactive Function to Generate Correlation Dataset ####
  gene_correlation <- reactive({
    shinyjs::show("loading_gene_corr") # Show loading indicator for gene correlation analysis
    on.exit(shinyjs::hide("loading_gene_corr")) # Hide loading indicator when function finishes executing
    Scored_Dataset <- dataset() # Retrieve the dataset to be used for correlation analysis
    # Ensure the dataset is numeric; if not, stop execution and show an error
    if (!all(sapply(Scored_Dataset, is.numeric))) {
      stop("Correlation analysis requires a numeric dataset.")
    }
    
    # Handle missing values: replace NAs with the median of the entire dataset
    Scored_Dataset[is.na(Scored_Dataset)] <- median(as.numeric(unlist(Scored_Dataset)), na.rm = TRUE)
    
    # Transpose the dataset to compute correlations across genes (columns as genes)
    gene_cor <- t(as.matrix(Scored_Dataset))
    
    # Compute Pearson correlation coefficients and p-values using Hmisc::rcorr
    rcorr_results <- rcorr(as.matrix(gene_cor), type = "pearson")
    
    # Extract the correlation matrix (r-values) and p-value matrix from rcorr results
    gene_cor_matrix <- rcorr_results$r
    gene_p_matrix <- rcorr_results$P
    
    # Adjust p-values for multiple testing using the False Discovery Rate (FDR) method
    adjusted_p_matrix <- p.adjust(as.vector(gene_p_matrix), method = "fdr")
    
    # Reshape the adjusted p-value matrix to match the original dimensions
    adjusted_p_matrix <- matrix(adjusted_p_matrix, 
                                nrow = nrow(gene_p_matrix), 
                                ncol = ncol(gene_p_matrix), 
                                dimnames = dimnames(gene_p_matrix))
    
    # Melt the correlation matrix into a long format (Gene_1, Gene_2, Correlation)
    melted_gene_cor <- reshape2::melt(gene_cor_matrix, varnames = c("Gene_1", "Gene_2"), value.name = "Correlation (r)")
    
    # Melt the adjusted p-value matrix into a long format (Gene_1, Gene_2, qval)
    melted_p_values <- reshape2::melt(adjusted_p_matrix, varnames = c("Gene_1", "Gene_2"), value.name = "qval")
    
    # Combine the correlation and p-value data frames
    combined_data <- melted_gene_cor %>%
      left_join(melted_p_values, by = c("Gene_1", "Gene_2")) %>%   # Join by Gene_1 and Gene_2
      dplyr::filter(abs(`Correlation (r)`) > 0.4 & Gene_1 != Gene_2) %>%  # Apply correlation threshold and exclude self-correlations
      mutate(
        `Correlation (r)` = round(`Correlation (r)`, 4),  # Round correlation values to 4 decimal places
        qval = formatC(as.numeric(qval), format = "e", digits = 4)  # Format p-values in scientific notation with 4 decimal places
      )
    
    # Output the final combined data as a data frame
    as.data.frame(combined_data)
  })
  
  # Reactive function to dynamically populate the gene dropdown input
  observe({
    req(gene_correlation())
    
    # Extract unique gene names from the Gene_1 and Gene_2 columns in the correlation dataset
    unique_genes2 <- unique(c(gene_correlation()$Gene_1, gene_correlation()$Gene_2))
    
    # Dynamically update the Gene6 selectize input with the extracted gene names
    updateSelectizeInput(
      session,              
      "Gene6",              
      choices = unique_genes2,  
      selected = if (length(unique_genes2) >= 2) 
        unique_genes2[1:4]  
      else 
        unique_genes2,     
      options = list(        
        placeholder = "Type to search...",  
        maxOptions = length(unique_genes2)  
      ),
      server = TRUE          
    )
  })
  
  # Reactive Function for Filtering and Categorizing All Gene Correlations
  all_gene_cor_2 <- reactive({
    req(input$Gene6, gene_correlation())
    gene_correlation() %>%
      dplyr::filter(
        Gene_1 %in% input$Gene6) %>%
      # Add a new column to categorize correlations as Positive or Negative
      mutate(
        Correlation = case_when(
          `Correlation (r)` > 0 ~ "Positive",  
          `Correlation (r)` < 0 ~ "Negative"  
        )
      )
  })
  
  # Reactive Function for Significant Correlations 
  sig_gene_cor_2 <- reactive({
    req(all_gene_cor_2(), input$FDRq6)
    all_gene_cor_2() %>%
      dplyr::filter(
        as.numeric(qval) <= (input$FDRq6 / 100)  
      )
  })
  observeEvent(input$FDRq6, { 
    sig_gene_cor_2() 
  })
  
  # Render "Significant Correlations" Table 
  output$filetablegenecorsig <- DT::renderDataTable({
    req(sig_gene_cor_2())
    
    # Render the significant correlations dataset as a DataTable
    DT::datatable(
      sig_gene_cor_2(),  
      options = list(
        pageLength = 5,    
        scrollX = TRUE    
      ),
      rownames = FALSE 
    )
  })
  
  
  # Plot Significant Correlations
  output$barplotgenecorsig <- renderPlotly({
    req(sig_gene_cor_2())
    
    # Create the plot with flipped axes and unified Gene column
    plot_cor <- sig_gene_cor_2() %>%
      pivot_longer(
        cols = c(Gene_1), 
        names_to = "Gene_Column", 
        values_to = "Gene"
      ) %>%
      group_by(Gene, Correlation) %>%
      summarise(Count = n(), .groups = "drop") %>%  
      ggplot(aes(x = Count, y = Gene, fill = Correlation)) +  
      geom_bar(stat = "identity") +  
      scale_fill_manual(
        values = c("Positive" = "#08589e", "Negative" = "#edf8fb")
      ) +
      xlab("Count of Significant Correlations (> 0.4 or < -0.4)") +
      ylab(NULL) +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 10),          
        axis.text.y = element_text(size = 10),         
        legend.text = element_text(size = 10),        
        legend.title = element_text(size = 11),        
        axis.title.x = element_text(size = 11),         
        plot.title = element_text(size = 16, face = "bold")  
      )
    ggplotly(plot_cor)
  })
  
  #nRender "All Correlations" Table 
  output$ iletablegenecor <- DT::renderDataTable({
    req(all_gene_cor_2())
    
    # Render the dataset as an interactive DataTable
    DT::datatable(
      all_gene_cor_2(),  
      options = list(
        pageLength = 10,    
        scrollX = TRUE    
      ),
      rownames = FALSE  
    )
  })
  
  # Download Handler for "All Correlations" Table
  output$downloadAllGenesCorrelations <- downloadHandler(
    filename = function() {
      paste("All_Correlations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        all_gene_cor_2(),  
        file,              
        row.names = FALSE  
      )
    }
  )
  
  #### Download Handler for Significant Correlations ####
  output$downloadGeneCorSignificant <- downloadHandler(
    filename = function() { 
      "Genes_Correlation.csv" 
    },
    content = function(fname) {
      write.csv(
        sig_gene_cor_2(),  
        fname,             
        row.names = FALSE  
      )
    }
  )
  
  # Download Handler for Plot 
  output$downloadGeneCorPlot <- downloadHandler(
    filename = function() { 
      paste(input$Gene6, 'Genes_Correlation.pdf', sep = '_')  
    },
    content = function(file) {
      ggsave(
        file,               
        plot = plotInputcor(),  
        width = 8,         
        height = 5         
      )
    }
  )
  
  # Download handler for significant correlations table in Gene-Level
  output$downloadGeneCorSignificant <- downloadHandler(
    filename = function() {
      paste("Gene_Level_Significant_Correlations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(sig_gene_cor_2())
      write.csv(
        sig_gene_cor_2(),  
        file,              
        row.names = FALSE  
      )
    }
  )
  
  # Download handler for all correlations table in Gene-Level
  output$downloadAllGenesCorrelations <- downloadHandler(
    filename = function() {
      paste("Gene_Level_All_Correlations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(all_gene_cor_2())
      write.csv(
        all_gene_cor_2(),  
        file,              
        row.names = FALSE  
      )
    }
  )
  
  # Download handler for significant correlation bar plot at the gene level
  output$downloadGeneCorPlot <- downloadHandler(
    filename = function() { 
      paste("Gene_Level_Significant_Correlations_Plot_", Sys.Date(), ".pdf", sep = "") 
    },
    content = function(file) {
      ggsave(
        file,  
        
        # Create the plot
        plot = sig_gene_cor_2() %>%
          pivot_longer(
            cols = c(Gene_1), 
            names_to = "Gene_Column", 
            values_to = "Gene"
          ) %>%
          group_by(Gene, Correlation) %>%
          summarise(Count = n(), .groups = "drop") %>%  
          
          # Create the bar plot
          ggplot(aes(x = Count, y = Gene, fill = Correlation)) + 
          geom_bar(stat = "identity") +  
          scale_fill_manual(
            values = c("Positive" = "#08589e", "Negative" = "#edf8fb")  
          ) +
          theme_classic() +  # Apply a classic theme
          xlab("Count of Significant Correlations (> 0.4 or < -0.4)") +  
          theme(
            axis.text.x = element_text(size = 10),  
            axis.text.y = element_text(size = 10)   
          ),
        width = 8,  
        height = 6, 
        dpi = 300 
      )
    }
  )
  
  
  # Condition Correlation Analysis 
  
  # Reactive Function to Generate Correlation Dataset ####
  condition_correlation <- reactive({
    shinyjs::show("loading_gene_corr") 
    on.exit(shinyjs::hide("loading_gene_corr"))
    Scored_Dataset <- dataset()
    if (!all(sapply(Scored_Dataset, is.numeric))) {
      stop("Correlation analysis requires a numeric dataset.")
    }
    Scored_Dataset[is.na(Scored_Dataset)] <- median(as.numeric(unlist(Scored_Dataset)), na.rm = TRUE)
    cond_cor <- as.matrix(Scored_Dataset)
    rcorr_results <- rcorr(as.matrix(Scored_Dataset), type = "pearson")
    condition_cor_matrix <- rcorr_results$r
    condition_p_matrix <- rcorr_results$P
    adjusted_p_matrix <- p.adjust(as.vector(condition_p_matrix), method = "fdr")
    adjusted_p_matrix <- matrix(
      adjusted_p_matrix, 
      nrow = nrow(condition_p_matrix), 
      ncol = ncol(condition_p_matrix), 
      dimnames = dimnames(condition_p_matrix)
    )
    melted_cor <- melt(
      condition_cor_matrix, 
      varnames = c("Condition_1", "Condition_2"), 
      value.name = "Correlation (r)"
    )
    melted_p_values <- melt(
      adjusted_p_matrix, 
      varnames = c("Condition_1", "Condition_2"), 
      value.name = "qval"
    )
    combined_data <- melted_cor %>%
      left_join(melted_p_values, by = c("Condition_1", "Condition_2")) %>%
      dplyr::filter(
        abs(`Correlation (r)`) > 0.4 & Condition_1 != Condition_2  
      ) %>%
      mutate(
        `Correlation (r)` = round(`Correlation (r)`, 4),  
        qval = formatC(as.numeric(qval), format = "e", digits = 4)  
      )
    combined_data
  })
  
  
  # Reactive function to dynamically populate the condition dropdown input
  observe({
    req(condition_correlation())
    unique_conditions_2 <- unique(condition_correlation()$Condition_1)
    updateSelectizeInput(
      session,             
      "Condition2",         
      choices = unique_conditions_2,  
      selected = if (length(unique_conditions_2) >= 2) 
        unique_conditions_2[1:4]  
      else 
        unique_conditions_2,     
      options = list(         
        placeholder = "Type to search...",   
        maxOptions = length(unique_conditions_2)  
      ),
      server = TRUE           
    )
  })
  
  # Reactive Function for All Gene Correlations 
  cond_cor <- reactive({
    req(input$Condition2, condition_correlation())
    condition_correlation() %>%
      dplyr::filter(
        Condition_1 %in% input$Condition2 
      ) %>%
      mutate(
        Correlation = case_when(
          `Correlation (r)` > 0 ~ "Positive",  
          `Correlation (r)` < 0 ~ "Negative"   
        )
      )
  })
  
  # Reactive Function for Significant Correlations 
  cond_cor_sig <- reactive({
    req(cond_cor(), input$FDRq3)
    cond_cor() %>%
      dplyr::filter(
        as.numeric(qval) <= (input$FDRq3 / 100)  
      )
  })
  observeEvent(input$FDRq3, {
    cond_cor_sig()
  })

  # Render "Significant Correlations" Table 
  output$filetableconditioncorsig <- DT::renderDataTable({
    req(cond_cor_sig())
    DT::datatable(
      cond_cor_sig(),  
      options = list(
        pageLength = 5,    
        scrollX = TRUE     
      ),
      rownames = FALSE 
    )
  })
  
  # Plot Significant Correlations 
  output$barplotconditioncorsig <- renderPlotly({
    req(cond_cor_sig())
    plot_cor <- cond_cor_sig() %>%
      group_by(Condition_1, Correlation) %>%
      summarise(
        Count = n(),         
        .groups = "drop"     
      ) %>%
      ggplot(aes(x = Count, y = Condition_1, fill = Correlation)) +  
      geom_bar(stat = "identity") + 
      scale_fill_manual(
        values = c(
          "Positive" = "#756bb1", 
          "Negative" = "#dadaeb"   
        )
      ) +
      xlab("Count of Significant Correlations (> 0.4 or < -0.4)") +
      ylab(NULL) +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 10),         
        axis.text.y = element_text(size = 10),          
        legend.text = element_text(size = 10),          
        legend.title = element_text(size = 11),         
        axis.title.x = element_text(size = 11),         
        plot.title = element_text(size = 16, face = "bold")  
      )
    ggplotly(plot_cor)
  })
  
  # Render "All Correlations" Table 
  output$filetableconditioncor <- DT::renderDataTable({
    req(cond_cor())
    DT::datatable(
      cond_cor(),  
      options = list(
        pageLength = 10,  
        scrollX = TRUE      
      ),
      rownames = FALSE  
    )
  })
  
  # Download Handler for "All Correlations" Table 
  output$downloadAllConditionsCorrelations <- downloadHandler(
    filename = function() {
      paste("All_Conditions_Correlations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(cond_cor())  
      write.csv(
        cond_cor(),     
        file,           
        row.names = FALSE  
      )
    }
  )
  
  # Download Handler for Significant Correlations 
  output$downloadConditionCorSignificant <- downloadHandler(
    filename = function() { 
      "Conditions_Correlation.csv"  
    },
    content = function(fname) {
      req(cond_cor_sig())

      write.csv(
        cond_cor_sig(),  
        fname,           
        row.names = FALSE  
      )
    }
  )
  
  # Download Handler for Plot 
  output$downloadConditionCorPlot <- downloadHandler(
    filename = function() { 
      paste(input$Condition1, input$Condition2, 'Conditions_Correlation.pdf', sep = '_') 
    },
    content = function(file) {
      ggsave(
        file,              
        plot = plotInputcor(), 
        width = 8,          
        height = 5          
      )
    }
  )
  
  # Download handler for significant correlations table in Condition-Level
  output$downloadConditionCorSignificant <- downloadHandler(
    filename = function() { 
      paste("Condition_Level_Significant_Correlations_", Sys.Date(), ".csv", sep = "") 
    },
    content = function(file) {
      req(cond_cor_sig())
      write.csv(
        cond_cor_sig(),   
        file,         
        row.names = FALSE 
      )
    }
  )
  
  # Download handler for all correlations table in Condition-Level
  output$downloadAllConditionsCorrelations <- downloadHandler(
    filename = function() { 
      paste("Condition_Level_All_Correlations_", Sys.Date(), ".csv", sep = "") 
    },
    content = function(file) {
      req(cond_cor())
      write.csv(
        cond_cor(),     
        file,           
        row.names = FALSE 
      )
    }
  )
  
  # Download handler for significant correlation bar plot in Condition-Level
  output$downloadConditionCorPlot <- downloadHandler(
    filename = function() { 
      paste("Condition_Level_Significant_Correlations_Plot_", Sys.Date(), ".pdf", sep = "") 
    },
    content = function(file) {
      ggsave(
        file,              
        plot = cond_cor_sig() %>%  
          group_by(Condition_1, Correlation) %>%  
          summarise(Count = n(), .groups = "drop") %>%  
          ggplot(aes(x = Count, y = Condition_1, fill = Correlation)) +  
          geom_bar(stat = "identity") +  
          scale_fill_manual(
            values = c("Positive" = "#756bb1", "Negative" = "#dadaeb")  
          ) +
          theme_classic() + 
          xlab("Count of Significant Correlations (> 0.4 or < -0.4)") +  
          theme(  
            axis.text.x = element_text(size = 10), 
            axis.text.y = element_text(size = 10)
          ),
        width = 8,   
        height = 6,  
        dpi = 300    
      )
    }
  )
  
  # Interactive Heatmap 
  
  # Reactive function to prepare the heatmap matrix
  heatmap_matrix <- reactive({
    req(dataset())
    shinyjs::show("loading_heatmap")
    on.exit(shinyjs::hide("loading_heatmap"))
    as.matrix(dataset())
  })
    heatmap_data_2 <- reactive({
    req(input$genes2, input$conditions, heatmap_matrix())
    selected_genes2 <- if ("All" %in% input$genes2) 
      rownames(heatmap_matrix()) 
    else 
      input$genes2  
    selected_conditions <- if ("All" %in% input$conditions) 
      colnames(heatmap_matrix()) 
    else 
      input$conditions  
    data <- heatmap_matrix()[selected_genes2, selected_conditions, drop = FALSE]
    if (input$cluster_rows2) {
      row_dist <- dist(data, method = input$distance_metric2)  
      row_clust <- hclust(row_dist, method = input$clustering_method2)  
      data <- data[order.dendrogram(as.dendrogram(row_clust)), ] 
    }
    if (input$cluster_columns2) {
      col_dist <- dist(t(data), method = input$distance_metric2)  
      col_clust <- hclust(col_dist, method = input$clustering_method2)  
      data <- data[, order.dendrogram(as.dendrogram(col_clust))]  
    }
    data
  })
  
  # UI element for gene selection
  output$gene_selector2 <- renderUI({
    req(heatmap_matrix())
    genes2 <- rownames(heatmap_matrix())
    
    # Create a dropdown menu for gene selection
    selectInput(
      inputId = "genes2",          
      label = "Select Genes",     
      choices = c("All", genes2), 
      multiple = TRUE,             
      selected = genes2[1:7]       
    )
  })
  
  # UI element for condition selection
  output$condition_selector2 <- renderUI({
    req(heatmap_matrix())
    
    # Retrieve the column names (conditions) from the heatmap matrix
    conditions <- colnames(heatmap_matrix())
    
    # Create a dropdown menu for condition selection
    selectInput(
      inputId = "conditions",         
      label = "Select Conditions",    
      choices = c("All", conditions), 
      multiple = TRUE,                
      selected = conditions[1:12]     
    )
  })
  
  
  # Render interactive heatmap using ggplot and plotly
  output$heatmap_interactive2 <- renderPlotly({
    req(heatmap_data_2())
    melted_data_2 <- heatmap_data_2() %>%
      as.data.frame() %>%                           
      rownames_to_column(var = "Gene") %>%          
      reshape2::melt(                               
        id.vars = "Gene",                           
        variable.name = "Condition",               
        value.name = "Fitness"                    
      )
    
    # Generate heatmap using ggplot
    p2 <- ggplot(melted_data_2, aes(x = Condition, y = Gene, fill = Fitness)) +
      geom_tile() +                                
      scale_fill_viridis() +                        
      theme_minimal() +                             
      labs(                                         
        x = NULL,                                
        y = NULL,                                  
        fill = "Fitness Score"                     
      ) +
      theme(
        axis.text.x = element_text(               
          angle = 90,                            
          hjust = 1                               
        )
      )
    ggplotly(p2) %>%
      layout(xaxis = list(tickangle = 90))         
  })
  
  # Render dendrogram heatmap
  output$heatmap_dendrogram2 <- renderPlot({
    req(heatmap_data_2())
    # Generate the dendrogram heatmap 
    Heatmap(
      heatmap_data_2(),                            
      name = "Fitness Score",                      
      cluster_rows = input$cluster_rows2,          
      cluster_columns = input$cluster_columns2,  
      clustering_distance_rows = input$distance_metric2, 
      clustering_distance_columns = input$distance_metric2,  
      clustering_method_rows = input$clustering_method2, 
      clustering_method_columns = input$clustering_method2,  
      heatmap_legend_param = list(
        title_gp = gpar(fontsize = 14),             
        labels_gp = gpar(fontsize = 13),            
        legend_height = unit(4, "cm"),              
        legend_width = unit(3, "cm")                
      )
    )
  })
  
  # Download handler for heatmap data
  output$downloadHeatmapData2 <- downloadHandler(
    filename = function() {
      paste("Heatmap_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(heatmap_data_2())
      write.csv(
        as.data.frame(heatmap_data_2()),  
        file,                           
        row.names = TRUE                 
      )
    }
  )
  
  # Download handler for dendrogram plot
  output$downloadDendrogramPlot2 <- downloadHandler(
    filename = function() { 
      paste("Dendrogram_Plot_", Sys.Date(), ".pdf", sep = "") 
    },
    content = function(file) {
      pdf(file, width = 10, height = 8)  
      draw(
        Heatmap(
          heatmap_data_2(),                          
          name = "Fitness Score",                    
          cluster_rows = input$cluster_rows2,       
          cluster_columns = input$cluster_columns2,  
          clustering_distance_rows = input$distance_metric2,  
          clustering_distance_columns = input$distance_metric2,  
          clustering_method_rows = input$clustering_method2,  
          clustering_method_columns = input$clustering_method2 
        )
      )
      dev.off()
    }
  )
  
  # Download handler for interactive heatmap plot
  output$downloadInteractiveHeatmapPlot2 <- downloadHandler(
    filename = function() { 
      paste("Interactive_Heatmap_Plot_", Sys.Date(), ".pdf", sep = "") 
    },
    content = function(file) {
      melted_data_2 <- heatmap_data_2() %>%
        as.data.frame() %>%                          
        rownames_to_column(var = "Gene") %>%         
        reshape2::melt(                               
          id.vars = "Gene",                          
          variable.name = "Condition",               
          value.name = "Fitness"                     
        )
      
      # Create the heatmap plot using ggplot
      p2 <- ggplot(melted_data_2, aes(x = Condition, y = Gene, fill = Fitness)) +
        geom_tile() +                               
        scale_fill_viridis() +                        
        theme_minimal() +                             
        labs(                                         
          x = NULL,                                  
          y = NULL,                                 
          fill = "Fitness Score"                    
        ) +
        theme(
          axis.text.x = element_text(                
            angle = 90,                            
            hjust = 1                                
          )
        )
      
      # Save the plot as a high-resolution PDF
      ggsave(
        file,               
        plot = p2,          
        width = 10,         
        height = 8,        
        dpi = 300           
      )
    }
  )
  
}
