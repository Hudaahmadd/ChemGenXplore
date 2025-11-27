# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  ######## Server logic for the "E.coli" tab ########
  
  # Load key datasets required for the application:
  scores_file <- readRDS("3_Score_plus_FDR.rds")  # Fitness scores and False Discovery Rate (FDR) values
  gene_cor_file <- readRDS("3_gene_cor.rds")      # Gene correlation data
  cond_cor_file <- readRDS("3_cond_cor.rds")      # Condition correlation data
  go_file <- readRDS("3_go_enrich.rds")          # Gene Ontology (GO) enrichment data
  kegg_file <- readRDS("3_kegg_enrich.rds")      # KEGG pathway enrichment data
  
  pryr::mem_used()
  
  
  # ---- Pairwise-complete distance helper (no imputation) ----
  # Supports: euclidean, manhattan, pearson, spearman, cosine
  # min_prop: minimum proportion of overlapping (non-NA) entries required
  pc_dist <- function(M,
                      method = c("euclidean","manhattan","pearson","spearman","cosine"),
                      min_prop = 2/3, min_n = NULL) {
    method  <- match.arg(method)
    M       <- as.matrix(M)
    m       <- nrow(M); p <- ncol(M)
    if (is.null(min_n)) min_n <- max(1, floor(min_prop * p))
    ok <- !is.na(M)
    
    if (method %in% c("pearson","spearman")) {
      R <- suppressWarnings(cor(t(M), use = "pairwise.complete.obs", method = method))
      N <- tcrossprod(ok)                 # overlap counts per pair
      R[N < min_n] <- NA_real_            # enforce minimum overlap
      D <- as.dist(1 - R)                 # distance = 1 - correlation
      if (any(is.na(D))) stop("Not enough overlap to compute all pairwise correlations.")
      return(D)
    }
    
    if (method == "cosine") {
      S <- matrix(NA_real_, m, m); diag(S) <- 1
      for (i in 1:m) for (j in i:m) {
        idx <- which(ok[i,] & ok[j,])
        if (length(idx) < min_n) next
        xi <- M[i, idx]; xj <- M[j, idx]
        denom <- sqrt(sum(xi^2)) * sqrt(sum(xj^2))
        sim <- if (denom > 0) sum(xi * xj) / denom else NA_real_
        S[i,j] <- S[j,i] <- sim
      }
      D <- as.dist(1 - S)
      if (any(is.na(D))) stop("Not enough overlap to compute all cosine distances.")
      return(D)
    }
    
    # euclidean / manhattan with pairwise-complete overlap
    Dmat <- matrix(NA_real_, m, m); diag(Dmat) <- 0
    for (i in 1:m) for (j in i:m) {
      idx <- which(ok[i,] & ok[j,])
      if (length(idx) < min_n) next
      xi <- M[i, idx]; xj <- M[j, idx]
      d <- if (method == "euclidean") sqrt(sum((xi - xj)^2)) else sum(abs(xi - xj))
      Dmat[i,j] <- Dmat[j,i] <- d
    }
    D <- as.dist(Dmat)
    if (any(is.na(D))) stop("Not enough overlap to compute all distances.")
    D
  }
  
  
  
  ### test
  observe({
    req(dataset())
    
    size_dataset <- pryr::object_size(dataset())
    size_fdr <- pryr::object_size(fdr_results())
    size_score_fdr <- pryr::object_size(Score_plus_FDR())
    size_gene_cor <- if (!is.null(gene_correlation())) pryr::object_size(gene_correlation()) else pryr::object_size(0)
    size_cond_cor <- if (!is.null(condition_correlation())) pryr::object_size(condition_correlation()) else pryr::object_size(0)
    total <- size_dataset + size_fdr + size_score_fdr + size_gene_cor + size_cond_cor
    
    message("------ ChemGenXplore Memory Usage ------")
    message("Uploaded Dataset       : ", format(size_dataset, units = "auto"))
    message("FDR Results            : ", format(size_fdr, units = "auto"))
    message("Score + FDR Combined   : ", format(size_score_fdr, units = "auto"))
    message("Gene Correlation Table : ", format(size_gene_cor, units = "auto"))
    message("Condition Correlation  : ", format(size_cond_cor, units = "auto"))
    message("Estimated Total        : ", format(total, units = "auto"))
  })
  
  
  ##test
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
      label = "Select or Type Genes:",  # Label displayed above the dropdown
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
      label = "Select or Type Conditions:",  
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
    # Ensure qvalue is numeric once
    go_file %>%
      dplyr::mutate(qvalue = suppressWarnings(as.numeric(qvalue))) %>%
      dplyr::filter(Gene %in% input$Gene3)
  })
  
  # Render the "All GO Enrichment" table
  output$filetableGO <- DT::renderDataTable({
    DT::datatable(
      all_go(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Download handler for the "All GO Enrichment" table
  output$downloadgo <- downloadHandler(
    filename = function() { paste("All_GO_Enrichment_", Sys.Date(), ".csv", sep = "") },
    content  = function(file) { write.csv(all_go(), file, row.names = FALSE) }
  )
  
  # Reactive function to filter significant GO terms based on FDR threshold
  sig_go <- reactive({
    dplyr::filter(
      all_go(),
      !is.na(qvalue),
      qvalue <= (input$FDRq4 / 100)
    )
  })
  
  # Render the data table of significant GO enrichment
  output$filetableGOs <- DT::renderDataTable({
    dat <- sig_go()
    validate(need(nrow(dat) > 0, "No significant GO terms at this FDR. Try increasing % FDR."))
    DT::datatable(
      dat,
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Download handler for significant GO enrichment data
  output$downloadgos <- downloadHandler(
    filename = function() { "GO_enrichment.csv" },
    content  = function(fname) { write.csv(sig_go(), fname, row.names = FALSE) }
  )
  
  # Reactive function to generate a plot for significant GO enrichment
  plotInputGO <- reactive({
    df <- sig_go()
    validate(need(nrow(df) > 0, "No significant GO terms at this FDR. Try increasing % FDR."))
    
    plot_df <- df %>%
      dplyr::group_by(Description) %>%
      dplyr::summarise(
        Count  = sum(Count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        # Clean label before first comma (keeps your original style)
        ShortDesc = stringr::str_extract(Description, "^[^,]+"),
        ShortDesc = forcats::fct_reorder(ShortDesc, Count)
      )
    
    # Build a palette from the *final* factor levels (prevents color/label mismatch)
    labs <- levels(plot_df$ShortDesc)
    pal  <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(length(labs))
    names(pal) <- labs
    
    ggplot(plot_df, aes(x = ShortDesc, y = Count, fill = ShortDesc)) +
      geom_bar(stat = "identity", width = 0.8, colour = "grey30", linewidth = 0.15) +
      coord_flip() +
      ylab("Count of Genes") +
      xlab("") +
      scale_fill_manual(values = pal) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        plot.title   = element_blank(),
        legend.position = "none"
      )
  })
  
  # Render the interactive plot for GO enrichment
  output$barplotGO <- renderPlotly({
    ggplotly(plotInputGO(), tooltip = c("y", "x"))
  })
  
  # Download handler for GO enrichment plot
  output$downloadgoplot <- downloadHandler(
    filename = function() { paste(input$Gene3, 'GO_enrichment.pdf', sep = '_') },
    content  = function(file) { ggsave(file, plotInputGO(), width = 12, height = 5) }
  )
  
  #### Server logic for KEGG Enrichment ####
  
  # Reactive function to filter KEGG enrichment data based on selected genes
  all_kegg <- reactive({
    # Ensure qvalue is numeric once
    kegg_file %>%
      dplyr::mutate(qvalue = suppressWarnings(as.numeric(qvalue))) %>%
      dplyr::filter(Gene %in% input$Gene4)
  })
  
  # Render the "All KEGG Enrichment" table
  output$filetableKEGG <- DT::renderDataTable({
    DT::datatable(
      all_kegg(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Download handler for the "All KEGG Enrichment" table
  output$downloadkegg <- downloadHandler(
    filename = function() { paste("All_KEGG_Enrichment_", Sys.Date(), ".csv", sep = "") },
    content  = function(file) { write.csv(all_kegg(), file, row.names = FALSE) }
  )
  
  # Reactive function to filter significant KEGG terms based on the FDR threshold
  sig_kegg <- reactive({
    dplyr::filter(
      all_kegg(),
      !is.na(qvalue),
      qvalue <= (input$FDRq5 / 100)
    )
  })
  
  # Render the data table of significant KEGG enrichment
  output$filetableKEGGs <- DT::renderDataTable({
    dat <- sig_kegg()
    validate(need(nrow(dat) > 0, "No significant KEGG pathways at this FDR. Try increasing % FDR."))
    DT::datatable(
      dat,
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Download handler for significant KEGG enrichment data
  output$downloadkeggs <- downloadHandler(
    filename = function() { paste("KEGG_enrichment_", Sys.Date(), ".csv", sep = "") },
    content  = function(fname) { write.csv(sig_kegg(), fname, row.names = FALSE) }
  )
  
  # Reactive function to generate a plot for significant KEGG enrichment
  plotInputKEGG <- reactive({
    df <- sig_kegg()
    validate(need(nrow(df) > 0, "No significant KEGG pathways at this FDR. Try increasing % FDR."))
    
    plot_df <- df %>%
      dplyr::group_by(Description) %>%
      dplyr::summarise(
        Count  = sum(Count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        ShortDesc = Description,                          # KEGG labels often don't need trimming
        ShortDesc = forcats::fct_reorder(ShortDesc, Count)
      )
    
    # Palette built from final factor levels (not from nrow)
    labs <- levels(plot_df$ShortDesc)
    pal  <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(length(labs))
    names(pal) <- labs
    
    ggplot(plot_df, aes(x = ShortDesc, y = Count, fill = ShortDesc)) +
      geom_bar(stat = "identity", width = 0.8, colour = "grey30", linewidth = 0.15) +
      coord_flip() +
      ylab("Count of Genes") +
      xlab("") +
      scale_fill_manual(values = pal) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        plot.title   = element_blank(),
        legend.position = "none"
      )
  })
  
  # Render the interactive plot for KEGG enrichment
  output$barplotKEGG <- renderPlotly({
    ggplotly(plotInputKEGG(), tooltip = c("y", "x"))
  })
  
  # Download handler for KEGG enrichment plot
  output$downloadkeggplot <- downloadHandler(
    filename = function() { paste(input$Gene4, 'KEGG_enrichment.pdf', sep = '_') },
    content  = function(file) { ggsave(file, plotInputKEGG(), width = 12, height = 5) }
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
      label = "Select or Type Genes:",  
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
      label = "Select or Type Conditions:",  
      choices = c("All", conditions),  
      multiple = TRUE,  
      selected = conditions[1:15]  
    )
  })
  
  heatmap_data <- reactive({
    req(selected_heatmap_data(), input$genes, input$conditions)
    M <- selected_heatmap_data()
    
    # Resolve selections (supports "All") + guard against stale names
    selected_genes <- if ("All" %in% input$genes) rownames(M) else input$genes
    selected_conditions <- if ("All" %in% input$conditions) colnames(M) else input$conditions
    selected_genes <- intersect(selected_genes, rownames(M))
    selected_conditions <- intersect(selected_conditions, colnames(M))
    
    validate(
      need(length(selected_genes) > 0, "No matching genes in this dataset."),
      need(length(selected_conditions) > 0, "No matching conditions in this dataset.")
    )
    
    data <- M[selected_genes, selected_conditions, drop = FALSE]
    
    # Guard rails for clustering
    if (isTRUE(input$cluster_rows) && nrow(data) < 2) {
      showNotification("Please select at least 2 genes for clustering.", type = "error")
      validate(need(FALSE, "Please select at least 2 genes for clustering."))
    }
    if (isTRUE(input$cluster_columns) && ncol(data) < 2) {
      showNotification("Please select at least 2 conditions for clustering.", type = "error")
      validate(need(FALSE, "Please select at least 2 conditions for clustering."))
    }
    
    # --- Local clustering on the selected subset only ---
    if (isTRUE(input$cluster_rows)) {
      row_dist  <- pc_dist(data, method = input$distance_metric, min_prop = 2/3)
      row_clust <- hclust(row_dist, method = input$clustering_method)
      data <- data[order.dendrogram(as.dendrogram(row_clust)), , drop = FALSE]
    }
    if (isTRUE(input$cluster_columns)) {
      col_dist  <- pc_dist(t(data), method = input$distance_metric, min_prop = 2/3)
      col_clust <- hclust(col_dist, method = input$clustering_method)
      data <- data[, order.dendrogram(as.dendrogram(col_clust)), drop = FALSE]
    }
    
    data
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
      layout(xaxis = list(tickangle = 90)
      )
  })
  
  # Render a heatmap with dendrograms for clustering
  output$heatmap_dendrogram <- renderPlot({
    req(heatmap_data())
    Heatmap(
      heatmap_data(),
      name = "Fitness Score",
      cluster_rows    = input$cluster_rows,
      cluster_columns = input$cluster_columns,
      clustering_distance_rows    = function(x) pc_dist(x,    method = input$distance_metric, min_prop = 2/3),
      clustering_distance_columns = function(x) pc_dist(t(x), method = input$distance_metric, min_prop = 2/3),
      clustering_method_rows      = input$clustering_method,
      clustering_method_columns   = input$clustering_method,
      heatmap_legend_param = list(
        title_gp   = gpar(fontsize = 14),
        labels_gp  = gpar(fontsize = 13),
        legend_height = unit(4, "cm"),
        legend_width  = unit(3, "cm")
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
    content = function(file) {
      pdf(file, width = 10, height = 8)
      draw(
        Heatmap(
          heatmap_data(),
          name = "Fitness Score",
          cluster_rows    = input$cluster_rows,
          cluster_columns = input$cluster_columns,
          clustering_distance_rows    = function(x) pc_dist(x,    method = input$distance_metric, min_prop = 2/3),
          clustering_distance_columns = function(x) pc_dist(t(x), method = input$distance_metric, min_prop = 2/3),
          clustering_method_rows      = input$clustering_method,
          clustering_method_columns   = input$clustering_method
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

  
 
  ######## Server logic for the "S. cerevisiae tab ########
  
  # Load key datasets required for the application:
  sc_scores_file <- readRDS("SC_Score_plus_FDR.rds")  # Fitness scores and False Discovery Rate (FDR) values
  sc_gene_cor_file <- readRDS("SC_gene_cor.rds")      # Gene correlation data
  sc_cond_cor_file <- readRDS("SC_cond_cor.rds")      # Condition correlation data
  sc_go_file <- readRDS("SC_enrich.rds")          # Gene Ontology (GO) enrichment data
  sc_kegg_file <- readRDS("SC_kegg_enrich.rds")      # KEGG pathway enrichment data

  pryr::mem_used()
  
  # Observe block to ensure datasets are loaded before proceeding
  observe({
    req(sc_scores_file, sc_gene_cor_file, sc_cond_cor_file, sc_go_file, sc_kegg_file)
    
    # Extract unique gene names from the fitness scores file
    unique_genes <- unique(sc_scores_file$Gene)
    
    # Dynamically update the Gene1 dropdown menu in the UI
    updateSelectizeInput(
      session,
      "sc_Gene1",  # The input ID for the Gene1 dropdown
      choices = unique_genes,  # Populate dropdown with unique gene names
      selected = if (length(unique_genes) >= 2) unique_genes[1:6] else unique_genes,  # Pre-select up to the first 6 genes if available
      options = list(
        placeholder = "Type to search...",  # Add a placeholder guiding users to search for genes
        maxOptions = length(unique_genes)  # Limit displayed options to the total number of unique genes
      ),
      server = TRUE  # Enable server-side processing for better performance with large datasets
    )
  
    # Dynamically update the cond1 dropdown menu in the UI (now same logic as Gene1)
    unique_conditions <- sort(unique(sc_scores_file$Condition))
    
    updateSelectizeInput(
      session,
      "sc_cond1",
      choices = unique_conditions,
      selected = if (length(unique_conditions) >= 2) unique_conditions[1:6] else unique_conditions,
      options = list(
        placeholder = "Type to search...",
        maxOptions = length(unique_conditions)
      ),
      server = TRUE
    )
    
    
    # Extract unique gene names from the gene correlation dataset (either Gene_1 or Gene_2)
    unique_genes_cor <- unique(c(sc_gene_cor_file$Gene_1, sc_gene_cor_file$Gene_2))
    
    # Dynamically update the Gene2 dropdown menu in the UI
    updateSelectizeInput(
      session,
      "sc_Gene2",  
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
      "sc_cond2", 
      choices = unique(sc_cond_cor_file$Condition_1),  
      selected = c("TETRACYCLINE.0.75", "EGTA 1 MM", "NITRITE.20.MM"),  
      options = list(
        placeholder = "Type to search...",  
        maxOptions = length(unique(sc_cond_cor_file$Condition_1)) 
      ),
      server = TRUE  
    )
    
    # Extract unique genes from the Gene column in the GO file
    unique_genes_go <- unique(sc_go_file$Gene)  
    
    # Dynamically update the Gene3 dropdown menu in the UI
    updateSelectizeInput(
      session,
      "sc_Gene3",  
      choices = unique_genes_go,  
      selected = if (length(unique_genes_go) >= 2) unique_genes_go[1:2] else unique_genes_go,  
      options = list(
        placeholder = "Type to search...",  
        maxOptions = length(unique_genes_go)  
      ),
      server = TRUE  
    )
    
    # Extract unique genes from the Gene column in the KEGG file
    unique_genes_kegg <- unique(sc_kegg_file$Gene)  
    
    # Update the Gene4 dropdown in the UI dynamically
    updateSelectizeInput(
      session,
      "sc_Gene4",  
      choices = unique_genes_kegg,  
      selected = if (length(unique_genes_kegg) >= 2) unique_genes_kegg[1:2] else unique_genes_kegg,  
      options = list(
        placeholder = "Type to search...",  
        maxOptions = length(unique_genes_kegg)  
      ),
      server = TRUE 
    )
    
  })
  
  
  #### Server logic for Phenotypes of Selected Genes ####
  
  sc_Genes_phenotypes <- reactive({
    req(input$sc_Gene1)  # Ensure that the user has selected at least one gene (Gene1 input must not be NULL)
    
    # Filter the scores file to include only rows where the Gene matches the selected input (Gene1)
    dplyr::filter(sc_scores_file, Gene %in% input$sc_Gene1) %>%
      # Add a new column 'Fitness' based on the value of Score
      dplyr::mutate(Fitness = dplyr::case_when(
        Score > 0 ~ 'Increased',  # If score is greater than 0, label as 'Increased'
        Score < 0 ~ 'Decreased'   # If score is less than 0, label as 'Decreased'
      ))
  })
  
  
  # Render "All Phenotypes" table
  output$sc_genes_scores_filetable <- DT::renderDataTable({
    # Create a DataTable using the Genes_phenotypes reactive dataset
    DT::datatable(
      sc_Genes_phenotypes(),  # Use the data from the Genes_phenotypes reactive function
      options = list(
        pageLength = 10,  # Display 10 rows per page by default
        scrollX = TRUE    # Enable horizontal scrolling for wide tables
      ),
      rownames = FALSE  # Do not display row names in the table
    )
  })
  
  # Download handler for the "All Phenotypes" table
  output$sc_downloadAllGenesPhenotypes <- downloadHandler(
    # Define the filename dynamically with the current date
    filename = function() { 
      paste("All_Phenotypes_", Sys.Date(), ".csv", sep = "")  # Format: All_Phenotypes_YYYY-MM-DD.csv
    },
    
    # Define the content of the downloaded file
    content = function(file) {
      write.csv(
        sc_Genes_phenotypes(),  # Use the data from the Genes_phenotypes reactive function
        file,                # File path provided by the user
        row.names = FALSE    # Exclude row names from the CSV file for a cleaner output
      )
    }
  )
  
  # Reactive function for filtering significant phenotypes
  sc_Genes_phenotypes_sig <- reactive({
    sc_Genes_phenotypes() %>%  # Start with the data from the Genes_phenotypes reactive function
      dplyr::filter(
        as.numeric(FDR) <= (input$sc_FDRq / 100)  # Filter rows where the FDR is less than or equal to the user-specified threshold
      )
  })
  
  # Render a table for significant gene scores
  output$sc_genes_scores_sig_filetable <- DT::renderDataTable({
    # Create a DataTable using the filtered significant phenotypes dataset
    DT::datatable(
      sc_Genes_phenotypes_sig(),  
      options = list(
        pageLength = 5,  
        scrollX = TRUE  
      ),
      rownames = FALSE  
    )
  })
  
  # Reactive function to summarize significant phenotypes data for the bar plot
  sc_Genes_phenotypes_sig_with_totals <- reactive({
    sc_Genes_phenotypes_sig() %>%  # Start with the filtered significant phenotypes dataset
      dplyr::group_by(Gene, Fitness) %>%  # Group data by Dataset and Fitness
      dplyr::summarise(
        Count = dplyr::n(),       # Count the number of occurrences in each group
        .groups = "drop"   # Remove grouping to return a flat data frame
      ) %>%
      dplyr::mutate(
        Fitness = factor(Fitness, levels = c("Decreased", "Increased")),  # Ensure Fitness is a factor with a defined order
        Gene = forcats::fct_reorder(Gene, Count, .fun = sum)  # order by total count
      )
  })
  
  
  # Reactive function to generate the bar plot for significant phenotypes
  sc_plotInput <- reactive({
    ggplot(
      sc_Genes_phenotypes_sig_with_totals(),  # Use the summarized data for significant phenotypes
      aes(x = Count, y = Gene, fill = Fitness)  # Map Count to x-axis, Dataset to y-axis, and Fitness to fill
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
  output$sc_download <- downloadHandler(
    # Generate the filename dynamically with the current date
    filename = function() {
      paste("Significant_Phenotypes_Genes_", Sys.Date(), ".csv", sep = "")  
    },
    
    # Define the content of the file to be downloaded
    content = function(fname) {
      write.csv(
        sc_Genes_phenotypes_sig(),  
        fname,                   
        row.names = FALSE        
      )
    }
  )
  
  # Render the interactive bar plot for significant phenotypes
  output$sc_barplotps <- renderPlotly({
    dat <- sc_Genes_phenotypes_sig_with_totals()
    validate(need(nrow(dat) > 0, "No significant phenotypes at this FDR. Try increasing % FDR."))
    ggplotly(sc_plotInput())
  })
  
  
  # Download handler for the Significant Phenotypes bar plot
  output$sc_downloadPlot <- downloadHandler(
    filename = function() {
      sel <- input$sc_Gene1
      lab <- if (length(sel) > 5) paste0(length(sel), "_genes") else paste(sel, collapse = "_")
      paste0(lab, "_Significant_Phenotypes.pdf")
    },
    content = function(file) ggsave(file, sc_plotInput(), width = 8, height = 5)
  )

  
  #### Server logic for Phenotypes Conditions #####
  
  # Phenotypes of Selected Conditions (ALL)
  sc_Conditions_phenotypes <- reactive({
    req(input$sc_cond1)
    
    dplyr::filter(sc_scores_file, Condition %in% input$sc_cond1) %>%
      dplyr::mutate(
        Fitness = dplyr::case_when(
          Score > 0 ~ 'Increased',
          Score < 0 ~ 'Decreased')
      )
  })
  
  # Render "All Phenotypes" table
  output$sc_conditions_scores_filetable <- DT::renderDataTable({
    # Create a DataTable using the Genes_phenotypes reactive dataset
    DT::datatable(
      sc_Conditions_phenotypes(),  # Use the data from the Genes_phenotypes reactive function
      options = list(
        pageLength = 10,  # Display 10 rows per page by default
        scrollX = TRUE    # Enable horizontal scrolling for wide tables
      ),
      rownames = FALSE  # Do not display row names in the table
    )
  })
  
  
  # Download handler for the "All Phenotypes" table
  output$sc_downloadAllConditionsPhenotypes <- downloadHandler(
    # Define the filename dynamically with the current date
    filename = function() { 
      paste("All_Phenotypes_", Sys.Date(), ".csv", sep = "")  # Format: All_Phenotypes_YYYY-MM-DD.csv
    },
    
    # Define the content of the downloaded file
    content = function(file) {
      write.csv(
        sc_Conditions_phenotypes(),  # Use the data from the Genes_phenotypes reactive function
        file,                # File path provided by the user
        row.names = FALSE    # Exclude row names from the CSV file for a cleaner output
      )
    }
  )
  
  # Reactive function for filtering significant phenotypes
  sc_Conditions_phenotypes_sig <- reactive({
    sc_Conditions_phenotypes() %>%  # Start with the data from the Genes_phenotypes reactive function
      dplyr::filter(
        as.numeric(FDR) <= (input$sc_FDRqc / 100)  # Filter rows where the FDR is less than or equal to the user-specified threshold
      )
  })
  
  # Render a table for significant gene scores
  output$sc_conditions_scores_sig_filetable <- DT::renderDataTable({
    # Create a DataTable using the filtered significant phenotypes dataset
    DT::datatable(
      sc_Conditions_phenotypes_sig(),  
      options = list(
        pageLength = 5,  
        scrollX = TRUE  
      ),
      rownames = FALSE  
    )
  })
  
  
  # Reactive function to summarize significant phenotypes data for the bar plot
  sc_Conditions_phenotypes_sig_with_totals <- reactive({
    sc_Conditions_phenotypes_sig() %>%  # Start with the filtered significant phenotypes dataset
      dplyr::group_by(Condition, Fitness) %>%  # Group data by Dataset and Fitness
      dplyr::summarise(
        Count = dplyr::n(),       # Count the number of occurrences in each group
        .groups = "drop"   # Remove grouping to return a flat data frame
      ) %>%
      dplyr::mutate(
        Fitness = factor(Fitness, levels = c("Decreased", "Increased")),  # Ensure Fitness is a factor with a defined order
        Condition = forcats::fct_reorder(Condition, Count, .fun = sum)  # order by total count
      )
  })
  
  
  # Reactive function to generate the bar plot for significant phenotypes
  sc_plotInputc <- reactive({
    ggplot(
      sc_Conditions_phenotypes_sig_with_totals(),  # Use the summarized data for significant phenotypes
      aes(x = Count, y = Condition, fill = Fitness)  # Map Count to x-axis, Dataset to y-axis, and Fitness to fill
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
  
  
  # Download handler for the Significant Phenotypes table
  output$sc_downloadc <- downloadHandler(
    # Generate the filename dynamically with the current date
    filename = function() {
      paste("Significant_Phenotypes_Condition_", Sys.Date(), ".csv", sep = "")  
    },
    
    # Define the content of the file to be downloaded
    content = function(fname) {
      write.csv(
        sc_Conditions_phenotypes_sig(),  
        fname,                   
        row.names = FALSE        
      )
    }
  )
  
  # Render the interactive bar plot for significant phenotypes
  output$sc_barplotpsc <- renderPlotly({
    dat <- sc_Conditions_phenotypes_sig_with_totals()
    validate(need(nrow(dat) > 0, "No significant phenotypes at this FDR. Try increasing % FDR."))
    ggplotly(sc_plotInputc())
  })
  
  # Download handler for the Significant Phenotypes bar plot
  output$sc_downloadPlotc <- downloadHandler(
    filename = function() {
      sel <- input$sc_cond1
      lab <- if (length(sel) > 5) paste0(length(sel), "_conditions") else paste(sel, collapse = "_")
      paste0(lab, "_Significant_Phenotypes_Condition.pdf")
    },
    content = function(file) ggsave(file, sc_plotInputc(), width = 8, height = 5)
  )
  
  
  # Server logic for Genes Correlations
  
  # Robust column detection (handles "Correlation (r)", qval/qvalue/FDR)
  sc_pick_cols <- function(df) {
    nms <- names(df)
    first_match <- function(patterns) {
      for (p in patterns) {
        hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)
        if (length(hit)) return(hit[1])
      }
      NA_character_
    }
    r_name <- first_match(c("^r$", "pearson[_ ]?r$", "correlation\\s*\\(r\\)$", "\\(r\\)$", "^corr(elation)?_?r$"))
    q_name <- first_match(c("^qval$", "^qvalue$", "^fdr$", "q[_ ]?value"))
    list(r = r_name, q = q_name, dataset = if ("Dataset" %in% nms) "Dataset" else NULL)
  }
  
  # Normalise yeast correlation table to standard columns used downstream
  # -> Gene_1, Gene_2, r_num (numeric r), q_num (numeric q/FDR), Correlation (Pos/Neg/Other), Dataset_label
  sc_norm_sc_gene_cor <- reactive({
    df <- sc_gene_cor_file
    cols <- sc_pick_cols(df)
    
    if (!all(c("Gene_1", "Gene_2") %in% names(df))) {
      stop("Yeast gene-correlation file must contain columns 'Gene_1' and 'Gene_2'.")
    }
    
    # numeric r
    if (is.na(cols$r)) {
      df$r_num <- NA_real_
    } else {
      df$r_num <- suppressWarnings(as.numeric(df[[cols$r]]))
    }
    
    # numeric q
    if (is.na(cols$q)) {
      df$q_num <- NA_real_
    } else {
      df$q_num <- suppressWarnings(as.numeric(df[[cols$q]]))
    }
    
    # sign label (use existing 'Correlation' if present; else derive from r_num at ±0.4)
    if (!("Correlation" %in% names(df))) {
      df$Correlation <- dplyr::case_when(
        !is.na(df$r_num) & df$r_num >=  0.4 ~ "Positive",
        !is.na(df$r_num) & df$r_num <= -0.4 ~ "Negative",
        TRUE ~ "Other"
      )
    } else {
      df$Correlation <- dplyr::case_when(
        tolower(df$Correlation) == "positive" ~ "Positive",
        tolower(df$Correlation) == "negative" ~ "Negative",
        TRUE ~ "Other"
      )
    }
    
    # dataset label (fallback when missing)
    df$Dataset_label <- if (!is.null(cols$dataset)) df[[cols$dataset]] else "S. cerevisiae"
    
    dplyr::select(df, Gene_1, Gene_2, r_num, q_num, Correlation, Dataset_label, dplyr::everything())
  })
  
  # All correlations for selected Gene_1
  sc_all_gene_cor <- reactive({
    req(input$sc_Gene2)
    sc_norm_sc_gene_cor() %>% dplyr::filter(Gene_1 %in% input$sc_Gene2)
  })
  
  output$sc_genes_correlations_filetable <- DT::renderDataTable({
    DT::datatable(
      sc_all_gene_cor() %>% dplyr::select(-r_num, -q_num),  # hide helper cols
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$sc_downloadAllCorrelations <- downloadHandler(
    filename = function() paste("Yeast_All_Correlations_", Sys.Date(), ".csv", sep = ""),
    content  = function(file) write.csv(sc_all_gene_cor(), file, row.names = FALSE)
  )
  
  # Significant correlations by %FDR slider input$sc_FDRq2 (0–100) 
  sc_sig_gene_cor <- reactive({
    dat <- sc_all_gene_cor()
    # If no q-values available, return empty
    if (all(is.na(dat$q_num))) return(dat[0, , drop = FALSE])
    thresh <- (if (is.null(input$sc_FDRq2)) 5 else input$sc_FDRq2) / 100
    dplyr::filter(dat, !is.na(q_num), q_num <= thresh)
  })
  
  output$sc_genes_correlations_sig_filetable <- DT::renderDataTable({
    dat <- sc_sig_gene_cor()
    validate(need(nrow(dat) > 0, "No significant correlations at this FDR. Try increasing % FDR or confirm q-values exist."))
    DT::datatable(
      dat %>% dplyr::select(-r_num, -q_num),  # hide helper cols
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$sc_download_gene_cor <- downloadHandler(
    filename = function() paste("Yeast_Genes_Correlation_", Sys.Date(), ".csv", sep = ""),
    content  = function(file) write.csv(sc_sig_gene_cor(), file, row.names = FALSE)
  )
  
  # Plot: one bar per selected Gene_1 (stacked by Positive/Negative)
  sc_plotInputcor <- reactive({
    dat <- sc_sig_gene_cor()
    validate(need(nrow(dat) > 0, "No significant correlations at this FDR. Try increasing % FDR."))
    
    dat <- dat %>% dplyr::filter(Correlation %in% c("Positive", "Negative"))
    
    # order genes by total significant count (ascending for nicer y-axis)
    gene_order <- dat %>%
      dplyr::count(Gene_1, name = "Total") %>%
      dplyr::arrange(Total) %>%
      dplyr::pull(Gene_1)
    
    dat$Gene_1 <- factor(dat$Gene_1, levels = gene_order)
    
    ggplot(dat, aes(y = Gene_1, fill = Correlation)) +
      geom_bar(stat = "count") +
      scale_fill_manual(values = c("Positive" = "#08589e", "Negative" = "#edf8fb")) +
      ylab(NULL) +
      xlab("Count of Pearson r values > 0.4 or < -0.4 (significant by FDR)") +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        plot.title   = element_text(size = 16, face = "bold")
      )
  })
  
  # Safe ggplotly wrapper
  safe_plotly_sc <- function(p) {
    tryCatch(
      plotly::ggplotly(p, tooltip = c("y", "x", "fill")),
      error = function(e) {
        plotly::plot_ly() %>% layout(annotations = list(
          text = paste("Plot error:", e$message),
          showarrow = FALSE, x = 0.5, y = 0.5, xref = "paper", yref = "paper"
        ))
      }
    )
  }
  
  output$sc_barplot  <- plotly::renderPlotly({ safe_plotly_sc(sc_plotInputcor()) })
  output$sc_barplot2 <- plotly::renderPlotly({ safe_plotly_sc(sc_plotInputcor()) })
  
  output$sc_downloadcorplot <- downloadHandler(
    filename = function() {
      sel <- input$sc_Gene2
      lab <- if (length(sel) > 5) paste0(length(sel), "_genes") else paste(sel, collapse = "_")
      paste0(lab, "_Genes_Correlation.pdf")
    },
    content = function(file) ggsave(file, sc_plotInputcor(), width = 8, height = 5)
  )

    # Server logic for Condition Correlations

  # All available conditions (from either side of the pair table)
  cond_choices <- reactive({
    df <- sc_cond_cor_file
    if (is.null(df) || !all(c("Condition_1","Condition_2") %in% names(df))) return(character(0))
    sort(unique(c(df$Condition_1, df$Condition_2)))
  })
  
  # Auto-select a few default conditions on load / when dataset changes
  observeEvent(cond_choices(), {
    choices <- cond_choices()
    
    # keep user's current picks if they exist in new choices; otherwise pick first few
    current <- isolate(input$sc_cond2)
    selected <- if (!is.null(current) && length(current) > 0) {
      intersect(current, choices)
    } else {
      head(choices, min(5, length(choices)))  # default: first 5 (or fewer)
    }
    
    updateSelectizeInput(
      session, "sc_cond2",
      choices  = choices,
      selected = selected,
      server   = TRUE
    )
  }, ignoreInit = FALSE)
  
  # Condition_1, Condition_2, r_num, q_num, Correlation (Pos/Neg/Other), Dataset_label
  sc_norm_sc_cond_cor <- reactive({
    df <- sc_cond_cor_file                 # your loaded condition-correlation df
    cols <- sc_pick_cols(df)               # re-use your robust picker (r/q/dataset)
    
    # sanity check
    if (!all(c("Condition_1", "Condition_2") %in% names(df))) {
      stop("Condition correlation file must contain columns 'Condition_1' and 'Condition_2'.")
    }
    
    # numeric r
    if (is.na(cols$r)) {
      df$r_num <- NA_real_
    } else {
      df$r_num <- suppressWarnings(as.numeric(df[[cols$r]]))
    }
    
    # numeric q
    if (is.na(cols$q)) {
      df$q_num <- NA_real_
    } else {
      df$q_num <- suppressWarnings(as.numeric(df[[cols$q]]))
    }
    
    # sign label (use existing 'Correlation' if present; else derive from r_num at ±0.4)
    if (!("Correlation" %in% names(df))) {
      df$Correlation <- dplyr::case_when(
        !is.na(df$r_num) & df$r_num >=  0.4 ~ "Positive",
        !is.na(df$r_num) & df$r_num <= -0.4 ~ "Negative",
        TRUE ~ "Other"
      )
    } else {
      df$Correlation <- dplyr::case_when(
        tolower(df$Correlation) == "positive" ~ "Positive",
        tolower(df$Correlation) == "negative" ~ "Negative",
        TRUE ~ "Other"
      )
    }
    
    # dataset label (fallback)
    df$Dataset_label <- if (!is.null(cols$dataset)) df[[cols$dataset]] else "S. cerevisiae"
    
    # (optional) if your upstream saved "N_shared" (overlap size), keep it; else ignore
    dplyr::select(df, Condition_1, Condition_2, r_num, q_num, Correlation, Dataset_label, dplyr::everything())
  })
  
  # All correlations for selected Condition_1 (mirror of sc_all_gene_cor) 
  sc_all_cond_cor <- reactive({
    req(input$sc_cond2)
    sc_norm_sc_cond_cor() %>% dplyr::filter(Condition_1 %in% input$sc_cond2)
  })
  
  output$sc_conditions_correlations_filetable <- DT::renderDataTable({
    DT::datatable(
      sc_all_cond_cor() %>% dplyr::select(-r_num, -q_num),   # hide helper cols like you do for genes
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$sc_downloadAllCondCorrelations <- downloadHandler(
    filename = function() paste("All_Condition_Correlations_", Sys.Date(), ".csv", sep = ""),
    content  = function(file) write.csv(sc_all_cond_cor(), file, row.names = FALSE)
  )
  
  # Significant condition correlations by %FDR (mirror of sc_sig_gene_cor) 
  sc_condsig <- reactive({
    dat <- sc_all_cond_cor()
    if (all(is.na(dat$q_num))) return(dat[0, , drop = FALSE])     # no q-values available
    thresh <- (if (is.null(input$sc_FDRq3)) 5 else input$sc_FDRq3) / 100
    dplyr::filter(dat, !is.na(q_num), q_num <= thresh)
  })
  
  output$sc_conditions_correlations_sig_filetable <- DT::renderDataTable({
    dat <- sc_condsig()
    validate(need(nrow(dat) > 0, "No significant correlations at this FDR. Try increasing % FDR or confirm q-values exist."))
    DT::datatable(
      dat %>% dplyr::select(-r_num, -q_num),
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$sc_download_cond_cor <- downloadHandler(
    filename = function() paste("Conditions_Correlation_", Sys.Date(), ".csv", sep = ""),
    content  = function(fname) write.csv(sc_condsig(), fname, row.names = FALSE)
  )
  
  # Plot: one bar per selected Condition_1 (stacked by Positive/Negative)
  sc_plotInputcor_cond <- reactive({
    dat <- sc_condsig()
    validate(need(nrow(dat) > 0, "No significant correlations at this FDR. Try increasing % FDR."))
    
    dat <- dat %>% dplyr::filter(Correlation %in% c("Positive", "Negative"))
    
    # order conditions by total significant count (ascending for nicer y-axis)
    cond_order <- dat %>%
      dplyr::count(Condition_1, name = "Total") %>%
      dplyr::arrange(Total) %>%
      dplyr::pull(Condition_1)
    
    dat$Condition_1 <- factor(dat$Condition_1, levels = cond_order)
    
    ggplot(dat, aes(y = Condition_1, fill = Correlation)) +
      geom_bar(stat = "count") +
      scale_fill_manual(values = c("Positive" = "#756bb1", "Negative" = "#dadaeb")) +
      ylab(NULL) +
      xlab("Count of Pearson r values > 0.4 or < -0.4 (significant by FDR)") +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        plot.title   = element_text(size = 16, face = "bold")
      )
  })
  
  output$sc_barplot2 <- plotly::renderPlotly({
    safe_plotly_sc(sc_plotInputcor_cond())   # reuse your safe wrapper
  })
  
  output$sc_downloadcorplot2 <- downloadHandler(
    filename = function() {
      sel <- input$sc_cond2
      lab <- if (length(sel) > 5) paste0(length(sel), "_conditions") else paste(sel, collapse = "_")
      paste0(lab, "_Conditions_Correlation.pdf")
    },
    content = function(file) ggsave(file, sc_plotInputcor_cond(), width = 8, height = 5)
  )
  
  #### ---------------- S. cerevisiae GO Enrichment (safe version) ---------------- ####
  
  sc_all_go <- reactive({
    req(input$sc_Gene3)
    sc_go_file %>%
      dplyr::mutate(qvalue = suppressWarnings(as.numeric(qvalue))) %>%
      dplyr::filter(Gene %in% input$sc_Gene3)
  })
  
  sc_sig_go <- reactive({
    thr <- (if (is.null(input$sc_FDRq4)) 100 else input$sc_FDRq4) / 100
    sc_all_go() %>%
      dplyr::filter(!is.na(qvalue), qvalue <= thr)
  })
  
  output$sc_filetableGO <- DT::renderDataTable({
    DT::datatable(sc_all_go(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$sc_downloadgo <- downloadHandler(
    filename = function() paste("All_GO_Enrichment_", Sys.Date(), ".csv", sep = ""),
    content  = function(file) write.csv(sc_all_go(), file, row.names = FALSE)
  )
  
  output$sc_filetableGOs <- DT::renderDataTable({
    dat <- sc_sig_go()
    validate(need(nrow(dat) > 0, "No significant GO terms at this FDR. Try increasing % FDR."))
    DT::datatable(dat, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
  
  output$sc_downloadgos <- downloadHandler(
    filename = function() "GO_enrichment.csv",
    content  = function(fname) write.csv(sc_sig_go(), fname, row.names = FALSE)
  )
  
  sc_plotInputGO <- reactive({
    df <- sc_sig_go()
    validate(need(nrow(df) > 0, "No significant GO terms at this FDR. Try increasing % FDR."))
    
    plot_df <- df %>%
      dplyr::group_by(Description) %>%
      dplyr::summarise(
        Count  = sum(Count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        ShortDesc = stringr::str_extract(Description, "^[^,]+"),
        ShortDesc = forcats::fct_reorder(ShortDesc, Count)
      )
    
    labs <- levels(plot_df$ShortDesc)
    pal  <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(length(labs))
    names(pal) <- labs
    
    ggplot(plot_df, aes(x = ShortDesc, y = Count, fill = ShortDesc)) +
      geom_bar(stat = "identity", width = 0.8, colour = "grey30", linewidth = 0.15) +
      coord_flip() +
      ylab("Count of Genes") +
      xlab("") +
      scale_fill_manual(values = pal) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        plot.title   = element_blank(),
        legend.position = "none"
      )
  })
  
  output$sc_barplotGO <- plotly::renderPlotly({
    plotly::ggplotly(sc_plotInputGO(), tooltip = c("y","x"))
  })
  
  output$sc_downloadgoplot <- downloadHandler(
    filename = function() {
      sel <- input$sc_Gene3
      lab <- if (length(sel) > 5) paste0(length(sel), "_genes") else paste(sel, collapse = "_")
      paste0(lab, "_GO_enrichment.pdf")
    },
    content = function(file) ggsave(file, sc_plotInputGO(), width = 12, height = 5)
  )
  
  
  
  #### ---------------- S. cerevisiae KEGG Enrichment (safe version) ---------------- ####
  
  sc_all_kegg <- reactive({
    req(input$sc_Gene4)
    sc_kegg_file %>%
      dplyr::mutate(qvalue = suppressWarnings(as.numeric(qvalue))) %>%
      dplyr::filter(Gene %in% input$sc_Gene4)
  })
  
  sc_sig_kegg <- reactive({
    thr <- (if (is.null(input$sc_FDRq5)) 100 else input$sc_FDRq5) / 100
    sc_all_kegg() %>%
      dplyr::filter(!is.na(qvalue), qvalue <= thr)
  })
  
  output$sc_filetableKEGG <- DT::renderDataTable({
    DT::datatable(sc_all_kegg(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$sc_downloadkegg <- downloadHandler(
    filename = function() paste("All_KEGG_Enrichment_", Sys.Date(), ".csv", sep = ""),
    content  = function(file) write.csv(sc_all_kegg(), file, row.names = FALSE)
  )
  
  output$sc_filetableKEGGs <- DT::renderDataTable({
    dat <- sc_sig_kegg()
    validate(need(nrow(dat) > 0, "No significant KEGG pathways at this FDR. Try increasing % FDR."))
    DT::datatable(dat, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
  
  output$sc_downloadkeggs <- downloadHandler(
    filename = function() "KEGG_enrichment.csv",
    content  = function(fname) write.csv(sc_sig_kegg(), fname, row.names = FALSE)
  )
  
  sc_plotInputKEGG <- reactive({
    df <- sc_sig_kegg()
    validate(need(nrow(df) > 0, "No significant KEGG pathways at this FDR. Try increasing % FDR."))
    
    plot_df <- df %>%
      dplyr::group_by(Description) %>%
      dplyr::summarise(
        Count  = sum(Count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        ShortDesc = Description,
        ShortDesc = forcats::fct_reorder(ShortDesc, Count)
      )
    
    labs <- levels(plot_df$ShortDesc)
    pal  <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(length(labs))
    names(pal) <- labs
    
    ggplot(plot_df, aes(x = ShortDesc, y = Count, fill = ShortDesc)) +
      geom_bar(stat = "identity", width = 0.8, colour = "grey30", linewidth = 0.15) +
      coord_flip() +
      ylab("Count of Genes") +
      xlab("") +
      scale_fill_manual(values = pal) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        plot.title   = element_blank(),
        legend.position = "none"
      )
  })
  
  output$sc_barplotKEGG <- plotly::renderPlotly({
    plotly::ggplotly(sc_plotInputKEGG(), tooltip = c("y","x"))
  })
  
  output$sc_downloadkeggplot <- downloadHandler(
    filename = function() {
      sel <- input$sc_Gene4
      lab <- if (length(sel) > 5) paste0(length(sel), "_genes") else paste(sel, collapse = "_")
      paste0(lab, "_KEGG_enrichment.pdf")
    },
    content = function(file) ggsave(file, sc_plotInputKEGG(), width = 12, height = 5)
  )
  
  
  
  #### Server logic for Heatmaps ####

  #### ---------------- Yeast Heatmap (Mutant-level) — FINAL ---------------- ####
  
  # Helper to compact the strain for row labels
  short_strain <- function(s) {
    s <- as.character(s)
    first <- sub("__.*$", "", s)                          # before first "__"
    tail6 <- substr(s, pmax(1, nchar(s) - 5), nchar(s))   # last 6 chars
    paste0(first, "…", tail6)
  }
  
  # 0) Load mutant-level wide table (Mutant_ID, Strain, Gene, <conditions...>)
  sc_heatmap_tbl <- readRDS("SC_heatmap_mutant_x_condition.rds")
  
  # Defensive checks
  stopifnot(all(c("Mutant_ID","Strain","Gene") %in% names(sc_heatmap_tbl)))
  
  # Identify columns
  .meta_cols <- intersect(c("Mutant_ID","Strain","Gene"), colnames(sc_heatmap_tbl))
  .cond_cols <- setdiff(colnames(sc_heatmap_tbl), .meta_cols)
  
  # 1) Build numeric matrix: rows = Mutant_ID, cols = Conditions
  sc_heatmap_mat <- as.matrix(sc_heatmap_tbl[, .cond_cols, drop = FALSE])
  rownames(sc_heatmap_mat) <- sc_heatmap_tbl$Mutant_ID
  
  # 2) Selectors (Mutants from table, Conditions from matrix)
  output$sc_gene_selector <- renderUI({
    muts <- unique(sc_heatmap_tbl$Mutant_ID)
    muts <- muts[!is.na(muts)]
    muts <- sort(muts)
    
    validate(need(length(muts) > 0, "No mutants found in the heatmap table."))
    
    selectInput(
      inputId  = "sc_genes",
      label    = "Select or Type Mutants:",
      choices  = c("All", muts),
      multiple = TRUE,
      selected = head(muts, 7)
    )
  })
  
  output$sc_condition_selector <- renderUI({
    conds <- colnames(sc_heatmap_mat)
    selectInput(
      inputId  = "sc_conditions",
      label    = "Select or Type Conditions:",
      choices  = c("All", conds),
      multiple = TRUE,
      selected = head(conds, 15)
    )
  })
  
  # 3) Reactive: subset + optional local clustering
  sc_heatmap_data <- reactive({
    req(input$sc_genes, input$sc_conditions)
    M <- sc_heatmap_mat
    
    rows <- if ("All" %in% input$sc_genes) rownames(M) else intersect(input$sc_genes, rownames(M))
    cols <- if ("All" %in% input$sc_conditions) colnames(M) else intersect(input$sc_conditions, colnames(M))
    
    validate(
      need(length(rows) > 0, "No matching mutants."),
      need(length(cols) > 0, "No matching conditions.")
    )
    
    data <- M[rows, cols, drop = FALSE]
    
    # Optional clustering on the selected subset only
    if (isTRUE(input$sc_cluster_rows) && nrow(data) >= 2) {
      rd <- pc_dist(data, method = input$sc_distance_metric, min_prop = 2/3)
      rc <- hclust(rd, method = input$sc_clustering_method)
      data <- data[order.dendrogram(as.dendrogram(rc)), , drop = FALSE]
    }
    if (isTRUE(input$sc_cluster_columns) && ncol(data) >= 2) {
      cd <- pc_dist(t(data), method = input$sc_distance_metric, min_prop = 2/3)
      cc <- hclust(cd, method = input$sc_clustering_method)
      data <- data[, order.dendrogram(as.dendrogram(cc)), drop = FALSE]
    }
    
    data
  })
  
  # 4) Interactive heatmap (with Strain/Gene metadata in hover)
  # 4) Interactive heatmap (unique y = Mutant_ID; show short labels; full hover)
  output$sc_heatmap_interactive <- renderPlotly({
    req(sc_heatmap_data())
    
    df <- sc_heatmap_data() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Mutant_ID") %>%
      reshape2::melt(id.vars = "Mutant_ID", variable.name = "Condition", value.name = "Fitness")
    
    meta <- sc_heatmap_tbl[, c("Mutant_ID","Strain","Gene")]
    df   <- dplyr::left_join(df, meta, by = "Mutant_ID")
    
    # Map Mutant_ID -> "GENE · short(Strain)" (keeps rows unique, labels short)
    label_map <- df %>%
      dplyr::distinct(Mutant_ID, Gene, Strain) %>%
      dplyr::mutate(Short = ifelse(is.na(Gene) | Gene == "", Mutant_ID,
                                   paste0(Gene, " · ", short_strain(Strain)))) %>%
      { setNames(.$Short, .$Mutant_ID) }
    
    p <- ggplot(
      df,
      aes(
        x = Condition, y = Mutant_ID, fill = Fitness,
        text = paste0(
          "Mutant: ", Mutant_ID,
          "<br>Strain: ", Strain,
          "<br>Gene: ", Gene,
          "<br>Condition: ", Condition,
          "<br>Fitness: ", signif(Fitness, 4)
        )
      )
    ) +
      geom_tile() +
      scale_y_discrete(labels = label_map) +   # <-- short, unique labels shown
      scale_fill_viridis_c() +
      theme_minimal() +
      labs(x = NULL, y = NULL, fill = "Fitness Score") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(tickangle = 90))
  })
  
  
  # 5) Static dendrogram heatmap (ComplexHeatmap)
  # ---- 5) Static dendrogram heatmap (short, unique row labels, readable font) ----
  output$sc_heatmap_dendrogram <- renderPlot({
    req(sc_heatmap_data())
    
    # Build label map once: Mutant_ID -> "GENE · short(Strain)"
    lab_df <- sc_heatmap_tbl[, c("Mutant_ID","Strain","Gene")]
    lab_df$Short <- ifelse(is.na(lab_df$Gene) | lab_df$Gene == "",
                           lab_df$Mutant_ID,
                           paste0(lab_df$Gene, " · ", short_strain(lab_df$Strain)))
    lab_map <- setNames(lab_df$Short, lab_df$Mutant_ID)
    
    rn <- rownames(sc_heatmap_data())
    row_labels <- lab_map[rn]
    row_labels[is.na(row_labels) | row_labels == ""] <- rn  # fallback
    
    Heatmap(
      sc_heatmap_data(),
      name = "Fitness Score",
      row_labels = row_labels,
      row_names_gp = grid::gpar(fontsize = 12),   # ⬅️ Increased font size here
      column_names_gp = grid::gpar(fontsize = 12), # ⬅️ Optional: make condition names bigger too
      cluster_rows    = input$sc_cluster_rows,
      cluster_columns = input$sc_cluster_columns,
      clustering_distance_rows    = function(x) pc_dist(x,    method = input$sc_distance_metric, min_prop = 2/3),
      clustering_distance_columns = function(x) pc_dist(t(x), method = input$sc_distance_metric, min_prop = 2/3),
      clustering_method_rows      = input$sc_clustering_method,
      clustering_method_columns   = input$sc_clustering_method,
      heatmap_legend_param = list(
        title_gp = grid::gpar(fontsize = 12, fontface = "bold"),
        labels_gp = grid::gpar(fontsize = 12)
      )
    )
  })
  
  
  
  # 6) Downloads (include metadata in the CSV)
  output$sc_downloadHeatmapData <- downloadHandler(
    filename = function() paste("Heatmap_Data_", Sys.Date(), ".csv", sep = ""),
    content  = function(file) {
      mat <- sc_heatmap_data()
      out <- as.data.frame(mat) %>% tibble::rownames_to_column("Mutant_ID")
      out <- dplyr::left_join(out, sc_heatmap_tbl[, c("Mutant_ID","Strain","Gene")], by = "Mutant_ID")
      out <- dplyr::select(out, Mutant_ID, Strain, Gene, dplyr::everything())
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$sc_downloadDendrogramPlot <- downloadHandler(
    filename = function() paste("Dendrogram_Plot_", Sys.Date(), ".pdf", sep = ""),
    content  = function(file) {
      pdf(file, width = 10, height = 8)
      draw(
        Heatmap(
          sc_heatmap_data(),
          name = "Fitness Score",
          cluster_rows    = input$sc_cluster_rows,
          cluster_columns = input$sc_cluster_columns,
          clustering_distance_rows    = function(x) pc_dist(x,    method = input$sc_distance_metric, min_prop = 2/3),
          clustering_distance_columns = function(x) pc_dist(t(x), method = input$sc_distance_metric, min_prop = 2/3),
          clustering_method_rows      = input$sc_clustering_method,
          clustering_method_columns   = input$sc_clustering_method
        )
      )
      dev.off()
    }
  )
  
  output$sc_downloadInteractiveHeatmapPlot <- downloadHandler(
    filename = function() paste("Interactive_Heatmap_Plot_", Sys.Date(), ".pdf", sep = ""),
    content  = function(file) {
      df <- sc_heatmap_data() %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Mutant_ID") %>%
        reshape2::melt(id.vars = "Mutant_ID", variable.name = "Condition", value.name = "Fitness")
      
      meta <- sc_heatmap_tbl[, c("Mutant_ID","Strain","Gene")]
      df   <- dplyr::left_join(df, meta, by = "Mutant_ID")
      
      p <- ggplot(df, aes(x = Condition, y = Mutant_ID, fill = Fitness)) +
        geom_tile() +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(x = NULL, y = NULL, fill = "Fitness Score") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggsave(file, plot = p, width = 10, height = 8, dpi = 300)
    }
  )

  
  
  #server yeast ends here
  ######## Server logic for the "Upload Your Dataset" tab ########
  
  # Reactive function to read the uploaded dataset
  dataset <- reactive({
    req(input$file_upload)  # Ensure a file has been uploaded
    file <- input$file_upload$datapath
    
    tryCatch({
      # Check if the file is a CSV
      if (tools::file_ext(file) == "csv") {
        # Load the .csv file
        data <- read.csv(file, row.names = 1, check.names = FALSE)
      } else {
        stop("Unsupported file type. Please upload a .csv file.")
      }
      
      # Ensure all columns are numeric
      if (!all(sapply(data, is.numeric))) {
        stop("The uploaded dataset must contain only numeric values.")
      }
      
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
  
  # Function to compute Median Absolute Deviation (MAD)
  compute_mad <- function(data, c = 0.6745) {
    data <- na.omit(data)
    median_val <- median(data)
    median(abs(data - median_val)) / c
  }
  
  # >>> ADD THE HELPER RIGHT HERE <<<
  pc_dist <- function(M,
                      method = c("euclidean","manhattan","pearson","spearman","cosine"),
                      min_prop = 2/3, min_n = NULL) {
    method  <- match.arg(method)
    M       <- as.matrix(M)
    m       <- nrow(M); p <- ncol(M)
    if (is.null(min_n)) min_n <- max(1, floor(min_prop * p))
    ok <- !is.na(M)
    
    if (method %in% c("pearson","spearman")) {
      R <- suppressWarnings(cor(t(M), use = "pairwise.complete.obs", method = method))
      N <- tcrossprod(ok)
      R[N < min_n] <- NA_real_
      D <- as.dist(1 - R)
      if (any(is.na(D))) stop("Not enough overlap to compute all pairwise correlations.")
      return(D)
    }
    
    if (method == "cosine") {
      S <- matrix(NA_real_, m, m); diag(S) <- 1
      for (i in 1:m) for (j in i:m) {
        idx <- which(ok[i,] & ok[j,])
        if (length(idx) < min_n) next
        xi <- M[i, idx]; xj <- M[j, idx]
        denom <- sqrt(sum(xi^2)) * sqrt(sum(xj^2))
        sim <- if (denom > 0) sum(xi * xj) / denom else NA_real_
        S[i,j] <- S[j,i] <- sim
      }
      D <- as.dist(1 - S)
      if (any(is.na(D))) stop("Not enough overlap to compute all cosine distances.")
      return(D)
    }
    
    # euclidean / manhattan with pairwise-complete overlap
    Dmat <- matrix(NA_real_, m, m); diag(Dmat) <- 0
    for (i in 1:m) for (j in i:m) {
      idx <- which(ok[i,] & ok[j,])
      if (length(idx) < min_n) next
      xi <- M[i, idx]; xj <- M[j, idx]
      d <- if (method == "euclidean") sqrt(sum((xi - xj)^2)) else sum(abs(xi - xj))
      Dmat[i,j] <- Dmat[j,i] <- d
    }
    D <- as.dist(Dmat)
    if (any(is.na(D))) stop("Not enough overlap to compute all distances.")
    D
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
    shinyjs::show("loading_gene_corr"); on.exit(shinyjs::hide("loading_gene_corr"))
    Scored_Dataset <- dataset()
    if (!all(sapply(Scored_Dataset, is.numeric))) stop("Correlation analysis requires a numeric dataset.")
    
    gene_mat <- t(as.matrix(Scored_Dataset))  # keep NAs; rcorr uses pairwise-complete
    
    rc <- Hmisc::rcorr(gene_mat, type = "pearson")
    R <- rc$r; P <- rc$P; N <- rc$n
    
    Q <- matrix(p.adjust(as.vector(P), method = "fdr"),
                nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
    
    df_r <- reshape2::melt(R, varnames = c("Gene_1","Gene_2"), value.name = "Correlation (r)")
    df_q <- reshape2::melt(Q, varnames = c("Gene_1","Gene_2"), value.name = "qval")
    df_n <- reshape2::melt(N, varnames = c("Gene_1","Gene_2"), value.name = "Npair")
    
    # Require adequate overlap: ≥ 2/3 of conditions (or set a fixed number)
    min_overlap <- floor((2/3) * ncol(Scored_Dataset))
    
    df <- df_r %>%
      left_join(df_q, by = c("Gene_1","Gene_2")) %>%
      left_join(df_n, by = c("Gene_1","Gene_2")) %>%
      dplyr::filter(Gene_1 != Gene_2,
                    abs(`Correlation (r)`) > 0.4,
                    Npair >= min_overlap) %>%
      mutate(`Correlation (r)` = round(`Correlation (r)`, 4),
             qval = formatC(as.numeric(qval), format = "e", digits = 4))
    as.data.frame(df)
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
  output$filetablegenecor <- DT::renderDataTable({
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
    shinyjs::show("loading_gene_corr"); on.exit(shinyjs::hide("loading_gene_corr"))
    Scored_Dataset <- dataset()
    if (!all(sapply(Scored_Dataset, is.numeric))) stop("Correlation analysis requires a numeric dataset.")
    
    cond_mat <- as.matrix(Scored_Dataset)  # keep NAs
    
    rc <- Hmisc::rcorr(cond_mat, type = "pearson")
    R <- rc$r; P <- rc$P; N <- rc$n
    
    Q <- matrix(p.adjust(as.vector(P), method = "fdr"),
                nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
    
    df_r <- reshape2::melt(R, varnames = c("Condition_1","Condition_2"), value.name = "Correlation (r)")
    df_q <- reshape2::melt(Q, varnames = c("Condition_1","Condition_2"), value.name = "qval")
    df_n <- reshape2::melt(N, varnames = c("Condition_1","Condition_2"), value.name = "Npair")
    
    # Require adequate overlap: ≥ 2/3 of genes (or a fixed minimum)
    min_overlap <- floor((2/3) * nrow(Scored_Dataset))
    
    df <- df_r %>%
      left_join(df_q, by = c("Condition_1","Condition_2")) %>%
      left_join(df_n, by = c("Condition_1","Condition_2")) %>%
      dplyr::filter(Condition_1 != Condition_2,
                    abs(`Correlation (r)`) > 0.4,
                    Npair >= min_overlap) %>%
      mutate(`Correlation (r)` = round(`Correlation (r)`, 4),
             qval = formatC(as.numeric(qval), format = "e", digits = 4))
    df
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
    M <- heatmap_matrix()
    
    # Resolve selections (supports "All") and guard against stale selections
    sel_genes <- if ("All" %in% input$genes2) rownames(M) else input$genes2
    sel_conds <- if ("All" %in% input$conditions) colnames(M) else input$conditions
    sel_genes <- intersect(sel_genes, rownames(M))
    sel_conds <- intersect(sel_conds, colnames(M))
    
    validate(
      need(length(sel_genes) > 0, "No matching genes in the uploaded matrix."),
      need(length(sel_conds) > 0, "No matching conditions in the uploaded matrix.")
    )
    
    data <- M[sel_genes, sel_conds, drop = FALSE]
    
    # Guard rails for clustering
    if (isTRUE(input$cluster_rows2) && nrow(data) < 2) {
      showNotification("Please select at least 2 genes for clustering.", type = "error")
      validate(need(FALSE, "Please select at least 2 genes for clustering."))
    }
    if (isTRUE(input$cluster_columns2) && ncol(data) < 2) {
      showNotification("Please select at least 2 conditions for clustering.", type = "error")
      validate(need(FALSE, "Please select at least 2 conditions for clustering."))
    }
    
    # --- Local clustering on the selected subset only ---
    if (isTRUE(input$cluster_rows2)) {
      row_dist  <- pc_dist(data, method = input$distance_metric2, min_prop = 2/3)
      row_clust <- hclust(row_dist, method = input$clustering_method2)
      data <- data[order.dendrogram(as.dendrogram(row_clust)), , drop = FALSE]
    }
    if (isTRUE(input$cluster_columns2)) {
      col_dist  <- pc_dist(t(data), method = input$distance_metric2, min_prop = 2/3)
      col_clust <- hclust(col_dist, method = input$clustering_method2)
      data <- data[, order.dendrogram(as.dendrogram(col_clust)), drop = FALSE]
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
      label = "Select or Type Genes:",     
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
      label = "Select or Type Conditions:",    
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
    Heatmap(
      heatmap_data_2(),
      name = "Fitness Score",
      cluster_rows    = input$cluster_rows2,
      cluster_columns = input$cluster_columns2,
      clustering_distance_rows    = function(x) pc_dist(x,    method = input$distance_metric2, min_prop = 2/3),
      clustering_distance_columns = function(x) pc_dist(t(x), method = input$distance_metric2, min_prop = 2/3),
      clustering_method_rows      = input$clustering_method2,
      clustering_method_columns   = input$clustering_method2,
      heatmap_legend_param = list(
        title_gp = gpar(fontsize = 14),
        labels_gp = gpar(fontsize = 13),
        legend_height = unit(4, "cm"),
        legend_width  = unit(3, "cm")
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
    filename = function() paste("Dendrogram_Plot_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      pdf(file, width = 10, height = 8)
      draw(
        Heatmap(
          heatmap_data_2(),
          name = "Fitness Score",
          cluster_rows    = input$cluster_rows2,
          cluster_columns = input$cluster_columns2,
          clustering_distance_rows    = function(x) pc_dist(x,    method = input$distance_metric2, min_prop = 2/3),
          clustering_distance_columns = function(x) pc_dist(t(x), method = input$distance_metric2, min_prop = 2/3),
          clustering_method_rows      = input$clustering_method2,
          clustering_method_columns   = input$clustering_method2
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

  ### OMICS INTEGRATION 
  
  # Helpers
  `%||%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x
  
  read_matrix_csv <- function(path) {
    path <- normalizePath(as.character(path), mustWork = TRUE)
    df <- tryCatch(
      readr::read_csv(file = path, show_col_types = FALSE),
      error = function(e) utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    )
    if (is.null(df) || ncol(df) < 2)
      stop("CSV must have a first column for Gene IDs and ≥1 numeric value column.")
    rn <- as.character(df[[1]])
    df <- df[-1]
    
    # coerce numerics safely
    df <- as.data.frame(lapply(df, function(x) {
      if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x)))
    }), check.names = FALSE)
    
    if (!all(vapply(df, is.numeric, TRUE)))
      stop("All value columns must be numeric (after coercion). Please check your file.")
    
    # collapse duplicated gene IDs by row-mean
    if (anyDuplicated(rn)) {
      df$.__gene__ <- rn
      df <- df %>%
        dplyr::group_by(.__gene__) %>%
        dplyr::summarise(dplyr::across(where(is.numeric), ~ suppressWarnings(mean(.x, na.rm = TRUE))),
                         .groups = "drop")
      rn <- df$.__gene__; df$.__gene__ <- NULL
    }
    
    mat <- as.matrix(df)
    rownames(mat) <- rn
    mat
  }
  
  spearman_stats <- function(x, y, conf.level = 0.95) {
    ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; n <- length(x)
    if (n < 4) return(list(rho = NA, lo = NA, hi = NA, p = NA, n = n))
    ct <- suppressWarnings(suppressMessages(cor.test(x, y, method = "spearman", exact = FALSE)))
    rho <- unname(ct$estimate); p <- ct$p.value
    z  <- atanh(rho); se <- 1 / sqrt(max(1, n - 3))
    zcrit <- qnorm((1 + conf.level)/2)
    lo <- tanh(z - zcrit * se); hi <- tanh(z + zcrit * se)
    list(rho = rho, lo = lo, hi = hi, p = p, n = n)
  }
  
  fisher_overlap_stats <- function(chem, omics, thr_chem, thr_omics) {
    ok <- is.finite(chem) & is.finite(omics); chem <- chem[ok]; omics <- omics[ok]
    chem_sig  <- abs(chem)  >= thr_chem
    omics_sig <- abs(omics) >= thr_omics
    tab <- table(
      `Chemical Genomics` = factor(chem_sig,  levels = c(FALSE, TRUE), labels = c("No","Yes")),
      `Omics`             = factor(omics_sig, levels = c(FALSE, TRUE), labels = c("No","Yes"))
    )
    ft <- fisher.test(tab)
    OR <- unname(ft$estimate); lo <- if (!is.null(ft$conf.int)) ft$conf.int[1] else NA
    hi <- if (!is.null(ft$conf.int)) ft$conf.int[2] else NA
    list(OR = OR, lo = lo, hi = hi, p = ft$p.value, tab = tab)
  }
  
  fmt_num <- function(x, digs = 3) ifelse(is.na(x), "NA", formatC(x, digits = digs, format = "fg"))
  
  diverging_11 <- c("#313695","#4575b4","#74add1","#abd9e9","#e0f3f8",
                    "#ffffbf","#fee090","#fdae61","#f46d43","#d73027","#a50026")
  
  # shared palette for quadrants (used by plotly + ggplot)
  pal <- c(
    "Q1: ↑Chemical Genomics & ↑Omics" = "#1b9e77",
    "Q2: ↓Chemical Genomics & ↑Omics" = "#7570b3",
    "Q3: ↓Chemical Genomics & ↓Omics" = "#d95f02",
    "Q4: ↑Chemical Genomics & ↓Omics" = "#e7298a",
    "Not significant"                 = "#bdbdbd"
  )
  
  
  # File inputs
  chem_mat <- reactive({
    req(input$chem_file)
    read_matrix_csv(input$chem_file$datapath)
  })
  omics_mat <- reactive({
    req(input$omics_file)
    read_matrix_csv(input$omics_file$datapath)
  })
  
  # Previews
  output$chem_preview <- DT::renderDT({
    req(chem_mat())
    DT::datatable(
      as.data.frame(chem_mat(), check.names = FALSE) |> tibble::rownames_to_column("Gene"),
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$omics_preview <- DT::renderDT({
    req(omics_mat())
    DT::datatable(
      as.data.frame(omics_mat(), check.names = FALSE) |> tibble::rownames_to_column("Gene"),
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ---------- alignment ----------
  safe_intersect_rows <- function(A, B) {
    g <- intersect(rownames(A), rownames(B))  # order follows A
    list(A = A[g, , drop = FALSE], B = B[g, , drop = FALSE], genes = g)
  }
  aligned <- reactive({
    req(chem_mat(), omics_mat())
    safe_intersect_rows(chem_mat(), omics_mat())
  })
  
  output$align_stats <- renderText({
    req(chem_mat(), omics_mat())
    g_all   <- length(union(rownames(chem_mat()), rownames(omics_mat())))
    g_inter <- length(aligned()$genes)
    paste0(
      "Chemical Genomics genes: ", nrow(chem_mat()), "\n",
      "Omics genes:             ", nrow(omics_mat()), "\n",
      "Overlap (common genes):  ", g_inter, " (covers ",
      round(100 * g_inter / g_all, 1), "% of all unique genes across both lists)\n"
    )
  })
  
  # ---------- dynamic left controls ----------
  output$left_controls <- renderUI({
    req(input$inner, aligned())
    
    if (input$inner == "Scatter plot") {
      tagList(
        h4("Scatter options"),
        selectInput("chem_col_scatter", "Chemical Genomics column:",
                    choices = colnames(aligned()$A),
                    selected = colnames(aligned()$A)[1]),
        selectInput("omics_col_scatter", "Omics column:",
                    choices = colnames(aligned()$B),
                    selected = colnames(aligned()$B)[1]),
        
        sliderInput("thr_chem",  "Chemical Genomics |score| threshold", min = 0, max = 5, value = 1, step = 0.05),
        helpText("Treats |Chemical Genomics score| ≥ threshold as a ‘hit’."),
        
        sliderInput("thr_omics", "Omics |value| threshold",            min = 0, max = 5, value = 1, step = 0.05),
        helpText("Treats |Omics effect| ≥ threshold as a ‘hit’ (e.g., large |log2FC|)."),
        
        numericInput("label_top_n", "Label top N (highlighted genes)", value = 15, min = 0, max = 200),
        helpText("Labels the top N genes showing the strongest combined changes across both datasets (farther from the origin = larger joint effect)."),
        
        actionButton("scatter_refresh", "Generate Plot"),
        br(), br(),
        downloadButton("dl_scatter_pub", "Download Scatter (PDF)"),
        downloadButton("dl_hits", "Download Hits (CSV)")
      )
      
    } else if (input$inner == "Heatmaps") {
      genes <- aligned()$genes
      colsA <- colnames(aligned()$A)
      colsB <- colnames(aligned()$B)
      tagList(
        h4("Heatmap options"),
        selectizeInput("heat_genes", "Genes (optional subset)", choices = genes,
                       multiple = TRUE, options = list(placeholder = "Leave empty = all genes"), selected = NULL),
        selectizeInput("heat_cols_chem", "Chemical Genomics columns", choices = colsA, multiple = TRUE,
                       selected = colsA[1:min(4, length(colsA))]),
        selectizeInput("heat_cols_omics", "Omics columns", choices = colsB, multiple = TRUE,
                       selected = colsB[1:min(4, length(colsB))]),
        tags$hr(),
        br(),
        downloadButton("dl_heatmap_data", "Download Heatmap Data (CSV)"),
        downloadButton("dl_heatmap_pdf",  "Download Heatmaps (PDF)")    # <-- add
        
      )
      
    } else if (input$inner == "Overlap Summary") {
      tagList(
        h4("Overlap options"),
        sliderInput("thr_chem",  "Chemical Genomics |score| threshold", min = 0, max = 5, value = 1, step = 0.05),
        sliderInput("thr_omics", "Omics |value| threshold",            min = 0, max = 5, value = 1, step = 0.05),
        helpText("Counts use ±thresholds on each axis; Fisher’s exact test evaluates enrichment of double-hits."),
        downloadButton("dl_overlap", "Download Overlap (CSV)")
      )
    }
  })
  
  # ---------- scatter data & plot ----------
  scatter_df <- eventReactive(input$scatter_refresh, {
    req(aligned(), input$chem_col_scatter, input$omics_col_scatter)
    thrc <- if (is.null(input$thr_chem)) 1 else input$thr_chem
    thro <- if (is.null(input$thr_omics)) 1 else input$thr_omics
    
    A <- aligned()$A[, input$chem_col_scatter, drop = FALSE]
    B <- aligned()$B[, input$omics_col_scatter, drop = FALSE]
    
    tibble::tibble(
      Gene  = rownames(A),
      Chem  = as.numeric(A[,1]),
      Omics = as.numeric(B[,1]),
      absR  = sqrt(Chem^2 + Omics^2),
      Q     = dplyr::case_when(
        Chem >=  thrc & Omics >=  thro ~ "Q1: ↑Chemical Genomics & ↑Omics",
        Chem <= -thrc & Omics >=  thro ~ "Q2: ↓Chemical Genomics & ↑Omics",
        Chem <= -thrc & Omics <= -thro ~ "Q3: ↓Chemical Genomics & ↓Omics",
        Chem >=  thrc & Omics <= -thro ~ "Q4: ↑Chemical Genomics & ↓Omics",
        TRUE                           ~ "Not significant"
      )
    ) |>
      dplyr::mutate(
        Q = factor(Q, levels = c(
          "Q1: ↑Chemical Genomics & ↑Omics",
          "Q2: ↓Chemical Genomics & ↑Omics",
          "Q3: ↓Chemical Genomics & ↓Omics",
          "Q4: ↑Chemical Genomics & ↓Omics",
          "Not significant"
        )),
        Hover = paste0(
          "Gene: ", Gene,
          "<br>Omics: ", signif(Omics, 4),
          "<br>Chemical Genomics: ", signif(Chem, 4),
          "<br>Group: ", as.character(Q)
        )
      )
  }, ignoreInit = TRUE)
  
  
  # allow initial draw without clicking Update
  observeEvent(aligned(), {
    if (is.null(input$scatter_refresh) || input$scatter_refresh == 0) {
      isolate({ scatter_df() })
    }
  }, ignoreInit = TRUE)
  
  spearman_text <- reactive({
    req(scatter_df())
    df <- scatter_df()
    sp <- spearman_stats(df$Omics, df$Chem, conf.level = 0.95)
    paste0("Spearman \u03C1 = ", fmt_num(sp$rho),
           " [", fmt_num(sp$lo), ", ", fmt_num(sp$hi), "]",
           " | p = ", fmt_num(sp$p, 3),
           " | n = ", sp$n)
  })
  
  fisher_text <- reactive({
    req(scatter_df())
    df <- scatter_df()
    thrc <- if (is.null(input$thr_chem)) 1 else input$thr_chem
    thro <- if (is.null(input$thr_omics)) 1 else input$thr_omics
    fo <- fisher_overlap_stats(df$Chem, df$Omics, thrc, thro)
    paste0("Fisher OR = ", fmt_num(fo$OR),
           " [", fmt_num(fo$lo), ", ", fmt_num(fo$hi), "]",
           " | p = ", fmt_num(fo$p, 3))
  })
  
  output$scatter_plot <- renderPlotly({
    req(scatter_df())
    df <- scatter_df()
    
    nlab <- max(0, as.integer(if (is.null(input$label_top_n)) 0 else input$label_top_n))
    lab_df <- df |> dplyr::arrange(dplyr::desc(absR)) |> head(nlab)
    
    thrc <- if (is.null(input$thr_chem)) 1 else input$thr_chem
    thro <- if (is.null(input$thr_omics)) 1 else input$thr_omics
    

    # ONE marker layer (no duplicate add_markers before)
    p <- plot_ly(
      data = df,
      x = ~Omics, y = ~Chem,
      color = ~Q, colors = pal,
      text = ~Hover, hoverinfo = "text"
    ) |>
      add_markers(marker = list(size = 7, line = list(width = 0)), showlegend = TRUE)
    
    # Top-N labels (don’t create a legend entry)
    if (nrow(lab_df) > 0) {
      p <- add_text(
        p,
        data = lab_df,
        x = ~Omics, y = ~Chem, text = ~Gene,
        textposition = "top center",
        textfont = list(size = 12),
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    
    # Threshold lines
    shapes <- list(
      list(type="line", x0=-Inf, x1= Inf, y0= thrc,  y1= thrc,  line=list(dash="dash", width=1)),
      list(type="line", x0=-Inf, x1= Inf, y0=-thrc,  y1=-thrc,  line=list(dash="dash", width=1)),
      list(type="line", x0= thro, x1= thro, y0=-Inf, y1= Inf,   line=list(dash="dash", width=1)),
      list(type="line", x0=-thro, x1=-thro, y0=-Inf, y1= Inf,   line=list(dash="dash", width=1))
    )
    
    p <- layout(
      p,
      xaxis = list(title = "Omics value", zeroline = FALSE),
      yaxis = list(title = "Chemical Genomics score", zeroline = FALSE),
      shapes = shapes,
      margin = list(l = 60, r = 20, t = 60, b = 60),
      annotations = list(list(
        x = 1, y = 1.12, xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top",
        text = paste0(spearman_text(), "<br>", fisher_text()),
        showarrow = FALSE, align = "right",
        font = list(size = 12)
      ))
    )
    
    p
  })
  
  
  # Scatter downloads 
  output$dl_scatter_pub <- downloadHandler(
    filename = function() sprintf("scatter_%s_vs_%s.pdf",
                                  input$chem_col_scatter %||% "chemical_genomics",
                                  input$omics_col_scatter %||% "omics"),
    content = function(file) {
      req(scatter_df())
      df <- scatter_df()
      nlab <- ifelse(is.na(input$label_top_n), 0, input$label_top_n)
      lab_genes <- df |> dplyr::arrange(dplyr::desc(absR)) |> dplyr::slice(1:nlab) |> dplyr::pull(Gene)
      
      thrc <- if (is.null(input$thr_chem)) 1 else input$thr_chem
      thro <- if (is.null(input$thr_omics)) 1 else input$thr_omics
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Omics, y = Chem)) +
        ggplot2::geom_hline(yintercept = c(-thrc, thrc), linetype = "dashed", linewidth = 0.4) +
        ggplot2::geom_vline(xintercept = c(-thro, thro), linetype = "dashed", linewidth = 0.4) +
        ggplot2::geom_point(ggplot2::aes(color = Q), alpha = 0.8, size = 2.0, stroke = 0) +
        ggrepel::geom_text_repel(
          data = df[df$Gene %in% lab_genes, ],
          ggplot2::aes(label = Gene),
          size = 2.6, max.overlaps = 100, box.padding = 0.3, segment.size = 0.2, min.segment.length = 0
        ) +
        ggplot2::labs(x = "Omics value", y = "Chemical Genomics score", color = "Quadrant") +
        ggplot2::scale_color_manual(values = pal[levels(df$Q)], limits = levels(df$Q), drop = FALSE) +  
        ggplot2::theme_classic(base_size = 9) +
        ggplot2::theme(legend.position = "right", plot.margin = ggplot2::margin(4, 6, 4, 6)) +
        ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.02, vjust = 1.2, size = 2.5,
                          label = paste0(spearman_text(), "\n", fisher_text()))
      ggplot2::ggsave(file, plot = p, width = 7, height = 5, units = "in", dpi = 300)
    }
  )
  
  output$dl_hits <- downloadHandler(
    filename = function() sprintf("quadrant_hits_%s_vs_%s.csv",
                                  input$chem_col_scatter %||% "chemical_genomics",
                                  input$omics_col_scatter %||% "omics"),
    content = function(file) {
      req(scatter_df())
      write.csv(scatter_df() |> dplyr::filter(Q != "Not significant"), file, row.names = FALSE)
    }
  )
  
  # Heatmaps 
  
  heat_input <- reactive({
    req(aligned())
    A <- aligned()$A
    B <- aligned()$B
    
    # genes subset
    g_all <- rownames(A)
    g_sel <- if (is.null(input$heat_genes) || length(input$heat_genes) == 0) g_all else intersect(input$heat_genes, g_all)
    req(length(g_sel) > 0)
    
    # columns subset (if you kept selectors, otherwise first few)
    colsA <- if (!is.null(input$heat_cols_chem) && length(input$heat_cols_chem)) input$heat_cols_chem else colnames(A)[seq_len(min(4, ncol(A)))]
    colsB <- if (!is.null(input$heat_cols_omics) && length(input$heat_cols_omics)) input$heat_cols_omics else colnames(B)[seq_len(min(4, ncol(B)))]
    
    A <- A[g_sel, colsA, drop = FALSE]
    B <- B[g_sel, colsB, drop = FALSE]
    
    # DEFAULT: order genes by mean Chemical Genomics (descending)
    ord <- order(rowMeans(A, na.rm = TRUE), decreasing = TRUE)
    A <- A[ord, , drop = FALSE]
    B <- B[rownames(A), , drop = FALSE]
    
    # symmetric ranges centered at 0 for EACH
    limA <- max(abs(A), na.rm = TRUE); if (!is.finite(limA)) limA <- 1
    limB <- max(abs(B), na.rm = TRUE); if (!is.finite(limB)) limB <- 1
    
    list(A = A, B = B, limA = limA, limB = limB)
  })
  
  output$heatmaps <- renderPlotly({
    hi <- heat_input()
    A <- hi$A; B <- hi$B
    
    At <- t(A)  # rows = conditions, cols = genes
    Bt <- t(B)
    
    # shared symmetric limits
    lim <- max(hi$limA, hi$limB); if (!is.finite(lim)) lim <- 1
    
    mk_hover_t <- function(Mt, rowlabs, collabs) {
      outer(seq_along(rowlabs), seq_along(collabs), Vectorize(function(i, j) {
        paste0(
          "Gene: ", collabs[j],
          "<br>Condition: ", rowlabs[i],
          "<br>Value: ", signif(Mt[i, j], 4)
        )
      }))
    }
    hA <- mk_hover_t(At, rownames(At), colnames(At))
    hB <- mk_hover_t(Bt, rownames(Bt), colnames(Bt))
    
    # Left: Chemical Genomics (no colorbar shown)
    p1 <- plotly::plot_ly(
      x = colnames(At), y = rownames(At), z = At,
      type = "heatmap",
      colors = diverging_11,
      showscale = FALSE,            # hide on left
      zmid = 0, zmin = -lim, zmax = lim,
      text = hA, hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Gene", tickangle = 90, automargin = TRUE, titlefont = list(size = 13)),
        yaxis = list(title = "Condition", automargin = TRUE, titlefont = list(size = 13), tickfont = list(size = 11)),
        margin = list(l = 90, r = 30, t = 30, b = 90)
      )
    
    # Right: Omics (single shared colorbar on the right)
    p2 <- plotly::plot_ly(
      x = colnames(Bt), y = rownames(Bt), z = Bt,
      type = "heatmap",
      colors = diverging_11,
      showscale = TRUE,             # show only on right
      zmid = 0, zmin = -lim, zmax = lim,
      colorbar = list(x = 1.05),    # place colorbar slightly to the right
      text = hB, hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Gene", tickangle = 90, automargin = TRUE, titlefont = list(size = 13)),
        yaxis = list(title = "Condition", automargin = TRUE, titlefont = list(size = 13), tickfont = list(size = 11)),
        margin = list(l = 90, r = 30, t = 30, b = 90)
      )
    
    plotly::subplot(p1, p2, nrows = 1, shareX = FALSE, shareY = FALSE, widths = c(0.46, 0.46), margin = 0.06) %>%
      layout(
        annotations = list(
          list(
            x = 0.23, y = 1.08,
            text = "<b>Chemical Genomics</b>",
            showarrow = FALSE, xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "bottom",
            font = list(size = 15)
          ),
          list(
            x = 0.77, y = 1.08,
            text = "<b>Omics</b>",
            showarrow = FALSE, xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "bottom",
            font = list(size = 15)
          )
        ),
        margin = list(l = 100, r = 120, t = 100, b = 100) # a little extra right space for the colorbar
      )
  })
  
  
  
  
  output$dl_heatmap_plot <- downloadHandler(
    filename = function() paste0("heatmaps_", Sys.Date(), ".png"),
    content = function(file) {
      hi <- heat_input()
      A <- hi$A; B <- hi$B
      At <- t(A); Bt <- t(B)
      
      # shared symmetric limits
      lim <- max(hi$limA, hi$limB); if (!is.finite(lim)) lim <- 1
      
      mk_hover_t <- function(Mt, rowlabs, collabs) {
        outer(seq_along(rowlabs), seq_along(collabs), Vectorize(function(i, j) {
          paste0("Gene: ", collabs[j], "<br>Condition: ", rowlabs[i], "<br>Value: ", signif(Mt[i, j], 4))
        }))
      }
      hA <- mk_hover_t(At, rownames(At), colnames(At))
      hB <- mk_hover_t(Bt, rownames(Bt), colnames(Bt))
      
      p1 <- plotly::plot_ly(
        x = colnames(At), y = rownames(At), z = At,
        type = "heatmap", colors = diverging_11,
        showscale = FALSE,
        zmid = 0, zmin = -lim, zmax = lim, text = hA, hoverinfo = "text"
      )
      p2 <- plotly::plot_ly(
        x = colnames(Bt), y = rownames(Bt), z = Bt,
        type = "heatmap", colors = diverging_11,
        showscale = TRUE, colorbar = list(x = 1.05),
        zmid = 0, zmin = -lim, zmax = lim, text = hB, hoverinfo = "text"
      )
      
      plt <- plotly::subplot(p1, p2, nrows = 1, shareX = FALSE, shareY = FALSE, widths = c(0.46, 0.46), margin = 0.06) %>%
        layout(
          annotations = list(
            list(x = 0.23, y = 1.08, text = "<b>Chemical Genomics</b>", showarrow = FALSE, xref = "paper", yref = "paper",
                 xanchor = "center", yanchor = "bottom", font = list(size = 15)),
            list(x = 0.77, y = 1.08, text = "<b>Omics</b>", showarrow = FALSE, xref = "paper", yref = "paper",
                 xanchor = "center", yanchor = "bottom", font = list(size = 15))
          ),
          margin = list(l = 100, r = 120, t = 100, b = 100)
        )
      
      ok <- TRUE
      tryCatch({
        plotly::save_image(plt, file = file, width = 1600, height = 900, scale = 2)
      }, error = function(e) { ok <<- FALSE })
      if (!ok) {
        stop("PNG export requires the 'kaleido' (preferred) or 'orca' engine. Install with:\n  reticulate::py_install('kaleido')\nOr: install.packages('orcacapture') and set it up.\nAlternatively, I can add an HTML download.")
      }
    }
  )
  
  
  
  
  # heatmap data download
  output$dl_heatmap_data <- downloadHandler(
    filename = function() "heatmaps_data.csv",
    content = function(file) {
      req(aligned(), input$heat_cols_chem, input$heat_cols_omics)
      A0 <- aligned()$A; B0 <- aligned()$B
      g_all <- rownames(A0)
      g_sel <- if (is.null(input$heat_genes) || length(input$heat_genes) == 0) g_all else intersect(input$heat_genes, g_all)
      A <- A0[g_sel, input$heat_cols_chem, drop = FALSE]
      B <- B0[g_sel, input$heat_cols_omics, drop = FALSE]
      
      # same default ordering by mean Chemical Genomics
      ord <- order(rowMeans(A, na.rm = TRUE), decreasing = TRUE)
      A <- A[ord, , drop = FALSE]
      B <- B[rownames(A), , drop = FALSE]
      
      A_long <- as.data.frame(A) |> tibble::rownames_to_column("Gene") |>
        tidyr::pivot_longer(-Gene, names_to = "Condition", values_to = "Value") |>
        dplyr::mutate(Layer = "Chemical Genomics")
      B_long <- as.data.frame(B) |> tibble::rownames_to_column("Gene") |>
        tidyr::pivot_longer(-Gene, names_to = "Condition", values_to = "Value") |>
        dplyr::mutate(Layer = "Omics")
      write.csv(dplyr::bind_rows(A_long, B_long), file, row.names = FALSE)
    }
  )
  
  
  output$dl_heatmap_pdf <- downloadHandler(
    filename = function() paste0("heatmaps_", Sys.Date(), ".pdf"),
    content = function(file) {
      hi <- heat_input()
      A <- hi$A; B <- hi$B
      lim <- max(hi$limA, hi$limB); if (!is.finite(lim)) lim <- 1
      
      dfA <- as.data.frame(A) |> tibble::rownames_to_column("Gene") |>
        tidyr::pivot_longer(-Gene, names_to = "Condition", values_to = "Value") |>
        dplyr::mutate(Layer = "Chemical Genomics")
      dfB <- as.data.frame(B) |> tibble::rownames_to_column("Gene") |>
        tidyr::pivot_longer(-Gene, names_to = "Condition", values_to = "Value") |>
        dplyr::mutate(Layer = "Omics")
      df_all <- dplyr::bind_rows(dfA, dfB)
      
      p <- ggplot2::ggplot(df_all, ggplot2::aes(x = Gene, y = Condition, fill = Value)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.1) +
        ggplot2::facet_wrap(~Layer, nrow = 1, scales = "free_y") +
        ggplot2::scale_y_discrete(drop = TRUE) +
        ggplot2::scale_fill_gradient2(
          low = "#313695", mid = "#ffffbf", high = "#a50026",
          midpoint = 0, limits = c(-lim, lim),
          name = "Score") +
        ggplot2::labs(
          x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 8) +
        ggplot2::theme(
          strip.background = ggplot2::element_blank(),            # remove grey boxes
          strip.text = ggplot2::element_text(size = 9, face = "bold", color = "#2C3E50"),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
          axis.text.y = ggplot2::element_text(size = 6),
          panel.grid = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(0.4, "lines"),
          legend.position = "right",
          legend.title = ggplot2::element_text(size = 8),
          legend.text = ggplot2::element_text(size = 7)
        )
      
      ggplot2::ggsave(file, plot = p, width = 9, height = 4, units = "in", device = "pdf")
    }
  )
  

  
  # ---------- overlap summary ----------
  overlap_tbl <- reactive({
    req(aligned(), input$chem_col_scatter, input$omics_col_scatter)
    thrc <- if (is.null(input$thr_chem)) 1 else input$thr_chem
    thro <- if (is.null(input$thr_omics)) 1 else input$thr_omics
    
    A <- aligned()$A[, input$chem_col_scatter, drop = FALSE][,1]
    B <- aligned()$B[, input$omics_col_scatter, drop = FALSE][,1]
    chemical_genomics_sig  <- abs(A) >= thrc
    omics_sig              <- abs(B) >= thro
    tibble::tibble(
      Category = c("Chemical Genomics only", "Omics only", "Intersection", "Neither"),
      Count = c(sum(chemical_genomics_sig & !omics_sig),
                sum(!chemical_genomics_sig & omics_sig),
                sum(chemical_genomics_sig & omics_sig),
                sum(!chemical_genomics_sig & !omics_sig))
    )
  })
  
  output$overlap_plot <- renderPlot({
    req(scatter_df())
    df <- scatter_df()
    thrc <- if (is.null(input$thr_chem)) 1 else input$thr_chem
    thro <- if (is.null(input$thr_omics)) 1 else input$thr_omics
    fo <- fisher_overlap_stats(df$Chem, df$Omics, thrc, thro)
    
    tab <- as.matrix(fo$tab)
    plot_df <- tibble::tibble(
      Category = factor(
        c("Both significant", "Chemical Genomics only", "Omics only", "Neither significant"),
        levels = c("Both significant", "Chemical Genomics only", "Omics only", "Neither significant")
      ),
      Count = c(tab["Yes","Yes"], tab["Yes","No"], tab["No","Yes"], tab["No","No"])
    )
    
    # ChemGenXplore gradient palette (teal → blue → light gray)
    cgx_colors <- c(
      "Both significant"          = "#009999",  # Teal
      "Chemical Genomics only"    = "#1E88E5",  # Blue
      "Omics only"                = "#42A5F5",  # Lighter blue
      "Neither significant"       = "#D6DBDF"   # Soft gray
    )
    
    ggplot(plot_df, aes(x = Category, y = Count, fill = Category)) +
      geom_col(width = 0.65, color = "black", alpha = 0.9) +
      scale_fill_manual(values = cgx_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = "Overlap Summary",
        x = NULL,
        y = "Number of Genes"
      ) +
      theme_classic(base_size = 14) +
      theme(
        plot.title  = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2C3E50"),
        axis.text.x = element_text(size = 13, face = "bold", color = "#2C3E50"),
        axis.text.y = element_text(size = 13, face = "bold", color = "#2C3E50"),
        axis.title.y = element_text(size = 15, face = "bold", color = "#2C3E50"),
        legend.position = "none",
        plot.margin = margin(10, 15, 6, 10)
      )
  }, height = 350)
  
  
  
  output$overlap_table <- renderUI({
    req(scatter_df())
    df <- scatter_df()
    thrc <- if (is.null(input$thr_chem)) 1 else input$thr_chem
    thro <- if (is.null(input$thr_omics)) 1 else input$thr_omics
    fo <- fisher_overlap_stats(df$Chem, df$Omics, thrc, thro)
    
    mat <- as.matrix(fo$tab)
    counts <- data.frame(
      Metric = c(
        "Number of genes that pass the threshold in both datasets",
        "Number of genes that pass the threshold in Chemical Genomics only",
        "Number of genes that pass the threshold in Omics only",
        "Number of genes below threshold in both datasets"
      ),
      Value = c(
        mat["Yes", "Yes"],
        mat["Yes", "No"],
        mat["No",  "Yes"],
        mat["No",  "No"]
      ),
      row.names = NULL,
      check.names = FALSE
    )
    
    # Combine table + explanation in one renderUI output
    tagList(
      DT::renderDataTable({
        DT::datatable(
          counts,
          rownames = FALSE,
          options = list(dom = "t", pageLength = 5)
        )
      }),
      tags$div(
        style = "margin-top:12px; font-size:15px; color:#2C3E50;",
        HTML(sprintf("
      <p><b>Fisher’s exact test.</b> 
      <i>Odds Ratio (OR)</i> = %s, 
      95%% CI = [%s, %s], 
      <i>p</i> = %s.</p>
      <ul style='margin-top:6px;'>
        <li><b>Odds Ratio (OR):</b> measures enrichment of overlap. OR &gt; 1 → enrichment; OR ≈ 1 → no association; OR &lt; 1 → depletion.</li>
        <li><b>95%% Confidence Interval (CI):</b> range for the true OR. If the CI excludes 1, the overlap is statistically significant.</li>
        <li><b>p-value:</b> probability of observing this overlap by chance. Lower values indicate stronger evidence of a true relationship.</li>
      </ul>
    ",
                     fmt_num(fo$OR),
                     fmt_num(fo$lo),
                     fmt_num(fo$hi),
                     fmt_num(fo$p, 3)
        ))
      )
    )
  })
  
  
  output$dl_overlap <- downloadHandler(
    filename = function() "overlap_summary.csv",
    content = function(file) write.csv(overlap_tbl(), file, row.names = FALSE)
  )

}

  
  