<p align="center">
  <img src="www/logo.png" alt="Logo" width="500">
</p>

___

**ChemGenXplore** is an interactive **Shiny** application designed for the visualisation and analysis of chemical genomic screens. This tool enables the exploration of gene- and condition-specific phenotypes, correlation analyses, and enrichment analyses, providing an intuitive platform for investigating large-scale chemical-genomic datasets.

![Overview Figure](www/overview_figure.png)


## Introduction 

Chemical genomic screens are a valuable resource for uncovering gene functions and mapping biological pathways. However, their large-scale nature makes these datasets complex and challenging to interpret. ChemGenXplore addresses this challenge by simplifying access to these resources and providing researchers with an intuitive platform to explore and understand genotype-phenotype relationships across various species.


## Features

- **Phenotypic Visualisation**:
  - Explore gene- and condition-specific phenotypic scores through dynamic bar plots and interactive data tables.
  - Filter statistically significant phenotypes with an adjustable False Discovery Rate (FDR) threshold.

- **Correlation Analyses**:
  - Assess gene-gene and condition-condition correlations using Pearson’s correlation coefficient.
  - Filter for significant correlations exceeding ±0.4 with an adjustable FDR threshold.
  - Visualise positive and negative correlations for genes and conditions through interactive plots.
 
- **Enrichment Analysis**:
  - Perform Gene Ontology (GO) and KEGG pathway enrichment analyses for the *Escherichia coli* dataset.
  - Adjust the FDR threshold to refine enrichment results.
  - Visualise significant enrichment terms as interactive bar plots.

- **Interactive Heatmaps**:
  - Generate hierarchical heatmaps for gene-condition phenotypic scores.
  - Customise clustering with multiple methods (Complete, Single, Average, Ward D) and distance metrics (Euclidean, Manhattan, Maximum, Canberra).
  - Interact with heatmaps by zooming into specific regions.

- **Upload Your Own Dataset**:
  - Upload a CSV file where rows contain unique identifiers (e.g., genes, samples, strains) and columns contain experimental measurements (e.g., fitness scores, gene expression       levels, growth rates). Example Dataset Below:
  - Choose from multiple distance metrics (e.g., Euclidean, Manhattan) for clustering rows and columns.
  - Interact with heatmaps by zooming into specific regions.

- **Upload Your Own Dataset**:
  - Upload custom datasets formatted with genes as rows and conditions as columns (CSV format). Example below:
>>>>>>> 2eb2088 (Updated README from GitHub)
    <p>
      <img src="www/example_dataset.png" alt="Logo" width="300">
    </p>
    
  - Use the same phenotypic, correlation, enrichment, and heatmap analysis tools available for the E. coli dataset.

**All plots and data tables generated can be downloaded.**

## How to Run the App

There are two ways to use **ChemGenXplore**: via the hosted Shiny application or by running it locally.

### Option 1: Hosted Shiny Application

The easiest way to use **ChemGenXplore** is through the hosted Shiny application. Access it directly at:

[**ChemGenXplore Shiny App**](< https://hudaahmad.shinyapps.io/chemgenxplore/>)

---

### Option 2: Local Installation

To run **ChemGenXplore** locally, follow these steps:

1. **Clone the Repository**

   Open your terminal and clone the repository using:
   ```bash
   git clone https://github.com/Hudaahmadd/ChemGenXplore.git

2. **Navigate to the Project Directory**

    Change into the project directory:
     ```bash
      cd ChemGenXplore
     ```
3. **Open RStudio and Set the Working Directory**
    
    Launch RStudio and set the working directory to the ChemGenXplore folder.

4. **Install and Use renv to Restore Dependencies**

    In the R console, run the following commands:
    ```bash
    install.packages("renv")
    renv::restore()
    ```

5. **Run the Application**

    After the environment has been restored, run:
     ```bash
      shiny::runApp()
     ```
Or, you can open the app.R file in RStudio and click Run App.

Alternatively, if you do not wish to use renv

    You can manually install the required packages by running:
     ```bash
      source("packages.R")
     ```


## Data Sources
ChemGenXplore includes three pre-integrated, publicly available *Escherichia coli* datasets from chemical genomic screens. These datasets provide fitness scores across a wide range of conditions and serve as the foundation for the analyses performed in ChemGenXplore:

- Nichols et al., 2011: https://doi.org/10.1016/j.cell.2010.11.052
- Shiver et al., 2016: https://doi.org/10.1371/journal.pgen.1006124
- Price et al., 2018: https://doi.org/10.1038/s41586-018-0124-0

**Users also have the flexibility to upload their own datasets for analysis.**

## Acknowledgment

ChemGenXplore was developed with the support of The Banzhaf Lab and The Moradigaravand Lab.

- Institute of Microbiology and Infection, School of Biosciences, University of Birmingham, Birmingham, UK
- Biosciences Institute, Faculty of Medical Sciences, Newcastle University, Newcastle upon Tyne, UK
- Laboratory of Infectious Disease Epidemiology, KAUST Center of Excellence for Smart Health and Biological and Environmental Science and Engineering (BESE) Division, King Abdullah University of Science and Technology (KAUST), Thuwal, Saudi Arabia

This work is supported by The Darwin Trust of Edinburgh, which funds my PhD studies.

## Contact 

For enquiries, please contact Huda Ahmad, Institute of Microbiology and Infection, School of Biosciences, University of Birmingham.
