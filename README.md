<p align="center">
  <img src="www/logo.png" alt="Logo" width="500">
</p>

___

**ChemGenXplore** is an interactive **Shiny** application designed for the visualisation and analysis of chemical genomic screens. This tool enables the exploration of gene- and condition-specific phenotypes, correlation analyses, and enrichment analyses, providing an intuitive platform for investigating large-scale chemical-genomic datasets.

![Overview Figure](www/overview_figure.png)

___

## Introduction 

Chemical genomic screens are a valuable resource for uncovering gene functions and mapping biological pathways. However, their large-scale nature makes these datasets complex and challenging to interpret. ChemGenXplore addresses this challenge by simplifying access to these resources and providing researchers with an intuitive platform to explore and understand genotype-phenotype relationships across various species.


## Features

- **Phenotypic Visualisation**:
  - Explore gene- and condition-specific phenotypic scores through dynamic bar plots and interactive data tables.
  - Adjust the **False Discovery Rate (FDR)** threshold to highlight statistically significant phenotypes.

- **Correlation Analyses**:
  - Quantitatively assess **gene-gene** and **condition-condition** correlations using **Pearson’s correlation coefficient**.
  - Correlations exceeding ±0.4 are displayed as significant, with options to adjust the FDR threshold.
  - Explore positive and negative correlations for genes and conditions through interactive plots.

- **GO & KEGG Enrichment Analysis**:
  - Perform Gene Ontology (GO) and KEGG pathway enrichment analyses for the *Escherichia coli* dataset.
  - Visualise significant enrichment terms as interactive bar plots.
  - Download enrichment results and plots for further exploration.

- **Customisable Heatmaps**:
  - Generate hierarchical heatmaps for gene-condition fitness data.
  - Customise clustering options with methods like **complete**, **single**, **average**, and **Ward D**.
  - Choose from multiple distance metrics (e.g., **Euclidean**, **Manhattan**) for clustering rows and columns.
  - Zoom in on specific regions of the heatmap and download the results.

- **Upload Your Own Dataset**:
  - Use the app’s upload feature to analyse custom datasets.
  - Automatically compute Z-scores, FDR-adjusted p-values, and other metrics for your uploaded data.

- **Downloadable Results**:
  - Export processed datasets, plots, and heatmaps in user-friendly formats (e.g., **CSV**, **PDF**).


## How to Run the App

1. Clone the repository:
   ```bash
   git clone https://github.com/Hudaahmadd/ChemGenXplore.git

## Acknowledgment

ChemGenXplore was developed with the support of The Banzhaf Lab and The Moradigaravand Lab.

- Institute of Microbiology and Infection, School of Biosciences, University of Birmingham, Birmingham, UK
- Biosciences Institute, Faculty of Medical Sciences, Newcastle University, Newcastle upon Tyne, UK
- Laboratory of Infectious Disease Epidemiology, KAUST Center of Excellence for Smart Health, Biological and Environmental Science and Engineering (BESE) Division, King Abdullah University of Science and Technology (KAUST), Thuwal, Saudi Arabia

With gratitude, I acknowledge the funding provided by The Darwin Trust of Edinburgh for my PhD studies.


## Contact 

For enquiries, please contact Huda Ahmad, Institute of Microbiology and Infection, School of Biosciences, University of Birmingham.




