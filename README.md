# International-Research-Impact

## BACKGROUND
These codes will help you reproduce all plots and statistical analyses of two publications
**The Geographical Gap in Leading Medical Journals - a Computational Audit** by *Brück Oscar, MD PhD* and
**The Gender Gap in Leading Medical Journals - a Computational Audit** by *Brück Oscar, MD PhD*  


## DATA
### Web of Science
Certain data included herein are derived from Clarivate Web of Science. © Copyright Clarivate 2022. All rights reserved. The complete data cannot be shared based on Clarivate's policy, but a 100-row snapshot including publication metadata of the author names, page length of the article, address of the corresponding author, total number of citations, etc. is available in `./data/data_example_100rows.xlsx`. In addition, image examples of the document search process and queries are provided in `./WebOfScience/`. If you want to try the script for your own query (for example if you want to analyze other journals or other publication years, replace the `./data/data.xlsx` with your new data file.

### Population data
National population data were downloaded from the [World Bank database](https://data.worldbank.org/indicator/SP.POP.TOTL), and municipal data from the world.cities dataset of the utils R package.


## INSTRUCTIONS

### Instructions if you operate with RStudio
1. Install RStudio (the analyses here have been made with the version 3.5.1)
2. Install necessary R packages either by running the Rscript `./src/install_packages.R`
3. Run the analyses by running the Rscript `./src/full_script.R`. This will produce tables and images in `./src/results/`.

### Instructions if you operate with the terminal
1. Install RStudio (the analyses here have been made with the version 3.5.1)
2. `cd ./International-Research-Impact`
3. `Rscript src/install_packages.R`
4. Run the analyses by running `Rscript src/full_script.R`. This will produce results in `./src/results/`.


A detailed description of  the R and library versions can be found in `session_info.yaml`.
