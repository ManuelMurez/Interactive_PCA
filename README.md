# Interactive_PCA
This repository contains an R function to plot PCoA from a distance matrix with optional metadata and interactive filtering.


## Usage
```r
source("plot_pcoa.R")
distance_file <- "data/395k_rogers_distance.csv"
metadata_file <- "data/ploidy_metadata.csv"
color_var <- "ploidy"
filter_var <- "species"
plot_pcoa(distance_file, metadata_file, color_var, filter_var)
``` 
