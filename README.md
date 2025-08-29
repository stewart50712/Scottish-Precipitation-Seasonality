# Scottish Weather Analysis (1960–2018)

This project explores long-term weather data from the **Global Historical Climatology Network (GHCN)** for eight stations in Scotland between 1960 and 2018. It was originally developed as part of a statistical computing project and demonstrates data wrangling, statistical testing, and regression modelling in R.

## Contents
- `StatComp_Project2_ScottishWeather.Rmd` – main analysis in R Markdown  
- `functions.R` – helper functions used in the analysis  
- `data/` – contains station data and saved results (large .rds files ignored in `.gitignore`)  
- `figs/` – generated figures  

## Key Analysis
- **Seasonal variability**: Comparison of precipitation and temperature across summer vs. winter.  
- **Monte Carlo permutation tests**: Testing the null hypothesis that rainfall distribution is the same in both seasons.  
- **Confidence intervals**: Approximated for permutation test p-values.  
- **Spatial prediction models**: Linear regression models with seasonal harmonic terms to estimate monthly precipitation.  
- **Cross-validation**: Model performance evaluated using Dawid–Sebastiani and squared error scores.  

## Tools & Skills Demonstrated
- R (tidyverse, ggplot2, regression modelling)  
- Statistical inference (permutation tests, confidence intervals)  
- Model evaluation and cross-validation  
- Data visualization and reproducible reporting with R Markdown  

## How to Run
1. Clone this repo.  
2. Open `StatComp_Project2_ScottishWeather.Rmd` in RStudio.  
3. Install required R packages:  
   ```r
   install.packages(c("tidyverse", "dbplyr", "knitr"))
   # plus any course-provided packages if available
