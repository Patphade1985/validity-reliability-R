# validity-reliability-R
# Psychometric Analysis Toolkit

Professional assessment of survey instruments' reliability and validity using R

## Features
- Cronbach's Alpha calculation with confidence intervals
- Item-total correlation diagnostics
- Factor analysis implementation
- Automated reporting
- Problematic item identification

## Key Findings
- Identified 1 problematic item (r = 0.28) in organizational behavior scale
- Improved scale reliability from α=0.81 to α=0.84 through item refinement
- Verified unidimensionality of constructs through factor analysis

## How to Use
```r
# Run full analysis pipeline
source("analysis/reliability_analysis.R")
source("analysis/factor_analysis.R")

# Generate report
rmarkdown::render("reports/psychometric_report.Rmd")

- [LinkedIn](https://linkedin.com/in/patrick-chimadzuma)
