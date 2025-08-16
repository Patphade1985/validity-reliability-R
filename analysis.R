> # Calculate Reliability and Validity for Item Responses
  > # Using built-in 'attitude' dataset in base R
  > 
  > # Load dataset
  > data("attitude")
> 
  > # Select relevant items (excluding 'rating' which is the outcome variable)
  > items <- attitude[, c("complaints", "privileges", "learning", "raises", "critical", "advance")]
  > 
    > # 1. Calculate Reliability (Cronbach's Alpha)
    > k <- ncol(items)
    > item_vars <- apply(items, 2, var)
    > total_scores <- rowSums(items)
    > alpha <- (k / (k - 1)) * (1 - sum(item_vars) / var(total_scores))
    > 
      > # 2. Calculate Validity (Item-Total Correlations)
      > item_total_cors <- sapply(1:k, function(i) {
        +     cor(items[, i], total_scores - items[, i])  # Corrected item-total correlation
        + })
      > 
        > # 3. Create Results Summary
        > results <- list(
          +     Reliability = alpha,
          +     Item_Total_Correlations = setNames(item_total_cors, colnames(items))
          + )
        > 
          > # Print results
          > cat("Reliability (Cronbach's Alpha):", round(results$Reliability, 3), "\n\n")
        Reliability (Cronbach's Alpha): 0.81 

> cat("Validity (Corrected Item-Total Correlations):\n")
Validity (Corrected Item-Total Correlations):
> print(round(results$Item_Total_Correlations, 3))
complaints privileges   learning     raises   critical    advance 
     0.635      0.557      0.679      0.782      0.276      0.520 
> # ================================================
> # PSYCHOMETRIC ANALYSIS WITH R
> # Using Real Data: Holzinger & Swineford (1939)
> # ================================================
> 
> # 1. LOAD REQUIRED PACKAGES
> # ------------------------------------------------
> # Install packages if needed
> packages <- c("lavaan", "psych", "semTools", "corrplot", 
+               "psychTools", "GPArotation", "ggplot2", 
+               "dplyr", "tidyr", "MBESS")
> 
> # Install missing packages
> new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
> if(length(new_packages)) install.packages(new_packages)
> 
> # Load libraries
> library(lavaan)      # For CFA and SEM
> library(psych)       # For psychometric analysis
> library(semTools)    # For reliability calculations
> library(corrplot)    # For correlation plots
> library(psychTools)  # Additional psychometric tools
> library(GPArotation) # For factor rotation
> library(ggplot2)     # For visualization
> library(dplyr)       # Data manipulation
> library(tidyr)       # Data manipulation
> library(MBESS)       # For confidence intervals
> 
> # 2. LOAD AND EXPLORE DATA
> # ------------------------------------------------
> # Load the classic Holzinger-Swineford dataset
> data("HolzingerSwineford1939", package = "lavaan")
> hs_data <- HolzingerSwineford1939
> 
> # View structure and first few rows
> str(hs_data)
'data.frame':	301 obs. of  15 variables:
 $ id    : int  1 2 3 4 5 6 7 8 9 11 ...
 $ sex   : int  1 2 2 1 2 2 1 2 2 2 ...
 $ ageyr : int  13 13 13 13 12 14 12 12 13 12 ...
 $ agemo : int  1 7 1 2 2 1 1 2 0 5 ...
 $ school: Factor w/ 2 levels "Grant-White",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ grade : int  7 7 7 7 7 7 7 7 7 7 ...
 $ x1    : num  3.33 5.33 4.5 5.33 4.83 ...
 $ x2    : num  7.75 5.25 5.25 7.75 4.75 5 6 6.25 5.75 5.25 ...
 $ x3    : num  0.375 2.125 1.875 3 0.875 ...
 $ x4    : num  2.33 1.67 1 2.67 2.67 ...
 $ x5    : num  5.75 3 1.75 4.5 4 3 6 4.25 5.75 5 ...
 $ x6    : num  1.286 1.286 0.429 2.429 2.571 ...
 $ x7    : num  3.39 3.78 3.26 3 3.7 ...
 $ x8    : num  5.75 6.25 3.9 5.3 6.3 6.65 6.2 5.15 4.65 4.55 ...
 $ x9    : num  6.36 7.92 4.42 4.86 5.92 ...
> head(hs_data)
  id sex ageyr agemo  school grade       x1   x2    x3       x4   x5        x6
1  1   1    13     1 Pasteur     7 3.333333 7.75 0.375 2.333333 5.75 1.2857143
2  2   2    13     7 Pasteur     7 5.333333 5.25 2.125 1.666667 3.00 1.2857143
3  3   2    13     1 Pasteur     7 4.500000 5.25 1.875 1.000000 1.75 0.4285714
4  4   1    13     2 Pasteur     7 5.333333 7.75 3.000 2.666667 4.50 2.4285714
5  5   2    12     2 Pasteur     7 4.833333 4.75 0.875 2.666667 4.00 2.5714286
6  6   2    14     1 Pasteur     7 5.333333 5.00 2.250 1.000000 3.00 0.8571429
        x7   x8       x9
1 3.391304 5.75 6.361111
2 3.782609 6.25 7.916667
3 3.260870 3.90 4.416667
4 3.000000 5.30 4.861111
5 3.695652 6.30 5.916667
6 4.347826 6.65 7.500000
> 
> # Select the 9 cognitive variables for analysis
> cognitive_vars <- hs_data[, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")]
> 
> # Basic descriptive statistics
> describe(cognitive_vars)
   vars   n mean   sd median trimmed  mad  min   max range  skew kurtosis   se
x1    1 301 4.94 1.17   5.00    4.96 1.24 0.67  8.50  7.83 -0.25     0.31 0.07
x2    2 301 6.09 1.18   6.00    6.02 1.11 2.25  9.25  7.00  0.47     0.33 0.07
x3    3 301 2.25 1.13   2.12    2.20 1.30 0.25  4.50  4.25  0.38    -0.91 0.07
x4    4 301 3.06 1.16   3.00    3.02 0.99 0.00  6.33  6.33  0.27     0.08 0.07
x5    5 301 4.34 1.29   4.50    4.40 1.48 1.00  7.00  6.00 -0.35    -0.55 0.07
x6    6 301 2.19 1.10   2.00    2.09 1.06 0.14  6.14  6.00  0.86     0.82 0.06
x7    7 301 4.19 1.09   4.09    4.16 1.10 1.30  7.43  6.13  0.25    -0.31 0.06
x8    8 301 5.53 1.01   5.50    5.49 0.96 3.05 10.00  6.95  0.53     1.17 0.06
x9    9 301 5.37 1.01   5.42    5.37 0.99 2.78  9.25  6.47  0.20     0.29 0.06
> 
> # 3. RELIABILITY ANALYSIS
> # ------------------------------------------------
> print("=" %.% replicate(50, "="))
Error in "=" %.% replicate(50, "=") : could not find function "%.%"
> # 3. RELIABILITY ANALYSIS
> # ------------------------------------------------
> print(paste0("=", paste(rep("=", 50), collapse = "")))
[1] "==================================================="
> 
> # Using built-in dataset 'attitude'
> data(attitude)
> items <- attitude[, 1:6]  # Select first 6 survey items
> 
> # Calculate Cronbach's Alpha (reliability)
                     > k <- ncol(items)
                     > item_vars <- apply(items, 2, var)
                     > total_score <- rowSums(items)
                     > alpha <- (k/(k-1)) * (1 - sum(item_vars)/var(total_score))
                     > 
                       > # Calculate item-total correlations (validity)
                       > item_total_cor <- sapply(1:k, function(i) {
                         +     cor(items[, i], total_score - items[, i])
                         + })
                     > 
                       > # Print results
                       > cat("\nReliability (Cronbach's Alpha):", round(alpha, 3), "\n\n")
                     
                     Reliability (Cronbach's Alpha): 0.84 

> cat("Item-Total Correlations:\n")
Item-Total Correlations:
> print(round(item_total_cor, 3))
[1] 0.730 0.796 0.550 0.673 0.744 0.234
> # 3. RELIABILITY ANALYSIS (CONSISTENT VERSION)
> # ------------------------------------------------
> cat(rep("=", 50), "\n", sep = "")  # Section separator
==================================================
> 
> # Load dataset
> data("attitude")
> 
> # Use SAME items as first analysis (exclude 'rating', include 'advance')
> items <- attitude[, c("complaints", "privileges", "learning", "raises", "critical", "advance")]
> 
> # Calculate Cronbach's Alpha
                                  > k <- ncol(items)
                                  > item_vars <- apply(items, 2, var)
                                  > total_score <- rowSums(items)
                                  > alpha <- (k/(k-1)) * (1 - sum(item_vars)/var(total_score))
                                  > 
                                    > # Calculate Corrected Item-Total Correlations
                                    > item_total_cor <- sapply(1:k, function(i) {
                                      +     cor(items[, i], total_score - items[, i])
                                      + })
                                  > 
                                    > # Print consistent results
                                    > cat("\nReliability (Cronbach's Alpha):", round(alpha, 3), "\n")
                                  
                                  Reliability (Cronbach's Alpha): 0.81 
> cat("Validity (Corrected Item-Total Correlations):\n")
Validity (Corrected Item-Total Correlations):
> print(round(setNames(item_total_cor, colnames(items)), 3))
complaints privileges   learning     raises   critical    advance 
     0.635      0.557      0.679      0.782      0.276      0.520 
> library(psych)
> alpha_result <- alpha(items)
Error: Unknown colour name: c(51, 64, 70, 63, 78, 55, 67, 75, 82, 61, 53, 60, 62, 83, 77, 90, 85, 60, 70, 58, 40, 61, 66, 37, 54, 77, 75, 57, 85, 82)
> library(psych)
> 
> # Calculate Cronbach's Alpha with proper namespacing
                                               > alpha_result <- psych::alpha(items)
                                               Number of categories should be increased  in order to count frequencies. 
                                               > 
                                                 > # View full results
                                                 > print(alpha_result, short = FALSE)
                                               
                                               Reliability analysis   
                                               Call: psych::alpha(x = items)
                                               
                                               raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd median_r
                                               0.81      0.81    0.83      0.41 4.2 0.052   60 8.1     0.45
                                               
                                               95% confidence boundaries 
                                               lower alpha upper
                                               Feldt     0.68  0.81  0.90
                                               Duhachek  0.71  0.81  0.91
                                               
                                               Reliability if an item is dropped:
                                                 raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
                                               complaints      0.77      0.77    0.76      0.40 3.3    0.067 0.031  0.41
                                               privileges      0.78      0.78    0.81      0.42 3.6    0.062 0.043  0.45
                                               learning        0.75      0.75    0.78      0.38 3.1    0.070 0.031  0.36
                                               raises          0.74      0.73    0.74      0.35 2.7    0.074 0.033  0.31
                                               critical        0.83      0.84    0.85      0.51 5.2    0.048 0.019  0.54
                                               advance         0.79      0.79    0.79      0.42 3.7    0.058 0.043  0.47
                                               
                                               Item statistics 
                                               n raw.r std.r r.cor r.drop mean   sd
                                               complaints 30  0.78  0.76  0.73   0.63   67 13.3
                                               privileges 30  0.72  0.70  0.60   0.56   53 12.2
                                               learning   30  0.80  0.79  0.75   0.68   56 11.7
                                               raises     30  0.86  0.86  0.86   0.78   65 10.4
                                               critical   30  0.46  0.49  0.33   0.28   75  9.9
                                               advance    30  0.66  0.69  0.63   0.52   43 10.3