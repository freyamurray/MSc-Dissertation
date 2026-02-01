Inter-rater reliability
================
Freya Murray
2025-05-30

``` r
# dependencies
library(tidyverse)
library(irr)
```

``` r
# reading in raw data

freya <- read_csv("Coding_All_Freya_3.csv") %>% na.omit()
shuran <- read_csv("Coding_All_Shuran-3.csv") %>% na.omit()

# na.omit() removed participants 1, 2, 11, 17 and 29 who did not provide drawings, n=70
```

``` r
# calculate inter-rater reliability for identity dimension
identity <- tibble(freya$Identity, 
                   shuran$Identity) %>% kappa2(weight = "equal")
identity
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 70 
    ##    Raters = 2 
    ##     Kappa = 0.879 
    ## 
    ##         z = 11.1 
    ##   p-value = 0

``` r
# calculate inter-rater reliability for severity dimension
severity <- tibble(freya$Severity, 
                   shuran$Severity) %>% kappa2(weight = "equal")
severity
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 70 
    ##    Raters = 2 
    ##     Kappa = 0.769 
    ## 
    ##         z = 7.83 
    ##   p-value = 5.11e-15

``` r
# calculate inter-rater reliability for symptoms dimension
symptoms <- tibble(freya$Symptoms, 
                   shuran$Symptoms) %>% kappa2(weight = "equal")

symptoms
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 70 
    ##    Raters = 2 
    ##     Kappa = 0.631 
    ## 
    ##         z = 6.47 
    ##   p-value = 9.91e-11

``` r
# calculate inter-rater reliability for causes dimension
causes <- tibble(freya$Causes, 
                shuran$Causes) %>% kappa2(weight = "equal")
causes
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 70 
    ##    Raters = 2 
    ##     Kappa = 0.679 
    ## 
    ##         z = 6.92 
    ##   p-value = 4.57e-12

``` r
# calculate inter-rater reliability for prevention dimension
prevention <- tibble(freya$Prevention,
                   shuran$Prevention) %>% kappa2(weight = "equal")
prevention
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 70 
    ##    Raters = 2 
    ##     Kappa = 0.843 
    ## 
    ##         z = 7.59 
    ##   p-value = 3.29e-14

``` r
# calculate inter-rater reliability for timeline 1 dimension
timeline_1 <- tibble(freya$Timeline_1, 
                   shuran$Timeline_1) %>% kappa2(weight = "equal")
timeline_1
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 70 
    ##    Raters = 2 
    ##     Kappa = 0.854 
    ## 
    ##         z = 7.93 
    ##   p-value = 2.22e-15

``` r
# calculate inter-rater reliability for curability dimension
curability <- tibble(freya$Curability, 
                   shuran$Curability) %>% kappa2(weight = "equal")
curability
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 70 
    ##    Raters = 2 
    ##     Kappa = 0.743 
    ## 
    ##         z = 7.21 
    ##   p-value = 5.59e-13

``` r
# calculate inter-rater reliability for treatment dimension
treatment <- tibble(freya$Treatment, 
                   shuran$Treatment) %>% kappa2(weight = "equal")
treatment
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 70 
    ##    Raters = 2 
    ##     Kappa = 0.768 
    ## 
    ##         z = 7.28 
    ##   p-value = 3.27e-13

``` r
# calculate inter-rater reliability for timeline 2 dimension
timeline_2 <- tibble(freya$Timeline_2, 
                   shuran$Timeline_2) %>% kappa2(weight = "equal")
timeline_2
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 70 
    ##    Raters = 2 
    ##     Kappa = 0.881 
    ## 
    ##         z = 9.34 
    ##   p-value = 0

``` r
# calculate mean kappa value across all dimensions
total <- mean(c(identity$value, 
                severity$value, 
                symptoms$value, 
                causes$value, 
                prevention$value,
                timeline_1$value,
                curability$value,
                treatment$value,
                timeline_2$value))

print(total)
```

    ## [1] 0.7829927

``` r
freya_data_only <- freya %>% select(-Participant)
shuran_data_only <- shuran %>% select(-Participant) 
```

``` r
freya_one_column <- freya %>% pivot_longer(2:10, names_to = "dimension")
shuran_one_column <- shuran %>% pivot_longer(2:10, names_to = "dimension")

freya_shuran_one_col <- tibble(freya_one_column$value, shuran_one_column$value)
```

``` r
kappa2(freya_shuran_one_col, weight = "equal")
```

    ##  Cohen's Kappa for 2 Raters (Weights: equal)
    ## 
    ##  Subjects = 630 
    ##    Raters = 2 
    ##     Kappa = 0.814 
    ## 
    ##         z = 26.5 
    ##   p-value = 0

``` r
total_agree <- agree(freya_shuran_one_col)
print(total_agree)
```

    ##  Percentage agreement (Tolerance=0)
    ## 
    ##  Subjects = 630 
    ##    Raters = 2 
    ##   %-agree = 91.1

``` r
# 91.1% agreement!
```
