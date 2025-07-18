Demographics
================
freya murray
2025-05-07

# Set-up

``` r
library(tidyverse)
# dependencies
```

``` r
raw <- read_csv("Drawings_demographics.csv", show_col_types = FALSE) %>% filter(Drawing != "No")
# read in raw data, removing participants who did not provide a drawing - n=70 
```

------------------------------------------------------------------------

# Age

``` r
age <- raw %>% select(Age_subbed) %>%
        summarise("Mean Age" = mean(Age_subbed), "SD Age" = sd(Age_subbed), "Median Age" = median(Age_subbed), "Min Age" = min(Age_subbed), "Max Age" = max(Age_subbed)) %>% round(digits = 1) 

print(age)
```

    ## # A tibble: 1 × 5
    ##   `Mean Age` `SD Age` `Median Age` `Min Age` `Max Age`
    ##        <dbl>    <dbl>        <dbl>     <dbl>     <dbl>
    ## 1        8.5      2.1            9         5        11

``` r
# n=5 participants did not disclose their ages - they were assigned the average age for their grade (e.g. P1 = 5, P3 = 7 and so on)
# participants who answered "nearly x" were marked as x age 
# participants who answered "x.5" were rounded to next whole number
```

## Age Chart

``` r
age_only <- raw %>% select(Age_subbed) 

ggplot(age_only, aes(Age_subbed)) +
  geom_bar(fill = "grey60") +
  theme_classic() +
  labs(title = "Age Distribution", x = "Age", y = "Count")
```

![](demographics_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

------------------------------------------------------------------------

# Gender

``` r
gender_all <- raw %>% select(Gender_ID) # all participants in this one

gender <- raw %>% select(Gender_ID) %>% filter(!Gender_ID == "Other") 
# one participant removed for clarity of graph (answered "other")
```

## Gender Chart

``` r
ggplot(gender_all, aes(Gender_ID, fill = Gender_ID)) +
  geom_bar(alpha = .75, show.legend = FALSE, fill = c("skyblue", "plum", "olivedrab")) +
  theme_classic() +
  labs(title = "Gender Distribution (All)", x = "Gender Identity", y = "Count")
```

![](demographics_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(gender, aes(Gender_ID, fill = Gender_ID)) +
  geom_bar(alpha = .75, show.legend = FALSE, fill = c("skyblue", "plum")) +
  theme_classic() +
  labs(title = "Gender Distribution (Boys/Girls)", x = "Gender Identity", y = "Count")
```

![](demographics_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
gender_violins <- raw %>% select(Gender_ID,Age_subbed) %>% filter(Gender_ID != "Other")
```

``` r
ggplot(gender_violins, aes(Gender_ID, Age_subbed)) +
  geom_violin(trim = FALSE, show.legend = FALSE, alpha = .6, width = .5) +
  geom_boxplot(alpha = .4, show.legend = FALSE, width = .5, fill = c("skyblue", "plum"))+
  theme_classic() +
  labs(title = "Age Distribution by Gender", x = "Gender Identity", y = "Age")
```

![](demographics_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Gender Stats

``` r
gender_stats <- raw %>% count(Gender_ID) %>% mutate("percentage" = (n/70)*100) %>%
  reframe("Gender Identity" = Gender_ID, "Count" = n, "Percentage" = round(percentage, digits = 1))  

gender_stats
```

    ## # A tibble: 3 × 3
    ##   `Gender Identity` Count Percentage
    ##   <chr>             <int>      <dbl>
    ## 1 Boy                  35       50  
    ## 2 Girl                 34       48.6
    ## 3 Other                 1        1.4

``` r
gender_stats_2 <- gender %>% count(Gender_ID) %>% mutate("percentage" = (n/69)*100) %>%
  reframe("Gender Identity" = Gender_ID, "Count" = n, "Percentage" = round(percentage, digits = 1))  

gender_stats_2
```

    ## # A tibble: 2 × 3
    ##   `Gender Identity` Count Percentage
    ##   <chr>             <int>      <dbl>
    ## 1 Boy                  35       50.7
    ## 2 Girl                 34       49.3

``` r
# with "other" participant removed
```

------------------------------------------------------------------------

# Had COVID

``` r
had_cov <- raw %>% select(Had_covid)
```

## Chart

``` r
ggplot(had_cov, aes(Had_covid, fill = Had_covid)) +
  geom_bar(alpha = .6, show.legend=FALSE, fill = c("grey30","grey60","firebrick", "olivedrab", "grey80")) +
  theme_classic() +
  labs(title = "Had COVID-19", x="", y="Count")
```

![](demographics_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Stats

``` r
had_cov_stats <- had_cov %>% count(Had_covid) %>% mutate("percentage" = (n/70)*100) %>%
  reframe("Status" = Had_covid, "Count" = n, "Percentage" = round(percentage, digits = 1)) 

had_cov_stats
```

    ## # A tibble: 5 × 3
    ##   Status     Count Percentage
    ##   <chr>      <int>      <dbl>
    ## 1 Don't know     8       11.4
    ## 2 Maybe          1        1.4
    ## 3 No            16       22.9
    ## 4 Yes           44       62.9
    ## 5 <NA>           1        1.4

------------------------------------------------------------------------

# Knew Someone

``` r
knew_cov <- raw %>% select(Knew_someone_covid)
```

## Chart

``` r
ggplot(knew_cov, aes(Knew_someone_covid, fill = Knew_someone_covid)) +
  geom_bar(alpha = .6, show.legend = FALSE, fill = c("grey30","grey60","firebrick", "olivedrab", "grey80")) +
  theme_classic() +
  labs(title = "Knew Someone with COVID-19", x="", y="Count")
```

![](demographics_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Stats

``` r
knew_cov_stats <- knew_cov %>% count(Knew_someone_covid) %>% mutate("percentage" = (n/70)*100) %>%
  reframe("Status" = Knew_someone_covid, "Count" = n, "Percentage" = round(percentage, digits = 1)) 

knew_cov_stats
```

    ## # A tibble: 5 × 3
    ##   Status     Count Percentage
    ##   <chr>      <int>      <dbl>
    ## 1 Don't know     1        1.4
    ## 2 Maybe          2        2.9
    ## 3 No            15       21.4
    ## 4 Yes           50       71.4
    ## 5 <NA>           2        2.9

------------------------------------------------------------------------

# Drawing

``` r
drawing <- raw %>% select(Drawing)
```

## Chart

``` r
ggplot(drawing, aes(Drawing, fill = Drawing)) +
  geom_bar(alpha = .6, show.legend = FALSE, fill = c("firebrick", "olivedrab")) +
  theme_classic() +
  labs(title = "Drawing Content", x="", y="Count")
```

![](demographics_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## Stats

``` r
drawing_stats <- drawing %>% count(Drawing) %>% mutate("percentage" = (n/70)*100) %>%
  reframe("Status" = Drawing, "Count" = n, "Percentage" = round(percentage, digits = 1)) 

drawing_stats
```

    ## # A tibble: 2 × 3
    ##   Status     Count Percentage
    ##   <chr>      <int>      <dbl>
    ## 1 Irrelevant     9       12.9
    ## 2 Yes           61       87.1
