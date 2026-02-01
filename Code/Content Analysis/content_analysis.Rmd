---
title: "Content Analysis"
author: "Freya Murray"
date: "2025-06-12"
output: html_document
---
## Setup
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, message=FALSE}
# Load dependency containing necessary functions

library(tidyverse)
```

```{r, message=FALSE}
# Load raw data
# Removal of n=5 participants who did not provide drawings

dat <- read_csv("Coding_All_Freya_3.csv") %>% 
  na.omit() 
```

```{r, message=FALSE}
# Load in demographic information
# Removal of n=5 participants who did not provide drawings
# Selecting columns containing ID, grade, age and gender

demog <- read_csv("demographics.csv") %>%
  filter(!Drawing == "No") %>%
  select(Participant, School_Grade, Age_subbed, Gender_ID)
```

## Wrangling
```{r}
# Create additional "Accuracy" column measuring overall accuracy 
# by computing mean scores across CSM dimensions

dat_accuracy <- dat %>%
  group_by(Participant) %>%
  mutate("Accuracy" = mean(c(Identity, 
                           Severity, 
                           Symptoms, 
                           Causes, 
                           Prevention, 
                           Timeline_1, 
                           Curability, 
                           Treatment, 
                           Timeline_2)))
```

```{r}
# Insert demographic information matched to participant IDs

dat_all <- inner_join(dat_accuracy, demog, by = "Participant") %>% 
  group_by(Age_subbed)
```

```{r}
# Assigning each participant to one of two age groups
# Ages 5-7 assigned to group "pre-operational"
# Ages 8-11 assigned to group "concrete operational" 

dat_age_groups <- dat_all %>%
  mutate("Age_Group" = ifelse(Age_subbed<8, "Preoperational", "Concrete Operational"))
```

## How does Accuracy change by Age?

### ANOVAs
#### Assumption checks
```{r}
# Running preliminary ANOVAs to establish residuals
res_age <- aov(Accuracy~Age_Group, dat_age_groups)
res_gender <- aov(Accuracy~Age_Group, dat_age_groups)
```

```{r}
# Shapiro-Wilk to test assumption of normality on accuracy by age
shapiro.test(res_age$residuals)

# Assumption of normality not met, as p<.05
```
```{r}
# Shapiro-Wilk to test assumption of normality on accuracy by gender identity
shapiro.test(res_gender$residuals)

# Assumtion of normality not met
```


```{r}
# Assumption checks - homoscedasticity and outliers

ggplot(dat_age_groups, aes(x = Age_Group, y = Accuracy, fill = Age_Group)) +
  geom_violin(trim = FALSE, alpha = .5, colour = FALSE, show.legend = FALSE) +
  geom_boxplot(width = .3, alpha = .5, show.legend = FALSE) +
  theme_bw() +
  labs(title = "ANOVA assumption checks", x = "Age Group", y = "Accuracy")

# Assumption of homoscedasticity met
# Some outliers identified
```

```{r}
# Performing logarithmic transformation on accuracy data
# to improve normality and mitigate outliers

dat_log <- dat_age_groups %>% 
  mutate("log_acc" = (log(Accuracy+1)))
  
```

```{r}
# Checking residuals for the transformed data
res2 <- aov(log_acc~Age_Group, dat_log)
```

```{r}
# Shapiro Wilk to test assumption of normality
shapiro.test(res2$residuals)

# Assumption of normality still not met, p<.05
```


```{r}
ggplot(dat_log, aes(x = Age_Group, y = log_acc, fill = Age_Group)) +
  geom_violin(trim = FALSE, alpha = .5, colour = FALSE, show.legend = FALSE) +
  geom_boxplot(width = .3, alpha = .5, show.legend = FALSE) +
  theme_bw() +
  labs(title = "ANOVA assumption checks", x = "Age Group", y = "log(Accuracy)")

# Outliers remain, and normality is still not met - therefore a Wilcoxon 
# test may be more appropriate than an ANOVA
```

```{r, warning=FALSE}
# Wilcoxon test on Overall Accuracy by Age Group
age_accuracy <- wilcox.test(Accuracy ~ Age_Group, data = dat_age_groups)
print(age_accuracy)
```

```{r}
# Computing relevant descriptives for write-up

median_preop <- dat_age_groups %>%
  ungroup() %>%
  filter(Age_Group == "Preoperational") %>%
  summarise(median(Accuracy))

median_con_op <- dat_age_groups %>%
  ungroup() %>%
  filter(Age_Group == "Concrete Operational") %>%
  summarise(median(Accuracy))
```

## How does accuracy vary by gender?

```{r}
# Making new object containing only participant id, accuracy score and gender identity

dat_gender  <- dat_age_groups %>% 
  ungroup() %>%
  select(Participant, Accuracy, Gender_ID, Age_Group) %>%
  filter(Gender_ID !="Other") %>%
  group_by(Gender_ID)

# Making new object containing all CSM scores and gender identity
dat_all_gender <-dat_all %>%
  filter(Gender_ID != "Other")
# removes n=1 participants who answered "other" for gender identity
```

```{r}
# Establishing median values for gender ids overall and within age groups

# All girls
med_girls <- dat_age_groups %>%
  ungroup() %>%
  filter(Gender_ID == "Girl") %>%
  summarise("all" = median(Accuracy))

# Girls in preoperational age group
med_preop_girls <- dat_age_groups %>%
  ungroup() %>%
  filter(Gender_ID == "Girl", Age_Group == "Preoperational") %>%
  summarise("1preop"=median(Accuracy))

# Girls in concrete operational age group
med_con_op_girls <- dat_age_groups %>%
  ungroup() %>%
  filter(Gender_ID == "Girl", Age_Group == "Concrete Operational") %>%
  summarise("2conop"= median(Accuracy))

# Putting all girl medians in one data-frame 
med_girls_all <- bind_cols(med_girls, med_preop_girls, med_con_op_girls) %>%
  mutate("Gender_id"= "girl")


# All boys
med_boys <- dat_age_groups %>%
  ungroup() %>%
  filter(Gender_ID == "Boy") %>%
  summarise("all"= median(Accuracy))

# Boys in preoperational age group
med_preop_boys <- dat_age_groups %>%
  ungroup() %>%
  filter(Gender_ID == "Boy", Age_Group == "Preoperational") %>%
  summarise("1preop" = median(Accuracy))

# Boys in concrete operational age group
med_con_op_boys <- dat_age_groups %>%
  ungroup() %>%
  filter(Gender_ID == "Boy", Age_Group == "Concrete Operational") %>%
  summarise("2conop" = median(Accuracy))

# Putting all boy medians in one data-frame
med_boys_all <- bind_cols(med_boys, med_preop_boys, med_con_op_boys) %>%
  mutate("Gender_id"= "boy")


# Putting all medians in one df
medians_all <- bind_rows(med_girls_all, med_boys_all) %>%
  pivot_longer(cols = 2:3, names_to = "age_group", values_to = "med") %>%
  group_by(age_group)

```

```{r}
ggplot(medians_all, aes(reorder(age_group), x=age_group, y=med, col=Gender_id)) + 
  geom_point() +
  geom_line(aes(group = Gender_id), show.legend = FALSE) +
  theme_bw() +
  labs(title = "Median Accuracy by Age Group and Gender", x="Age Group", y="Median Accuracy", fill="Gender Identity") +
  scale_x_discrete(labels=c("Pre-Operational", "Concrete Operational")) +
  scale_fill_discrete(name="Gender")
```



```{r}
ggplot(dat_gender, aes(x = Gender_ID, y = Accuracy, col=Age_Group)) +
  geom_point(position = "jitter") +
  #geom_crossbar(medians_all, aes())
  theme_bw() +
  labs(title = "Accuracy by Age Group and Gender")
```

```{r, warning=FALSE}
# Wilcoxon test on Overall Accuracy by Gender_id
age_accuracy <- wilcox.test(Accuracy ~ Gender_ID, data = dat_gender)
print(age_accuracy)
```
## correlations
```{r}
ggplot(dat_all, aes(x=Age_subbed, y=Accuracy, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se = FALSE, method = lm) +
  theme_bw() +
  labs(title = "Overall Accuracy by Age", x = "Age", y = "Accuracy")
```

```{r}
ggplot(dat_all, aes(Age_subbed, Identity, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se=FALSE, method = lm) +
  theme_bw() +
  labs(title = "Identity", x = "Age", y = "Identity")
```
```{r, warning=FALSE}
# Wilcoxon test on Identity by Age Group
age_identity <- wilcox.test(Identity ~ Age_Group, data = dat_age_groups)
print(age_identity)

# significant
```
```{r, warning=FALSE}
# Wilcoxon test on Identity by Gender ID
gender_identity <- wilcox.test(Identity ~ Gender_ID, data = dat_all_gender)
print(gender_identity)

# non significant
```


```{r}
ggplot(dat_all, aes(Age_subbed, Severity, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se=FALSE, method = lm) +
  theme_bw() +
  labs(title = "Severity", x= "Age", y = "Severity")
```

```{r, warning=FALSE}
# Wilcoxon test on Severity by age group
age_severity <- wilcox.test(Severity ~ Age_Group, data = dat_age_groups)
print(age_severity)

# significant
```
```{r, warning=FALSE}
# Wilcoxon test on Severity by Gender ID
gender_severity <- wilcox.test(Severity ~ Gender_ID, data = dat_all_gender)
print(gender_severity)

# non significant
```

```{r}
ggplot(dat_all, aes(Age_subbed, Symptoms, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se=FALSE, method = lm) +
  theme_bw() +
  labs(title = "Symptoms", x = "Age", y = "Symptoms")
```

```{r, warning=FALSE}
# Wilcoxon test on Symptoms by age group
age_symptoms <- wilcox.test(Symptoms ~ Age_Group, data = dat_age_groups)
print(age_symptoms)

# non significant
```
```{r, warning=FALSE}
# Wilcoxon test on Symptoms by Gender ID
gender_symptoms <- wilcox.test(Symptoms ~ Gender_ID, data = dat_all_gender)
print(gender_symptoms)

# non significant
```

```{r}
ggplot(dat_all, aes(Age_subbed, Causes, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se=FALSE, method = lm) +
  theme_bw() +
  labs(title = "Causes", x = "Age", y = "Causes")
```

```{r, warning=FALSE}
# Wilcoxon test on Causes by age group
age_causes <- wilcox.test(Causes ~ Age_Group, data = dat_age_groups)
print(age_causes)

# significant
```
```{r, warning=FALSE}
# Wilcoxon test on Causes by Gender ID
gender_causes <- wilcox.test(Causes ~ Gender_ID, data = dat_all_gender)
print(gender_causes)

# non significant
```

```{r}
ggplot(dat_all, aes(Age_subbed, Prevention, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se=FALSE, method = lm) +
  theme_bw() +
  labs(title = "Prevention", x = "Age", y = "Prevention")
```

```{r, warning=FALSE}
# Wilcoxon test on Prevention by age group
age_prevention <- wilcox.test(Prevention ~ Age_Group, data = dat_age_groups)
print(age_prevention)

# non significant
```
```{r, warning=FALSE}
# Wilcoxon test on Prevention by Gender ID
gender_prevention <- wilcox.test(Prevention ~ Gender_ID, data = dat_all_gender)
print(gender_prevention)

# non significant
```

```{r}
ggplot(dat_all, aes(Age_subbed, Timeline_1, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se=FALSE, method = lm) +
  theme_bw() +
  labs(title = "Timeline (Infection to Symptoms)", x = "Age", y = "Timeline")
```

```{r, warning=FALSE}
# Wilcoxon test on Timeline_1 by age group
age_timeline_1 <- wilcox.test(Timeline_1 ~ Age_Group, data = dat_age_groups)
print(age_timeline_1)

# non significant
```
```{r, warning=FALSE}
# Wilcoxon test on Timeline_1 by Gender ID
gender_tl1 <- wilcox.test(Timeline_1 ~ Gender_ID, data = dat_all_gender)
print(gender_tl1)

# non significant
```

```{r}
ggplot(dat_all, aes(Age_subbed, Curability, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se=FALSE, method = lm) +
  theme_bw() +
  labs(title = "Curability", x = "Age", y = "Curability")
```

```{r, warning=FALSE}
# Wilcoxon test on Curability by age group
age_curability <- wilcox.test(Curability ~ Age_Group, data = dat_age_groups)
print(age_curability)

# non significant
```
```{r, warning=FALSE}
# Wilcoxon test on Curability by Gender ID
gender_curability <- wilcox.test(Curability ~ Gender_ID, data = dat_all_gender)
print(gender_curability)

# non significant
```

```{r}
ggplot(dat_all, aes(Age_subbed, Treatment, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se=FALSE, method = lm) +
  theme_bw() +
  labs(title = "Treatment", x = "Age", y = "Treatment")
```
```{r, warning=FALSE}
# Wilcoxon test on Treatment by age group
age_treatment <- wilcox.test(Treatment ~ Age_Group, data = dat_age_groups)
print(age_treatment)

# non significant
```
```{r, warning=FALSE}
# Wilcoxon test on Treatment by Gender ID
gender_treatment <- wilcox.test(Treatment ~ Gender_ID, data = dat_all_gender)
print(gender_treatment)

# non significant
```

```{r}
ggplot(dat_all, aes(Age_subbed, Timeline_2, col= Gender_ID)) +
  geom_point(position = "jitter") +
  geom_smooth(se=FALSE, method = lm) +
  theme_bw() +
  labs(title = "Timeline (Symptoms to Recovery)", x = "Age", y = "Timeline")
```

```{r, warning=FALSE}
# Wilcoxon test on Timeline_2 by age group
age_timeline_2 <- wilcox.test(Timeline_2 ~ Age_Group, data = dat_age_groups)
print(age_timeline_2)

# non significant
```
```{r, warning=FALSE}
# Wilcoxon test on Timeline 2 by Gender ID
gender_tl2 <- wilcox.test(Timeline_2 ~ Gender_ID, data = dat_all_gender)
print(gender_tl2)

# non significant
```

```{r}
# Computing medians for significant dimensions - only causes would make a good graph unfortunately

dat_medians <- dat_age_groups %>%
  group_by(Age_Group) %>%
  summarise("med_identity"=median(Identity), "med_severity" = median(Severity), "med_causes" = median(Causes))
```
















