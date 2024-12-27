---
title: "1901 final"
author: "Sungjoo Yoon"
date: "2024-11-27"
output: pdf_document
---



## Data wrangling

```r
## load data remove unnecessary rows
all.data <- read.csv("Test Data 1125.csv") 
all.data <- all.data[-c(1, 2), ]

# combine columns E1, E2, E3, E4 into one vector
External_score <- unlist(all.data[, c("E1.", "E2", "E3", "E4")])
External_score <- as.numeric(External_score)
External_score <- External_score[!is.na(External_score)]
print(External_score)
```

```
##   [1] 5 5 5 5 5 5 4 5 5 5 5 4 4 4 5 5 3 5 5 4 5 5 5 4 5 5 4 5 5 5 5 5 3 5 4 4 5 5 4 5 3 2 3 3 4 4 4 4 4 4 5 3 4 5 4 4 2 5 3 5 3 3 5 4 4 5 1 4 4 5 4 3 3 4 2 5 3 4 5 5 4 2 5 3 4 3 2 2 1 2 2 2 2 4 5 2 3 4 3 4 2 2 1 2 5 4 1 3 3 2 2 1 2 1 2 2 4 1 2 4
## [121] 5 1 5 1 3 5 1 2 2 4 3 1 1 2 4 4 2 2 4 4 2 4 2 5 5 4 5 4 4 4 4 4 3 5 5 4 3 5 1 2 3 1 1 5 4 4 4 3
```

```r
# combine columns I1, I2, I3, I4 into one vector
Internal_score <- unlist(all.data[, c("I1", "I2", "I3", "I4")])
Internal_score <- as.numeric(Internal_score)
Internal_score <- Internal_score[!is.na(Internal_score)]
print(Internal_score)
```

```
##   [1] 5 4 5 4 4 5 4 5 5 4 5 5 4 2 4 5 5 3 3 2 5 5 5 5 4 5 5 5 5 5 5 4 4 4 4 5 5 5 5 5 4 5 4 5 4 3 4 4 5 2 5 5 4 4 5 3 4 4 5 4 4 5 3 4 4 5 5 4 5 5 3 5 5 4 5 3 4 4 4 4 4 4 3 3 2 4 5 3 5 2 2 4 2 3 3 5 2 2 5 4 5 5 3 4 2 3 2 5 4 5 4 5 5 3 5 5 3 3 4 4
## [121] 5 2 4 4 5 4 2 4 3 4 2 4 2 5 1 4 4 3 4 4 1 3 4 4 4 5 5 1 3 4 1 5 2 5 4 4 4 4 4 3 5 1 2 1 3 2 1 3
```

```r
# Combine values from columns Q5, Q6, Q7, Q8, Q9, Q10, Q5.1, Q6.1, Q7.1, Q8.1, Q9.1, Q10.1 into one vector
Filler_score <- unlist(all.data[, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q5.1", "Q6.1", "Q7.1", "Q8.1", "Q9.1", "Q10.1")])
Filler_score <- as.numeric(Filler_score)
Filler_score <- Filler_score[!is.na(Filler_score)]
print(Filler_score)
```

```
##   [1] 5 3 5 5 4 5 5 4 5 5 4 5 5 5 4 5 3 5 5 4 5 3 4 5 4 5 5 5 5 5 5 4 3 5 4 4 5 4 4 4 5 4 5 4 2 4 4 3 3 3 3 4 3 2 4 3 5 3 2 3 5 4 4 1 1 4 4 4 4 4 4 5 4 3 3 3 3 3 3 1 3 4 2 3 3 5 3 5 5 4 4 5 5 4 4 5 5 5 5 5 5 4 5 5 5 4 5 5 5 5 5 3 3 5 5 4 5 3 5 5
## [121] 3 5 5 5 4 5 4 4 5 1 1 1 3 3 2 2 1 2 2 1 3 2 1 1 2 3 2 2 2 1 3 3 1 2 4 2 2 2 2 3 3 2 5 2 3 1 2 1 1 1 2 1 4 5 5 3 5 4 3 4 4 5 2 4 5 2 4 2 4 5 4 4 4 3 4 5 5 3 5 5 3 5 5 4 4 1 4 4 1 4 5 4 5 2 5 4 4 5 4 4 3 2 4 2 4 4 3 4 4 2 2 4 4 4 4 3 1 4 5 4
## [241] 5 4 5 4 5 4 4 3 5 3 3 5 4 5 4 2 4 5 4 5 4 5 4 5 4 4 5 4 5 5 5 5 5 3 5 4 5 4 5 5 5 2 4 5 4 5 3 4 4 5 5 5 4 4 5 5 1 4 4 3 3 3 4 4 3 4 3 4 5 3 4 4 2 4 3 4 2 4 4 4 4 4 4 4 3 4 4 5 4 4 4 3 4 3 4 5 5 5 5 4 4 4 3 4 5 5 5 5 5 5 5 5 4 5 3 5 5 5 5 4
## [361] 3 4 4 5 4 5 5 3 5 4 4 5 5 5 4 5 5 5 2 5 4 1 3 2 3 1 2 2 2 2 1 1 1 2 1 2 1 1 2 1 4 3 1 2 2 2 1 4 3 3 3 2 1 2 2 1 1 5 2 5 1 1 5 5 4 3 4 5 3 5 5 4 4 4 4 4 4 4 5 2 4 2 4 4 4 5 4 5 4 4 3 4 5 4 5 5 3 3 5 4 5 2 5 2 5 4 4 2 5 2 4 5 4 2 4 4 1 3 4 5
## [481] 4 3 5 4 2 5 2 4 4 4 4 4 4 2 2 4 4 2 3 5 4 4 2 5
```

```r
# drop incomplete
all.data <- all.data[!(is.na(all.data$I4) & is.na(all.data$E4) | 
                       (all.data$I4 == "" & all.data$E4 == "")), ]
row.names(all.data) <- NULL

# mutate for form version
all.data <- all.data |>
  mutate(form_version = case_when(
    !is.na(I4) & I4 != "" ~ "A", # Assign "A" if I4 has a response
    !is.na(E4) & E4 != "" ~ "B"  # Assign "B" if E4 has a response
  ))


# create vars for scores based on question regardless of framing
all.data <- all.data |>
  mutate(
    v1 = ifelse(!is.na(`E1.`) & `E1.` != "", `E1.`, I1)
  )

all.data <- all.data |>
  mutate(
    v2 = ifelse(!is.na(E2) & E2 != "", E2, I2)
  )

all.data <- all.data |>
  mutate(
    v3 = ifelse(!is.na(E3) & E3 != "", E3, I3)
  )

all.data <- all.data |>
  mutate(
    v4 = ifelse(!is.na(E4) & E4 != "", E4, I4)
  )

all.data <- all.data |>
  mutate(across(c(v1, v2, v3, v4), ~ as.numeric(as.character(.))))
```

## Demographic data

```r
# Pulling counts, and countsd by demographic
non_empty_I4 <- sum(!is.na(all.data$I4) & all.data$I4 != "")
non_empty_E4 <- sum(!is.na(all.data$E4) & all.data$E4 != "")
cat("Number of non-null/non-empty values in I4:", non_empty_I4, "\n")
```

```
## Number of non-null/non-empty values in I4: 43
```

```r
cat("Number of non-null/non-empty values in E4:", non_empty_E4, "\n")
```

```
## Number of non-null/non-empty values in E4: 41
```

```r
group1stats <- all.data |>
  group_by(Q27) |>
  summarize(Count = n(), .groups = 'drop') |>
  mutate(Grade = case_when(
    Q27 == 1 ~ "Sophomore",
    Q27 == 2 ~ "Junior",
    Q27 == 3 ~ "Senior",
    TRUE ~ "Other"
  ))

group1stats <- group1stats |>
  select(Grade, Count)

all.data <- all.data |>
  mutate(gender = case_when(
    Q35 == 1 ~ "Male",
    Q35 == 2 ~ "Female",
    Q35 == 3 ~ "Nonbinary",
    Q35 == 4 ~ "Prefer not to answer",
    TRUE ~ "Unknown"  # Handles unexpected or missing values
  ))

all.data <- all.data |>
  mutate(Grade = case_when(
    Q27 == 1 ~ "Sophomore",
    Q27 == 2 ~ "Junior",
    Q27 == 3 ~ "Senior",
    TRUE ~ "Unknown"
  ))
```


```r
# demographics by form version
gender_stats_by_form <- all.data |>
  group_by(form_version, Q35) |>
  summarize(
    Count = n(),                            
    .groups = 'drop'                          
  ) |>
  group_by(form_version) |>
  mutate(
    Proportion = Count / sum(Count) * 100,    
    Gender = case_when(                       
      Q35 == 1 ~ "Male",
      Q35 == 2 ~ "Female",
      Q35 == 3 ~ "Non-binary/Third Gender",
      Q35 == 4 ~ "Prefer not to say",
      TRUE ~ "Unknown"
    )
  ) |>
  select(form_version, Gender, Count, Proportion) 
```

## Power analysis

```r
# "a priori" power calculation
pwr.r.test(r = .2, power = .80, sig.level = .05, alternative = "greater")
```

```
## 
##      approximate correlation power calculation (arctangh transformation) 
## 
##               n = 152.416
##               r = 0.2
##       sig.level = 0.05
##           power = 0.8
##     alternative = greater
```

```r
# "observed"/post hoc power calculation for correlation
pwr.r.test(n = dim(all.data)[1], r = 0.3, sig.level = .05, alternative = "greater") 
```

```
## 
##      approximate correlation power calculation (arctangh transformation) 
## 
##               n = 84
##               r = 0.3
##       sig.level = 0.05
##           power = 0.8764926
##     alternative = greater
```

## t-tests

```r
# pooled data on global level
t_test_result1 <- t.test(Internal_score, External_score, var.equal = TRUE)
print(t_test_result1)
```

```
## 
## 	Two Sample t-test
## 
## data:  Internal_score and External_score
## t = 2.3082, df = 334, p-value = 0.0216
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.04573728 0.57331034
## sample estimates:
## mean of x mean of y 
##  3.869048  3.559524
```

```r
t_test_result2 <- t.test(External_score, Filler_score)
print(t_test_result2)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  External_score and Filler_score
## t = -1.0807, df = 272.96, p-value = 0.2808
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.3527188  0.1027188
## sample estimates:
## mean of x mean of y 
##  3.559524  3.684524
```

```r
t_test_result3 <- t.test(Internal_score, Filler_score)
print(t_test_result3)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Internal_score and Filler_score
## t = 1.7812, df = 310.68, p-value = 0.07585
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.01931152  0.38835914
## sample estimates:
## mean of x mean of y 
##  3.869048  3.684524
```

```r
# effect sizes
External_score <- na.omit(External_score)
Internal_score <- na.omit(Internal_score)
External_score <- as.numeric(External_score)
Internal_score <- as.numeric(Internal_score)
d <- cohensD(External_score, Internal_score)
print(d)
```

```
## [1] 0.251841
```


```r
# Levenes for t-test type
library(car)  

levene_test_result1 <- leveneTest(v1 ~ form_version, data = all.data)
```

```
## Warning in leveneTest.default(y = y, group = group, ...): group coerced to factor.
```

```r
levene_test_result2 <- leveneTest(v2 ~ form_version, data = all.data)
```

```
## Warning in leveneTest.default(y = y, group = group, ...): group coerced to factor.
```

```r
levene_test_result3 <- leveneTest(v3 ~ form_version, data = all.data)
```

```
## Warning in leveneTest.default(y = y, group = group, ...): group coerced to factor.
```

```r
levene_test_result4 <- leveneTest(v4 ~ form_version, data = all.data)
```

```
## Warning in leveneTest.default(y = y, group = group, ...): group coerced to factor.
```

```r
print(levene_test_result1)
```

```
## Levene's Test for Homogeneity of Variance (center = median)
##       Df F value Pr(>F)
## group  1  0.1784 0.6738
##       82
```

```r
print(levene_test_result2)
```

```
## Levene's Test for Homogeneity of Variance (center = median)
##       Df F value Pr(>F)
## group  1  1.1574 0.2852
##       82
```

```r
print(levene_test_result3)
```

```
## Levene's Test for Homogeneity of Variance (center = median)
##       Df F value Pr(>F)
## group  1  0.0631 0.8023
##       82
```

```r
print(levene_test_result4)
```

```
## Levene's Test for Homogeneity of Variance (center = median)
##       Df F value Pr(>F)
## group  1   0.008 0.9289
##       82
```

## t test for local level data

```r
external_vector1 <- all.data$E1.[!is.na(all.data$E1.)] |> as.numeric()
external_vector2 <- all.data$E2[!is.na(all.data$E2)] |> as.numeric()
external_vector3 <- all.data$E3[!is.na(all.data$E3)] |> as.numeric()
external_vector4 <- all.data$E4[!is.na(all.data$E4)] |> as.numeric()

internal_vector1 <- all.data$I1[!is.na(all.data$I1)] |> as.numeric()
internal_vector2 <- all.data$I2[!is.na(all.data$I2)] |> as.numeric()
internal_vector3 <- all.data$I3[!is.na(all.data$I3)] |> as.numeric()
internal_vector4 <- all.data$I4[!is.na(all.data$I4)] |> as.numeric()

t_test_result1 <- t.test(internal_vector1, external_vector1, var.equal = TRUE)
t_test_result2 <- t.test(internal_vector2, external_vector2, var.equal = TRUE)
t_test_result3 <- t.test(internal_vector3, external_vector3, var.equal = TRUE)
t_test_result4 <- t.test(internal_vector4, external_vector4, var.equal = TRUE)

d1 <- cohensD(external_vector1, internal_vector1)
d2 <- cohensD(external_vector2, internal_vector2)
d3 <- cohensD(external_vector3, internal_vector3)
d4 <- cohensD(external_vector4, internal_vector4)

print(t_test_result1)
```

```
## 
## 	Two Sample t-test
## 
## data:  internal_vector1 and external_vector1
## t = -0.42242, df = 82, p-value = 0.6738
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.4145209  0.2693138
## sample estimates:
## mean of x mean of y 
##  4.439024  4.511628
```

```r
print(t_test_result2)
```

```
## 
## 	Two Sample t-test
## 
## data:  internal_vector2 and external_vector2
## t = 1.5969, df = 82, p-value = 0.1141
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.07624234  0.69677552
## sample estimates:
## mean of x mean of y 
##  4.139535  3.829268
```

```r
print(t_test_result3)
```

```
## 
## 	Two Sample t-test
## 
## data:  internal_vector3 and external_vector3
## t = 4.0629, df = 82, p-value = 0.0001104
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.5627653 1.6425665
## sample estimates:
## mean of x mean of y 
##  3.707317  2.604651
```

```r
print(t_test_result4)
```

```
## 
## 	Two Sample t-test
## 
## data:  internal_vector4 and external_vector4
## t = -0.29068, df = 82, p-value = 0.772
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.6540030  0.4872418
## sample estimates:
## mean of x mean of y 
##  3.209302  3.292683
```

```r
print(d1)
```

```
## [1] 0.09220497
```

```r
print(d2)
```

```
## [1] 0.3485726
```

```r
print(d3)
```

```
## [1] 0.8868455
```

```r
print(d4)
```

```
## [1] 0.06345036
```

```r
sd(internal_vector1, na.rm = TRUE)
```

```
## [1] 0.807737
```

```r
sd(external_vector1, na.rm = TRUE)
```

```
## [1] 0.7675593
```

```r
sd(internal_vector2, na.rm = TRUE)
```

```
## [1] 0.7740246
```

```r
sd(external_vector2, na.rm = TRUE)
```

```
## [1] 0.997558
```

```r
sd(internal_vector3, na.rm = TRUE)
```

```
## [1] 1.167131
```

```r
sd(external_vector3, na.rm = TRUE)
```

```
## [1] 1.311842
```

```r
sd(internal_vector4, na.rm = TRUE)
```

```
## [1] 1.319418
```

```r
sd(external_vector4, na.rm = TRUE)
```

```
## [1] 1.308509
```


```r
# one more t-test for pooled absent manipulation 3
# combine columns E1, E2, E4 into new vector
External_score_absent3 <- unlist(all.data[, c("E1.", "E2", "E4")])
External_score_absent3 <- as.numeric(External_score_absent3)
External_score_absent3 <- External_score_absent3[!is.na(External_score_absent3)]

# combine columns I1, I2, I4 into new vector
Internal_score_absent3 <- unlist(all.data[, c("I1", "I2", "I4")])
Internal_score_absent3 <- as.numeric(Internal_score_absent3)
Internal_score_absent3 <- Internal_score_absent3[!is.na(Internal_score_absent3)]

t_test_result_absent3 <- t.test(Internal_score_absent3, External_score_absent3, var.equal = TRUE)
print(t_test_result_absent3)
```

```
## 
## 	Two Sample t-test
## 
## data:  Internal_score_absent3 and External_score_absent3
## t = 0.23201, df = 250, p-value = 0.8167
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.2490742  0.3155939
## sample estimates:
## mean of x mean of y 
##   3.92126   3.88800
```



```r
# pull stats
t_test_result1 <- t.test(External_score, Internal_score)

t_table <- tibble::tibble(
  Statistic = round(t_test_result1$statistic, 2),
  `Degrees of Freedom` = round(t_test_result1$parameter, 2),
  `p-value` = ifelse(t_test_result1$p.value < 0.001, "< 0.001", round(t_test_result1$p.value, 3)),
  `Mean Difference` = round(t_test_result1$estimate[1] - t_test_result1$estimate[2], 2),
  `95% CI Lower` = round(t_test_result1$conf.int[1], 2),
  `95% CI Upper` = round(t_test_result1$conf.int[2], 2)
)

library(gt)
formatted_table <- t_table |>
  gt() |>
  tab_header(
    title = "Self-Reported Satisfaction by Perceived Agency",
    subtitle = "Comparing Valence Scores between Internal and External Framing"
  ) |>
  fmt_number(columns = c(Statistic, `Mean Difference`), decimals = 2) |>  # Corrected column name
  tab_options(
    table.font.size = "small",
    heading.align = "center"
  )

formatted_table
```

<!--html_preserve--><div id="aayibihyhf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#aayibihyhf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#aayibihyhf thead, #aayibihyhf tbody, #aayibihyhf tfoot, #aayibihyhf tr, #aayibihyhf td, #aayibihyhf th {
  border-style: none;
}

#aayibihyhf p {
  margin: 0;
  padding: 0;
}

#aayibihyhf .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: small;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#aayibihyhf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#aayibihyhf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#aayibihyhf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#aayibihyhf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#aayibihyhf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#aayibihyhf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#aayibihyhf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#aayibihyhf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#aayibihyhf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#aayibihyhf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#aayibihyhf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#aayibihyhf .gt_spanner_row {
  border-bottom-style: hidden;
}

#aayibihyhf .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#aayibihyhf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#aayibihyhf .gt_from_md > :first-child {
  margin-top: 0;
}

#aayibihyhf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#aayibihyhf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#aayibihyhf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#aayibihyhf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#aayibihyhf .gt_row_group_first td {
  border-top-width: 2px;
}

#aayibihyhf .gt_row_group_first th {
  border-top-width: 2px;
}

#aayibihyhf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#aayibihyhf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#aayibihyhf .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#aayibihyhf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#aayibihyhf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#aayibihyhf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#aayibihyhf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#aayibihyhf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#aayibihyhf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#aayibihyhf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#aayibihyhf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#aayibihyhf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#aayibihyhf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#aayibihyhf .gt_left {
  text-align: left;
}

#aayibihyhf .gt_center {
  text-align: center;
}

#aayibihyhf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#aayibihyhf .gt_font_normal {
  font-weight: normal;
}

#aayibihyhf .gt_font_bold {
  font-weight: bold;
}

#aayibihyhf .gt_font_italic {
  font-style: italic;
}

#aayibihyhf .gt_super {
  font-size: 65%;
}

#aayibihyhf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#aayibihyhf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#aayibihyhf .gt_indent_1 {
  text-indent: 5px;
}

#aayibihyhf .gt_indent_2 {
  text-indent: 10px;
}

#aayibihyhf .gt_indent_3 {
  text-indent: 15px;
}

#aayibihyhf .gt_indent_4 {
  text-indent: 20px;
}

#aayibihyhf .gt_indent_5 {
  text-indent: 25px;
}

#aayibihyhf .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#aayibihyhf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal" style>Self-Reported Satisfaction by Perceived Agency</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Comparing Valence Scores between Internal and External Framing</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Statistic">Statistic</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Degrees-of-Freedom">Degrees of Freedom</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p-value">p-value</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Mean-Difference">Mean Difference</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a95%-CI-Lower">95% CI Lower</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a95%-CI-Upper">95% CI Upper</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Statistic" class="gt_row gt_right">−2.31</td>
<td headers="Degrees of Freedom" class="gt_row gt_right">326.91</td>
<td headers="p-value" class="gt_row gt_right">0.022</td>
<td headers="Mean Difference" class="gt_row gt_right">−0.31</td>
<td headers="95% CI Lower" class="gt_row gt_right">-0.57</td>
<td headers="95% CI Upper" class="gt_row gt_right">-0.05</td></tr>
  </tbody>
  
  
</table>
</div><!--/html_preserve-->


```r
# subgroup analysis counts+anovas to check for demographic level disparities
all.data.aov <- all.data |>
  filter(Q27 != 3)

subgroup_analysis_gender <- all.data.aov |>
  group_by(gender) |> 
  summarise(
    mean_satisfaction_1 = mean(v1, na.rm = TRUE),
    mean_satisfaction_2 = mean(v2, na.rm = TRUE),
    mean_satisfaction_3 = mean(v3, na.rm = TRUE),
    mean_satisfaction_4 = mean(v4, na.rm = TRUE),
    sd_satisfaction_1 = sd(v1, na.rm = TRUE),
    sd_satisfaction_2 = sd(v2, na.rm = TRUE),
    sd_satisfaction_3 = sd(v3, na.rm = TRUE),
    sd_satisfaction_4 = sd(v4, na.rm = TRUE),
    n = n()  
  )

summary(aov(v1 ~ gender, data = all.data.aov))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)  
## gender       1  2.038  2.0380   3.335 0.0737 .
## Residuals   51 31.170  0.6112                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(v2 ~ gender, data = all.data.aov))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## gender       1   0.98  0.9768   1.083  0.303
## Residuals   51  46.00  0.9020
```

```r
summary(aov(v3 ~ gender, data = all.data.aov))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## gender       1   1.51   1.510   1.078  0.304
## Residuals   51  71.47   1.401
```

```r
summary(aov(v4 ~ gender, data = all.data.aov))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## gender       1   0.07  0.0657   0.044  0.835
## Residuals   51  76.24  1.4948
```

```r
subgroup_analysis_class <- all.data |>
  group_by(Grade) |>  
  summarise(
    mean_satisfaction_1 = mean(v1, na.rm = TRUE),
    mean_satisfaction_2 = mean(v2, na.rm = TRUE),
    mean_satisfaction_3 = mean(v3, na.rm = TRUE),
    mean_satisfaction_4 = mean(v4, na.rm = TRUE),
    sd_satisfaction_1 = sd(v1, na.rm = TRUE),
    sd_satisfaction_2 = sd(v2, na.rm = TRUE),
    sd_satisfaction_3 = sd(v3, na.rm = TRUE),
    sd_satisfaction_4 = sd(v4, na.rm = TRUE),
    n = n()  
  )

summary(aov(v1 ~ Grade, data = all.data))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## Grade        2   0.00  0.0018   0.003  0.997
## Residuals   81  50.95  0.6290
```

```r
summary(aov(v2 ~ Grade, data = all.data))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## Grade        2   0.29  0.1449   0.176  0.839
## Residuals   81  66.70  0.8234
```

```r
summary(aov(v3 ~ Grade, data = all.data))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## Grade        2   3.77   1.884   1.028  0.362
## Residuals   81 148.52   1.833
```

```r
summary(aov(v4 ~ Grade, data = all.data))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## Grade        2   1.02  0.5106   0.294  0.746
## Residuals   81 140.73  1.7374
```

## Visualizations


```r
# mutate to categorize by internal or external response to given q
all.data$ei1 <- ifelse(!is.na(all.data$E1.) & all.data$E1. != "", "external", "internal")
all.data$ei2 <- ifelse(!is.na(all.data$E2) & all.data$E2 != "", "external", "internal")
all.data$ei3 <- ifelse(!is.na(all.data$E3) & all.data$E3 != "", "external", "internal")
all.data$ei4 <- ifelse(!is.na(all.data$E4) & all.data$E4 != "", "external", "internal")

int_vector_1 <- all.data |>
  filter(ei1 == "internal") |>
  pull(v1)

ext_vector_1 <- all.data |>
  filter(ei1 == "external") |>
  pull(v1)

int_vector_2 <- all.data |>
  filter(ei2 == "internal") |>
  pull(v2)

ext_vector_2 <- all.data |>
  filter(ei2 == "external") |>
  pull(v2)

int_vector_3 <- all.data |>
  filter(ei3 == "internal") |>
  pull(v3)

ext_vector_3 <- all.data |>
  filter(ei3 == "external") |>
  pull(v3)

int_vector_4 <- all.data |>
  filter(ei4 == "internal") |>
  pull(v4)

ext_vector_4 <- all.data |>
  filter(ei4 == "external") |>
  pull(v4)

library(ggplot2)
library(patchwork)

plot_data <- list(
  data.frame(Type = "External", Value = int_vector_1, Group = "Group 1"), # Corrected Type
  data.frame(Type = "Internal", Value = ext_vector_1, Group = "Group 1"), # Corrected Type
  data.frame(Type = "External", Value = int_vector_2, Group = "Group 2"), # Corrected Type
  data.frame(Type = "Internal", Value = ext_vector_2, Group = "Group 2"), # Corrected Type
  data.frame(Type = "External", Value = int_vector_3, Group = "Group 3"), # Corrected Type
  data.frame(Type = "Internal", Value = ext_vector_3, Group = "Group 3"), # Corrected Type
  data.frame(Type = "External", Value = int_vector_4, Group = "Group 4"), # Corrected Type
  data.frame(Type = "Internal", Value = ext_vector_4, Group = "Group 4")  # Corrected Type
)

combined_data <- do.call(rbind, plot_data)
value_limits <- range(combined_data$Value, na.rm = TRUE)
combined_data$Type <- factor(combined_data$Type, levels = c("Internal", "External"))

# box whisker plot discussion viz

create_plot <- function(group_name, new_label) {
  ggplot(combined_data[combined_data$Group == group_name, ], aes(x = Type, y = Value, fill = Type)) +
    geom_boxplot() +
    labs(title = new_label, x = NULL, y = "Satisfaction Score") +
    scale_fill_manual(values = c("Internal" = "skyblue", "External" = "orange")) +
    coord_cartesian(ylim = value_limits) + # Consistent y-axis range
    theme_minimal() +
    theme(legend.position = "none")
}

plot1 <- create_plot("Group 1", "Manipulation 1")
plot2 <- create_plot("Group 2", "Manipulation 2")
plot3 <- create_plot("Group 3", "Manipulation 3")
plot4 <- create_plot("Group 4", "Manipulation 4")

library(patchwork)
final_plot <- (plot1 + plot2 + plot3 + plot4) +
  plot_layout(ncol = 4) +
  plot_annotation(title = "Box-and-Whisker Plots, Satisfaction Score by Framing Type and Manipulation", theme = theme(plot.title = element_text(hjust = 0.5)))

print(final_plot)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
ggsave("box00.png", final_plot)
```

```
## Saving 7 x 7 in image
```


```r
# scatteplot discussion viz
value_limits <- range(combined_data$Value, na.rm = TRUE) + c(-0.5, 0.5)

create_scatterplot <- function(data_subset, manipulation_label) {
  ggplot(data_subset, aes(x = Group, y = Value, color = Type)) +
    geom_jitter(width = 0.2, size = 2, alpha = 0.7) + 
    labs(x = manipulation_label, y = "Sat. Score", color = "Framing Type") + 
    scale_color_manual(values = c("Internal" = "skyblue", "External" = "orange")) +
    coord_cartesian(ylim = value_limits) + 
    theme_minimal() +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          legend.position = "side") 
}

scatterplots <- list(
  create_scatterplot(subset(combined_data, Group == "Group 1" & Type == "Internal"), "Manipulation 1"),
  create_scatterplot(subset(combined_data, Group == "Group 1" & Type == "External"), "Manipulation 1"),
  create_scatterplot(subset(combined_data, Group == "Group 2" & Type == "Internal"), "Manipulation 2"),
  create_scatterplot(subset(combined_data, Group == "Group 2" & Type == "External"), "Manipulation 2"),
  create_scatterplot(subset(combined_data, Group == "Group 3" & Type == "Internal"), "Manipulation 3"),
  create_scatterplot(subset(combined_data, Group == "Group 3" & Type == "External"), "Manipulation 3"),
  create_scatterplot(subset(combined_data, Group == "Group 4" & Type == "Internal"), "Manipulation 4"),
  create_scatterplot(subset(combined_data, Group == "Group 4" & Type == "External"), "Manipulation 4")
)

scatter_plot_grid <- wrap_plots(scatterplots, ncol = 4) +
  plot_annotation(
    title = "Scatterplots for Satisfaction Scores by Framing Type and Manipulation",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14)) 
  )

print(scatter_plot_grid)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

```r
ggsave("scatterplots_grid.png", scatter_plot_grid, width = 10, height = 8, units = "in", dpi = 800)
```

## pages integration/index.html generator script


```r
knit("index.Rmd", output = "index.md")
```

```
## 
## 
## processing file: index.Rmd
```

```
## Error in parse_block(g[-1], g[1], params.src, markdown_mode): Duplicate chunk label 'setup', which has been used for the chunk:
## knitr::opts_chunk$set(echo = TRUE)
## if (!require(car)) {install.packages("car"); require(car)}
## if (!require(pwr)) {install.packages("pwr"); require(pwr)}
## if (!require(effsize)) {install.packages("effsize"); require(effsize)}
## if (!require(lsr)) {install.packages("lsr"); require(lsr)}
## if (!require(corrplot)) {install.packages("corrplot"); require(corrplot)}
## if (!require(lm.beta)) {install.packages("lm.beta"); require(lm.beta)}
## if (!require(lme4)) {install.packages("lme4"); require(lme4)}
## if (!require(effects)) {install.packages("effects"); require(effects)}
## if (!require(psych)) {install.packages("psych"); require(psych)}
## if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
## if (!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
## if (!require(knitr)) {install.packages("knitr"); require(knitr)}
## if (!require(gt)) {install.packages("gt"); require(gt)}
## if (!require(markdown)) {install.packages("markdown"); require(markdown)}
```

```r
markdownToHTML("index.md", "index.html")
```

