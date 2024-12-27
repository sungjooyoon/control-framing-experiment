---
title: "Control and Valence, draft"
author: "Sungjoo Yoon — Harvard University Dept. of Psychology"
date: "2024-11-27"
output:
  distill::distill_article:
---

# Abstract
Does perceived agency in the way a question is framed have an effect on the emotional valence of its response? This study examines the relationship between control-based framing and self-reported satisfaction. Building on existing literature regarding framing effects and loci of control, we conduct an experiment (n = 84) testing how emotional evaluations differ based on whether questions are framed to emphasize an internal or external loci of control. We find that the pooled data, corrected for multiple comparisons (α = .0125), does not reveal a significant effect in the relationship (p = .02). Granular analysis of each manipulated question pair reveals further inconsistency, with only one of four framing manipulations producing significant findings in the hypothesized direction. Resultantly, we hold that there is insufficient evidence to confirm our hypothesis. We also identify potential confounding factors/limitations on our methodology, so that future research can better understand the nuances of this complex relationship.

# Setup
Package reqs: 'car', 'pwr', 'effsize', 'lsr', 'corrplot', 'lm.beta', 'lme4', 'effects', 'psych', 'ggplot2', 'tidyverse', 'knitr', 'gt', 'markdown'

```r
knitr::opts_chunk$set(echo = TRUE)
if (!require(car)) {install.packages("car"); require(car)}
if (!require(pwr)) {install.packages("pwr"); require(pwr)}
if (!require(effsize)) {install.packages("effsize"); require(effsize)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)}
if (!require(corrplot)) {install.packages("corrplot"); require(corrplot)}
if (!require(lm.beta)) {install.packages("lm.beta"); require(lm.beta)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(effects)) {install.packages("effects"); require(effects)}
if (!require(psych)) {install.packages("psych"); require(psych)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if (!require(knitr)) {install.packages("knitr"); require(knitr)}
if (!require(gt)) {install.packages("gt"); require(gt)}
if (!require(markdown)) {install.packages("markdown"); require(markdown)}
```

# Introduction
Limited research exists analyzing the relationship between control-based framing and its effect on self-reported valence. In other words, there is a gap in understanding how presenting information as stemming from internal or external influences shapes individuals' emotional evaluations and judgments of positivity or negativity toward a given subject. This paper attempts to bridge that gap, by conducting an experiment to isolate said relationship between perceived agency and valence. It does so as the implications of these conclusions extend beyond academic exercise—it influences how we think about the role of agency in everything from healthcare interventions to economic decision-making. Simply put, these conclusions can be strategically utilized, as influencing perceptions and emotional responses is critical to driving desired outcomes in various fields; from design of public policy to private savings.

# Literature review
Existing research in the literature has long investigated the effect of framing on respondents' perceptions, decision-making processes, and subsequent behaviors. For example, studies have focused on the effect of gain and loss framing to measure individuals’ differential risk tolerance (Tversky & Kahneman, 1981; Li & Xie, 2006). Other studies have investigated the effect of valence in framing on valence in responses, such as questions loaded with framing related to perceived security or stress (Bloem, 2024; Guyatt et al., 1999). Research has even investigated and established how individuals allocate material resources and make decisions in response to the type of control they are primed with (Razak, 2019). This framing-related research has been employed to develop interventions and strategies aimed at influencing behavior in practical applications, and our research is geared to continue this trend and further this domain.

Similarly, the literature has also long investigated the importance of the locus of control—that is, the degree to which individuals believe that they, and not external forces, can exert influence over outcomes in their lives. Said domain of research is relevant as it sheds light on how perceptions of agency influence emotional responses, behavior, and overall psychological well-being in various contexts. For example and most notably, research has held that an internal locus of control is related to greater levels of well-being and mental health (Kesavayuth et al., 2022; Eren et al., 2023; Jain & Singh, 2015). Relatedly, research on the relationship between locus of control and health outcomes has found a strong association between perceived control and positive outcomes, mechanized and mediated by self-control (Botha and Dahmann, 2023). Studies on the locus of control have even investigated how it affects employee performance, finding that it mediates the relationship between expectations of leadership and creativity in one’s role (Xu et al., 2022). And in the domain of consumer preferences, perceived control has even been found to affect purchasing patterns for services such as supplementary health insurance (Bonsang & Costa-Font, 2022).

Given the wide practical application of both domains—framing effects and the locus of control—understanding how they intersect is critical in advancing both fields. In turn, prescriptions related to their practical application become refined, given insights from both areas enhance engagement with the other. Yet, the literature has not yet meaningfully expanded to research said intersection; this paper aims to address this gap. In light of the discussed literature, then, we broadly hypothesize there to be a causal relationship between the framing of a question with an internal locus of control and positive responsive valence, and framing of an external locus and negative valence. Inferentially, this pattern would make sense, as prior research shows individuals tend to associate internal loci of control with positive emotional states and personal empowerment. Conversely, external loci are often related to a sense of constraint, and thus negative emotional states—this conceptual hypothesis builds on the patterns of existing foundational research.

# Materials
To test this, we propose an experimental design where participants are randomly assigned to one of two questionnaires, which ask them questions regarding their lived experiences and living situations. Specifically, however, the questionnaires are structured such that the underlying content remains the same, with the manipulation being on the level of internal and external loci of control. It does so by framing questions about the same subject in ways that emphasize either personal agency or external influences, and requesting responses about self-reported satisfaction. In this way, it broadly isolates the relationship between perceived agency (in the way a question is asked) and emotional valence (in the way a question is answered).

Data wrangling:

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

Participants and demographic data:

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

More granularly, we recruit non-freshman undergraduates at Harvard College (n = 84), who reside within one of the college’s twelve residential houses. We selected this group primarily for two reasons: convenience and shared identity. First, this population is easily accessible to us, making recruitment and participation logistically feasible. Second, the shared living arrangements within the residential house system provide a common context among participants. This shared context allows us to control for variability in responses that might arise from differing environments and subject matter within questions, enabling us to focus specifically on the effect of control-based framing. However, it is important to note that this limits the generalizability of our study, and further research among diverse demographics is both needed and welcomed (for further details, confer our later discussion on the limitations of this experiment).

In regards to our recruitment process, participants were recruited primarily through two methods. First, we advertised our study at all twelve residential houses, via postering at entrances and designated message boards. Second, we utilized house listservs, which sent our questionnaire to undergraduates registered to receive emails from said mailing lists. Participants from all houses were included in our results. Participants were not compensated on the individual level; however, as to incentivize participation, we advertised that one randomly-selected participant would receive a $100 gift card. 

With respect to the demographic information of our sample, we report two key metrics: class year and gender. We do so to ensure that demographic factors such as age and gender differences were not driving statistical differences, and to provide transparency regarding our sample’s composition and representativeness (for further details, confer the results of our subgroup analysis). Of those surveyed, approximately 38.1% (n = 31) of our participants were sophomores, 25% were juniors (n = 21), and 36.9% were seniors (n = 31). Further, approximately 38% identified as male (n = 34), 59% identified as female (n = 53), 2% identified as non-binary (n = 2), and 1% preferred not to disclose (n = 1). We did not delete any complete responses (n = 84), and all consenting respondents were included in our results. 

A priori power analysis:

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

Procedurally, each participant completed the study in three stages: consent, questionnaire completion, and debriefing. To begin, participants were provided a consent agreement. Upon arriving at the study website hosted on Qualtrics, participants were informed of the voluntary nature of the experiment, and their ability to decline participation at any time. Participants were then informed of the purpose of the study in broad terms, without revealing the specific hypothesis about control-based framing. Finally, they were informed of the study’s format and their associated rights (time required, risks associated, benefits, compensation, confidentiality, and contact information for further questions). Only participants who provided electronic consent were allowed to proceed.

Eligible participants then proceeded to the questionnaire segment, starting with the collection of demographic information (details regarding their residential house, class year, and gender). Subsequently, participants were then randomly assigned to be shown one of the two versions of the questionnaire, and informed of how to answer the questionnaire (e.g. “For these next questions, please move the slider based on your satisfaction”). Both questionnaires consisted of ten items, with six of these items as filler questions intended to mask the purpose of the study, and four items manipulated for the purpose of the experiment. In both versions of the questionnaire, the content of the four manipulated items was identical, but the framing differed systematically. The first version presented three items framed with an internal locus of control, and the other three with an external locus of control. The second version reversed these framings along each item, ensuring that all respondents were exposed to the same content, with the only variation being the perspective from which the questions were presented. The manipulated items were counterbalanced across the two questionnaires such that the participants responded to internal-framed and external-framed items in alternating order, with half of the participants starting with internally-framed items and the other half with externally-framed items—all to minimize potential order effects.

Upon completion of the questionnaire segment, participants were directed to a debriefing segment. In the debriefing, participants were provided with a detailed explanation of the study’s nature and design. To begin, participants were informed that the study’s purpose was to investigate the effect of control-based framing on self-reported satisfaction. Subsequently, participants were informed of the methods of the study, and how the experimental manipulations were conducted. Specifically, participants were notified of how four questions were manipulated with either an internal or external locus of control, and how comparing differences in responses within that data would inform us of how control-based framing influences perceived satisfaction. Participants were also informed of our hypothesis and expectation that questions framed with higher control would result in higher satisfaction.

Afterwards, participants were informed of the practical implications of the research. They were informed on how understanding this relationship would implicate engagement, satisfaction, and perceptions across various fields; ranging from education to healthcare and marketing. Finally, participants were provided additional resources; these resources consisted of relevant supplementary literature for further research, and contact information for any withstanding questions and complaints. Participants were also notified of where to contact for a research summary upon the conclusion of the study. Participants were thanked for their time and contribution, and taken to an external link to register for the compensatory raffle.

# Analysis and results

We test for the existence of a significant relationship by comparing mean differences across the responses to internal and external framing. Specifically, we analyze the data by employing a two-sample t-test approach, assuming equal variances across samples. We apply this analysis at the level of each individual manipulation (i.e. comparing mean satisfaction scores between internal and external framing on a given subject), and at the level of the pooled data. Additionally, we assume equal variance after running Levene’s test on all aforementioned levels (Levene, 1960). None returned statistically significant variance between the distributions of the two samples being compared, allowing us to proceed without corrections for unequal variances. We simultaneously adjust the alpha value down to a factor of .25 (from .05 to .0125), to correct for multiple comparisons using the Bonferroni method.

Tests:

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

local level data:

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

<!--html_preserve--><div id="rgebydjcby" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rgebydjcby table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rgebydjcby thead, #rgebydjcby tbody, #rgebydjcby tfoot, #rgebydjcby tr, #rgebydjcby td, #rgebydjcby th {
  border-style: none;
}

#rgebydjcby p {
  margin: 0;
  padding: 0;
}

#rgebydjcby .gt_table {
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

#rgebydjcby .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rgebydjcby .gt_title {
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

#rgebydjcby .gt_subtitle {
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

#rgebydjcby .gt_heading {
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

#rgebydjcby .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rgebydjcby .gt_col_headings {
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

#rgebydjcby .gt_col_heading {
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

#rgebydjcby .gt_column_spanner_outer {
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

#rgebydjcby .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rgebydjcby .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rgebydjcby .gt_column_spanner {
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

#rgebydjcby .gt_spanner_row {
  border-bottom-style: hidden;
}

#rgebydjcby .gt_group_heading {
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

#rgebydjcby .gt_empty_group_heading {
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

#rgebydjcby .gt_from_md > :first-child {
  margin-top: 0;
}

#rgebydjcby .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rgebydjcby .gt_row {
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

#rgebydjcby .gt_stub {
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

#rgebydjcby .gt_stub_row_group {
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

#rgebydjcby .gt_row_group_first td {
  border-top-width: 2px;
}

#rgebydjcby .gt_row_group_first th {
  border-top-width: 2px;
}

#rgebydjcby .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rgebydjcby .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rgebydjcby .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rgebydjcby .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rgebydjcby .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rgebydjcby .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rgebydjcby .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rgebydjcby .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rgebydjcby .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rgebydjcby .gt_footnotes {
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

#rgebydjcby .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rgebydjcby .gt_sourcenotes {
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

#rgebydjcby .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rgebydjcby .gt_left {
  text-align: left;
}

#rgebydjcby .gt_center {
  text-align: center;
}

#rgebydjcby .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rgebydjcby .gt_font_normal {
  font-weight: normal;
}

#rgebydjcby .gt_font_bold {
  font-weight: bold;
}

#rgebydjcby .gt_font_italic {
  font-style: italic;
}

#rgebydjcby .gt_super {
  font-size: 65%;
}

#rgebydjcby .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rgebydjcby .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rgebydjcby .gt_indent_1 {
  text-indent: 5px;
}

#rgebydjcby .gt_indent_2 {
  text-indent: 10px;
}

#rgebydjcby .gt_indent_3 {
  text-indent: 15px;
}

#rgebydjcby .gt_indent_4 {
  text-indent: 20px;
}

#rgebydjcby .gt_indent_5 {
  text-indent: 25px;
}

#rgebydjcby .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#rgebydjcby div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

Accordingly, Table 2 presents the results from the analysis of the mean differences between internal and external framing on a given manipulation, alongside the mean differences for pooled data (pooling all internal scores and external scores respectively, to detect a difference on the global level). Critically, we analyze the data at both the individual and global level to avoid a reversal paradox (sometimes referred to as Simpson’s paradox), and to account for potential nesting in our data (Blyth, 1972). This bears a variety of notable statistical metrics worth highlighting. To begin, the pooled results do not reflect a significant effect of control-based framing on self-reported satisfaction. Specifically, while internally-framed questions produced higher levels of self-reported satisfaction (M = 3.9, SD = 1.3) than externally-framed questions, t(334) = 2.31, the p-value is insignificant at p = .02 (compared to our Bonferroni-corrected significance level of α = .0125).

Granular examination of the data at the level of each individual manipulation reveals further inconsistencies. In Manipulation 1, the internally-framed variant produced levels of self-reported satisfaction (M = 4.4, SD = 0.8) insignificantly lower than its externally-framed counterpart, t(82) = -0.42, p = .674. In Manipulation 2, the internally-framed variant produced insignificantly higher levels of satisfaction compared to its externally-framed counterpart (M = 4.1, SD = 0.8), t(82) = 1.6, p = .117. And in Manipulation 4, the internally-framed variant again produced insignificantly lower levels of satisfaction (M = 3.2, SD = 1.3), t(82) = -0.29, p = .772. The only manipulation which resulted in a statistically significant effect was Manipulation 3. Said manipulation produced the effect in the direction of our hypothesis (M = 3.7, SD = 1.2), t(82) = 4.06, p < 0.001. A final equal-variance two-sample t-test in absence of that manipulation mirrors this broader pattern of the insignificance of control-based framing. Here, the pooled data for internal framing in Manipulations 1, 2, and 4 are not significantly different from their externally framed counterparts (M = 3.92, SD = 1.1), t(250) = 0.23, p = .817.

We subsequently apply one last series of subgroup tests, as to ensure that demographic differences are not responsible for the aforementioned results. Specifically, we test for significant differences in aggregate self-reported satisfaction scores, on the level of the two demographic variables we collect (gender and class year). We do so by applying a one-way analysis of variance test on the level of each manipulation, grouped by the relevant demographic variable. Here, we find no significant difference along either of the demographic variables, as presented in Table 3; therefore, we conclude that demographic differences in gender and class year do not account for the observed effects in our primary analysis.

# Discussion

In examining the relationship between perceived agency and emotional valence, we investigated the effect of control-based framing on self-reported satisfaction; we did so through an experimental survey that included targeted framing manipulations. Broadly, however, the results of this experiment fail to support our original hypothesis; that is, that a relationship exists between an internal locus of control and positive responsive valence, and between an external locus of control and negative responsive valence. 

To begin, the pooled results did not reflect a significant effect of control-based framing on self-reported satisfaction, contrary to our directional hypothesis. Furthermore, at the level of the manipulation, our directional hypothesis is further challenged by the lack of clear differences between the experimental and control groups. Critically, in three of the four manipulations, the type of control did not have a significant effect on self-reported satisfaction; in two of the four, the direction of the relationship was insignificantly inverse to our hypothesis.


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

Figure 2 reflects this trend of inconsistency and insignificance, through the visualization of the distribution of manipulation-level data points. Additionally, in the sole manipulation which did produce a significant effect, there is reason to suspect the presence of a confounding variable such as emotional acquiescence (Guyatt et al., 1999). Specifically, the underlying content of Manipulation 3 addressed a potentially emotionally charged topic (emotions related to leaving college), in contrast to the logistical subject matter tested in the other three manipulations (e.g., satisfaction in relation to housing situation, flexible major declaration, and course requirements). While our original construct did not make an attempt to separate acquiescence from perceived agency, as we assumed that acquiescence was one of the mechanisms through which perceived agency would operate and thus account for, the observed results suggest that this is not the case. Rather, agency appears to be generally insignificant as a predictor of valence, while acquiescence does not. As such, we do not feel comfortable confirming our hypothesis on the basis of this manipulation alone. This discrepancy serves as a basis for further examination; future iterations of the construct could benefit from explicitly disentangling these factors.

Taken together, this research is somewhat inconsistent with previous findings in the literature. As discussed previously, prior research has held a significant relationship between framing effects and responses, often demonstrating that the way information is presented has a systematic effect on perceptions. Resultantly, we expected control-based framing to behave similarly, with greater perceived agency resulting in positively-oriented perceptions. However, the trends in our experimental data do not give us compelling evidence in favor of this expectation, and we therefore fail to reject the null hypothesis.

# Limitations
A final relevant discussion concerns the five unique limitations of this study. To begin, the demographic homogeneity of our sample may have inadvertently introduced bias and reduced external validity. Beyond the subgroup analysis, which demonstrated that the class year and gender of the participants did not significantly affect scores, the participants were still drawn exclusively from a niche population—non-freshman undergraduates at Harvard College. Because the value that individuals put on perceived agency has been shown to differ based on lived experiences, it is important that this analysis be conducted within samples drawn from diverse populations (Leotti et al., 2010). In that vein, the effect of individual agency on perceptions and decision-making have been shown to differ across cultures (Jiao & Zhao, 2023). Accordingly, future studies should also apply cross-cultural approaches to examine whether cultural norms about agency influence the relationship between control-based framing and valence.

Second, the external validity of our experiment was constrained by the methodology we employed. Specifically, the controlled experimental design inherent to the manipulations in our survey, while useful for isolating variables, does not fully reflect the circumstances under which individuals make decisions in the real world. Moreover and intuitively, factors such as the intensity of the circumstances may affect the perceived importance of agency. Thus, future studies should expand to examine this relationship under a variety of diverse environments and circumstances, and employ methods beyond a manipulated survey.

Third, limited statistical power due to our sample size may have constrained our ability to detect significant differences across manipulations. Unfortunately, due to logistical constraints, our sample capped out at a size of n = 84. Under a one-sided power calculation that tests for a moderate effect size (r = 0.3) and a significance level of a = 0.05, our study retained a power of .876. However, a greater sample size would both improve said power, and permit testing for more subtle effect sizes as well; improving the experiment’s ability to detect significant differences across manipulations.

Finally, as previously stated, future studies in this realm should more explicitly address confounders such as emotional acquiescence, and broadly standardize the content of the manipulated questions beyond their respective framings.

# Conclusion
Summarily, this study sought to investigate the relationship between control-based framing and self-reported valence; it did so by manipulating the locus of control (internal vs. external) within an experimental survey and examining its effects on self-reported satisfaction ratings. Resultantly, the pooled data failed to reflect a statistically significant relationship in favor of the hypothesis—that internal framing bears positive emotional valence, and that external framing bears negative valence. Subsequently, across the individual manipulations, only one of four manipulations demonstrated significance, and was found to skew the direction of the findings at the pooled/global level as well. And because we have reason to believe that the one significant manipulation may distinctly suffer from confounding factors, we do not believe there is sufficient evidence to confirm our hypothesis.

These findings challenge the idea that perceived agency in framing is a reliable cause and consistent predictor of valence, demanding further research to understand the relationship between contextual factors, perceived agency, and emotional responses. It also highlights the need for future studies to carefully separate potential confounders such as emotional acquiescence from perceived agency, as to produce more narrow insights. Although the results of this specific study were not conclusive in supporting the hypothesis, it contributes to the broader literature by recognizing key constraints and limitations within its methodology, whose lessons can be employed by successive experiments. Ultimately and in turn, a more robust understanding of agency and its effect on valence will have significant implications across real-world contexts, from healthcare and education to consumer preferences and the design of policy.

# References:
Bloem, J. R., & Khandker Wahedur Rahman. (2024). What I say depends on how you ask: Experimental evidence of the effect of framing on the measurement of attitudes. Economics Letters, 238, 111686–111686. https://doi.org/10.1016/j.econlet.2024.111686

Blyth, C. R. (1972). On Simpson’s Paradox and the Sure-Thing Principle. Journal of the American Statistical Association, 67(338), 364–366. https://doi.org/10.2307/2284382

Bonsang, E., & Costa-Font, J. (2022). Buying control? “Locus of control” and the uptake of supplementary health insurance. Journal of Economic Behavior & Organization, 204, 476–489. https://doi.org/10.1016/j.jebo.2022.10.035

Botha, F., & Dahmann, S. C. (2024). Locus of control, self-control, and health outcomes. SSM-Population Health, 25, 101566–101566. https://doi.org/10.1016/j.ssmph.2023.101566

Eren, F., Kousignian, I., Solène Wallez, Melchior, M., & Murielle Mary-Krause. (2023). Association between individuals’ locus of control and mental health during the COVID-19 pandemic. Journal of Affective Disorders Reports, 14. https://doi.org/10.1016/j.jadr.2023.100678

Guyatt, G. H., Cook, D. J., King, D., Norman, G. R., Kane, S. L., & van Ineveld, C. (1999). Effect of the framing of questionnaire items regarding satisfaction with training on residents’ responses. Academic Medicine: Journal of the Association of American Medical Colleges, 74(2), 192–194. https://doi.org/10.1097/00001888-199902000-00018

Jain, M., & Singh, S. (2015). Locus of control and its relationship with mental health and adjustment among adolescent females. Journal of Mental Health and Human Behaviour, 20(1), 16. https://doi.org/10.4103/0971-8990.164803

Jiao, J., & Zhao, J. (2023). Individualism, Collectivism, and Allocation Behavior: Evidence from the Ultimatum Game and Dictator Game. Behavioral Sciences, 13(2), 169. https://doi.org/10.3390/bs13020169

Kesavayuth, D., Binh Tran, D., & Zikos, V. (2022). Locus of control and subjective well-being: Panel evidence from Australia. PLOS ONE, 17(8). https://doi.org/10.1371/journal.pone.0272714

Leotti, L. A., Iyengar, S. S., & Ochsner, K. N. (2010). Born to choose: The origins and value of the need for control. Trends in Cognitive Sciences, 14(10), 457–463. https://doi.org/10.1016/j.tics.2010.08.001

Levene, H. (1960). Robust Tests for the Equality of Variances. In Contributions to Probability and Statistics: Essays in Honor of Harold Hotelling (1st ed., pp. 278–292). Stanford University Press; Stanford Un. https://epubs.siam.org/doi/10.1137/1003016

Li, S., & Xie, X. (2006). A new look at the “Asian disease” problem: A choice between the best possible outcomes or between the worst possible outcomes?. Thinking & Reasoning, 12(2), 129–143. https://doi.org/10.1080/13546780500145652

Razak, L. A. (2019, August 1). Effect of Framing and Locus of Control on Commitment Escalation in Investment Decisions. Www.atlantis-Press.com; Atlantis Press. https://doi.org/10.2991/icame-18.2019.27

Tversky, A., & Kahneman, D. (1981). The framing of decisions and the psychology of choice. Science, 211(4481), 453–458. https://www.jstor.org/stable/1685855

Xu, L., Liu, Z., Ji, M., Dong, Y., & Wu, C.-H. (2021). Leader Perfectionism—Friend or Foe of Employee Creativity? Locus of Control as a Key Contingency. Academy of Management Journal, 65(6). https://doi.org/10.5465/amj.2019.0165

# Pages integration/index.html generator script


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

