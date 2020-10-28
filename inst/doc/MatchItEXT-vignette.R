## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install, eval= FALSE-----------------------------------------------------
#  pkg <- c("MatchIt", "MatchItEXT", "ggplot2")
#  install.packages(pkg)

## ----setup--------------------------------------------------------------------
library(MatchIt)
library(MatchItEXT)
library(ggplot2)
data(lalonde)

## -----------------------------------------------------------------------------
## Checking the size and structure of lalonde.
str(lalonde)

## Checking the first 5 rows of lalonde.
head(lalonde, 5)

## Checking whether there is any missing value in each column.
sapply(lalonde, function(x) sum(is.na(x)))

## No missing value was found.

## -----------------------------------------------------------------------------
## Noticing that dichotomous variables that would be used  are of numeric type, 
## they should be converted to factor type beforehand.
lalonde$treat <- as.factor(lalonde$treat)
lalonde$black <- as.factor(lalonde$black)
lalonde$hispan <- as.factor(lalonde$hispan)
## Creating a propensity score estimation formula.
## In the formula, treat, black, and hispan are treated as binary variables.
formu <- as.formula(treat ~ re74 + re75 + age + educ + black + hispan)
## Applying Nearest neighbor matching on the selected covariates. The result 
## would be used as an input in the MatchItEXT functions. You can try other 
## matching methods such as "optimal", "full, "genetic", etc.
m_near <- MatchIt::matchit(formula = formu, data = lalonde, distance = "logit",
                           method = "nearest")

## -----------------------------------------------------------------------------
## Computing Standardized Mean Difference (SMD).
## If the matching result is satisfying, most SMD after matching would drop in 
## the range of -0.1 ~ 0.1, see the column "smd_after".
smd_near <- compute_smd(mi_obj = m_near, sd = "pooled")
smd_near

## Column names explanation:
## mean_tr_bf: mean of treatment group before matching
## mean_ctl_bf: mean of control group before matching
## mean_tr_af: mean of treatment group after matching
## mean_ctl_af: mean of control group after matching
## var_type: variable type
## var_tr_bf: variance of treatment group before matching
## var_ctl_bf: variance of control group before matching
## sd_bf_pooled: pooled standard deviation before matching
## sd_bf_tr: standard deviation of treatment group before matching
## smd_before: SMD before matching
## smd_after: SMD after matching

## Switching "pooled" SD to "treatment" SD.
smd_near <- compute_smd(mi_obj = m_near, sd = "treatment")
smd_near

## -----------------------------------------------------------------------------
## Generating a subclassification result from matchit() with the same formula.
m_sub <- MatchIt::matchit(formula = formu, data = lalonde, distance = "logit",
                           method = "subclass", subclass = 5)
smd_sub <- compute_sub_smd(mi_obj = m_sub, sd = "pooled")
## The threshold of good matching for SMD is ± 0.1. The closer to 0, the better.
smd_sub

## ---- fig.width = 7, fig.height = 5-------------------------------------------
## If the matching result is satisfying, most SMD after matching would rest in 
## the range of -0.1 ~ 0.1.
plot_smd_near <- plot_smd(smd_near)
plot_smd_near$plot
## Increased SMD are stored in the returning result, R code for plotting and 
## other relevant data are available as well, use '$' to obtain them.
plot_smd_near$smd_increase
plot_smd_near$plot_code

## ---- fig.width = 7, fig.height = 5-------------------------------------------
## QQ plot for overall distance measure (propensity score in our case).
## If the matching result is good enough, most points in the QQ plot after 
## matching would rest on the 45-degree line.
m_near_qq <- plot_ps_qq(m_near)
m_near_qq$plot

## -----------------------------------------------------------------------------
## Computing variance ratio of propensity score
m_near_var_ratio <- compute_var_ratio(m_near)
m_near_var_ratio

## Column names explanation:
## var_tr_bf: variance of treatment group before matching
## var_ctl_bf: variance of control group before matching
## var_tr_af: variance of treatment group after matching
## var_ctl_af: variance of control group after matching
## ratio_bf: variance ratio before matching
## ratio_af: variance ratio after matching

## Computing residual variance ratio for each covariate
## Checking the formula for propensity score estimation
parse_formula(m_near)
## This auxiliary function shows covariates in the formula to help decide 
## variable types.

## The function requires the original data set, the MatchIt object (except for 
## results from "exact" or "subclass" method), and a vector specifying 
## covariate types.
## Valid values for covariate types:
## do not include this covariate: 'excluded', '0', 0, NA;
## continuous variable: 'continuous', '1', 1;
## dichotomous variable: 'binary', '2', 2;
## ordinal variable: 'ordinal', '3', 3.
## Note a vector in R does not support different types of values, thus these 
## valid values cannot be mixed.

## Note multinomial variable is not applicable to this function.

## The 'discard' argument is logical to judge if some cases were discarded 
## before matching. The default is FALSE, no discard before matching. 
compute_res_var_ratio(original_data = lalonde, mi_obj = m_near, type_vec = 
                        c(0, 1, 1, 1, 2, 2), discard = FALSE)

