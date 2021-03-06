
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MatchItEXT

<!-- badges: start -->

<!-- badges: end -->

MatchItEXT is a supplementary pacakge to MatchIt in R. Its functions
generate diagnostics that are unavailable in MatchIt and help assess the
matching result (e.g., propensity score matching).

## Installation

You can install the released version of MatchItEXT from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("MatchItEXT")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zoudj/MatchItEXT")
```

## Example

The code chunk below demonstrates the basic usage of MatchItEXT
functions. For more details, please see the vignette.

``` r
library(MatchItEXT)
## Computing Standardized Mean Difference (SMD).
## m_near is the Nearest Matching result from matchit() in MatchIt package.
smd_near <- compute_smd(mi_obj = m_near, sd = "pooled")
smd_near

## Computing Standardized Mean Difference (SMD) of subclassification result.
## m_sub is the subclassification result from matchit() in MatchIt package.
smd_sub <- compute_sub_smd(mi_obj = m_sub, sd = "pooled")
smd_sub

## Drawing SMD comparison dot-and-line plot.
plot_smd_near <- plot_smd(smd_near)
plot_smd_near$plot

## Drawing QQ plot for group comparison on the distance measure.
m_near_qq <- plot_ps_qq(m_near)
m_near_qq$plot

## Computing variance ratio and residual variance ratio
m_near_var_ratio <- compute_var_ratio(m_near)
m_near_var_ratio

compute_res_var_ratio(original_data = lalonde, mi_obj = m_near, type_vec = 
                        c(0, 1, 1, 1, 2, 2), discard = FALSE)
```
