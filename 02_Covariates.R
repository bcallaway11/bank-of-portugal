## ----echo=FALSE---------------------------------------------------------------
library(revealEquations)


## ----echo=FALSE, results="asis"-----------------------------------------------

title <- "Identification under Conditional Parallel Trends"

before <- "Under conditional parallel trends, we have that"

eqlist <- list(
  "ATT &= \\E[\\Delta Y | D=1] - \\E[\\Delta Y(0) | D=1] \\hspace{150pt}",
  "&=\\E\\Big[ \\E[\\Delta Y | X,D=1] \\Big| D=1\\Big] - \\E\\Big[ \\E[\\Delta Y(0) | X, D=1] \\Big| D=1\\Big]",
  "&=\\E\\Big[ \\underbrace{\\E[\\Delta Y | X,D=1] - \\E[\\Delta Y(0) | X, D=0]}_{=:ATT(X)} \\Big| D=1\\Big]",
  "&= \\E[\\Delta Y | D=1] - \\E\\Big[ \\underbrace{\\E[\\Delta Y(0) | X, D=0]}_{=:m_0(X)} \\Big| D=1\\Big]"
)


after <- "

. . .

Intuition: (i) Compare path of outcomes for treated group to (conditional on covariates) path of outcomes for untreated group, (ii) adjust for differences in the distribution of covariates between groups.

. . .

This argument also require an [overlap condition]{.alert}

* For all possible values of the covariates, $p(x) := \\P(D=1|X=x) < 1$.

* In words: for all treated units, we can find untreated units that have the same characteristics

"

step_by_step_eq(eqlist, before, after, title)


## ----echo=FALSE, results="asis"-----------------------------------------------

title <- "Alternative Identification Strategy: Covariate Balancing"

before <- "[Example:]{.alert} Momentarily, suppose that the distribution of $X$ was the same for both groups, then

. . .

"

eqlist <- list(
  "ATT &= \\E[\\Delta Y | D = 1] - \\E[\\Delta Y(0) | D = 1] \\hspace{150pt}",
  "&= \\E[\\Delta Y | D = 1] - \\E\\Big[ \\E[\\Delta Y(0) | X, D=0 ] \\Big| D=1\\Big]",
  "&= \\E[\\Delta Y | D = 1] - \\E\\Big[ \\E[\\Delta Y(0) | X, D=0 ] \\Big| D=0\\Big]",
  "&= \\E[\\Delta Y | D = 1] - \\E[\\Delta Y(0) | D=0]"
)


after <- "

. . .

$\\implies$ (even under conditional parallel trends) we can recover $ATT$ by just directly comparing paths of outcomes for treated and untreated groups."

step_by_step_eq(eqlist, before, after, title)


## ----echo=FALSE, results="asis"-----------------------------------------------

title <- "Alternative Identification Strategy: Covariate Balancing"

before <- "[More generally:]{.alert} We would not expect the distribution of covariates to be the same across groups.

However the idea of covariate balancing is to come up with [balancing weights]{.alert} $\\nu_0(X)$ such that the distribution of $X$ is the same in the untreated group as it is in the treated group after applying the balancing weights.  Then we would have that

. . .

"

eqlist <- list(
  "ATT &= \\E[\\Delta Y | D=1] - \\E[\\Delta Y(0) | D=1] \\hspace{150pt}",
  "&= \\E[\\Delta Y | D=1] - \\E\\Big[ \\E[\\Delta Y(0) | X, D=0 ] \\Big| D=1\\Big]",
  "&= \\E[\\Delta Y | D=1] - \\E\\Big[ \\nu_0(X) \\E[\\Delta Y(0) | X, D=0 ] \\Big| D=0\\Big]",
  "&= \\E[\\Delta Y | D=1] - \\E[\\nu_0(X) \\Delta Y(0) | D=0]"
)


after <- "

. . .

$\\implies$ We can recover $ATT$ by re-weighting the untreated group to have the same distribution of covariates as the treated group has...and then just average"

step_by_step_eq(eqlist, before, after, title)


## -----------------------------------------------------------------------------
#| echo: false
library(did)
library(BMisc)
library(twfeweights)
library(fixest)
library(modelsummary)
library(ggplot2)
load("data2.RData")
data2$region <- droplevels(data2$region)
data2 <- as.data.frame(data2)

## -----------------------------------------------------------------------------
# run TWFE regression
# data2 <- subset(data2, year %in% c(2003,2007))
# data2 <- subset(data2, G %in% c(0, 2006))
data2 <- subset(data2, year %in% c(2003, 2004))
data2 <- subset(data2, G %in% c(0, 2004))

twfe_x <- fixest::feols(lemp ~ post + lpop | year + id,
  data = data2, cluster = "id"
)
modelsummary(twfe_x, gof_omit = ".*")

tp_wts <- two_period_reg_weights(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G", xformula = ~lpop,
  data = data2
)

summary(tp_wts)
tp_wts$est
ggtwfeweights(tp_wts)
## -----------------------------------------------------------------------------
library(twfeweights)
data2$tempG <- data2$G
twfe_wts <- implicit_twfe_weights(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformula = ~lpop,
  data = data2
  # base_period = "gmin1"
)


summary(twfe_wts)
twfe_wts_summary <- summary(twfe_wts)


## -----------------------------------------------------------------------------
covariate_balance <- twfe_cov_bal(twfe_wts, ~ region + lpop + lavg_pay + -1)
summary(covariate_balance)


## -----------------------------------------------------------------------------
ggtwfeweights(covariate_balance,
  absolute_value = FALSE,
  standardize = TRUE,
  plot_relative_to_target = FALSE
) +
  xlim(c(-1, 1))


## -----------------------------------------------------------------------------
# run TWFE regression
twfe_x <- fixest::feols(lemp ~ post | id + region^year,
  data = data2
)
modelsummary(twfe_x, gof_omit = ".*")


## ----eval=FALSE---------------------------------------------------------------
#| code-line-numbers: "|6"
## # callaway and sant'anna including covariates
## cs_x <- att_gt(yname="lemp",
##                tname="year",
##                idname="id",
##                gname="G",
##                xformla=~region,
##                control_group="nevertreated",
##                base_period="universal",
##                data=data2)
## cs_x_res <- aggte(cs_x, type="group")
## summary(cs_x_res)
## cs_x_dyn <- aggte(cs_x, type="dynamic")
## ggdid(cs_x_dyn)


## ----echo=FALSE---------------------------------------------------------------
cs_x <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformla = ~region,
  control_group = "nevertreated",
  base_period = "universal",
  data = data2
)
cs_x_res <- aggte(cs_x, type = "group")
summary(cs_x_res)


## ----echo=FALSE---------------------------------------------------------------
cs_x_dyn <- aggte(cs_x, type = "dynamic")
ggdid(cs_x_dyn)


## -----------------------------------------------------------------------------
# run TWFE regression
twfe_x <- fixest::feols(lemp ~ post + lpop + lavg_pay | id + region^year,
  data = data2
)
modelsummary(twfe_x, gof_omit = ".*")


## ----eval=FALSE---------------------------------------------------------------
#| code-line-numbers: "|6|9"
## # callaway and sant'anna including covariates
## cs_x <- att_gt(yname="lemp",
##                tname="year",
##                idname="id",
##                gname="G",
##                xformla=~region + lpop + lavg_pay,
##                control_group="nevertreated",
##                base_period="universal",
##                est_method="reg",
##                data=data2)
## cs_x_res <- aggte(cs_x, type="group")
## summary(cs_x_res)
## cs_x_dyn <- aggte(cs_x, type="dynamic")
## ggdid(cs_x_dyn)


## ----echo=FALSE---------------------------------------------------------------
cs_x <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformla = ~ region + lpop + lavg_pay,
  control_group = "nevertreated",
  base_period = "universal",
  est_method = "reg",
  data = data2
)
cs_x_res <- aggte(cs_x, type = "group")
summary(cs_x_res)


## ----echo=FALSE---------------------------------------------------------------
cs_x_dyn <- aggte(cs_x, type = "dynamic")
ggdid(cs_x_dyn)


## ----eval=FALSE---------------------------------------------------------------
#| code-line-numbers: "|9"
## # callaway and sant'anna including covariates
## cs_x <- att_gt(yname="lemp",
##                tname="year",
##                idname="id",
##                gname="G",
##                xformla=~region + lpop + lavg_pay,
##                control_group="nevertreated",
##                base_period="universal",
##                est_method="ipw",
##                data=data2)
## cs_x_res <- aggte(cs_x, type="group")
## summary(cs_x_res)
## cs_x_dyn <- aggte(cs_x, type="dynamic")
## ggdid(cs_x_dyn)


## ----echo=FALSE---------------------------------------------------------------
cs_x <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformla = ~ region + lpop + lavg_pay,
  control_group = "nevertreated",
  base_period = "universal",
  est_method = "ipw",
  data = data2
)
cs_x_res <- aggte(cs_x, type = "group")
summary(cs_x_res)


## ----echo=FALSE---------------------------------------------------------------
cs_x_dyn <- aggte(cs_x, type = "dynamic")
ggdid(cs_x_dyn)


## ----eval=FALSE---------------------------------------------------------------
#| code-line-numbers: "|9"
## # callaway and sant'anna including covariates
## cs_x <- att_gt(yname="lemp",
##                tname="year",
##                idname="id",
##                gname="G",
##                xformla=~region + lpop + lavg_pay,
##                control_group="nevertreated",
##                base_period="universal",
##                est_method="dr",
##                data=data2)
## cs_x_res <- aggte(cs_x, type="group")
## summary(cs_x_res)
## cs_x_dyn <- aggte(cs_x, type="dynamic")
## ggdid(cs_x_dyn)


## ----echo=FALSE---------------------------------------------------------------
cs_x <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformla = ~ region + lpop + lavg_pay,
  control_group = "nevertreated",
  base_period = "universal",
  est_method = "dr",
  data = data2
)
cs_x_res <- aggte(cs_x, type = "group")
summary(cs_x_res)


## ----echo=FALSE---------------------------------------------------------------
cs_x_dyn <- aggte(cs_x, type = "dynamic")
ggdid(cs_x_dyn)
