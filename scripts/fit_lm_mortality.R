

library(plm)
library(tidyverse)
library(readr)
library(table1)
library(xtable)
library(texreg)

# Import preprocessed data from previous assignments
armed_conflict <- read.csv(here("data", "processed", "final_data.csv"), header = TRUE)

# Fixed effect models with lm + plm, accounting only for country iso
lmmod <- lm(matmor ~ -1 + had_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso, 
            data = armed_conflict)

plmmod <- plm(matmor ~ had_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso"), model = "within", data = armed_conflict)

summary(lmmod)
summary(plmmod)

# Fixed effect models accounting for both effects from iso and year
lmmod <- lm(matmor ~ -1 + had_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso + as.factor(year), 
            data = armed_conflict)

plmmod <- plm(matmor ~ had_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = armed_conflict)

summary(lmmod)
summary(plmmod)

# We notice that GDP has a positive association with maternal mortality rate; intuitively, this doesn't make sense. 
# Upon investigating the distribution of matmor data over GDP, we notice a highly right-skewed distribution shape. 

# Fit plm model, but taking the log transformation of gdp_1000 instead
armed_conflict$loggdp <- log(armed_conflict$gdp_1000)

preds <- as.formula(" ~ had_conflict + loggdp + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")

matmormod <- plm(
  update.formula(preds, matmor ~ .), 
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = armed_conflict
)
un5mormod <- plm(
  update.formula(preds, un5mor ~ .), 
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = armed_conflict
)
infmormod <- plm(
  update.formula(preds, infmor ~ .), 
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = armed_conflict
)
neomormod <- plm(
  update.formula(preds, neomor ~ .), 
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = armed_conflict
)

summary(matmormod)
summary(un5mormod)
summary(infmormod)
summary(neomormod)

# Modify model texreg structure to include armed conflict reference level ("no")
matmor_tr <- extract(matmormod)
un5mor_tr <- extract(un5mormod)
infmor_tr <- extract(infmormod)
neomor_tr <- extract(neomormod)

htmlreg(
  list(matmormod, un5mormod, infmormod, neomormod),
  ci.force = TRUE,
  # groups = list("Armed Conflict" = 1),
  custom.coef.map = list("had_conflict" = "Armed Conflict"),
  custom.model.names = c(
    "Maternal mortality",
    "Under-5 mortality",
    "Infant mortality",
    "Neonatal mortality"
  ),
  single.row = TRUE
)
