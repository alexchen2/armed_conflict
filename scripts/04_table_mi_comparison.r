library(mice)
library(naniar)
library(plm)
library(tidyverse)
library(readr)
library(table1)
library(xtable)
library(texreg)
library(here)

finaldata <- read.csv(here("data", "processed", "final_data.csv"), header = TRUE)
finaldata$loggdp <- log(finaldata$gdp_1000)

# Visualize missing data
finaldata |>
  arrange(year, iso) |>
  dplyr::select(-iso, -year) |>
  vis_miss()

finaldata |> dplyr::filter(is.na(infmor))
finaldata |> dplyr::filter(is.na(pop_dens))

# Complete case analysis - checking number of unique countries remaining afterwards
length(unique(finaldata$iso))

matmordata <- finaldata |> dplyr::select(-infmor, -un5mor, -neomor)
completedata <- matmordata[complete.cases(matmordata),]
length(unique(completedata$iso))

childmordata <- finaldata |> dplyr::select(-matmor)
completedata <- childmordata[complete.cases(childmordata),]
length(unique(completedata$iso))

# PLM from previous assignment; complete cases
preds <- as.formula(" ~ had_conflict + loggdp + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")

out.matmor <- plm(
  update.formula(preds, matmor ~ .), 
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = finaldata
)
out.un5mor <- plm(
  update.formula(preds, un5mor ~ .), 
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = finaldata
)
out.infmor <- plm(
  update.formula(preds, infmor ~ .), 
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = finaldata
)
out.neomor <- plm(
  update.formula(preds, neomor ~ .), 
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = finaldata
)

# Preparing data for MI
# From earlier EDA observations, we assume the data is MAR (mortality missingness linked to certain year-country combos)
# Note to self on log(gdp): mixed opinions on order of log transform, some prefer transform after MI and others vice-versa
# Transparency about chosen method is most important

midata <- finaldata |>
  mutate(iso_num = as.numeric(as.factor(finaldata$iso))) |>
  select(-iso)

# Dry run MI to get methods and pred
mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "matmor", "infmor", "neomor", "un5mor")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "matmor", "infmor", "neomor", "un5mor"), "iso_num"] <- -2

# MI with m=10, maxit=20 imputations
# mice.multi.out  <- mice(
#   midata, seed = 42069, m = 10, maxit = 20,                      
#   method = meth,
#   predictorMatrix = pred
# )
# save(mice.multi.out, file = "data/processed/mice_out.RData")
load("data/processed/mice_out.RData")

plot(mice.multi.out)



# Fit lm model on imputed data for each mortality variable
fit.mi.matmor <- with(
  mice.multi.out, 
  lm(
    matmor ~ -1 + had_conflict + loggdp + oecd + pop_dens + urban + 
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                  as.factor(iso_num) + as.factor(year)
  )
)
fit.mi.infmor <- with(
  mice.multi.out, 
  lm(
    infmor ~ -1 + had_conflict + loggdp + oecd + pop_dens + urban + 
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                  as.factor(iso_num) + as.factor(year)
  )
)
fit.mi.un5mor <- with(
  mice.multi.out, 
  lm(
    un5mor ~ -1 + had_conflict + loggdp + oecd + pop_dens + urban + 
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                  as.factor(iso_num) + as.factor(year)
  )
)
fit.mi.neomor <- with(
  mice.multi.out, 
  lm(
    neomor ~ -1 + had_conflict + loggdp + oecd + pop_dens + urban + 
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                  as.factor(iso_num) + as.factor(year)
  )
)

# Pooling with Rubin's Rules
out.mi.matmor <- pool(fit.mi.matmor)
out.mi.infmor <- pool(fit.mi.infmor)
out.mi.un5mor <- pool(fit.mi.un5mor)
out.mi.neomor <- pool(fit.mi.neomor)
summary(out.mi.matmor)
summary(out.mi.infmor)
summary(out.mi.un5mor)
summary(out.mi.neomor)

# Save HTML table output into a file
sink("reports/table04_mi_comparison.html")
htmlreg(
  list(
    out.matmor, out.mi.matmor, 
    out.un5mor, out.mi.un5mor, 
    out.infmor, out.mi.infmor, 
    out.neomor, out.mi.neomor
  ),
  ci.force = TRUE,
  # groups = list("Armed Conflict" = 1),
  custom.coef.map = list(
    "had_conflict" = "Armed Conflict",
    "loggdp" = "log(GDP)",
    "oecd" = "OECD",
    "pop_dens" = "Population Density",
    "urban" = "Urban",
    "age_dep" = "Age Dependency",
    "male_edu" = "Male Education",
    "temp" = "Average Temperature",
    "rainfall_1000" = "Average Rainfall",
    "earthquake" = "Earthquake",
    "drought" = "Drought"
  ),
  custom.model.names = c(
    "CC Model",
    "MI Model",
    "CC Model",
    "MI Model",
    "CC Model",
    "MI Model",
    "CC Model",
    "MI Model"  
  ),
  custom.header = list(
    "Maternal mortality" = 1:2,
    "Under-5 mortality" = 3:4,
    "Infant mortality" = 5:6,
    "Neonatal mortality" = 7:8
  ),
  custom.note = "Note: Multiple imputation performed with m = 10, number of iterations = 20. %stars"
)

sink()
