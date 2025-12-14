# Final project 506

# Load packages
library(gssr)       # GSS data
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(MASS)

# Test
gss18 <- gss_get_yr(2018)
head(gss18)

# data
years_all <- c(1972:1978,
               1980, 1982:1991, 1993, 1994, 1996, 1998, 2000,
               2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018,
               2021, 2022, 2024)


years_all <- years_all[years_all >= 1980 & years_all <= 2024]
years_all
gss_all <- bind_rows(lapply(years_all, gssr::gss_get_yr))

# Inspect variable 
names(gss_all)[1:80]

# Check 
c("happy","satfin","wtssall","wtssnr","wtss") %in% names(gss_all)
grep("wt", names(gss_all), value = TRUE)

# quick check 
summary(gss_all$year)
summary(gss_all$happy)
summary(gss_all$satfin)
summary(gss_all$incom16)

# Clean data
df_happy <- gss_all %>%
  transmute(
    year   = as.integer(year),
    happy  = as.numeric(happy),
    satfin = as.numeric(satfin),   # 1–3 scale
    age    = as.numeric(age),
    educ   = as.numeric(educ),
    incom16 = as.numeric(incom16),
    sex    = factor(sex),
    race   = factor(race),
    marital = factor(marital),
    reg16  = factor(reg16),
    wt     = as.numeric(wtssall)
  ) %>%
  filter(
    year >= 1980 & year <= 2024,
    happy %in% 1:3,
    satfin %in% 1:3,
    age >= 18 & age <= 89
  ) %>%
  mutate(
    year_c = year - 1980,
    satfin_f = factor(
      satfin,
      levels = 1:3,
      labels = c("Not satisfied", "More or less", "Satisfied")
    )
  )

# Check result
summary(df_happy$happy)
summary(df_happy$satfin)
table(df_happy$satfin_f, useNA = "ifany")


# EDA
eda_year <- df_happy %>%
  group_by(year) %>%
  summarize(
    mean_happy = weighted.mean(happy, wt),
    n = n(),
    .groups = "drop"
  )

plot_time <- ggplot(eda_year, aes(year, mean_happy)) +
  geom_line() +
  labs(
    title = "Mean Happiness Over Time (1980–2024)",
    x = "Survey year",
    y = "Weighted mean happiness (1–3)"
  )

plot_time


# financial satisfaction eda
df_gap <- df_happy%>%
  group_by(year, satfin_f) %>%
  summarize(
    mean_happy = weighted.mean(happy, wt),
    n = n(),
    .groups = "drop"
  )

plot_gap <- ggplot(df_gap, aes(year, mean_happy, color = satfin_f)) +
  geom_line() +
  labs(
    title = "Mean Happiness by Financial Satisfaction",
    x = "Survey year",
    y = "Weighted mean happiness (1–3)",
    color = "Financial satisfaction"
  )

plot_gap


# regression model
rhs_controls <- c("age", "educ", "incom16", "sex", "race", "marital", "reg16")
fml <- as.formula(
  paste("happy ~ satfin * year_c +", paste(rhs_controls, collapse = " + "))
)

fit_main <- lm(fml, data = df_happy, weights = wt)
summary(fit_main)


wmean2 <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  weighted.mean(x[ok], w[ok])
}

wmean2(df_happy$age, df_happy$wt)
wmean2(df_happy$educ, df_happy$wt)
wmean2(df_happy$incom16, df_happy$wt)


# Monte Carlo
year_seq <- seq(0, 40, by = 5)

grid <- crossing(
  satfin = c(1, 3),
  year_c = year_seq
) %>%
  mutate(
    age = wmean2(df_happy$age, df_happy$wt),
    educ = wmean2(df_happy$educ, df_happy$wt),
    incom16 = wmean2(df_happy$incom16, df_happy$wt),
    sex = factor(levels(df_happy$sex)[1], levels = levels(df_happy$sex)),
    race = factor(levels(df_happy$race)[1], levels = levels(df_happy$race)),
    marital = factor(levels(df_happy$marital)[1], levels = levels(df_happy$marital)),
    reg16 = factor(levels(df_happy$reg16)[1], levels = levels(df_happy$reg16))
  )

# design matrix 
X <- model.matrix(delete.response(terms(fml)), data = grid)

dim(X)
length(coef(fit_main))

beta_hat <- coef(fit_main)
V_hat <- vcov(fit_main)

simB <- mvrnorm(n = 2000, mu = beta_hat, Sigma = V_hat)
pred_sims <- X %*% t(simB)

qs <- t(apply(pred_sims, 1, quantile, probs = c(0.05, 0.5, 0.95)))
grid$lower <- qs[,1]
grid$median <- qs[,2]
grid$upper <- qs[,3]
grid$year <- grid$year_c + 1980
grid$satfin <- factor(grid$satfin, levels = c(1,3),
                      labels = c("Not satisfied", "Satisfied"))

p_mc <- ggplot(grid, aes(year, median, color = satfin)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = satfin),
              alpha = 0.2, color = NA) +
  labs(title = "Predicted Happiness by Financial Satisfaction",
       x = "Survey year", y = "Predicted happiness (1–3)",
       color = "Financial satisfaction", fill = "Financial satisfaction")

p_mc


fit_no_int <- lm(
  happy ~ satfin + year_c + age + educ + incom16 + sex + race + marital + reg16,
  data = df_happy, weights = wt
)

# Compare models
anova(fit_no_int, fit_main)

# AIC comparison
AIC(fit_no_int, fit_main)

comp <- data.frame(
  model = c("No interaction", "With interaction"),
  AIC = c(AIC(fit_no_int), AIC(fit_main)),
  R2 = c(summary(fit_no_int)$r.squared, summary(fit_main)$r.squared),
  AdjR2 = c(summary(fit_no_int)$adj.r.squared, summary(fit_main)$adj.r.squared),
  n = c(nobs(fit_no_int), nobs(fit_main))
)
comp

# if satfin a factor?
fit_satfin_factor <- lm(
  happy ~ satfin_f * year_c + age + educ + incom16 + sex + race + marital + reg16,
  data = df_happy, weights = wt
)

summary(fit_satfin_factor)
AIC(fit_main, fit_satfin_factor)

# change model, satfin_factor one actually better - redo mcmc
beta_hat_f <- coef(fit_satfin_factor)
V_hat_f <- vcov(fit_satfin_factor)

year_seq <- seq(0, 40, by = 5)

grid_f <- tidyr::crossing(
  satfin_f = factor(c("Not satisfied", "Satisfied"),
                    levels = levels(df_happy$satfin_f)),
  year_c = year_seq
) %>%
  mutate(
    age = wmean2(df_happy$age, df_happy$wt),
    educ = wmean2(df_happy$educ, df_happy$wt),
    incom16 = wmean2(df_happy$incom16, df_happy$wt),
    sex = factor(levels(df_happy$sex)[1], levels = levels(df_happy$sex)),
    race = factor(levels(df_happy$race)[1], levels = levels(df_happy$race)),
    marital = factor(levels(df_happy$marital)[1], levels = levels(df_happy$marital)),
    reg16 = factor(levels(df_happy$reg16)[1], levels = levels(df_happy$reg16))
  )

X_f <- model.matrix(delete.response(terms(fit_satfin_factor)), data = grid_f)

simB_f <- MASS::mvrnorm(n = 2000, mu = beta_hat_f, Sigma = V_hat_f)
pred_sims_f <- X_f %*% t(simB_f)

qs_f <- t(apply(pred_sims_f, 1, quantile, probs = c(0.05, 0.5, 0.95)))
grid_f$lower <- qs_f[,1]
grid_f$median <- qs_f[,2]
grid_f$upper <- qs_f[,3]
grid_f$year <- grid_f$year_c + 1980

p_mc_f <- ggplot(grid_f, aes(year, median, color = satfin_f)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = satfin_f),
              alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Happiness by Financial Satisfaction (Factor Model)",
    x = "Survey year",
    y = "Predicted happiness (1–3)",
    color = "Financial satisfaction",
    fill = "Financial satisfaction"
  )

p_mc_f


df_new <- df_happy %>%
  mutate(
    post_2000 = if_else(year >= 2000, 1, 0),
    post_2010 = if_else(year >= 2010, 1, 0),
    post_2020 = if_else(year >= 2020, 1, 0)
  )

# covid analysis
fit_covid <- lm(
  happy ~ post_2020 + age + educ + incom16 + sex + race + marital + reg16,
  data = df_new,
  weights = wt
)

summary(fit_covid)

fit_covid_interaction <- lm(
  happy ~ satfin_f * post_2020 +
    age + educ + incom16 + sex + race + marital + reg16,
  data = df_new,
  weights = wt
)

summary(fit_covid_interaction)
AIC(fit_satfin_factor, fit_covid_interaction)


# Pre-2000 vs Post-2000
fit_2000 <- lm(
  happy ~ satfin_f * post_2000 +
    age + educ + incom16 + sex + race + marital + reg16,
  data = df_new,
  weights = wt
)

summary(fit_2000)
AIC(fit_satfin_factor, fit_2000)

# Pre-2010 vs Post-2010
fit_2010 <- lm(
  happy ~ satfin_f * post_2010 +
    age + educ + incom16 + sex + race + marital + reg16,
  data = df_new,
  weights = wt
)

summary(fit_2010)
AIC(fit_satfin_factor, fit_2010)

# Pre-2020 vs Post-2020
fit_2020 <- lm(
  happy ~ satfin_f * post_2020 +
    age + educ + incom16 + sex + race + marital + reg16,
  data = df_new,
  weights = wt
)

summary(fit_2020)

model_compare <- data.frame(
  model = c("Main factor model",
            "COVID interaction",
            "Post-2000 interaction",
            "Post-2010 interaction"),
  AIC = c(
    AIC(fit_satfin_factor),
    AIC(fit_covid_interaction),
    AIC(fit_2000),
    AIC(fit_2010)
  )
)

model_compare


saveRDS(
  list(
    main = fit_satfin_factor,
    covid = fit_covid_interaction,
    post2000 = fit_2000,
    post2010 = fit_2010
  ),
  "final_models.rds"
)

# bayesian
library(brms)
keep <- c("happy","satfin_f","year_c","age","educ","incom16","sex","race","marital","reg16","wt")
df_bayes <- df_happy[, keep]

# drop missing and nonpositive weights
df_bayes <- df_bayes[complete.cases(df_bayes) & is.finite(df_bayes$wt) & df_bayes$wt > 0, ]

dim(df_bayes)
head(df_bayes)

# prior
library(brms)
priors <- c(
  prior(normal(1.7, 0.5), class = "Intercept"),
  prior(normal(0, 0.2), class = "b"),
  prior(exponential(1), class = "sigma")
)

fit_brms_unw <- brm(
  happy ~ satfin_f + year_c + age + educ + incom16 + sex + race + marital + reg16,
  data = df_bayes,
  family = gaussian(),
  prior = priors,
  chains = 1, iter = 1200, warmup = 600,
  cores = 1,
  seed = 506,
  control = list(adapt_delta = 0.95)
)

posterior_summary(
  fit_brms_unw,
  variable = c("b_satfin_fMore or less", "b_satfin_fSatisfied")
)

bayes_R2(fit_brms_unw)

# ht
fit_factor_no_int <- lm(
  happy ~ satfin_f + year_c + age + educ + incom16 +
    sex + race + marital + reg16,
  data = df_happy,
  weights = wt
)

anova(fit_factor_no_int, fit_satfin_factor)

