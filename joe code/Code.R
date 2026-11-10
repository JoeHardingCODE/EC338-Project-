library(dplyr)
library(fixest)

mode_val <- function(x) as.numeric(names(which.max(table(x))))

# Load data
data <- HBSC_Luxembourg_master

if (!"wave" %in% names(data)) {
  names(data) <- make.names(trimws(as.character(unlist(data[1, ]))), unique = TRUE)
  data <- data[-1, ]
}

data <- data %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.x))))) %>%
  filter(wave %in% c(2006, 2010, 2014)) %>%
  mutate(
    across(c(irritable, nervous, sleepdifficulty, feellow), ~ 6 - .x),
    mental_health = (irritable + nervous + sleepdifficulty + feellow) / 4,
    girl = as.numeric(sex == 2),
    sib_total = brothershome1 + sistershome1,
    brother_share = ifelse(sib_total > 0, brothershome1 / sib_total, NA_real_),
    fas = fasfamcar + fasbedroom + fasholidays + fascomputers,
    two_parents = as.numeric(motherhome1 == 1 & fatherhome1 == 1),
    school_id = paste(wave, id2)
  )

# Samples
sibling_data <- filter(data, !is.na(brother_share))

class_data <- data %>%
  filter(!is.na(id2), !is.na(id3), id2 != 999, id3 != 999,
         !is.na(girl), !is.na(agecat)) %>%
  group_by(wave, id2, id3) %>%
  filter(n() > 1) %>%
  mutate(class_id = paste(wave, id2, id3),
         class_n = n(),
         class_age = mode_val(agecat),
         class_girl_share_loo = (sum(girl) - girl) / (n() - 1)) %>%
  ungroup() %>%
  mutate(cell = paste(wave, id2, class_age)) %>%
  group_by(cell) %>%
  mutate(cell_girl_loo = (sum(girl) - girl) / (n() - 1)) %>%
  ungroup()

# Descriptive statistics
desc <- function(d, vars) {
  sapply(d[vars], function(x) c(N = sum(!is.na(x)), Mean = mean(x, na.rm = TRUE),
                                SD = sd(x, na.rm = TRUE)))
}
desc(sibling_data, c("mental_health", "girl", "age", "sib_total", "brother_share"))
desc(class_data, c("mental_health", "girl", "age", "class_girl_share_loo", "class_n"))
n_distinct(class_data$class_id)

# Sibling gender: mean vs. share of boys at birth
p_boy <- 105 / 205
t.test(sibling_data$brother_share, mu = p_boy)
sibling_data %>%
  group_by(wave) %>%
  summarise(mean = mean(brother_share), p = t.test(brother_share, mu = p_boy)$p.value)
binom.test(sum(sibling_data$brothershome1), sum(sibling_data$sib_total), p_boy)

# Balance tests on predetermined characteristics
bal_vars <- c("girl", "age", "fas", "two_parents")
balance <- function(d, x, fe, cl) {
  lapply(bal_vars, function(v)
    feols(as.formula(paste(v, "~", x, "|", fe)), d, cluster = as.formula(paste0("~", cl))))
}
etable(balance(sibling_data, "brother_share", "sib_total + wave", "school_id"))
etable(balance(class_data, "class_girl_share_loo + cell_girl_loo", "cell", "class_id"))

# Classroom gender: observed vs. random allocation within school x age cell
class_sd <- function(d) {
  cl <- d %>%
    group_by(cell, class_id) %>%
    summarise(share = mean(girl), .groups = "drop") %>%
    group_by(cell) %>%
    filter(n() > 1) %>%
    ungroup()
  sd(resid(feols(share ~ 1 | cell, cl)))
}

sim_sd <- function(d, B = 999) {
  replicate(B, class_sd(d %>% group_by(cell) %>% mutate(girl = sample(girl)) %>% ungroup()))
}

perm_p <- function(obs, sim) {
  (1 + sum(abs(sim - mean(sim)) >= abs(obs - mean(sim)))) / (length(sim) + 1)
}

set.seed(338)
samples <- list("All classes" = class_data,
                "Age 11 classes" = filter(class_data, class_age == 1))

class_randomness <- do.call(rbind, lapply(names(samples), function(s) {
  obs <- class_sd(samples[[s]])
  sim <- sim_sd(samples[[s]])
  data.frame(Sample = s, Observed_SD = obs, Simulated_SD = mean(sim),
             Lower_95 = quantile(sim, .025), Upper_95 = quantile(sim, .975),
             P_value = perm_p(obs, sim), row.names = NULL)
}))
class_randomness