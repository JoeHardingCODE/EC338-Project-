library(dplyr)
library(fixest)

p_boy <- 105 / 205

#Sibling gender tests

# pooled test of the share of brothers against the proportion share of boys at birth
binom.test(sum(sibling_data$brothershome1), sum(sibling_data$sib_total), p_boy)

# same test with a binomial goodness of fit within each family size
sibling_data %>%
  group_by(sib_total) %>%
  summarise(N = n(),
            mean_share = mean(brother_share),
            p_mean = binom.test(sum(brothershome1), sum(sib_total), p_boy)$p.value,
            p_dist = chisq.test(tabulate(brothershome1 + 1, first(sib_total) + 1),
                                p = dbinom(0:first(sib_total), first(sib_total), p_boy),
                                simulate.p.value = TRUE)$p.value) %>%
  as.data.frame()

# balance regressions, one characteristic at a time
bal_sib <- c("girl", "age", "fas", "two_parents")
etable(lapply(bal_sib, function(v)
  feols(as.formula(paste(v, "~ brother_share | sib_total + wave")),
        sibling_data, cluster = ~school_id)))

# joint test. do predetermined characteristics predict brother share.?
m_sib <- feols(brother_share ~ girl + age + fas + two_parents | sib_total + wave,
               sibling_data, cluster = ~school_id)
wald(m_sib, keep = "girl|age|fas|two_parents")

# Class gender composition

# balance regressions (girl is left out, it is colinear with cell_girl_loo)
bal_cl <- c("age", "fas", "two_parents")
etable(lapply(bal_cl, function(v)
  feols(as.formula(paste(v, "~ class_girl_share_loo + cell_girl_loo | cell")),
        class_data, cluster = ~class_id)))

# joint test
m_cl <- feols(class_girl_share_loo ~ age + fas + two_parents + cell_girl_loo | cell,
              class_data, cluster = ~class_id)
wald(m_cl, keep = "age|fas|two_parents")

# observed girls per class vs mean and variance
chi_stat <- function(d) {
  s <- d %>%
    group_by(cell) %>%
    mutate(N = n(), G = sum(girl)) %>%
    group_by(cell, class_id) %>%
    summarise(n_c = n(), G_c = sum(girl), N = first(N), G = first(G), .groups = "drop") %>%
    mutate(e = n_c * G / N,
           v = n_c * (G / N) * (1 - G / N) * (N - n_c) / (N - 1)) %>%
    filter(v > 0) %>%
    group_by(cell) %>%
    filter(n() > 1) %>%
    ungroup()
  c(chi = sum((s$G_c - s$e)^2 / s$v), df = nrow(s))
}

samples <- list("All classes" = class_data,
                "Age 11 classes" = filter(class_data, class_age == 1))

# p value
do.call(rbind, lapply(names(samples), function(s) {
  r <- chi_stat(samples[[s]])
  data.frame(Sample = s, chi = r["chi"], df = r["df"],
             p_value = 2 * min(pchisq(r["chi"], r["df"]),
                               1 - pchisq(r["chi"], r["df"])),
             row.names = NULL)
}))

# permutation p value for the same statistic, shuffling girl within cell ???? 
set.seed(338)
do.call(rbind, lapply(names(samples), function(s) {
  d <- samples[[s]]
  obs <- chi_stat(d)["chi"]
  sim <- replicate(999, chi_stat(d %>% group_by(cell) %>%
                                   mutate(girl = sample(girl)) %>% ungroup())["chi"])
  data.frame(Sample = s, Observed = obs, Simulated_mean = mean(sim),
             P_value = (1 + sum(abs(sim - mean(sim)) >= abs(obs - mean(sim)))) / 1000,
             row.names = NULL)
}))