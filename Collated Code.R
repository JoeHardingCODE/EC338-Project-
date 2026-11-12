library(fixest)

setFixest_nthreads(1)

setwd("C:/Users/joeha/Documents/Third Year/EC338 Micro Metrics/Assignment 1")

# load data
data <- read.csv("HBSC_Luxembourg_master.csv")
data <- data[data$wave %in% c(2006, 2010, 2014), ]

# mental health: four psychological symptoms
# values outside 1-5 are missing, reversed so that higher = more frequent
items <- c("feellow", "irritable", "nervous", "sleepdifficulty")
for (v in items) {
  data[[v]] <- ifelse(data[[v]] %in% 1:5, 6 - data[[v]], NA)
}
# standardised mean of the four items, higher = worse mental health
data$mental_health <- as.numeric(scale(rowMeans(data[items])))

# other variables
data$girl <- ifelse(data$sex == 2, 1, ifelse(data$sex == 1, 0, NA))
data$sib_total <- ifelse(data$brothershome1 %in% 0:10 & data$sistershome1 %in% 0:10,
                         data$brothershome1 + data$sistershome1, NA)
data$sister_share <- ifelse(data$sib_total %in% 1:10, data$sistershome1 / data$sib_total, NA)
# family affluence without the bedroom item, which depends on sibling gender
data$fas <- data$fasfamcar + data$fasholidays + data$fascomputers
data$two_parents <- as.numeric(data$motherhome1 == 1 & data$fatherhome1 == 1)
data$school_id <- ifelse(is.na(data$id2), NA, paste(data$wave, data$id2))

# class data
# share of girls among classmates, excluding the student
cd <- data[!is.na(data$id2) & !is.na(data$id3) & !is.na(data$girl), ]
cd$class_id <- paste(cd$wave, cd$id2, cd$id3)
cd$class_n <- ave(cd$girl, cd$class_id, FUN = length)
# keep classes with enough respondents for the share of girls to be measured precisely
min_class_n <- 5
cd <- cd[cd$class_n >= min_class_n, ]
cd$class_girls <- ave(cd$girl, cd$class_id, FUN = sum)
cd$class_girl_share_loo <- (cd$class_girls - cd$girl) / (cd$class_n - 1)

# cell = school and age group within a wave (grade is missing in every wave)
cd <- cd[!is.na(cd$agecat), ]
cd$cell <- paste(cd$wave, cd$id2, cd$agecat)
cd$cell_n <- ave(cd$girl, cd$cell, FUN = length)
cd$cell_girls <- ave(cd$girl, cd$cell, FUN = sum)
cd$cell_girl_loo <- (cd$cell_girls - cd$girl) / (cd$cell_n - 1)

# students and girls from each class within the cell
cd$n_c <- ave(cd$girl, cd$cell, cd$class_id, FUN = length)
cd$G_c <- ave(cd$girl, cd$cell, cd$class_id, FUN = sum)

# one row per class in a cell
cls <- unique(cd[c("cell", "class_id", "agecat", "n_c", "G_c")])

# estimation samples
sib_vars <- c("mental_health", "girl", "age", "fas", "sib_total", "sister_share")
cl_vars <- c("mental_health", "girl", "age", "fas", "class_n",
             "class_girl_share_loo", "cell_girl_loo")

sib <- data[complete.cases(data[c(sib_vars, "school_id")]), ]
# age 11 (agecat 1)
cl <- cd[complete.cases(cd[cl_vars]) & cd$agecat %in% 1, ]

# recount the classes in each cell in the final sample, keep cells with at least two
cl_cells <- unique(cl[c("cell", "class_id")])
cl_cells$n <- ave(rep(1, nrow(cl_cells)), cl_cells$cell, FUN = sum)
cl$cell_classes <- cl_cells$n[match(paste(cl$cell, cl$class_id),
                                    paste(cl_cells$cell, cl_cells$class_id))]
cl <- cl[cl$cell_classes > 1, ]

# Descriptive statistics
summary(sib[sib_vars])
sapply(sib[sib_vars], sd)
nrow(sib)
length(unique(sib$school_id))
table(sib$wave)

summary(cl[cl_vars])
sapply(cl[cl_vars], sd)
nrow(cl)
length(unique(cl$class_id))
table(cl$wave)

# sibling gender
# share of sisters against the share of girls at birth (105 boys per 100 girls)
p_girl <- 100 / 205
binom.test(sum(sib$sistershome1), sum(sib$sib_total), p_girl)

# same tests within each family size: the mean and the whole distribution
set.seed(338)
sizes <- sort(unique(sib$sib_total))
fam <- data.frame(size = sizes, N = NA, mean_share = NA, p_mean = NA, p_dist = NA)
for (i in seq_along(sizes)) {
  s <- sib[sib$sib_total == sizes[i], ]
  fam$N[i] <- nrow(s)
  fam$mean_share[i] <- mean(s$sister_share)
  fam$p_mean[i] <- binom.test(sum(s$sistershome1), nrow(s) * sizes[i], p_girl)$p.value
  fam$p_dist[i] <- chisq.test(tabulate(s$sistershome1 + 1, sizes[i] + 1),
                              p = dbinom(0:sizes[i], sizes[i], p_girl),
                              simulate.p.value = TRUE)$p.value
}
fam

# balance tests, one characteristic at a time
sb1 <- feols(girl ~ sister_share | sib_total + wave, sib, cluster = ~school_id)
sb2 <- feols(age ~ sister_share | sib_total + wave, sib, cluster = ~school_id)
sb3 <- feols(fas ~ sister_share | sib_total + wave, sib, cluster = ~school_id)
sb4 <- feols(two_parents ~ sister_share | sib_total + wave, sib, cluster = ~school_id)
etable(sb1, sb2, sb3, sb4)

# joint test
sjoint <- feols(sister_share ~ girl + age + fas + two_parents | sib_total + wave,
                sib, cluster = ~school_id)
wald(sjoint, keep = "girl|age|fas|two_parents")

# Class gender: is it as good as random?
# balance tests (girl is left out, it is built into cell_girl_loo)
cb1 <- feols(age ~ class_girl_share_loo + cell_girl_loo | cell, cl, cluster = ~class_id)
cb2 <- feols(fas ~ class_girl_share_loo + cell_girl_loo | cell, cl, cluster = ~class_id)
cb3 <- feols(two_parents ~ class_girl_share_loo + cell_girl_loo | cell, cl, cluster = ~class_id)
etable(cb1, cb2, cb3)

# joint test
cjoint <- feols(class_girl_share_loo ~ age + fas + two_parents + cell_girl_loo | cell,
                cl, cluster = ~class_id)
wald(cjoint, keep = "age|fas|two_parents")

# is the share of girls the same in every class of a cell?
# chi-square test of equal shares across the classes of each cell, summed over cells
cls$N <- ave(cls$n_c, cls$cell, FUN = sum)
cls$G <- ave(cls$G_c, cls$cell, FUN = sum)
cls$p <- cls$G / cls$N
cls <- cls[cls$p > 0 & cls$p < 1, ]
cls$m <- ave(rep(1, nrow(cls)), cls$cell, FUN = sum)
cls$x2 <- (cls$G_c - cls$n_c * cls$p)^2 / (cls$n_c * cls$p * (1 - cls$p))

# a cell with m classes adds m - 1 degrees of freedom
chi <- tapply(cls$x2, cls$agecat, sum)
df <- tapply(1 - 1 / cls$m, cls$agecat, sum)
p_value <- 2 * pmin(pchisq(chi, df), 1 - pchisq(chi, df))
data.frame(age_group = names(chi), df = as.vector(df), chi = as.vector(chi),
           p_value = as.vector(p_value))

# Regressions

sib$brother_share <- 1 - sib$sister_share

sib_reg <- feols(mental_health ~ brother_share + girl + age + fas | sib_total + wave, data = sib, cluster = ~school_id)
cl_reg <- feols(mental_health ~ class_girl_share_loo + cell_girl_loo + girl + age + fas + two_parents | school_id + wave, data = cl, cluster = ~class_id)

etable(sib_reg, cl_reg)

# heterogeneity

sib_het <- feols(mental_health ~ brother_share * girl + age + fas | sib_total + wave, data = sib, cluster = ~school_id)
cl_het <- feols(mental_health ~ class_girl_share_loo * girl + cell_girl_loo + age + fas + two_parents | school_id + wave, data = cl, cluster = ~class_id)

etable(sib_reg, sib_het, cl_reg, cl_het)


# Wald tests

wald(sib_het, keep = "^brother_share$")
wald(sib_het, keep = "^brother_share:girl$")

wald(cl_het, keep = "^class_girl_share_loo$")
wald(cl_het, keep = "^class_girl_share_loo:girl$")
