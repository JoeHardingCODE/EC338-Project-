library(data.table)
setDT(data)

# Share of sisters among siblings
data[, sistershare := ifelse(nsib > 0, nsis / nsib, NA_real_)]

# Main variables
gender_vars <- c("sistershare", "class_girlshare")


# ==================================================
# 1. SUMMARY STATISTICS
# ==================================================

# Mental health: mean, SD, median, observed min/max and N
stats_main <- function(x) {
  x <- na.omit(x)
  
  c(
    mean = mean(x),
    sd = sd(x),
    median = median(x),
    min = min(x),
    max = max(x),
    N = length(x)
  )
}

# Gender composition: mean, SD, quartiles, median,
# observed min/max and N
stats_gender <- function(x) {
  x <- na.omit(x)
  
  q <- quantile(x, c(0.25, 0.50, 0.75), names = FALSE)
  
  c(
    mean = mean(x),
    sd = sd(x),
    min = min(x),
    p25 = q[1],
    median = q[2],
    p75 = q[3],
    max = max(x),
    N = length(x)
  )
}


# ==================================================
# 2. OVERALL STATISTICS
# ==================================================

cat("\nOVERALL DESCRIPTIVE STATISTICS\n")

cat("\nMental health\n")
print(round(stats_main(data$mental_health), 2))

cat("\nSibling and classroom gender composition\n")
print(round(t(sapply(
  gender_vars,
  function(v) stats_gender(data[[v]])
)), 2))


# ==================================================
# 3. STATISTICS BY SURVEY WAVE
# ==================================================

for (w in c(2006, 2010, 2014)) {
  
  wave_data <- data[wave == w]
  
  cat("\n====================================\n")
  cat("WAVE:", w, "\n")
  cat("====================================\n")
  
  cat("\nMental health\n")
  print(round(stats_main(wave_data$mental_health), 2))
  
  cat("\nSibling and classroom gender composition\n")
  print(round(t(sapply(
    gender_vars,
    function(v) stats_gender(wave_data[[v]])
  )), 2))
}


# ==================================================
# 4. FEMALE RESPONDENTS (SEX = 2)
# ==================================================

cat("\nFEMALE RESPONDENTS BY WAVE\n")

print(data[, .(
  respondents = .N,
  female = sum(sex == 2, na.rm = TRUE),
  female_percentage = mean(sex == 2, na.rm = TRUE) * 100
), by = wave])


# ==================================================
# 5. SAMPLE COMPOSITION BY WAVE
# ==================================================

cat("\nSAMPLE COMPOSITION BY WAVE\n")

print(data[, .(
  respondents = .N,
  schools = uniqueN(id2, na.rm = TRUE)
), by = wave])


# ==================================================
# 6. CLASS STATISTICS BY WAVE
# ==================================================

classes <- data[
  !is.na(id2) & !is.na(id3),
  .(responses = .N),
  by = .(wave, id2, id3)
]

cat("\nCLASS STATISTICS BY WAVE\n")

print(classes[, .(
  classes = .N,
  avg_responses_per_class = mean(responses)
), by = wave])