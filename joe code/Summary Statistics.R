library(data.table)
setDT(data)

# share of sisters among siblings
data[, sistershare := ifelse(nsib > 0, nsis / nsib, NA_real_)]

# main variables
gender_vars <- c("sistershare", "class_girlshare")


# mental health: mean, sd, median, observed min/max and N obs
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

# gender composition: mean, sd, quartiles, median, min/max and N obs
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


# overall stats



print(round(stats_main(data$mental_health), 2))

print(round(t(sapply(
  gender_vars,
  function(v) stats_gender(data[[v]])
)), 2))



# by wave 


for (w in c(2006, 2010, 2014)) {
  
  wave_data <- data[wave == w]
    print(round(stats_main(wave_data$mental_health), 2))
    print(round(t(sapply(
    gender_vars,
    function(v) stats_gender(wave_data[[v]])
  )), 2))
}


# female by wave 


print(data[, .(
  respondents = .N,
  female = sum(sex == 2, na.rm = TRUE),
  female_percentage = mean(sex == 2, na.rm = TRUE) * 100
), by = wave])



# sample by wave

print(data[, .(
  respondents = .N,
  schools = uniqueN(id2, na.rm = TRUE)
), by = wave])



# class statistics by wave 


classes <- data[
  !is.na(id2) & !is.na(id3),
  .(responses = .N),
  by = .(wave, id2, id3)
]


print(classes[, .(
  classes = .N,
  avg_responses_per_class = mean(responses)
), by = wave])