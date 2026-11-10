#library 
library(data.table)

# folder with the extracted raw csv files
dir_raw <- "C:/Users/joeha/Documents/Third Year/EC338 Micro Metrics/Assignment 1"
dir_out <- dir_raw

lux    <- c(442, 442000)  
maxsib <- 10               

# return a column as numeric, or NA if this wave does not have it
getv <- function(d, v) {
  if (v %in% names(d)) return(suppressWarnings(as.numeric(d[[v]])))
  rep(NA_real_, nrow(d))
}

# school and class numbers come as 17 or 100017, so only keep the last three digits
last3 <- function(x) ifelse(x >= 1000, x %% 1000, x)

# names that differ between waves (old name -> new name) to match them up

old <- c("schoolno", "classno", "sampleweights", "famcar", "bedroom", "holidays",
         "computers", "sleepdificulty", "teachertust")
new <- c("id2", "id3", "weight", "fasfamcar", "fasbedroom", "fasholidays",
         "fascomputers", "sleepdifficulty", "teachertrust")

# variables kept, the rest are dropped
keep <- c("wave", "countryno", "respid", "id2", "id3", "weight",
          "sex", "age", "agecat", "grade", "monthbirth", "yearbirth",
          # mental health
          "lifesat", "health", "headache", "stomachache", "backache", "feellow",
          "irritable", "nervous", "sleepdifficulty", "dizzy", "thinkbody",
          # family and friends
          "talkfather", "talkmother", "talkstepfa", "talkstepmo", "talkbrother",
          "talksister", "talkbestfriend", "talksamesex", "talkopposex",
          "famhelp", "famsup", "famtalk", "famdec",
          "friendhelp", "friendcounton", "friendshare", "friendtalk",
          "malefriends", "femalefriends", "friendsafternoon", "friendsevening",
          # school and bullying
          "likeschool", "schoolpressure", "studtogether", "studhelpful", "studaccept",
          "teacheraccept", "teachercare", "teachertrust", "acachieve",
          "beenbullied", "bulliedothers",
          # time use
          "tvwd", "tvwe", "playgamewd", "playgamewe", "compusewd", "compusewe", "physact60",
          # household
          "motherhome1", "fatherhome1", "stepmohome1", "stepfahome1", "grandmohome1",
          "grandfahome1", "fosterhome1", "elsehome1", "brothershome1", "sistershome1",
          "havehome2", "stayhome2", "brothershome2", "sistershome2",
          # background
          "fasfamcar", "fasbedroom", "fasholidays", "fascomputers", "fasbathroom",
          "fasdishwash", "welloff", "occupsesfa", "occupsesmo", "employfa", "employmo",
          "mfses", "mmses", "cntryborn_num", "cntrybornmo_num", "cntrybornfa_num")

# data files for the three waves, in the folder or any subfolder, codebooks excluded
files <- list.files(dir_raw, full.names = TRUE, recursive = TRUE)
files <- files[grepl("HBSC(2006|2010|2014)", basename(files)) &
                 !grepl("\\.(pdf|zip|docx?)$", files, ignore.case = TRUE)]
if (length(files) == 0) stop("no HBSC 2006, 2010 or 2014 data files in ", dir_raw)
yrs <- sub(".*HBSC([0-9]{4}).*", "\\1", basename(files))
if (anyDuplicated(yrs)) stop("")
if (!all(c("2006", "2010", "2014") %in% yrs))
  stop("missing wave(s): ", paste(setdiff(c("2006", "2010", "2014"), yrs), collapse = ", "))

# read one wave, only the columns we need (csv, or Excel if the file is .xlsx/.xls)
want <- unique(c(keep, old, "countryno", "uniqueid", "seqno_int", "classid"))
readwave <- function(f) {
  if (tolower(tools::file_ext(f)) %in% c("xlsx", "xls")) {
    d <- as.data.table(readxl::read_excel(f))
    setnames(d, tolower(names(d)))
    return(d[, intersect(names(d), want), with = FALSE])
  }
  hdr <- names(fread(f, nrows = 0))
  d <- fread(f, select = hdr[tolower(hdr) %in% want])
  setnames(d, tolower(names(d)))
  d
}

waves <- list()

for (f in files) {
  wv <- as.integer(sub(".*HBSC([0-9]{4}).*", "\\1", basename(f)))
  message("Reading ", basename(f))
  
  d <- readwave(f)
  if (!"countryno" %in% names(d)) stop("no countryno column in ", basename(f))
  codes <- unique(d$countryno)
  n_all <- nrow(d)
  d <- d[countryno %in% lux]
  if (nrow(d) == 0)
    stop("no Luxembourg rows in ", basename(f), ", country codes in the file: ",
         paste(head(sort(codes), 15), collapse = ", "))
  set(d, j = "wave", value = wv)
  
  rid <- if ("uniqueid" %in% names(d)) d$uniqueid else d$seqno_int
  if (is.null(rid)) stop("no respondent id in ", basename(f))
  set(d, j = "respid", value = as.character(rid))
  
  # in 2014 the long class code is a fixed prefix plus id3
  if (wv == 2014) {
    cl <- getv(d, "classid")
    if (length(unique(na.omit(cl - getv(d, "id3")))) > 1)
      warning(wv, ": class code and id3 do not match")
  }
  
  setnames(d, old, new, skip_absent = TRUE)
  d <- d[, intersect(keep, names(d)), with = FALSE]
  
  # numbers written with a decimal comma (11,83) are read as text, turn them into numbers
  for (v in names(d)) {
    x <- d[[v]]
    if (is.character(x)) {
      y <- suppressWarnings(as.numeric(sub(",", ".", x)))
      if (all(!is.na(y) | is.na(x) | x == "")) set(d, j = v, value = y)
    }
  }
  
  # school and class ids, 999 means not recorded so record these as empty
  s0 <- getv(d, "id2")
  c0 <- getv(d, "id3")
  s1 <- last3(s0)
  c1 <- last3(c0)
  if (nrow(unique(data.frame(s0, c0))) != nrow(unique(data.frame(s1, c1))))
    warning(wv, ": keeping the last three digits merges different classes")
  s1[which(s1 == 999)] <- NA
  c1[which(c1 == 999)] <- NA
  set(d, j = "id2", value = s1)
  set(d, j = "id3", value = c1)
  set(d, j = "class_id",
      value = ifelse(is.na(s1) | is.na(c1), NA_character_, paste(wv, s1, c1, sep = "_")))
  
  # girl dummy and share of girls among classmates (own sex excluded)
  sex  <- getv(d, "sex")
  girl <- ifelse(sex == 2, 1, ifelse(sex == 1, 0, NA_real_))
  set(d, j = "girl", value = girl)
  set(d, j = "class_n", value = NA_integer_)
  set(d, j = "class_girls", value = NA_real_)
  d[!is.na(class_id), `:=`(class_n = sum(!is.na(girl)),
                           class_girls = sum(girl, na.rm = TRUE)),
    by = class_id]
  set(d, j = "class_girlshare",
      value = ifelse(!is.na(girl) & !is.na(d$class_n) & d$class_n >= 2,
                     (d$class_girls - girl) / (d$class_n - 1), NA_real_))
  set(d, j = "class_girls", value = NULL)
  
  # brothers and sisters living at home
  nbro <- getv(d, "brothershome1")
  nsis <- getv(d, "sistershome1")
  toobig <- which(nbro + nsis > maxsib)
  nbro[toobig] <- NA
  nsis[toobig] <- NA
  nsib <- nbro + nsis
  set(d, j = "nbro", value = nbro)
  set(d, j = "nsis", value = nsis)
  set(d, j = "nsib", value = nsib)
  set(d, j = "brothershare", value = ifelse(nsib > 0, nbro / nsib, NA_real_))
  
  # symptom scores, items reversed so that higher means more frequent
  psy  <- do.call(cbind, lapply(c("feellow", "irritable", "nervous", "sleepdifficulty"),
                                function(v) 6 - getv(d, v)))
  soma <- do.call(cbind, lapply(c("headache", "stomachache", "backache", "dizzy"),
                                function(v) 6 - getv(d, v)))
  set(d, j = "psych_sym", value = rowSums(psy))
  set(d, j = "soma_sym", value = rowSums(soma))
  
  # family affluence score, 0 to 9
  fas <- (getv(d, "fasfamcar") - 1) + (getv(d, "fasbedroom") - 1) +
    (getv(d, "fasholidays") - 1) + (getv(d, "fascomputers") - 1)
  set(d, j = "fas_sum", value = fas)
  
  waves[[length(waves) + 1]] <- d
}

dat <- rbindlist(waves, use.names = TRUE, fill = TRUE)

first <- c("wave", "countryno", "respid", "id2", "id3", "class_id", "sex", "age", "agecat",
           "grade", "weight", "girl", "class_n", "class_girlshare", "nbro", "nsis", "nsib",
           "brothershare", "psych_sym", "soma_sym", "lifesat", "health", "fas_sum")
setcolorder(dat, intersect(first, names(dat)))
setorder(dat, wave)

# basic checks
stopifnot(!any(duplicated(dat[, .(wave, respid)])))
stopifnot(!any(dat$id2 == 999 | dat$id3 == 999, na.rm = TRUE))
print(dat[, .N, by = wave])

# share of non-missing values for every variable and wave
a <- dat[, lapply(.SD, function(x) round(mean(!is.na(x)), 2)), by = wave]
m <- t(as.matrix(a[, !"wave"]))
colnames(m) <- a$wave
print(m)

# save
out <- file.path(dir_out, "HBSC_Luxembourg_master.csv")
ok  <- tryCatch({ fwrite(dat, out); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)
if (!ok) {
  out <- sub("\\.csv$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"), out)
  fwrite(dat, out)
}
message("Saved ", out)