library(dplyr)
library(fixest)

# cap family size, as in the dataset build
sibling_data <- filter(sibling_data, sib_total <= 10)

# age 11 classes only
class11 <- filter(class_data, class_age == 1)

# keep only students with complete data, so both columns use the same sample
sib_c <- filter(sibling_data, !is.na(mental_health), !is.na(girl), !is.na(age), !is.na(fas))
cl_c <- filter(class11, !is.na(mental_health), !is.na(age), !is.na(fas))

# sibling gender: family size and wave fixed effects, clustered by school
sib1 <- feols(mental_health ~ brother_share | sib_total + wave,
              sib_c, cluster = ~school_id)
sib2 <- feols(mental_health ~ brother_share + girl + age | sib_total + wave,
              sib_c, cluster = ~school_id)

# class gender composition: cell fixed effects, clustered by class
cl1 <- feols(mental_health ~ girl + class_girl_share_loo + cell_girl_loo | cell,
             cl_c, cluster = ~class_id)
cl2 <- feols(mental_health ~ girl +class_girl_share_loo + cell_girl_loo + age | cell,
             cl_c, cluster = ~class_id)

etable(sib1, sib2, cl1, cl2)

# heterogeneity by own sex: interaction models
sib_h1 <- feols(mental_health ~ brother_share * girl | sib_total + wave,
                sib_c, cluster = ~school_id)
sib_h2 <- feols(mental_health ~ brother_share * girl + age | sib_total + wave,
                sib_c, cluster = ~school_id)

cl_h1 <- feols(mental_health ~ girl + class_girl_share_loo + class_girl_share_loo:girl +
                 cell_girl_loo + cell_girl_loo:girl | cell,
               cl_c, cluster = ~class_id)
cl_h2 <- feols(mental_health ~ girl + class_girl_share_loo + class_girl_share_loo:girl +
                 cell_girl_loo + cell_girl_loo:girl + age  | cell,
               cl_c, cluster = ~class_id)

etable(sib_h1, sib_h2, cl_h1, cl_h2)

# effect for boys and for girls separately
etable(feols(mental_health ~ brother_share + age | sib_total + wave,
             sib_c, cluster = ~school_id, split = ~girl))
etable(feols(mental_health ~ class_girl_share_loo + age | cell,
             cl_c, cluster = ~class_id, split = ~girl))