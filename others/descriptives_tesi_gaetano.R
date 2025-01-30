library(rio)
library(ggplot2)
library(margins)
library(stringr)
library(dplyr)
library(corrplot)
library(texreg)
library(patchwork)
library(MASS)
library(gtools)
library(gt)
library(gtsummary)
library(openxlsx)

#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
context = "POOL"


dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

# import the dataset with row=respondent

data = readRDS(paste0(dataset_rep, "data_recoded_", context, ".RDS"))

setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Scaduto et al 2025a/")

#CHECK THE STATE OF THE COUNTRY VARIABLE
#data |> filter(country=="IT")

descriptive <- data |> 
  select(gender, age_r, education, region, citysize, socialposition) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, 
    label = list(
      gender ~ "Gender",
      age_r ~ "Age",
      education ~ "Education",
      region ~ "Macroegion",
      citysize ~ "Size of the city",
      socialposition ~ "Placement on 0-10 social scale"
    )
  )


descriptive |>
  as_gt() |>
  gtsave(filename = "Descriptive_general.docx")


### Descriptives for the parallel design experiment
 data1 = data |> 
  select(gender, AGE_GROUP, EDU_LEVEL, region_feel, 
         TIPI_CON_REC, TIPI_OPE_REC,
         diet, animal, holiday,
         ideology_r)

data1 <- data1 %>%
  mutate(across(everything(), ~ str_to_title(as.character(.))))



descriptive <- data1 |> 
  select(gender, AGE_GROUP, EDU_LEVEL, region_feel, 
         TIPI_CON_REC, TIPI_OPE_REC,
         diet, animal, holiday,
         ideology_r) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, 
    label = list(
      gender ~ "Gender",
      AGE_GROUP ~ "Age group",
      EDU_LEVEL ~ "Has a university degree",
      region_feel ~ "Region felt the closest to",
      TIPI_CON_REC ~ "Conscientiousness",
      TIPI_OPE_REC ~ "Openness",
      diet ~ "Diet", 
      animal ~ "Favorite pet",
      holiday ~ "Favorite holiday",
      ideology_r ~ "Political ideology"
    )
  )


descriptive |>
  as_gt() |>
  gtsave(filename ="Descriptive_parallel.docx")


data = readRDS(paste0(dataset_rep, "cjdata_cpd_", context, ".RDS"))


descriptive <- data |> 
  select(cpd_match_gender, cpd_match_age, cpd_match_educ, cpd_match_regionfeel, 
         cpd_match_consc, cpd_match_ope,
         cpd_match_diet, cpd_match_animal, cpd_match_holiday,
         cpd_match_ideology) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, 
    # label = list(
    #   # gender ~ "Gender",
    #   # AGE_GROUP ~ "Age group",
    #   # EDU_LEVEL ~ "Has a university degree",
    #   # region_feel ~ "Region felt the closest to",
    #   # TIPI_CON_REC ~ "Conscientiousness",
    #   # TIPI_OPE_REC ~ "Openness",
    #   # diet ~ "Diet", 
    #   # animal ~ "Favorite pet",
    #   # holiday ~ "Favorite holiday",
    #   # ideology_r ~ "Political ideology"
    # )
  )


descriptive |>
  as_gt() |>
  gtsave(filename ="Descriptive_match.docx")

descriptive <- data |> 
  select(cpd_gender, cpd_age, cpd_educ, cpd_regionfeel, 
         cpd_consc, cpd_ope,
         cpd_diet, cpd_animal, cpd_holiday,
         cpd_ideology) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, 
    # label = list(
    #   # gender ~ "Gender",
    #   # AGE_GROUP ~ "Age group",
    #   # EDU_LEVEL ~ "Has a university degree",
    #   # region_feel ~ "Region felt the closest to",
    #   # TIPI_CON_REC ~ "Conscientiousness",
    #   # TIPI_OPE_REC ~ "Openness",
    #   # diet ~ "Diet", 
    #   # animal ~ "Favorite pet",
    #   # holiday ~ "Favorite holiday",
    #   # ideology_r ~ "Political ideology"
    # )
  )


descriptive |>
  as_gt() |>
  gtsave(filename ="Descriptive_cpd_attributes.docx")
