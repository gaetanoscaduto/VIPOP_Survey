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


setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Scaduto et al 2025a/descriptive/")

#CHECK THE STATE OF THE COUNTRY VARIABLE
#data |> filter(country=="IT")


contexts = c("IT", "FR", "CZ", "SW")

for (context in contexts)
{
  
  data = readRDS(paste0(dataset_rep, "data_recoded_", context, ".RDS"))
  
  data$AGE_GROUP_r = ifelse(data$AGE_GROUP == "over60", "Over 60",
                            ifelse(data$AGE_GROUP== "under35", "Under 35",
                                   "Between 35 and 59"))
  
  data$IDEOLOGY_REC_r = ifelse(data$IDEOLOGY_REC == "left", "Left (0-3)",
                               ifelse(data$IDEOLOGY_REC == "right", "Right (7-10)",
                                      ifelse(data$IDEOLOGY_REC == "center", "Center 4-6",
                                             data$IDEOLOGY_REC == "Refuses to collocate"
                                             )
                                      )
                               )
  # 
  # descriptive <- data |> 
  #   select(gender, AGE_GROUP_r, EDU_LEVEL, region_feel, citysize, socialposition) |>
  #   tbl_summary(
  #     missing = "ifany",
  #     digits = everything() ~ 1, 
  #     label = list(
  #       gender ~ "Gender",
  #       AGE_GROUP_r ~ "Age",
  #       EDU_LEVEL ~ "Education",
  #       region_feel ~ "Macroegion",
  #       citysize ~ "Size of the city",
  #       socialposition ~ "Placement on 0-10 social scale"
  #     )
  #   )
  # 
  # 
  # descriptive |>
  #   as_gt() |>
  #   gtsave(filename = paste0("Descriptive_general.docx", "_", context, ".docx"))
  # 
  
  ### Descriptives for the parallel design experiment
  data1 = data |> 
    select(gender, AGE_GROUP_r, EDU_LEVEL, region_feel, 
           TIPI_CON_REC, TIPI_OPE_REC,
           diet, animal, holiday,
           ideology_r)
  
  data1 <- data1 %>%
    mutate(across(everything(), ~ str_to_title(as.character(.))))
  
  
  
  descriptive <- data1 |> 
    select(gender, AGE_GROUP_r, EDU_LEVEL, region_feel, 
           TIPI_CON_REC, TIPI_OPE_REC,
           diet, animal, holiday,
           ideology_r) |>
    tbl_summary(
      missing = "ifany",
      digits = everything() ~ 1, 
      label = list(
        gender ~ "Gender",
        AGE_GROUP_r ~ "Age group",
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
    gtsave(filename = paste0("Descriptive_parallel.docx", "_", context, ".docx"))
  
  
  data = readRDS(paste0(dataset_rep, "cjdata_cpd_", context, ".RDS"))
  
  data$AGE_GROUP_r = ifelse(data$AGE_GROUP == "over60", "Over 60",
                            ifelse(data$AGE_GROUP== "under35", "Under 35",
                                   "Between 35 and 59"))
  
  
  descriptive <- data |> 
    select(cpd_match_gender, cpd_match_age, cpd_match_educ, cpd_match_regionfeel, 
           cpd_match_consc, cpd_match_ope,
           cpd_match_diet, cpd_match_animal, cpd_match_holiday,
           cpd_match_ideology) |>
    tbl_summary(
      missing = "ifany",
      digits = everything() ~ 1, 
      label = list(
        cpd_match_gender ~ "Gender",
        cpd_match_age ~ "Age group",
        cpd_match_educ ~ "Has a university degree",
        cpd_match_regionfeel ~ "Region felt the closest to",
        cpd_match_consc ~ "Conscientiousness",
        cpd_match_ope ~ "Openness",
        cpd_match_diet ~ "Diet",
        cpd_match_animal ~ "Favorite pet",
        cpd_match_holiday ~ "Favorite holiday",
        cpd_match_ideology ~ "Political ideology"
      )
    )
  
  
  descriptive |>
    as_gt() |>
    gtsave(filename = paste0("Descriptive_match.docx", "_", context, ".docx"))
  
  descriptive <- data |> 
    select(cpd_gender, cpd_age, cpd_educ, cpd_regionfeel, 
           cpd_consc, cpd_ope,
           cpd_diet, cpd_animal, cpd_holiday,
           cpd_ideology) |>
    tbl_summary(
      missing = "ifany",
      digits = everything() ~ 1, 
      label = list(
        cpd_gender ~ "Gender",
        cpd_age ~ "Age group",
        cpd_educ ~ "Has a university degree",
        cpd_regionfeel ~ "Region felt the closest to",
        cpd_consc ~ "Conscientiousness",
        cpd_ope ~ "Openness",
        cpd_diet ~ "Diet",
        cpd_animal ~ "Favorite pet",
        cpd_holiday ~ "Favorite holiday",
        cpd_ideology ~ "Political ideology"
      )
    )
  
  
  descriptive |>
    as_gt() |>
    gtsave(filename = paste0("Descriptive_cpd_attributes", "_", context, ".docx"))
  
}

