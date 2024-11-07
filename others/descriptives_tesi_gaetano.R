library(rio)
library(ggplot2)
library(margins)
library(dplyr)
library(corrplot)
library(texreg)
library(patchwork)
library(MASS)
library(gtools)
library(gt)
library(gtsummary)
library(openxlsx)

context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"


dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

# import the dataset with row=respondent

data = readRDS(paste0(dataset_rep, "data_recoded_", context, ".RDS"))

setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Dissertation/Tables/")

#CHECK THE STATE OF THE COUNTRY VARIABLE
data |> filter(country=="IT")

descriptive <- data |> 
  select(gender, age, education, region, citysize, socialposition) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, 
    label = list(
      gender ~ "Gender",
      age ~ "Age",
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

descriptive <- data |> 
  select(gender, AGE_GROUP, EDU_LEVEL, region_feel, 
         TIPI_CON_REC, TIPI_OPE_REC,
         diet, animal, holiday,
         ideology) |>
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
      ideology ~ "Political ideology"
    )
  )


descriptive |>
  as_gt() |>
  gtsave(filename ="Descriptive_parallel.docx")
