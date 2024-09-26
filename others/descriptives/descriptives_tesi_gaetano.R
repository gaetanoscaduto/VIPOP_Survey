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

data = readRDS("data_recoded.RDS")

setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/others/descriptives/Descriptives_tesi_gaetano/")

#CHECK THE STATE OF THE COUNTRY VARIABLE
data |> filter(country=="IT")

descriptive <- data |> 
  select(gender, age, education, macroregion, citysize, socialposition) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, 
    label = list(
      gender ~ "Gender",
      age ~ "Age",
      education ~ "Education",
      macroregion ~ "Macroegion",
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
      holiday ~ "Favorite holiday"
      ideology ~ "Political ideology"
    )
  )


descriptive |>
  as_gt() |>
  gtsave(filename = "Descriptive_parallel.docx")
