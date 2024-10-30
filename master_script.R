#Master script

# if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
# pacman::p_load(
#   cregg, dplyr, ggpubr, cowplot, 
#   MASS, cjoint, corrplot, dplyr, 
#   forcats, ggplot2, gt, gtools, 
#   gtsummary, margins, openxlsx, 
#   patchwork, rio, texreg, tools, 
#   lme4, ggeffects, wesanderson
# )


setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/")

dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

#generate pooled dataset
source("rbind_country_datasets.R")

for(context in c("IT", "FR", "CZ", "SW", "POOL"))
{
  ## recoding script
  source("others/recodings/recoding_demo.R")
  
  #descriptives script
  source("others/descriptives/descriptives_internal.R")
  
  #classic conjoint scripts
  source("classic_conjoint/data_manipulation_ccd.R")
  source("classic_conjoint/randomization_checks_ccd.R")
  source("classic_conjoint/analyses_ccd_singlecountry.R")
  
  #visual conjoint scripts
  source("visual_conjoint/scripts/analysis/data_manipulation_vcd.R")
  source("visual_conjoint/scripts/analysis/randomization_checks_vcd.R")
  #
  for(outcome in c("ideology", "trust", "populism"))
  {
    source("visual_conjoint/scripts/analysis/analyses_vcd_singlecountry.R")
  }
  
  
  # #parallel conjoint scripts
  source("parallel_conjoint/data_manipulation_cpd.R")
  source("parallel_conjoint/randomization_checks_cpd.R")
  source("parallel_conjoint/analyses_cpd_singlecountry.R")
  
}


source("classic_conjoint/analyses_ccd_bycountry.R")

for(outcome in c("ideology", "trust", "populism"))
{
  source("visual_conjoint/scripts/analysis/analyses_vcd_bycountry.R")
}

source("parallel_conjoint/analyses_cpd_bycountry.R")
