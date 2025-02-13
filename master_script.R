#Master script

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  cregg, dplyr, ggpubr, cowplot,
  stringr,MASS, cjoint, corrplot, 
  dplyr,forcats, ggplot2, gt, 
  gtools, gtsummary, margins, 
  openxlsx, patchwork, rio, texreg,
  tools,lme4, ggeffects, wesanderson,
  tidyr
)


#setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/")

#gdrive_code is personal for the google drive user. Change it if you are not Gaetano

gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"
dataset_rep = paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/")

#If you want the dataset cleansed by laggards, speeders, and people who fail 
#attention checks, clean must be true. Otherwise set it to false

contexts = c("IT", "FR", "CZ", "SW")
clean = T

for(context in contexts)
{
  ## recoding script
  source("others/recoding_demo.R")
  
  print(paste("Ok recoding", context, Sys.time()))
  
}
#generate pooled dataset
source("others/rbind_country_datasets.R")


#recoding pooled dataset
source("others/recodings/recoding_demo.R")

contexts = c("IT", "FR", "CZ", "SW", "POOL")

context = c("POOL")

for(context in contexts)
{
  #descriptives script
  source("others/descriptives_internal.R")
  
  print(paste("Ok descriptives", context, Sys.time()))
  
  #classic conjoint scripts
  source("classic_conjoint/data_manipulation_ccd.R")
  print(paste("Ok ccd data man", context, Sys.time()))

  for(outcome in c("ideology", "populism"))
  {
      withoutNB=F
      source("classic_conjoint/randomization_checks_ccd.R")
      print(paste("Ok ccd randcheck", context, Sys.time()))
      source("classic_conjoint/analyses_ccd_singlecountry.R")
      print(paste("Ok ccd analyses", outcome, context, "WithNB", Sys.time()))
    # if(outcome == "ideology") 
    # {
    #   withoutNB = T
    #   source("classic_conjoint/analyses_ccd_singlecountry.R")
    #   print(paste("Ok ccd analyses", outcome, context, "WithoutNB", Sys.time()))
    # }
  }
  
  #visual conjoint scripts
  source("visual_conjoint/scripts/analysis/data_manipulation_vcd.R")
  print(paste("Ok vcd data man", context, Sys.time()))
  
  
  for(outcome in c("ideology", "trust", "populism"))
  {
    # source("visual_conjoint/scripts/analysis/randomization_checks_vcd.R")
    # print(paste("Ok vcd randcheck", outcome, context, Sys.time()))
    recoding_functional_equivalents = T #this variable controls if the script
    #analyses_vcd_singlecountry is run considering functional equivalents
    #as equivalents (if true)or as distinguished (if false)
    source("visual_conjoint/scripts/analysis/analyses_vcd_singlecountry.R")
    print(paste("Ok vcd analyses recoded", outcome, context, Sys.time()))
    
    recoding_functional_equivalents = F #this variable controls if the script
    #analyses_vcd_singlecountry is run considering functional equivalents
    #as equivalents (if true)or as distinguished (if false)
    source("visual_conjoint/scripts/analysis/analyses_vcd_singlecountry.R")
    print(paste("Ok vcd analyses non recoded", outcome, context, Sys.time()))
  }
  
  
  # #parallel conjoint scripts
  #source("parallel_conjoint/data_manipulation_cpd.R")
  print(paste("Ok cpd dataman", context, Sys.time()))
  source("parallel_conjoint/randomization_checks_cpd.R")
  print(paste("Ok cpd randcheck", context, Sys.time()))
  source("parallel_conjoint/analyses_cpd_singlecountry.R")
  print(paste("Ok cpd analyses", context, Sys.time()))
  
  if(context == "IT")
  {
    recoding_functional_equivalents = T
    source("others/extra_graphs_thesis.R")
    # recoding_functional_equivalents = F
    # source("others/extra_graphs_thesis.R")
  }
}

for(outcome in c("ideology", "populism"))
{
  source("classic_conjoint/analyses_ccd_bycountry.R")
  print(paste("Ok cpd byocuntry", context, Sys.time()))
  
}

for(outcome in c("ideology", "trust", "populism"))
{
  recoding_functional_equivalents = T 
  source("visual_conjoint/scripts/analysis/analyses_vcd_bycountry.R")
  print(paste("Ok vcd bycountry", outcome, Sys.time()))
}

source("parallel_conjoint/analyses_cpd_bycountry.R")
print(paste("Ok cpd bycountry", context, Sys.time()))
