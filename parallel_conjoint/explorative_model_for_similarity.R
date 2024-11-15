###############################################################################
#this script is specifically for the exploratory analyses regarding the 
#effects of the number of matches on the outcome for the parallel design
#conjoint experiment
###############################################################################


#############################################################
#LIBRARY CALLS
#############################################################

# pacman::p_load(
#   cregg, dplyr, ggpubr, cowplot,
#   MASS, cjoint, corrplot, dplyr,
#   forcats, ggplot2, gt, gtools,
#   gtsummary, margins, openxlsx,
#   patchwork, rio, texreg, tools,
#   lme4, ggeffects
# )

#############################################################
# DEFINING FUNCTIONS
#############################################################


#function to draw and save the plots related to the effect of the number of matches
# (regardless of the actual attribute displayed) into the probaility of 
#selecting someone as their conversation partners (with and without politics,
#depending on the experimental arm selected)

full_match_effects = function(data, 
                              formula, 
                              exparm = c("natural", "mediated"),
                              typeofmodel){
  
  exparm=match.arg(exparm)
  
  filtered_data = data |>
    filter(cpd_exparm == exparm) |> 
    select(cpd_chosen, cpd_n_matches,
           cpd_gender, cpd_age, cpd_educ, cpd_regionfeel, 
           cpd_consc, cpd_ope,
           cpd_diet, cpd_animal, cpd_holiday, cpd_ideology,
           respid, respondent_task)
  
  
  filtered_data$respid = as.factor(filtered_data$respid)
  typeof(data$cpd_chosen)
  typeof(filtered_data$respid)
  
  
  model =  glmer(formula,  # Random intercept for each respondent
                 data = filtered_data,
                 family = binomial(link="logit"))
  
  predictions = as.data.frame(ggpredict(model, terms = "cpd_n_matches"))
  
  
  # Convert effect object to a data frame
  effect_df <- data.frame(
    x = predictions$x,  # The levels of the predictor
    fit = predictions$predicted,   # Fitted values (predicted)
    lower = predictions$conf.low, # Lower bound of confidence intervals
    upper = predictions$conf.high  # Upper bound of confidence intervals
  )
  
  nattr = ifelse(exparm == "natural", 9, 9+1)
  
  # Create caterpillar plot
  p = ggplot(effect_df, aes(x = x, y = fit)) +
    geom_point() +  # Add points for the fitted values
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Add error bars
    labs(
      x = "Number of attribute matches",
      y = "Effect on the probability of engaging conversation",
      title = ""
    )+
    scale_y_continuous(breaks=seq(0,1, by=0.1))+
    scale_x_continuous(breaks=seq(0,nattr, by=1))
  
  ggsave(paste0(output_wd,"estimations/", 
                subdir, "singlecountry_", exparm, typeofmodel, ".png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", 
                           subdir, "singlecountry_", exparm, typeofmodel, ".rds"))
  
  return(model)
  
}



#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context

#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"


#############################################################

#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/conjoint_parallel_design/", context, "/")

data = readRDS(paste0(dataset_rep, "cjdata_cpd_", context, ".RDS"))



### Analyses on the effects of multiple matches


subdir = "MatchesEffects/"

#DI define the two formulas: the one for the natural mediation arm 
# (which is without ideology) and the one for the mediated mediation arm
# (which is with ideology)

#In this case, I add random effects of the respndent (respid) but not yet of the task.

formula_natural_nmatches_randintercept = cpd_chosen~cpd_n_matches+
  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday +
  (1 | respid) #random effects of respid


formula_mediated_nmatches_randintercept = cpd_chosen~cpd_n_matches+
  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday+
  cpd_ideology +
  (1 | respid)

# i generate the variable to include respondent_task effects

data$respondent_task = factor(paste0(data$respid, data$cpd_task_number))


model_randind_natural_withouttask = full_match_effects(data,
                   formula_natural_nmatches_randintercept,
                   exparm="natural",
                   typeofmodel ="randintecepts_withouttasklevel")

#evaluate model fit

summary(model_randind_natural_withouttask)


model_randind_mediated_withouttask = full_match_effects(data,
                   formula_mediated_nmatches_randintercept,
                   "mediated",
                   typeofmodel = "randintercept")

summary(model_randind_mediated_withouttask)



formula = cpd_chosen~cpd_n_matches+
  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday + respid + respondent_task

model =  glm(formula,  # Random intercept for each respondent
               data = data,
               family = binomial(link="logit"))

summary(model)





