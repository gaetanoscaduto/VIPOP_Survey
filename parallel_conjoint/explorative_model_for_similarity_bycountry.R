###############################################################################
#this script is for the analyses related to the parallel conjoint design,
# considering all countries at the same time
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
#   lme4, ggeffects, wesanderson
# )

#############################################################
# DEFINING FUNCTIONS
#############################################################


#function to draw and save the plots related to the effect of the number of matches
# (regardless of the actual attribute displayed) into the probaility of 
#selecting someone as their conversation partners (with and without politics,
#depending on the experimental arm selected)

full_match_effects_bycountry = function(data, 
                                        formula, 
                                        exparm){
  
  #browser()
  
  # exparm="natural"
  # formula=formula_natural_nmatches
  # country="IT"
  
  # exparm=match.arg(exparm)
  
  full_df = data.frame()
  for(context_loc in c("IT", "FR","SW","CZ", "POOL"))
  {
    if(context_loc != "POOL")
    {
      #filter the data by country and experimental arm 
      filtered_data = data |>
        filter(cpd_country == context_loc & cpd_exparm == exparm)
      
    }
    else
    {
      #filter the data by experimental arm (I want the pooled data)
      filtered_data = data |>
        filter(cpd_exparm == exparm)
    }
    
    #make sure respid is a factor
    filtered_data$respid = as.factor(filtered_data$respid)
    
    #I fit a multilevel model (random effects with respID)
    
    model =  glmer(cpd_chosen ~ cpd_n_matches +
                     cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
                     cpd_consc + cpd_ope +
                     cpd_diet + cpd_animal + cpd_holiday + 
                     (1 | respid),  # Random intercept for each respondent
                   data = filtered_data,
                   family = binomial)
    
    #using the library ggeffects I make the prediction for the different
    #values of cpd_n_matches when the other variables are in the reference category
    predictions = as.data.frame(ggpredict(model, terms = "cpd_n_matches"))
    
    # Convert predictions object to a data frame
    effect_df <- data.frame(
      x = predictions$x,  # The levels of the predictor
      fit = predictions$predicted,   # Fitted values (predicted)
      lower = predictions$conf.low, # Lower bound of confidence intervals
      upper = predictions$conf.high  # Upper bound of confidence intervals
    )
    
    #I add the country variable because then I row bind all the datasets created
    #by the loop toghether
    effect_df$cpd_country = context_loc
    
    #I bind the datasets
    full_df=rbind(full_df, effect_df)
    
  }
  
  # Create caterpillar plot
  p = ggplot() +
    geom_pointrange(data = full_df[full_df$cpd_country == "IT", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="IT", shape="IT"),
                    position = position_nudge(x = -1/5)) +
    geom_pointrange(data = full_df[full_df$cpd_country == "FR", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="FR", shape="FR"),
                    position = position_nudge(x = -1/10)) + 
    geom_pointrange(data = full_df[full_df$cpd_country == "SW", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="SW", shape="SW"),
                    position = position_nudge(x = 0)) + 
    geom_pointrange(data = full_df[full_df$cpd_country == "CZ", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="CZ", shape="CZ"),
                    position = position_nudge(x = 1/10)) + 
    geom_pointrange(data = full_df[full_df$cpd_country == "POOL", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="POOL", shape="POOL"),
                    position = position_nudge(x = 1/5)) + 
    labs(
      x = "Number of attribute matches",
      y = "Marginal effect on the probability of choosing the profile",
      title = ""
    )+
    scale_x_continuous(seq(1, 1+lengths(gregexpr("\\+", as.character(formula)[3])),by=1))+
    scale_color_manual(
      values = c("IT" = wesanderson::wes_palettes$Darjeeling1[1],
                 "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                 "SW" = wesanderson::wes_palettes$Darjeeling1[3],
                 "CZ" = wesanderson::wes_palettes$Darjeeling1[4],
                 "POOL" = 'black'),
      name = "Country",
      limits = c("IT", "FR", "SW", "CZ", "POOL")
    ) +
    scale_shape_manual(
      values = c("IT" = 19, 
                 "FR" = 17, 
                 "SW" = 15, 
                 "CZ" = 18, 
                 "POOL" = 1),
      name = "Country",
      limits = c("IT", "FR", "SW", "CZ", "POOL")
    ) +
    theme(
      legend.position = "right",  # You can change this to "top", "bottom", etc.
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12)
    ) 
  
  #saving the plots
  
  ggsave(paste0(output_wd,"estimations/", 
                subdir,"bycountry_", exparm, ".png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", 
                           subdir,"bycountry_", exparm, ".rds"))
  
  
  
}


#############################################################


#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/conjoint_parallel_design/")

data = readRDS(paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_cpd_POOL.RDS"))





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







