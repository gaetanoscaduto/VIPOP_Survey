###############################################################################
#this script is for the analyses related to the parallel conjoint design, 
#when considering a country at a time or pooled all together
###############################################################################


#############################################################
#LIBRARY CALLS
#############################################################

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools,
  lme4, ggeffects
)

#############################################################
# DEFINING FUNCTIONS
#############################################################

###SET CATEGORIES AND LEVELS

#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio

set_categories_and_levels = function(effects, 
                                     type=c("match","nominal"), 
                                     nominal_attributes=nominal_attributes){
  
  type=match.arg(type)
  
  effects$category="Sociodemographics"
  
  effects$category=ifelse(grepl("ope",effects$feature) | grepl("consc",effects$feature), 
                     "Psychological",
                     ifelse(grepl("diet",effects$feature) | grepl("animal",effects$feature) | grepl("holiday",effects$feature),
                            "Lifestyle",
                            "Sociodemographics"))
  
  if(type=="match")
  {
    effects$variable = as.character(effects$level)
  
    for(i in 1:nrow(effects))
    {
      effects$variable[i] = toTitleCase(paste(strsplit(as.character(effects$level[i]), "_")[[1]], collapse=" "))
    }
  
    effects$level=factor(effects$variable)
  
  }
  if(type=="nominal")
  {
    effects$feature = factor(nominal_attributes, levels = unique(nominal_attributes))
    effects$level=factor(levels_vector, levels = levels_vector)
  }
  
  return(effects)
}



##Function to draw plots for the effects

draw_plot_effects = function(effects, 
                             type=c("match", "nominal"), #"match" or "nominal" 
                             categories=c("Sociodemographics", "Psychological", "Lifestyle", "Political"), #vector of thee categories 
                             #("sociodemo", "psycho", "lifestyle")
                             estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                             y_labels=y_labels_plots,
                             leftlim=999, #the left limit of the plot
                             rightlim=999,#the right limit of the plot
                             x_intercept=999 #the vertical line to signal the difference from the insignificance
                           ){
  
  estimator=match.arg(estimator)
  type=match.arg(type)
  
  v = list()
  
  if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
    #with [-1; 1] for amces and [0, 1] for mm
  {
    
  leftlim=ifelse(estimator!="mm", -1, 0)
  rightlim=1
  intercept = ifelse(estimator!="mm", 0, 0.5)
  
  }
  
  for(category in categories[1:3])
  {
    p = ggplot(effects[effects$category == category, ])+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level, col=feature))+
      ylab("")+
      xlab(category)+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = rev(y_labels[[type]][[category]])) +
      theme(legend.position = "none")
    
    v[[category]] = p
    
  }
  
  p = patchwork::wrap_plots(v[["Sociodemographics"]],v[["Psychological"]],v[["Lifestyle"]], ncol=1)
    
  return(p)
}




full_analysis = function(data,
                         formula, #the conjoint formula
                         effect=c("ATEs", "ACDEs", "EEs"), #the three possible effects to compute
                         type=c("match", "nominal"), #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                         estimator=c("mm","amce"), #marginal means and amces
                         arm=c("natural", "ideology_match", "ideology_mismatch"), #natural mediation arm, or manipulated mediation arm with ideological match, 
                         #or manipulated mediation arm with ideological mismatch
                         subdir #the subdirectory where the plots will be saved
){
  
  
  ###### This function performs the whole analysis, draws the graphs and saves
  #them in the appropriate repositories. 
  #It calls the other functions previously defined plus the functions in cjregg and
  #patchwork
  
  #Notice that the function behaves slightly differently for EEs, that need to be
  #treated a bit differntly due to the complications
  
  effect=match.arg(effect)
  type=match.arg(type)
  estimator=match.arg(estimator)
  arm=match.arg(arm)
  
  if(effect!= "EEs")
  {
    effects_pooled <- data |>
      filter(cpd_exparm2 == arm) |>
      cj(formula, id = ~respid,
         estimate = estimator)
  }
  if(effect== "EEs")
  {
    estimator= paste0(estimator, "_differences")
    
    effects_pooled <- data |>
      filter(cpd_exparm2 == "natural" | cpd_exparm2 == arm) |>
      cj(formula_match,
         id = ~respid,
         estimate = estimator,
         by = ~cpd_exparm)
    
  }
  
  
  
  effects_pooled = set_categories_and_levels(effects_pooled,
                                                     type,
                                                     nominal_attributes=nominal_attributes)
  
  p = draw_plot_effects(effects_pooled,
                        type = type,
                        categories=categories,
                        estimator=estimator,
                        y_labels=y_labels_plots)
  
  p=p+patchwork::plot_annotation(title = paste(effect, "of the Parallel Design Conjoint Experiment, ", type),
                                                           caption= paste0(toupper(estimator), " s of the", arm, " mediation arm"))
    
    ggsave(paste0(output_wd,"estimations/", subdir,"singlecountry.png"), 
           p, 
           height = 10, 
           width = 10)
  
}



#function to draw and save the plots related to the effect of the number of matches
# (regardless of the actual attribute displayed) into the probaility of 
#selecting someone as their conversation partners (with and without politics,
#depending on the experimental arm selected)
full_match_effects = function(data, 
                              formula, 
                              exparm = c("natural", "mediated")){
  
  exparm=match.arg(exparm)
  
   # exparm="natural"
   # formula=formula_natural_nmatches
   # 
  
  filtered_data = data[data$cpd_exparm == exparm, ]
  
  filtered_data$respid = as.factor(filtered_data$respid)

  
  
  model =  glmer(cpd_chosen ~ cpd_n_matches +
                   cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
                   cpd_consc + cpd_ope +
                   cpd_diet + cpd_animal + cpd_holiday + 
                   (1 | respid),  # Random intercept for each respondent
                 data = filtered_data,
                 family = binomial)
  
  predictions = as.data.frame(ggpredict(model, terms = "cpd_n_matches"))

  
  # Convert effect object to a data frame
  effect_df <- data.frame(
    x = predictions$x,  # The levels of the predictor
    fit = predictions$predicted,   # Fitted values (predicted)
    lower = predictions$conf.low, # Lower bound of confidence intervals
    upper = predictions$conf.high  # Upper bound of confidence intervals
  )
  
  # Create caterpillar plot
  p = ggplot(effect_df, aes(x = x, y = fit)) +
    geom_point() +  # Add points for the fitted values
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Add error bars
    labs(
      x = "Number of attribute matches",
      y = "Marginal effect on the probability of choosing the profile",
      title = ""
    )
  
  ggsave(paste0(output_wd,"estimations/", 
                subdir,"singlecountry_", exparm, ".png"), 
         p, 
         height = 10, 
         width = 10)
}





#Our categories of apolitical traits
categories= c("Sociodemographics", "Psychological", "Lifestyle", "Political")

#Our levels regarding match and mismatches (for labeling)
y_labels_match = list(Sociodemographics=c("Gender Match", "Gender Mismatch",
                                         "Age Match", "Age Mismatch",
                                         "Educ Match", "Educ Mismatch",
                                         "Regionfeel Match", "Regionfeel Mismatch"),
                     Psychological = c("Consc Match", "Consc Mismatch", 
                                       "Ope Match", "Ope Mismatch"),
                     Lifestyle =c("Diet Match", "Diet Mismatch",
                                  "Animal Match", "Animal Mismatch",
                                  "Holiday Match", "Holiday Mismatch"
                     ),
                     Political = c("Ideology Match",
                                   "Ideology Mismatch"))

y_labels_nominal = list(Sociodemographics = c("Female", "Male",
                                         "Under 30", "Between 30 and 59","Over 60",
                                         "Degree","No degree",
                                         "Regionfeel1","Regionfeel2","Regionfeel3"),
                   Psychological = c("High Consc.","Med. Consc.","Low Consc.",
                                     "High Ope.","Med. Ope.","Low Ope."),
                   Lifestyle = c("Omnivore","Vegetarian","Vegan",
                                 "Cat","Dog","No pet",
                                 "City","Outdoor","Relax"),
                   Political = c("Right-wing",
                                 "Left-wing",
                                 "Center",
                                 "Not collocated"))

y_labels_plots=list(match=y_labels_match, 
                    nominal=y_labels_nominal)

#Our nominal attributes (here called nominal_attributes)

nominal_attributes= c("Gender", "Gender",
            "Age", "Age","Age",
            "Education","Education",
            "Regionfeel","Regionfeel","Regionfeel",
            "Conscientiousness","Conscientiousness","Conscientiousness",
            "Openness","Openness","Openness",
            "Diet","Diet","Diet",
            "Animal","Animal","Animal",
            "Holiday","Holiday","Holiday")

#Levels (as a vector)
levels_vector= c("Female", "Male",
          "Under 30", "Between 30 and 59","Over 60",
          "Degree","No degree",
          "Regionfeel1","Regionfeel2","Regionfeel3",
          "High Consc.","Med. Consc.","Low Consc.",
          "High Ope.","Med. Ope.","Low Ope.",
          "Omnivore","Vegetarian","Vegan",
          "Cat","Dog","No pet",
          "City","Outdoor","Relax")


formula_match = cpd_chosen ~  cpd_match_gender + cpd_match_age + 
  cpd_match_educ + cpd_match_regionfeel +
  cpd_match_consc + cpd_match_ope +
  cpd_match_diet + cpd_match_animal + cpd_match_holiday

formula_nominal = cpd_chosen ~  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday



formula_natural_nmatches = cpd_chosen~cpd_n_matches+
  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday

formula_mediated_nmatches = cpd_chosen~cpd_n_matches+
  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday+
  cpd_ideology

#############################################################


setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/parallel_conjoint/")

output_wd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/conjoint_parallel_design/"
data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_cpd.RDS")

data=rbind(data, data, data, data)
data=rbind(data, data, data, data)

context = "IT"
# context = "FR"
# context = "SW"
# context = "CZ"
# 
# data = data |>
#   filter(country == "IT")

#############################################################

####################################
############ ATEs (MATCH/MISMATCH)
######################################

# Estimation: The marginal mean associated with S_i^k=1 for respondents 
#in the natural mediation arm

# Interpretation: The total effect that observing similarity in attribute k 
#has on the willingness to engage in political conversation when no political 
#information is given to the respondent, whether through political inferences or not.

subdir = "ATEs/match/MMs/"

# formula=formula_match
# effect = "ATEs"
# type="match"
# estimator="mm"
# arm="natural"

full_analysis(data,
              formula_match,
              "ATEs",
              "match",
              "mm",
              "natural",
              subdir)


### Same as before, but with AMCes (for appendix)

subdir = "ATEs/match/AMCEs/"

full_analysis(data,
              formula_match,
              "ATEs",
              "match",
              "amce",
              "natural",
              subdir)

############ ATEs (nominal value)

subdir = "ATEs/nominal/MMs/"

full_analysis(data,
              formula_nominal,
              "ATEs",
              "nominal",
              "mm",
              "natural",
              subdir)


#same but with amce

subdir = "ATEs/nominal/AMCEs/"

full_analysis(data,
              formula_nominal,
              "ATEs",
              "nominal",
              "amce",
              "natural",
              subdir)

########################################
############ ADCEs (MATCH/MISMATCH)#####
########################################

#ESTIMATION
# The marginal mean associated with S_i^k=1 for respondents 
# in the maniulated mediation arm with ideological similarity condition

# The marginal mean associated with S_i^k=1 for respondents in the 
#maniulated mediation arm with ideological dissimilarity condition

# INTERPRETATION
#The effects that similarity in each attribute k has on the willingness to
#engage in political conversations that is due neither to mediation nor to 
#interaction with political inferences.

######################################
#### ACDEs for ideological match with MM
######################################

subdir = "ACDEs/match/MMs/"

full_analysis(data,
              formula_match,
              "ACDEs",
              "match",
              "mm",
              "ideology_match",
              subdir)


######################################
#### ACDEs for ideological mismatch with MM
###################################### 

subdir = "ACDEs/mismatch/MMs/"

full_analysis(data,
              formula_match,
              "ACDEs",
              "match",
              "mm",
              "ideology_mismatch",
              subdir)

######################################
#### ACDEs for ideological match with AMCE
###################################### 

subdir = "ACDEs/match/AMCEs/"

full_analysis(data,
              formula_match,
              "ACDEs",
              "match",
              "amce",
              "ideology_match",
              subdir)

############################################################################
################ ACDEs for ideological mismatch with AMCE################### 
############################################################################

subdir = "ACDEs/mismatch/AMCEs/"


full_analysis(data,
              formula_match,
              "ACDEs",
              "match",
              "amce",
              "ideology_mismatch",
              subdir)


############################################################################
########################## ELIMINATED EFFECTS ##############################
############################################################################

#ESTIMATION
# Point estimation: the difference between ATE and ACDE. 
#for standard errors see Acharya et al. (2018) Or Lòpez-Ortega 2023

#INTERPRETATION
# The portion of the ATE explained by political inferences, either through 
#mediation or through interaction between S_i^k and the specific ideology inferred.


##### ELIMINATED EFFECTS WITH MM FOR IDEOLOGICAL MATCH

subdir = "EEs/match/MMs/"


# formula=formula_match
# effect = "EEs"
# type="match"
# estimator="mm"
# arm="ideology_match"

full_analysis(data,
              formula_match,
              "EEs",
              "match",
              "mm",
              "ideology_match",
              subdir)

##### ELIMINATED EFFECTS WITH MM FOR IDEOLOGICAL MISMATCH

subdir = "EEs/mismatch/MMs/"

full_analysis(data,
              formula_match,
              "EEs",
              "match",
              "mm",
              "ideology_mismatch",
              subdir)

##### ELIMINATED EFFECTS WITH AMCE FOR IDEOLOGICAL MATCH

subdir = "EEs/match/AMCEs/"

full_analysis(data,
              formula_match,
              "EEs",
              "match",
              "amce",
              "ideology_match",
              subdir)

##### ELIMINATED EFFECTS WITH AMCE FOR IDEOLOGICAL MISMATCH

subdir = "EEs/mismatch/AMCEs/"

full_analysis(data,
              formula_match,
              "EEs",
              "match",
              "amce",
              "ideology_mismatch",
              subdir)



### Analyses on the effects of multiple matches

subdir = "MatchesEffects/"
#model with just the number of matches predicting the probability of being chosen
# summary(glm(data$cpd_chosen~data$cpd_n_matches, family = 
#               "binomial"))

#now i call the function to draw the effects in the model with the actual controls

full_match_effects(data,
                   formula_natural_nmatches,
                   exparm="natural")



full_match_effects(data,
                   formula_mediated_nmatches,
                   "mediated")

