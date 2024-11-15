###############################################################################
#this script is for the analyses related to the parallel conjoint design, 
#when considering a country at a time or pooled all together
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
                             effect = "Indifferent", #the possible effects
                             estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                             y_labels=y_labels_plots,
                             leftlim=999, #the left limit of the plot
                             rightlim=999,#the right limit of the plot
                             x_intercept=999,#the vertical line to signal the difference from the insignificance
                             for_comparison=F #If =T then the function returns the list with the plots separated 
                             #for category. If =F it returns the already assembld plot
                             
){
  
  estimator=match.arg(estimator)
  type=match.arg(type)
  
  v = list()
  
  if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
    #with [-1; 1] for amces and [0, 1] for mm
  {
    leftlim=ifelse(estimator!="mm" | effect == "EEs", -1, 0)
    rightlim=1
  }
  
  intercept = x_intercept
  
  if(x_intercept==999)
  {
    intercept = ifelse(estimator!="mm" | effect == "EEs", 0, 0.5)
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
    
    # if(for_comparison == T)
    # {
    #   p= p+ 
    #     theme(legend.position = "none",
    #           axis.text.y = element_text(angle = 90, hjust = 0.5, vjust=0.5))
    # }
    if(for_comparison==F)
    {
      p= p+ 
        theme(legend.position = "none")
    }
    
    v[[category]] = p
    
  }
  
  if(for_comparison == F & type =="match")
  {
    p = patchwork::wrap_plots(v[["Sociodemographics"]],v[["Psychological"]],v[["Lifestyle"]], ncol=1, heights = c(4,2,3))
  }
  if(for_comparison == F & type =="nominal")
  {
    p = patchwork::wrap_plots(v[["Sociodemographics"]],v[["Psychological"]],v[["Lifestyle"]], ncol=1, heights = c(5,3,5))
  }
  if(for_comparison == T)
  {
    p=v
  }
  return(p)
}




full_analysis = function(data,
                         formula, #the conjoint formula
                         effect=c("ATEs", "ACDEs", "EEs"), #the three possible effects to compute
                         type=c("match", "nominal"), #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                         estimator=c("mm","amce"), #marginal means and amces
                         arm=c("natural", "ideology_match", "ideology_mismatch"), #natural mediation arm, or manipulated mediation arm with ideological match, 
                         #or manipulated mediation arm with ideological mismatch
                         subdir,
                         leftlim=999,
                         rightlim=999#the subdirectory where the plots will be saved
){
  
  #browser()
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
    
    effects_pooled <- data |>
      filter(cpd_exparm2 == "natural" | cpd_exparm2 == arm) |>
      cj(formula_match,
         id = ~respid,
         estimate =  paste0(estimator, "_differences"),
         by = ~cpd_exparm)
    
    ##browser()
    
    #I need the dataset with the reference to plot, I create it artificially
    #through this stupid trick because I'm lazy
    
    if(estimator == "amce")
    {
      
      temp <- data |>
        filter(cpd_exparm2 == "natural" | cpd_exparm2 == arm) |>
        cj(formula_match,
           id = ~respid,
           estimate =  "amce")
      
      temp = temp[is.na(temp$std.error), ]
      
      temp$BY="natural-mediated"
      temp$cpd_exparm = "natural"
      
      effects_pooled = rbind(effects_pooled, temp)
    }
    
  }
  
  
  effects_pooled = set_categories_and_levels(effects_pooled,
                                             type,
                                             nominal_attributes=nominal_attributes)
  
  p = draw_plot_effects(effects_pooled,
                        type = type,
                        effect = effect,
                        categories=categories,
                        estimator=estimator,
                        y_labels=y_labels_plots,
                        leftlim=leftlim,
                        rightlim=rightlim)
  
  p=p+patchwork::plot_annotation(title = paste(effect, "of the Parallel Design Conjoint Experiment, ", arm),
                                 caption= paste0(toupper(estimator), "s of the", arm, " mediation arm"))
  
  ggsave(paste0(output_wd,"estimations/", subdir, "singlecountry.png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir, "singlecountry.rds"))
  
  
}



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
  
}


compare_effects = function(data,
                           formula, 
                           type=c("match", "nominal"), #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                           estimator=c("mm","amce"), #marginal means and amces
                           arm=c("ideology_match", "ideology_mismatch"), #manipulated mediation arm with ideological match, 
                           #or manipulated mediation arm with ideological mismatch
                           subdir,
                           leftlim=999,
                           rightlim=999,
                           x_intercept=999#the subdirectory where the plots will be saved
){
  
  
  ###### This function ends up drawing the graphs with the three effects compared like
  #Acharya et al
  
  
  type=match.arg(type)
  estimator=match.arg(estimator)
  arm=match.arg(arm)
  
  ### Compute the ATEs
  ates <- data |>
    filter(cpd_exparm2 == "natural") |>
    cj(formula, id = ~respid,
       estimate = estimator)
  
  ###Compute the ACDEs
  acdes <- data |>
    filter(cpd_exparm2 == arm) |>
    cj(formula, id = ~respid,
       estimate = estimator)
  
  ### Compute the EEs
  
  ees = data |>
    filter(cpd_exparm2 == "natural" | cpd_exparm2 == arm) |>
    cj(formula,
       id = ~respid,
       estimate = paste0(estimator, "_differences"),
       by = ~cpd_exparm)
  
  
  
  if(estimator == "amce")
  {
    #I need the dataset with the reference to plot, I create it artificially
    #through this stupid trick because I'm lazy
    
    temp <- data |>
      filter(cpd_exparm2 == "natural" | cpd_exparm2 == arm) |>
      cj(formula_match,
         id = ~respid,
         estimate =  "amce")
    
    temp = temp[is.na(temp$std.error), ]
    
    temp$BY="natural-mediated"
    temp$cpd_exparm = "natural"
    
    ees = rbind(ees, temp)
  }
  
  ##Set the categories and levels for the three datasets
  
  ates = set_categories_and_levels(ates,
                                   type,
                                   nominal_attributes=nominal_attributes)
  
  acdes = set_categories_and_levels(acdes,
                                    type,
                                    nominal_attributes=nominal_attributes)
  
  ees = set_categories_and_levels(ees,
                                  type,
                                  nominal_attributes=nominal_attributes)
  
  #Call draw effects with the for_comparison argument ==T, which means that it will return
  #the vector separately, not the already assembled immage
  
  ##browser()
  
  x_intercept = ifelse(estimator=="mm_differences" | estimator=="amce_differences" | estimator=="amce",
                       0, 
                       0.5)
  
  
  pates = draw_plot_effects(ates,
                            type = type,
                            categories=categories,
                            estimator=estimator,
                            y_labels=y_labels_plots,
                            leftlim=-0.2,
                            rightlim=0.2,
                            x_intercept = x_intercept,
                            for_comparison = T)
  
  pacdes = draw_plot_effects(acdes,
                             type = type,
                             categories=categories,
                             estimator=estimator,
                             y_labels=y_labels_plots,
                             leftlim=-0.2,
                             rightlim=0.2,
                             x_intercept = x_intercept,
                             for_comparison = T)
  
  pees = draw_plot_effects(ees,
                           type = type,
                           categories=categories,
                           estimator=estimator,
                           y_labels=y_labels_plots,
                           leftlim=-0.2,
                           rightlim=0.2,
                           x_intercept = 0,
                           for_comparison = T)
  
  #Now I assemble three plots (for each category) so that they are easy to compare
  
  p_socio = (pates[["Sociodemographics"]]+labs(title = "Average Treatment Effects (ATEs)"))/(pacdes[["Sociodemographics"]]+labs(title = "Average Conditional Direct Effects (ACDEs)"))/(pees[["Sociodemographics"]]+labs(title = "Eliminated Effects (EEs)"))
  
  p_psycho = (pates[["Psychological"]]+labs(title = "Average Treatment Effects (ATEs)"))/(pacdes[["Psychological"]]+labs(title = "Average Conditional Direct Effects (ACDEs)"))/(pees[["Psychological"]]+labs(title = "Eliminated Effects (EEs)"))
  
  p_lifestyle = (pates[["Lifestyle"]]+labs(title = "Average Treatment Effects (ATEs)"))/(pacdes[["Lifestyle"]]+labs(title = "Average Conditional Direct Effects (ACDEs)"))/(pees[["Lifestyle"]]+labs(title = "Eliminated Effects (EEs)"))
  
  
  #I save the three plots
  

  ggsave(paste0(output_wd,"estimations/", subdir, "socio_singlecountry.png"), 
         p_socio, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir, "socio_singlecountry.rds"))
  
  
  ggsave(paste0(output_wd,"estimations/", subdir, "psycho_singlecountry.png"), 
         p_psycho, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir, "psycho_singlecountry.rds"))
  
  
  ggsave(paste0(output_wd,"estimations/", subdir, "lifestyle_singlecountry.png"), 
         p_lifestyle, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir, "lifestyle_singlecountry.rds"))
  
  
  
  #I create the more flexible list to return
  
  plots = list(ates_plots=pates, acdes_plots=pacdes, ees_plots=pees)
  
  return(plots)
  
}





########The following function draws the plots to compare ATEs and the two EEs
#Like acharya et al figure 1. It works complementary to compare_effects
plot_compare_effects = function(plots_match,#plot list outputted from compare_effects
                                #with the effects related to the ideology match condition
                                plots_mismatch, #plot list outputted from compare_effects
                                #with the effects related to the ideology mismatch condition
                                subdir){
  
  plots_match$ates_plots$Sociodemographics = (plots_match$ates_plots$Sociodemographics + labs(title = "ATEs (natural mediation arm)"))
  
  plots_match$ees_plots$Sociodemographics  = (plots_match$ees_plots$Sociodemographics + labs(title = "EEs (ideological similarity)"))
  
  plots_mismatch$ees_plots$Sociodemographics = (plots_mismatch$ees_plots$Sociodemographics  + labs(title = "EEs (ideological dissimilarity)"))
  
  p = plots_match$ates_plots$Sociodemographics | plots_match$ees_plots$Sociodemographics  | plots_mismatch$ees_plots$Sociodemographics                   
  
  
  ggsave(paste0(output_wd,"estimations/", subdir,  "sociodemographics_singlecountry.png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir,  "sociodemographics_singlecountry.rds"))
  
  
  
  plots_match$ates_plots$Psychological = (plots_match$ates_plots$Psychological + labs(title = "ATEs (natural mediation arm)"))
  
  plots_match$ees_plots$Psychological  = (plots_match$ees_plots$Psychological + labs(title = "EEs (ideological similarity)"))
  
  plots_mismatch$ees_plots$Psychological = (plots_mismatch$ees_plots$Psychological  + labs(title = "EEs (ideological dissimilarity)"))
  
  p = plots_match$ates_plots$Psychological | plots_match$ees_plots$Psychological  | plots_mismatch$ees_plots$Psychological                   
  
  
  ggsave(paste0(output_wd,"estimations/", subdir, "psychological_singlecountry.png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir, "psychological_singlecountry.rds"))
  
  
  plots_match$ates_plots$Lifestyle = (plots_match$ates_plots$Lifestyle + labs(title = "ATEs (natural mediation arm)"))
  
  plots_match$ees_plots$Lifestyle  = (plots_match$ees_plots$Lifestyle + labs(title = "EEs (ideological similarity)"))
  
  plots_mismatch$ees_plots$Lifestyle = (plots_mismatch$ees_plots$Lifestyle  + labs(title = "EEs (ideological dissimilarity)"))
  
  p = plots_match$ates_plots$Lifestyle | plots_match$ees_plots$Lifestyle  | plots_mismatch$ees_plots$Lifestyle                   
  
  
  ggsave(paste0(output_wd,"estimations/", subdir,  "Lifestyle_singlecountry.png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir,  "Lifestyle_singlecountry.rds"))
  
  
  
}

#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context

#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"



#Our categories of apolitical traits
categories= c("Sociodemographics", "Psychological", "Lifestyle", "Political")

#Our levels regarding match and mismatches (for labeling)
y_labels_match = list(Sociodemographics=c("Gender Mismatch", "Gender Match",
                                          "Age Mismatch", "Age Match",
                                          "Educ Mismatch", "Educ Match",
                                          "Regionfeel Mismatch", "Regionfeel Match"),
                      Psychological = c("Consc Mismatch", "Consc Match", 
                                        "Ope Mismatch", "Ope Match"),
                      Lifestyle =c("Diet Mismatch", "Diet Match",
                                   "Animal Mismatch", "Animal Match",
                                   "Holiday Mismatch", "Holiday Match"),
                      Political = c("Ideology Mismatch",
                                    "Ideology Match"))




#Our nominal attributes (here called nominal_attributes)

if(context == "IT")
{
  #if we are not in france, three regionfeel categories!)               
  nominal_attributes= c("Gender", "Gender",
                        "Age", "Age","Age",
                        "Education","Education",
                        "Regionfeel","Regionfeel","Regionfeel",
                        "Conscientiousness","Conscientiousness","Conscientiousness",
                        "Openness","Openness","Openness",
                        "Diet","Diet","Diet",
                        "Animal","Animal","Animal",
                        "Holiday","Holiday","Holiday")
  
  levels_vector= c("Female", "Male",
                   "Under 30", "Between 30 and 59","Over 60",
                   "Degree","No degree",
                   "Central Italy","Northern Italy","Southern Italy",
                   "High Consc.","Med. Consc.","Low Consc.",
                   "High Ope.","Med. Ope.","Low Ope.",
                   "Omnivore","Vegetarian","Vegan",
                   "Cat","Dog","No pet",
                   "City","Outdoor","Relax")
  
  y_labels_nominal = list(Sociodemographics = c("Female", "Male",
                                                "Under 30", "Between 30 and 59","Over 60",
                                                "Degree","No degree",
                                                "Central Italy","Northern Italy","Southern Italy"),
                          Psychological = c("High Consc.","Med. Consc.","Low Consc.",
                                            "High Ope.","Med. Ope.","Low Ope."),
                          Lifestyle = c("Omnivore","Vegetarian","Vegan",
                                        "Cat","Dog","No pet",
                                        "City","Outdoor","Relax"),
                          Political = c("Right-wing",
                                        "Left-wing",
                                        "Center",
                                        "Not collocated"))
  
}

if(context=="FR")
{
  # if we are in france, only two regionfeel (paris vs all)
  nominal_attributes= c("Gender", "Gender",
                        "Age", "Age","Age",
                        "Education","Education",
                        "Regionfeel","Regionfeel",
                        "Conscientiousness","Conscientiousness","Conscientiousness",
                        "Openness","Openness","Openness",
                        "Diet","Diet","Diet",
                        "Animal","Animal","Animal",
                        "Holiday","Holiday","Holiday")
  
  levels_vector= c("Female", "Male",
                   "Under 30", "Between 30 and 59","Over 60",
                   "Degree","No degree",
                   "No Paris","Paris",
                   "High Consc.","Med. Consc.","Low Consc.",
                   "High Ope.","Med. Ope.","Low Ope.",
                   "Omnivore","Vegetarian","Vegan",
                   "Cat","Dog","No pet",
                   "City","Outdoor","Relax")
  
  y_labels_nominal = list(Sociodemographics = c("Female", "Male",
                                                "Under 30", "Between 30 and 59","Over 60",
                                                "Degree","No degree",
                                                "No Paris","Paris"),
                          Psychological = c("High Consc.","Med. Consc.","Low Consc.",
                                            "High Ope.","Med. Ope.","Low Ope."),
                          Lifestyle = c("Omnivore","Vegetarian","Vegan",
                                        "Cat","Dog","No pet",
                                        "City","Outdoor","Relax"),
                          Political = c("Right-wing",
                                        "Left-wing",
                                        "Center",
                                        "Not collocated"))
  
}


if(context == "CZ")
{
  #if we are not in france, three regionfeel categories!)               
  nominal_attributes= c("Gender", "Gender",
                        "Age", "Age","Age",
                        "Education","Education",
                        "Regionfeel","Regionfeel","Regionfeel",
                        "Conscientiousness","Conscientiousness","Conscientiousness",
                        "Openness","Openness","Openness",
                        "Diet","Diet","Diet",
                        "Animal","Animal","Animal",
                        "Holiday","Holiday","Holiday")
  
  levels_vector= c("Female", "Male",
                   "Under 30", "Between 30 and 59","Over 60",
                   "Degree","No degree",
                   "Cechia","Moravia","Prague",
                   "High Consc.","Med. Consc.","Low Consc.",
                   "High Ope.","Med. Ope.","Low Ope.",
                   "Omnivore","Vegetarian","Vegan",
                   "Cat","Dog","No pet",
                   "City","Outdoor","Relax")
  
  y_labels_nominal = list(Sociodemographics = c("Female", "Male",
                                                "Under 30", "Between 30 and 59","Over 60",
                                                "Degree","No degree",
                                                "Cechia","Moravia","Prague"),
                          Psychological = c("High Consc.","Med. Consc.","Low Consc.",
                                            "High Ope.","Med. Ope.","Low Ope."),
                          Lifestyle = c("Omnivore","Vegetarian","Vegan",
                                        "Cat","Dog","No pet",
                                        "City","Outdoor","Relax"),
                          Political = c("Right-wing",
                                        "Left-wing",
                                        "Center",
                                        "Not collocated"))
  
}

if(context == "SW")
{
  #if we are not in france, three regionfeel categories!)               
  nominal_attributes= c("Gender", "Gender",
                        "Age", "Age","Age",
                        "Education","Education",
                        "Regionfeel","Regionfeel","Regionfeel",
                        "Conscientiousness","Conscientiousness","Conscientiousness",
                        "Openness","Openness","Openness",
                        "Diet","Diet","Diet",
                        "Animal","Animal","Animal",
                        "Holiday","Holiday","Holiday")
  
  levels_vector= c("Female", "Male",
                   "Under 30", "Between 30 and 59","Over 60",
                   "Degree","No degree",
                   "Gotland","Norrland","Svealand",
                   "High Consc.","Med. Consc.","Low Consc.",
                   "High Ope.","Med. Ope.","Low Ope.",
                   "Omnivore","Vegetarian","Vegan",
                   "Cat","Dog","No pet",
                   "City","Outdoor","Relax")
  
  y_labels_nominal = list(Sociodemographics = c("Female", "Male",
                                                "Under 30", "Between 30 and 59","Over 60",
                                                "Degree","No degree",
                                                "Gotland","Norrland","Svealand"),
                          Psychological = c("High Consc.","Med. Consc.","Low Consc.",
                                            "High Ope.","Med. Ope.","Low Ope."),
                          Lifestyle = c("Omnivore","Vegetarian","Vegan",
                                        "Cat","Dog","No pet",
                                        "City","Outdoor","Relax"),
                          Political = c("Right-wing",
                                        "Left-wing",
                                        "Center",
                                        "Not collocated"))
  
}


if(context=="POOL")
{
  # if we are in france, only two regionfeel (paris vs all)
  nominal_attributes= c("Gender", "Gender",
                        "Age", "Age","Age",
                        "Education","Education",
                        "Regionfeel","Regionfeel","Regionfeel","Regionfeel",
                        "Regionfeel","Regionfeel","Regionfeel","Regionfeel",
                        "Regionfeel","Regionfeel","Regionfeel",
                        "Conscientiousness","Conscientiousness","Conscientiousness",
                        "Openness","Openness","Openness",
                        "Diet","Diet","Diet",
                        "Animal","Animal","Animal",
                        "Holiday","Holiday","Holiday")
  
  levels_vector= c("Female", "Male",
                   "Under 30", "Between 30 and 59","Over 60",
                   "Degree","No degree",
                   "Cechia (CZ)","Center (IT)", "Gotland (SW)",
                   "Moravia (CZ)", "No Paris (FR)", "North (IT)",
                   "Norrland (SW)", "Paris (FR)", "Prague (CZ)",
                   "South (IT)", "Svealand (SW)",
                   "High Consc.","Med. Consc.","Low Consc.",
                   "High Ope.","Med. Ope.","Low Ope.",
                   "Omnivore","Vegetarian","Vegan",
                   "Cat","Dog","No pet",
                   "City","Outdoor","Relax")
  
  y_labels_nominal = list(Sociodemographics = c("Female", "Male",
                                                "Under 30", "Between 30 and 59","Over 60",
                                                "Degree","No degree",
                                                "Cechia (CZ)","Center (IT)", "Gotland (SW)",
                                                "Moravia (CZ)", "No Paris (FR)", "North (IT)",
                                                "Norrland (SW)", "Paris (FR)", "Prague (CZ)",
                                                "South (IT)", "Svealand (SW)"),
                          Psychological = c("High Consc.","Med. Consc.","Low Consc.",
                                            "High Ope.","Med. Ope.","Low Ope."),
                          Lifestyle = c("Omnivore","Vegetarian","Vegan",
                                        "Cat","Dog","No pet",
                                        "City","Outdoor","Relax"),
                          Political = c("Right-wing",
                                        "Left-wing",
                                        "Center",
                                        "Not collocated"))
  
}




y_labels_plots=list(match=y_labels_match, 
                    nominal=y_labels_nominal)

#Levels (as a vector)



formula_match = cpd_chosen ~  cpd_match_gender + cpd_match_age + 
  cpd_match_educ + cpd_match_regionfeel +
  cpd_match_consc + cpd_match_ope +
  cpd_match_diet + cpd_match_animal + cpd_match_holiday

formula_nominal = cpd_chosen ~  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday



#############################################################

#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/conjoint_parallel_design/", context, "/")

data = readRDS(paste0(dataset_rep, "cjdata_cpd_", context, ".RDS"))


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
              subdir,
              leftlim=0.35,
              rightlim=0.65)


### Same as before, but with AMCes (for appendix)

subdir = "ATEs/match/AMCEs/"

full_analysis(data,
              formula_match,
              "ATEs",
              "match",
              "amce",
              "natural",
              subdir,
              leftlim=-0.25,
              rightlim=0.25)

############ ATEs (nominal value)

subdir = "ATEs/nominal/MMs/"

full_analysis(data,
              formula_nominal,
              "ATEs",
              "nominal",
              "mm",
              "natural",
              subdir,
              leftlim=0.35,
              rightlim=0.65)


#same but with amce

subdir = "ATEs/nominal/AMCEs/"

full_analysis(data,
              formula_nominal,
              "ATEs",
              "nominal",
              "amce",
              "natural",
              subdir,
              leftlim=-0.25,
              rightlim=0.25)

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
              subdir,
              leftlim=0.4,
              rightlim=0.6)


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
              subdir,
              leftlim=0.4,
              rightlim=0.6)

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
              subdir,
              leftlim=-0.1,
              rightlim=0.1)

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
              subdir,
              leftlim=-0.1,
              rightlim=0.1)


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
              subdir,
              leftlim=-0.2,
              rightlim=0.2)

##### ELIMINATED EFFECTS WITH MM FOR IDEOLOGICAL MISMATCH

subdir = "EEs/mismatch/MMs/"

full_analysis(data,
              formula_match,
              "EEs",
              "match",
              "mm",
              "ideology_mismatch",
              subdir,
              leftlim=-0.2,
              rightlim=0.2)

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

# subdir = "MatchesEffects/"
# 
# #model with just the number of matches predicting the probability of being chosen
# # summary(glm(data$cpd_chosen~data$cpd_n_matches, family = 
# # #               "binomial"))
# # 
# # #now i call the function to draw the effects in the model with the actual controls
# # 
# 
# # i generate the variable to include respondent_task effects
# 
# data$respondent_task = factor(paste0(data$respid, data$cpd_task_number))
# 
# # formula_natural_nmatches_randslopes = cpd_chosen~cpd_n_matches+
# #   cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
# #   cpd_consc + cpd_ope +
# #   cpd_diet + cpd_animal + cpd_holiday +
# #   (1+cpd_n_matches | respid)
# # 
# # 
# # full_match_effects(data,
# #                    formula_natural_nmatches_randslopes,
# #                    exparm="natural",
# #                    typeofmodel = "randslopes")
# 
# formula_natural_nmatches_randintercept = cpd_chosen~cpd_n_matches+
#   cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
#   cpd_consc + cpd_ope +
#   cpd_diet + cpd_animal + cpd_holiday + 
#   #respondent_task+ #this variable adds me fixed effect of the respondent_task level
#   (1 | respid) #random effects of respid
# 
# full_match_effects(data,
#                    formula_natural_nmatches_randintercept,
#                    exparm="natural",
#                    typeofmodel ="randintecepts_withouttasklevel")
# 
# 
# # formula_natural_nmatches_randintercept = cpd_chosen~cpd_n_matches+
# #   cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
# #   cpd_consc + cpd_ope +
# #   cpd_diet + cpd_animal + cpd_holiday + 
# #   respondent_task+ #this variable adds me fixed effect of the respondent_task level
# #   (1 | respid) #random effects of respid
# # 
# # full_match_effects(data,
# #                    formula_natural_nmatches_randintercept,
# #                    exparm="natural",
# #                    typeofmodel ="randintecepts_wtasklevel")
# 
# 
# # formula_mediated_nmatches_randslopes = cpd_chosen~cpd_n_matches+
# #   cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
# #   cpd_consc + cpd_ope +
# #   cpd_diet + cpd_animal + cpd_holiday+
# #   cpd_ideology +
# #   (1+cpd_n_matches | respid)
# # 
# # 
# # full_match_effects(data,
# #                    formula_mediated_nmatches_randslopes,
# #                    "mediated",
# #                    typeofmodel = "randslopes")
# 
# 
# formula_mediated_nmatches_randintercept = cpd_chosen~cpd_n_matches+
#   cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
#   cpd_consc + cpd_ope +
#   cpd_diet + cpd_animal + cpd_holiday+
#   cpd_ideology +
#   (1 | respid)
# 
# full_match_effects(data,
#                    formula_mediated_nmatches_randintercept,
#                    "mediated",
#                    typeofmodel = "randintercept")
# 
# 
# 






# 
# #########################################
# ### Now I want to have the three effects close to each other. How do I do that?
# 

subdir="CompareEffects/Ideology_match/MMs/"



plots_match = compare_effects(data,
                              formula_match,
                              type="match", #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                              estimator="mm", #marginal means and amces
                              arm="ideology_match", #manipulated mediation arm with ideological match, 
                              #or manipulated mediation arm with ideological mismatch
                              subdir,#the subdirectory where the plots will be saved
                              leftlim=0.4,
                              rightlim=0.6#,
                              #x_intercept=0.5
)



subdir="CompareEffects/Ideology_mismatch/MMs/"


plots_mismatch = compare_effects(data,
                                 formula_match,
                                 type="match", #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                                 estimator="mm", #marginal means and amces
                                 arm="ideology_mismatch", #manipulated mediation arm with ideological match, 
                                 #or manipulated mediation arm with ideological mismatch
                                 subdir,#the subdirectory where the plots will be saved
                                 leftlim=0.4,
                                 rightlim=0.6#,
                                 # #x_intercept=0.5
)



subdir="CompareEffects/ATES_vs_EEs/MMs/"

plot_compare_effects(plots_match,
                     plots_mismatch, 
                     subdir)



#Same as above but with amces

subdir="CompareEffects/Ideology_match/AMCEs/"

plots_match = compare_effects(data,
                formula_match,
                type="match", #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                estimator="amce", #marginal means and amces
                arm="ideology_match", #manipulated mediation arm with ideological match, 
                #or manipulated mediation arm with ideological mismatch
                subdir,#the subdirectory where the plots will be saved
                leftlim=-0.1,
                rightlim=0.1,
                x_intercept=0
                )



subdir="CompareEffects/Ideology_mismatch/AMCEs/"


plots_mismatch = compare_effects(data,
                formula_match,
                type="match", #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                estimator="amce", #marginal means and amces
                arm="ideology_mismatch", #manipulated mediation arm with ideological match, 
                #or manipulated mediation arm with ideological mismatch
                subdir,#the subdirectory where the plots will be saved
                leftlim=-0.1,
                rightlim=0.1,
                x_intercept=0
)
  


#Now I also draw the plots like Achara et al figure 1, namely by comparing EEs on the
#two manipulated mediation arms

subdir="CompareEffects/ATES_vs_EEs/AMCEs/"

plot_compare_effects(plots_match,
                     plots_mismatch, 
                     subdir)







### Now I also want to run the simple ATE on those that see Idelogy as well,
#because I'm starting to think that maybe ideology does not have that much of an effect
#in this subsample

subdir = "ATEs/checkIdeology/"
# formula=formula_match
# effect = "ATEs"
# type="match"
# estimator="mm"
# arm="natural"

formula_match_w_ideo = cpd_chosen ~  cpd_match_gender + cpd_match_age + 
  cpd_match_educ + cpd_match_regionfeel +
  cpd_match_consc + cpd_match_ope +
  cpd_match_diet + cpd_match_animal + cpd_match_holiday+
  cpd_match_ideology

eff_check <- data |>
  filter(cpd_exparm == "mediated") |>
  cj(formula_match_w_ideo,
     id = ~respid,
     estimate = "amce")

plot(eff_check)
