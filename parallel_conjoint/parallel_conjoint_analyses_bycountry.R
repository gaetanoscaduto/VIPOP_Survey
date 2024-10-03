###############################################################################
#this script is for the analyses related to the parallel conjoint design,
# considering all countries at the same time
###############################################################################


#############################################################
#LIBRARY CALLS
#############################################################

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)

#############################################################
# DEFINING FUNCTIONS
#############################################################

###SET CATEGORIES AND LEVELS

#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio

set_categories_and_levels_bycountry = function(effects, 
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

draw_plot_effects_bycountry = function(effects_pooled, effects_bycountry, 
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
  
  effects_IT= effects_bycountry |> filter(country=="IT")
  effects_FR= effects_bycountry |> filter(country=="FR")
  effects_SW= effects_bycountry |> filter(country=="SW")
  effects_CZ= effects_bycountry |> filter(country=="CZ")
  for(category in categories[1:3])
  {
    
    these_labels = y_labels[[type]][[category]]
    p = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(data=effects_IT[effects_IT$category == category, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level),
                      col=wesanderson::wes_palettes$Darjeeling1[1],
                      shape=19,
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/5),
                      show.legend = F)+
      geom_pointrange(data=effects_FR[effects_FR$category == category, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level),
                      col=wesanderson::wes_palettes$Darjeeling1[2],
                      shape=17,
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/10),
                      show.legend = F)+
      geom_pointrange(data=effects_SW[effects_SW$category == category, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level),
                      col=wesanderson::wes_palettes$Darjeeling1[3],
                      shape=15,
                      alpha = 1,
                      #size=1.3,
                      show.legend = F)+
      geom_pointrange(data=effects_CZ[effects_CZ$category == category, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level),
                      col=wesanderson::wes_palettes$Darjeeling1[4],
                      shape=18,
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = -1/10),
                      show.legend = F)+
      geom_pointrange(data=effects_pooled[effects_pooled$category == category, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level),
                      col='black',
                      shape=1,
                      alpha = 1,
                      position = position_nudge(y = -1/5),
                      #size=1.3,
                      show.legend = F)+
      ylab("")+
      xlab(category)+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = rev(these_labels)) +
      theme(legend.position = "none")+
      
      # Manually add the legend
      annotate("point", x = 0.93, y = 0.5+5*length(these_labels)/28,
               colour = wesanderson::wes_palettes$Darjeeling1[1], 
               size = 3, shape=19) + # Italy
      annotate("text", x = 0.95, y = 0.5+5*length(these_labels)/28, 
               label = "IT", hjust = 0) +
      
      annotate("point", x = 0.93, y = 0.5+4*length(these_labels)/28, 
               colour = wesanderson::wes_palettes$Darjeeling1[2], 
               size = 3, shape=17) +  # France
      annotate("text", x = 0.95, y = 0.5+4*length(these_labels)/28,
               label = "FR", hjust = 0) +
      
      annotate("point", x = 0.93, y = 0.5+3*length(these_labels)/28, 
               colour = wesanderson::wes_palettes$Darjeeling1[3], 
               size = 3, shape=15) +  # Sweden
      annotate("text", x = 0.95, y = 0.5+3*length(these_labels)/28, 
               label = "SW", hjust = 0) +
      
      annotate("point", x = 0.93, y = 0.5+2*length(these_labels)/28,
               colour = wesanderson::wes_palettes$Darjeeling1[4], 
               size = 3, shape=18) +  # Czech Republic
      annotate("text", x = 0.95, y = 0.5+2*length(these_labels)/28, 
               label = "CZ", hjust = 0)+ 
      
      annotate("point", x = 0.93, y = 0.5+1*length(these_labels)/28, 
               colour = 'black', 
               size = 3, shape=1) +  # Pooled
      annotate("text", x = 0.95, y = 0.5+1*length(these_labels)/28,
               label = "POOL", hjust = 0)
    
    v[[category]] = p
    
  }
  
  return(v)
}


full_analysis_bycountry = function(data,
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
    
    effects_bycountry <- data |>
      filter(cpd_exparm2 == arm) |>
      cj(formula, id = ~respid, by= ~country,
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
    
    effects_bycountry =data.frame()
    for(country in c("IT", "FR", "SW", "CZ"))
    {
      temp_effects_bycountry <- data |>
        filter((cpd_exparm2 == "natural" | cpd_exparm2 == arm) & country == country) |>
        cj(formula_match,
           id = ~respid,
           estimate = estimator,
           by = ~cpd_exparm)
      
      temp_effects_bycountry$country = country
      
      effects_bycountry=rbind(effects_bycountry, temp_effects_bycountry)
    }
    
  }
  
  
  
  effects_pooled=set_categories_and_levels_bycountry(effects_pooled,
                                                     type,
                                                     nominal_attributes=nominal_attributes)
  
  effects_bycountry=set_categories_and_levels_bycountry(effects_bycountry,
                                                        type,
                                                        nominal_attributes=nominal_attributes)
  
  
  
  v = draw_plot_effects_bycountry(effects_pooled,
                                  effects_bycountry, 
                                  type = type, 
                                  categories=categories, 
                                  estimator=estimator, 
                                  y_labels=y_labels_plots)
  
  for(category in categories[1:3])
  {
    v[[category]]=v[[category]]+patchwork::plot_annotation(title = paste(effect, "of the Parallel Design Conjoint Experiment, ", type),
                                                           caption= paste0(toupper(estimator), "s of the", arm, " mediation arm"))
    
    ggsave(paste0(output_wd,"estimations/", subdir, category, "_bycountry.png"), 
           v[[category]], 
           height = 10, 
           width = 10)
    
  }
  
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


#############################################################


setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/parallel_conjoint/")

output_wd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/conjoint_parallel_design/"

data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_cpd.RDS")

#genero fake dataset per provare, cancella quando arrivano dati definitivi
data=rbind(data, data, data, data)
data=rbind(data, data, data, data)

data$country=factor(sample(c("IT", "FR", "SW","CZ"), nrow(data), T))


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


full_analysis_bycountry(data, 
              formula_match,
              "ATEs",
              "match",
              "mm",
              "natural",
              subdir)


### Same as before, but with AMCes (for appendix)
subdir = "ATEs/match/MMs/"

full_analysis_bycountry(data, 
              formula_match,
              "ATEs",
              "match",
              "amce",
              "natural",
              subdir)



############ ATEs (nominal value)

subdir = "ATEs/nominal/MMs/"


full_analysis_bycountry(data, 
              formula_nominal,
              "ATEs",
              "nominal",
              "mm",
              "natural",
              subdir)


#same but with amce

subdir = "ATEs/nominal/AMCEs/"

full_analysis_bycountry(data, 
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


full_analysis_bycountry(data, 
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


full_analysis_bycountry(data, 
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

full_analysis_bycountry(data, 
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

full_analysis_bycountry(data, 
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

full_analysis_bycountry(data, 
              formula_match,
              "EEs",
              "match",
              "mm",
              "ideology_match",
              subdir)


##### ELIMINATED EFFECTS WITH MM FOR IDEOLOGICAL MISMATCH

subdir = "EEs/mismatch/MMs/"

full_analysis_bycountry(data, 
              formula_match,
              "EEs",
              "match",
              "mm",
              "ideology_mismatch",
              subdir)


##### ELIMINATED EFFECTS WITH AMCE FOR IDEOLOGICAL MATCH

subdir = "EEs/match/AMCEs/"

full_analysis_bycountry(data, 
              formula_match,
              "EEs",
              "match",
              "amce",
              "ideology_match",
              subdir)

##### ELIMINATED EFFECTS WITH AMCE FOR IDEOLOGICAL MISMATCH

subdir = "EEs/mismatch/AMCEs/"

full_analysis_bycountry(data, 
              formula_match,
              "EEs",
              "match",
              "amce",
              "ideology_mismatch",
              subdir)


