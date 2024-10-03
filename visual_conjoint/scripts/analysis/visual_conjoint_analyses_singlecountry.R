#visual_cnjoint_analyses_singlecountry

###############################################################################
#this script is for the analyses related to the visual conjoint design, 
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
  patchwork, rio, texreg, tools
)

#############################################################
# DEFINING FUNCTIONS
#############################################################

###SET CATEGORIES AND LEVELS

#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio

set_categories_and_levels_visual = function(effects, 
                                     attributes=attributes){
  # effects=effects_pooled
  # attributes=attributes
  effects$feature = factor(attributes, levels = unique(attributes))
  effects$level=factor(levels_vector, levels = levels_vector)
  
  return(effects)
}



##Function to draw plots for the effects

draw_plot_effects = function(effects, 
                             estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                             y_labels=y_labels_plots,
                             leftlim=999, #the left limit of the plot
                             rightlim=999,#the right limit of the plot
                             x_intercept=999 #the vertical line to signal the difference from the insignificance
){
  
  estimator=match.arg(estimator)
  
  v = list()
  
  if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
    #with [-1; 1] for amces and [0, 1] for mm
  {
    
    leftlim=ifelse(estimator!="mm", -1, 0)
    rightlim=1
    intercept = ifelse(estimator!="mm", 0, 0.5)
    
  }
  

    p = ggplot(effects)+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level, col=feature))+
      ylab("")+
      xlab("Effects")+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = rev(levels_vector)) +
      theme(legend.position = "none")
  
  
  return(p)
}




full_analysis = function(data,
                         formula, #the conjoint formula
                         estimator=c("mm","amce"), #marginal means and amces
                         subdir #the subdirectory where the plots will be saved
){
  
  
  ###### This function performs the whole analysis, draws the graphs and saves
  #them in the appropriate repositories. 
  #It calls the other functions previously defined plus the functions in cjregg and
  #patchwork

  # formula=formula_rw
  # estimator="mm"
  
   estimator=match.arg(estimator)

  effects_pooled <- data |>
    cj(formula, 
       id = ~respid,
       estimate = estimator)
  
  effects_pooled = set_categories_and_levels_visual(effects_pooled,
                                                    attributes = attributes)
  
  p = draw_plot_effects(effects_pooled,
                        estimator=estimator,
                        y_labels=y_labels_plots)
  
  p=p+patchwork::plot_annotation(title = paste("Effects of the attributes Visual Conjoint Experiment, "),
                                 caption= toupper(estimator))
  
  ggsave(paste0(output_wd,"estimations/", subdir,"singlecountry.png"), 
         p, 
         height = 10, 
         width = 10)
  
}

#Our levels regarding match and mismatches (for labeling)


y_labels_plots = list(ethnicity=c("Black","White"),
                      gender=c("Female","Male"),
                      age=c("35", "70"),
                      job=c("Lawyer","Waiter","Entrepreneur","Teacher","Politician"),
                      issue=c("Leftneg","Leftpos","Rightneg","Rightpos"),
                      nostalgia=c("Past1","Past2","Future1","Future2"),
                      valence=c("Corruption1","Corruption2", "Honesty1", "Honesty2"),
                      food=c("Vegan","Ethnic","Meatpoor","Meatrich"),
                      animal=c("Catpoor","Catrich","Dogpoor","Dogrich"),
                      crowd=c("Mixedpeople","Whitepeople","Mixedelite","Whiteelite")
                      )

levels_vector= unlist(y_labels_plots, use.names = F)

attributes= c("Ethnicity", "Ethnicity",
                      "Gender", "Gender", 
                      "Age","Age",
                      "Job","Job","Job","Job","Job",
                      "Issue", "Issue", "Issue", "Issue",
                      "Nostalgia", "Nostalgia", "Nostalgia", "Nostalgia",
                      "Valence","Valence","Valence","Valence",
                      "Food","Food","Food","Food",
                      "Animal","Animal","Animal","Animal",
                      "Crowd","Crowd","Crowd","Crowd")





formula_rw = vcd_chosen_rw ~  vcd_ethnicity + 
  vcd_gender + vcd_age + vcd_job + 
  vcd_issue + vcd_nostalgia + vcd_valence +
  vcd_animal + vcd_food + vcd_crowd


#############################################################


setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/parallel_conjoint/")

output_wd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/"
data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_vcd.RDS")

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

######################################
############ EFFECTS ################# 
######################################


subdir = ""

full_analysis(data,
              formula_rw,
              "mm",
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

