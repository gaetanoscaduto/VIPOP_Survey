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
  }
  if(x_intercept==999)
  {
    intercept = ifelse(estimator!="mm", 0, 0.5)
  }
  
v=list()
  for(attribute in unique(attributes))
  {
   p = ggplot(effects[effects$feature==attribute, ])+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                          y=level, col=feature))+
      ylab(attribute)+
      xlab("\n")+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = y_labels_plots[[tolower(attribute)]]) +
      theme(legend.position = "none",
            axis.text.y = element_text(size=10),
            axis.title.y = element_text(size=12))
   
   v[[attribute]] = p
  }
  
  p = (v[["Ethnicity"]]/v[["Gender"]]/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+xlab("Effect size")))|(v[["Nostalgia"]]/v[["Valence"]]/v[["Food"]]/v[["Animal"]]/(v[["Crowd"]]+xlab("Effect size")))
  return(p)
}





draw_interaction_effects = function(effects){
  
  p=ggplot(effects)+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                        y=level, col=feature))+
    labs(y="",x="Marginal Mean")+
    xlim(-0.1,1.1)+
    theme(legend.position = "none",
          axis.text.y = element_text(size=10),
          axis.title.y = element_text(size=12))
  
  return(p)
}



full_analysis = function(data,
                         formula, #the conjoint formula
                         estimator=c("mm","amce"), #marginal means and amces
                         subdir,
                         leftlim=999, 
                         rightlim=999#the subdirectory where the plots will be saved
){
  
  
  ###### This function performs the whole analysis, draws the graphs and saves
  #them in the appropriate repositories. 
  #It calls the other functions previously defined plus the functions in cjregg and
  #patchwork

  # formula=formula_outcome
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
                        y_labels=y_labels_plots,
                        leftlim,
                        rightlim)
  
  p=p+patchwork::plot_annotation(title = paste("Effects of the attributes Visual Conjoint Experiment"),
                                 caption= toupper(estimator))
  
  ggsave(paste0(output_wd,"estimations/", subdir,"singlecountry.png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
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

##################

# outcome = "ideology"
# outcome = "trust"
# outcome = "populism"

if(outcome == "ideology")
{
  formula_outcome = vcd_chosen_rw ~  vcd_ethnicity + 
    vcd_gender + vcd_age + vcd_job + 
    vcd_issue + vcd_nostalgia + vcd_valence +
    vcd_animal + vcd_food + vcd_crowd
}

if(outcome == "trust")
{
  formula_outcome = vcd_chosen_trust ~ vcd_ethnicity + 
    vcd_gender + vcd_age + vcd_job + 
    vcd_issue + vcd_nostalgia + vcd_valence +
    vcd_animal + vcd_food + vcd_crowd
}
if(outcome == "ideology")
{
  formula_outcome = vcd_chosen_pop ~ vcd_ethnicity + 
    vcd_gender + vcd_age + vcd_job + 
    vcd_issue + vcd_nostalgia + vcd_valence +
    vcd_animal + vcd_food + vcd_crowd
}


#############################################################


#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context
#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"


#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/", outcome, "/", context, "/")
data = readRDS(paste0(dataset_rep, "cjdata_vcd_", context, ".RDS"))



######################################
############ EFFECTS ################# 
######################################


subdir = "MMs/"

full_analysis(data,
              formula_outcome,
              "mm",
              subdir,
              leftlim = 0.3,
              rightlim=0.7)


### Same as before, but with AMCes (for appendix)

subdir = "AMCEs/"

full_analysis(data,
              formula_outcome,
              "amce",
              subdir)

##### ACIE of Sociodemos


data$interacted_sociodemos = interaction(data$vcd_age, data$vcd_ethnicity, data$vcd_gender, sep =" ")

data$interacted_cultural = interaction(data$vcd_food, data$vcd_animal, sep =" ")

data$interacted_political = interaction(data$vcd_issue, data$vcd_valence, sep =" ")




if(outcome=="ideology")
{
  formula_interaction_sociodemos = vcd_chosen_rw ~ interacted_sociodemos
  formula_interaction_cultural = vcd_chosen_rw ~ interacted_cultural
  formula_interaction_political = vcd_chosen_rw ~ interacted_political
  
  
}

if(outcome=="trust")
{
  formula_interaction_sociodemos = vcd_chosen_trust ~ interacted_sociodemos  
  formula_interaction_cultural = vcd_chosen_trust ~ interacted_cultural
  formula_interaction_political = vcd_chosen_trust ~ interacted_political
  
  
}

if(outcome=="populism")
{
  formula_interaction_sociodemos = vcd_chosen_pop ~ interacted_sociodemos
  formula_interaction_cultural = vcd_chosen_pop ~ interacted_cultural
  formula_interaction_political = vcd_chosen_pop ~ interacted_political
  
}


subdir = "Interactions/"

effects <- data |>
  cj(formula_interaction_sociodemos, 
     id = ~respid,
     estimate = "mm")

p = draw_interaction_effects(effects)

p

ggsave(paste0(output_wd,"estimations/", subdir,"interacted_sociodemos_singlecountry.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)



##### ACIE of the cultural dimensions


subdir = "Interactions/"

effects <- data |>
  cj(formula_interaction_cultural, 
     id = ~respid,
     estimate = "mm")

p = draw_interaction_effects(effects)

p

ggsave(paste0(output_wd,"estimations/", subdir,"interacted_cultural_singlecountry.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)


#####  ACIE of the political dimensions


subdir = "Interactions/"

effects <- data |>
  cj(formula_interaction_political, 
     id = ~respid,
     estimate = "mm")

p = draw_interaction_effects(effects)

p

ggsave(paste0(output_wd,"estimations/", subdir,"interacted_political_singlecountry.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

