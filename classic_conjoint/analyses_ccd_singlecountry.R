# DA RICONTROLLARE


###############################################################################
#this script is for the analyses related to the CLASSIC conjoint design, 
#when considering a country at a time or pooled all together
###############################################################################


#############################################################
#LIBRARY CALLS
#############################################################
# 
# pacman::p_load(
#   cregg, dplyr, ggpubr, cowplot,
#   MASS, cjoint, corrplot, dplyr,
#   forcats, ggplot2, gt, gtools,
#   gtsummary, margins, openxlsx,
#   patchwork, rio, texreg, tools
# )

#############################################################
# DEFINING FUNCTIONS
#############################################################

###SET CATEGORIES AND LEVELS

#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio

set_categories_and_levels = function(effects, 
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
                             intercept=999,
                             continuous = F
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
  
  v=list()
  for(attribute in unique(attributes))
  {
    
    p = ggplot(effects[effects$feature==attribute, ])+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                          y=level, col=feature))+
      ylab(attribute)+
      xlab("\n")+
      scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]]))+
      scale_x_continuous(limits = c(leftlim, rightlim), 
                         breaks = round(seq(leftlim, rightlim, length.out = 9), digits=3))+
      theme(legend.position = "none",
            axis.text.y = element_text(size=10),
            axis.title.y = element_text(size=12))
    
    v[[attribute]] = p
  }
  
  if(continuous == F) #outcome not continuous
  {
    v[["Gender"]] = v[["Gender"]]+xlim(0.3,0.7)
  }
  
  p1 = (v[["Gender"]]/v[["Age"]]/v[["Religion"]]/v[["Citysize"]]/(v[["Job"]]+xlab("Effect size")))+plot_layout(heights = c(3,3,3,3,4))
  p2 = (v[["Conscientiousness"]]/v[["Openness"]]/v[["Neuroticism"]]/v[["Restaurant"]]/v[["Transport"]]/(v[["Animal"]]+xlab("Effect size")))+plot_layout(heights = c(2,2,2,4,3,4))
  p=p1|p2
  
  return(p)
}



full_interaction_effects = function(data, 
                                    formula,
                                    type_of_interaction,
                                    estimator,
                                    leftlim = 999,
                                    rightlim = 999,
                                    intercept = 999){
  
  effects <- data |>
    cj(formula, 
       id = ~respid,
       estimate = estimator)
  
  if(leftlim == 999)
  {
    leftlim = min(effects$lower)-0.05
    rightlim = max(effects$upper)+0.05
    intercept = ifelse(estimator=="mm", 0.5, 0)
  }
  
  
  p=ggplot(effects)+
    geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                        y=fct_reorder(level, desc(estimate)), col=feature))+
    labs(y="",x="Estimate")+
    scale_x_continuous(limits = c(leftlim, rightlim), 
                       breaks = round(seq(leftlim, rightlim, length.out = 9), digits=3))+
    theme(legend.position = "none",
          axis.text.y = element_text(size=10),
          axis.title.y = element_text(size=12))
  
  ggsave(paste0(output_wd, subdir,"interacted_", type_of_interaction, estimator, ".png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir,"interacted_", type_of_interaction, estimator, ".rds"))
  
  saveRDS(effects, file = paste0(output_wd, subdir,"interacted_", type_of_interaction, "_data.rds"))
  
}





full_subgroup_analysis = function(data,
                                  formula,
                                  estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                                  y_labels=y_labels_plots,
                                  subdir, #the subdirectory where plots will be saved
                                  leftlim=999, #the left limit of the plot
                                  rightlim=999,#the right limit of the plot
                                  x_intercept=999, #the vertical line to signal the difference from the insignificance
                                  subgroup_variable, #the name of the variable on which subgroup analysis is conducted
                                  subgroup_name, #the grouping name in natural language in natural language (eg. età, titolo di studio)
                                  subgroup1, #the name of the first subgroup (variable level)
                                  subgroup2 #the name of the second subgroup (variable level)
){
  
  estimator=match.arg(estimator)
  
  # ()
  
  data$temp_subgroup = factor(data[[subgroup_variable]])
  
  if(estimator == "mm" | estimator == "amce")
  {
    effects_pooled <- data |>
      cj(formula, 
         id = ~respid,
         by = ~temp_subgroup,
         estimate = estimator)
    
    effects_pooled = set_categories_and_levels(effects_pooled,
                                               attributes = attributes)
    
    
    
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
    
    effects_pooled = effects_pooled[effects_pooled$level != "Non-binary", ] #not enough power!
    
    effects_subgroup1 = effects_pooled |>
      filter(temp_subgroup == subgroup1)
    
    effects_subgroup2 = effects_pooled |>
      filter(temp_subgroup == subgroup2)
    
    for(attribute in unique(attributes))
    {
      
      p = ggplot(effects_subgroup1[effects_subgroup1$feature==attribute, ])+
        geom_vline(aes(xintercept=intercept), 
                   col="black", 
                   alpha=1/4)+
        geom_pointrange(aes(x=estimate, 
                            xmin=lower, 
                            xmax=upper,
                            y=level), 
                        col=wesanderson::wes_palettes$Darjeeling1[1],
                        shape=19,
                        position = position_nudge(y = 1/10),
                        show.legend = T)+
        geom_pointrange(data=effects_subgroup2[effects_subgroup2$feature==attribute, ],
                        aes(x=estimate,
                            xmin=lower,
                            xmax=upper,
                            y=level), 
                        col=wesanderson::wes_palettes$Darjeeling1[2],
                        shape=17,
                        position = position_nudge(y = -1/10),
                        show.legend = T)+
        ylab(attribute)+
        xlab("\n")+
        scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
        scale_x_continuous(limits = c(leftlim, rightlim), 
                           breaks = round(seq(leftlim, rightlim, length.out = 9), digits=2))+
        theme(legend.position = "right",
              axis.text.y = element_text(size=10),
              axis.title.y = element_text(size=12))
      
      v[[attribute]] = p
    }
    
    p1 = (v[["Gender"]]/v[["Age"]]/v[["Religion"]]/v[["Citysize"]]/(v[["Job"]]+xlab("Effect size")))+plot_layout(heights = c(3,3,3,3,4))
    p2 = (v[["Conscientiousness"]]/v[["Openness"]]/v[["Neuroticism"]]/v[["Restaurant"]]/v[["Transport"]]/(v[["Animal"]]+xlab("Effect size")))+plot_layout(heights = c(2,2,2,4,3,4))
    p=p1|p2
    
    p = p+patchwork::plot_annotation(caption= paste0("Circle = ", subgroup1, "\nTriangle = ", subgroup2))
    
  }
  
  if(estimator == "mm_differences" | estimator == "amce_differences")
    
  {
    effects_pooled <- data |>
      cj(formula, 
         id = ~respid,
         by = ~temp_subgroup,
         estimate = estimator)
    
    effects_pooled = set_categories_and_levels(effects_pooled,
                                               attributes = attributes)
    
    
    effects_pooled = effects_pooled[effects_pooled$level != "Non-binary", ] #not enough power!
    
    
    
    v = list()
    
    if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
      #with [-1; 1] for amces and [0, 1] for mm
    {
      
      leftlim=-0.5
      rightlim=0.5
    }
    if(x_intercept==999)
    {
      intercept = 0
    }
    
    v=list()
    

    for(attribute in unique(attributes))
    {
      p = ggplot(effects_pooled[effects_pooled$feature==attribute, ])+
        geom_vline(aes(xintercept=intercept), 
                   col="black", 
                   alpha=1/4)+
        geom_pointrange(aes(x=estimate, 
                            xmin=lower, 
                            xmax=upper,
                            y=level), 
                        col=wesanderson::wes_palettes$Darjeeling1[1],
                        shape=19)+
        ylab(attribute)+
        xlab("\n")+
        scale_x_continuous(limits = c(leftlim, rightlim), 
                           breaks = round(seq(leftlim, rightlim, length.out = 9), digits=3))+
        theme(legend.position = "right",
              axis.text.y = element_text(size=10),
              axis.title.y = element_text(size=12))
      
      v[[attribute]] = p
    }
    
    p1 = (v[["Gender"]]/v[["Age"]]/v[["Religion"]]/v[["Citysize"]]/(v[["Job"]]+xlab("Effect size")))+plot_layout(heights = c(3,3,3,3,4))
    p2 = (v[["Conscientiousness"]]/v[["Openness"]]/v[["Neuroticism"]]/v[["Restaurant"]]/v[["Transport"]]/(v[["Animal"]]+xlab("Effect size")))+plot_layout(heights = c(2,2,2,4,3,4))
    p=p1|p2
    
    p = p+patchwork::plot_annotation(caption= paste0("Differences ", unique(effects_pooled$BY)))
                                                     
    
  }
  
  ggsave(paste0(output_wd, subdir, subgroup_name, estimator, ".png"), 
         p, 
         height = 12, 
         width = 12, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir, subgroup_name, estimator, ".rds"))
  
  saveRDS(effects_pooled, file = paste0(output_wd, subdir, subgroup_name, estimator, "_data.rds"))
}







full_analysis = function(data,
                         formula, #the conjoint formula
                         estimator=c("mm","amce"), #marginal means and amces
                         leftlim=999,
                         rightlim=999,
                         intercept=999,
                         subdir,#the subdirectory where the plots will be saved
                         continuous=F #to change if we are dealing with continuous outcome
){
  
  
  ###### This function performs the whole analysis, draws the graphs and saves
  #them in the appropriate repositories. 
  #It calls the other functions previously defined plus the functions in  the R
  #libraries cjregg and patchwork
  

  estimator=match.arg(estimator)
  
  effects_pooled <- data |>
    cj(formula, 
       id = ~respid,
       estimate = estimator)
  
  effects_pooled = set_categories_and_levels(effects_pooled,
                                                    attributes = attributes)
  
  if(continuous==F)
  {
  p = draw_plot_effects(effects_pooled,
                        estimator=estimator,
                        y_labels=y_labels_plots,
                        leftlim,
                        rightlim,
                        intercept)
  }
  if(continuous == T)
  {
    p = draw_plot_effects(effects_pooled,
                          estimator=estimator,
                          y_labels=y_labels_plots,
                          leftlim = leftlim, 
                          rightlim = rightlim,
                          intercept = intercept,
                          continuous=T)
  }
  
  p=p+patchwork::plot_annotation(title = paste("Effects of the attributes Classic Conjoint Experiment"),
                                 #caption= toupper(estimator)
                                 )
  
  ggsave(paste0(output_wd, subdir,"singlecountry.png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir,"singlecountry.rds"))

  saveRDS(effects_pooled, file = paste0(output_wd, subdir,"_data_singlecountry.rds"))
  
  return(p)
  
}

#Our levels regarding match and mismatches (for labeling)


y_labels_plots = list(gender=c("Female", "Male", "Non-binary"),
                      age=c("25 years old","45 years old","65 years old"),
                      religion=c("Non believer","Non practitioner", "Practitioner"),
                      citysize=c("Big","Medium", "Small"), #ricorda di correggere l'ordine di sti factor
                      job=c("Entrepreneur", "Lawyer", "Teacher", "Waiter"),
                      conscientiousness=c("Disorganized", "Reliable"),
                      openness=c("Open", "Rigid"),
                      neuroticism=c("Anxious", "Calm"),
                      restaurant=c("Asian","Steakhouse", "Traditional", "Vegan"),
                      transport=c("Bicycle","Public Transport","SUV"),
                      animal=c("Cat","Large dog","No pets","Small dog")
)


levels_vector= unlist(y_labels_plots, use.names = F)

attributes= c("Gender", "Gender", "Gender",
              "Age","Age","Age",
              "Religion","Religion","Religion",
              "Citysize","Citysize","Citysize",
              "Job","Job","Job","Job",
              "Conscientiousness","Conscientiousness",
              "Openness","Openness",
              "Neuroticism","Neuroticism",
              "Restaurant","Restaurant","Restaurant","Restaurant",
              "Transport","Transport","Transport",
              "Animal","Animal","Animal","Animal"
)



#############################################################

#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context
#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"

#outcome = "ideology"
#outcome = "populism"

# gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"
# dataset_rep = paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/")

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/classic_conjoint_design/singlecountry/", outcome,"/", context, "/")
data = readRDS(paste0(dataset_rep, "cjdata_ccd_", context, ".RDS"))


#The continuous outcome should only be used for profile A, which is the one to the left
data_continuous = data[data$ccd_profile_number == 1, ]


if(outcome == "ideology")
{
  formula_outcome = ccd_chosen_rw ~ ccd_gender+
    ccd_age+ccd_religion+ccd_citysize+ccd_job+
    ccd_consc+ccd_ope+ccd_neu+
    ccd_restaurant+ccd_transport+ccd_animal
  
  formula_continuous = ccd_continuous ~ ccd_gender+
    ccd_age+ccd_religion+ccd_citysize+ccd_job+
    ccd_consc+ccd_ope+ ccd_neu+
    ccd_restaurant+ccd_transport+ccd_animal
}
if(outcome == "populism")
{
  formula_outcome = ccd_populism ~ ccd_gender+
    ccd_age+ccd_religion+ccd_citysize+ccd_job+
    ccd_consc+ccd_ope+ccd_neu+
    ccd_restaurant+ccd_transport+ccd_animal
}


#############################################################

######################################
############ EFFECTS ################# 
######################################

################# MAIN EFFECTS #####################


subdir = "MMs/"

full_analysis(data,
              formula=formula_outcome,
              estimator="mm",
              subdir=subdir,
              leftlim = 0.4,
              rightlim = 0.6,
              intercept = 0.5)


### Same as before, but with AMCEs (for appendix)

subdir = "AMCEs/"

full_analysis(data,
              formula=formula_outcome,
              estimator="amce",
              subdir=subdir,
              leftlim = -0.2,
              rightlim = 0.2,
              intercept=0)

######### Continuous outcome, mm

if(outcome == "ideology")
{
  subdir = "Continuous/"
  
  full_analysis(data_continuous,
                formula=formula_continuous,
                estimator="mm",
                subdir=subdir,
                leftlim = 5, 
                rightlim = 8,
                intercept =5,
                continuous = T)
}



################# ACIEs (interaction effects) #####################

subdir = "Interactions/MMs/"

###sociodemos

# age and religion
data$interacted_sociodemos = interaction(data$ccd_age, data$ccd_religion, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos 
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
}


full_interaction_effects(data, formula_interaction_sociodemos,
                         estimator = "mm",
                         leftlim = 0.35,
                         rightlim = 0.65,
                         intercept = 0.5,
                         "sociodemos_religionage")

#job and age

data$interacted_sociodemos = interaction(data$ccd_age, data$ccd_job, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos 
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
}

full_interaction_effects(data, formula_interaction_sociodemos,
                         estimator = "mm",
                         leftlim = 0.35,
                         rightlim = 0.65,
                         intercept = 0.5,
                         "sociodemos_jobage")

### job and religion


data$interacted_sociodemos = interaction(data$ccd_religion, data$ccd_job, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos 
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
}

full_interaction_effects(data, formula_interaction_sociodemos,
                         estimator = "mm",
                         leftlim = 0.35,
                         rightlim = 0.65,
                         intercept = 0.5,
                         "sociodemos_jobreligion")

#psycho

data$interacted_psycho = interaction(data$ccd_consc, data$ccd_ope, data$ccd_neu, sep ="\n")

if(outcome == "ideology")
{
  formula_interaction_psycho = ccd_chosen_rw ~ interacted_psycho
}

if(outcome == "populism")
{
  formula_interaction_psycho = ccd_populism ~ interacted_psycho
}

full_interaction_effects(data, formula_interaction_psycho,
                         estimator = "mm",
                         leftlim = 0.35,
                         rightlim = 0.65,
                         intercept = 0.5,
                         "psycho")

#cultural

data$interacted_cultural = interaction(data$ccd_restaurant, data$ccd_transport, sep ="\n")

if(outcome == "ideology")
{
  formula_interaction_cultural = ccd_chosen_rw ~ interacted_cultural
}

if(outcome == "populism")
{
  formula_interaction_cultural = ccd_populism ~ interacted_cultural
}

formula_interaction_cultural = ccd_chosen_rw ~ interacted_cultural

full_interaction_effects(data, formula_interaction_cultural,
                         estimator = "mm",
                         leftlim = 0.35,
                         rightlim = 0.65,
                         intercept = 0.5,
                         "cultural")



########## Same but with AMCEs

subdir = "Interactions/AMCEs/"

###sociodemos

# age and religion
data$interacted_sociodemos = interaction(data$ccd_age, data$ccd_religion, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos 
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
}


full_interaction_effects(data, formula_interaction_sociodemos,
                         estimator = "amce",
                         leftlim = -0.15,
                         rightlim = 0.15,
                         intercept = 0,
                         "sociodemos_religionage")

#job and age

data$interacted_sociodemos = interaction(data$ccd_age, data$ccd_job, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos 
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
}

full_interaction_effects(data, formula_interaction_sociodemos,
                         estimator = "amce",
                         leftlim = -0.15,
                         rightlim = 0.15,
                         intercept = 0,
                         "sociodemos_jobage")

### job and religion


data$interacted_sociodemos = interaction(data$ccd_religion, data$ccd_job, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos 
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
}

full_interaction_effects(data, formula_interaction_sociodemos,
                         estimator = "amce",
                         leftlim = -0.15,
                         rightlim = 0.15,
                         intercept = 0,
                         "sociodemos_jobreligion")

#psycho

data$interacted_psycho = interaction(data$ccd_consc, data$ccd_ope, data$ccd_neu, sep ="\n")

if(outcome == "ideology")
{
  formula_interaction_psycho = ccd_chosen_rw ~ interacted_psycho
}

if(outcome == "populism")
{
  formula_interaction_psycho = ccd_populism ~ interacted_psycho
}

full_interaction_effects(data, formula_interaction_psycho,
                         estimator = "amce",
                         leftlim = -0.15,
                         rightlim = 0.15,
                         intercept = 0,
                         "psycho")

#cultural

data$interacted_cultural = interaction(data$ccd_restaurant, data$ccd_transport, sep ="\n")

if(outcome == "ideology")
{
  formula_interaction_cultural = ccd_chosen_rw ~ interacted_cultural
}

if(outcome == "populism")
{
  formula_interaction_cultural = ccd_populism ~ interacted_cultural
}

formula_interaction_cultural = ccd_chosen_rw ~ interacted_cultural

full_interaction_effects(data, formula_interaction_cultural,
                         estimator = "amce",
                         leftlim = -0.15,
                         rightlim = 0.15,
                         intercept = 0,
                         "cultural")



################################################################
################ SUBGROUP ANALYSES #############################
################################################################

subdir = "Subgroup Analyses/"

data$gender_r = factor(ifelse(data$gender == "nonbinary", NA, toTitleCase(data$gender)))

full_subgroup_analysis(data,
                       formula=formula_outcome,
                       estimator="mm",
                       y_labels=y_labels_plots,
                       subdir,
                       leftlim = 0.3,
                       rightlim = 0.7,
                       subgroup_variable = "gender_r",
                       subgroup_name = "Gender",
                       subgroup1 = "Female",
                       subgroup2 = "Male" #the name of the second subgroup (variable level)
)

full_subgroup_analysis(data,
                       formula=formula_outcome,
                       estimator="mm_differences",
                       y_labels=y_labels_plots,
                       subdir,
                       subgroup_variable = "gender_r",
                       subgroup_name = "Gender",
                       subgroup1 = "Female",
                       subgroup2 = "Male" #the name of the second subgroup (variable level)
)


data$educ_r = ifelse(data$EDU_LEVEL =="nocollege", "No college", data$EDU_LEVEL)

data$educ_r = factor(toTitleCase(data$educ_r))

full_subgroup_analysis(data,
                       formula=formula_outcome,
                       estimator="mm",
                       y_labels=y_labels_plots,
                       subdir,
                       leftlim = 0.3,
                       rightlim = 0.7,
                       subgroup_variable = "educ_r",
                       subgroup_name = "Education level",
                       subgroup1 = "College",
                       subgroup2 = "No College" #the name of the second subgroup (variable level)
)

full_subgroup_analysis(data,
                       formula=formula_outcome,
                       estimator="mm_differences",
                       y_labels=y_labels_plots,
                       subdir,
                       subgroup_variable = "educ_r",
                       subgroup_name = "Education level",
                       subgroup1 = "College",
                       subgroup2 = "No College" #the name of the second subgroup (variable level)
)



data$interest_r = ifelse(data$interest_dummy =="no_interest", "Low", "High")

data$interest_r = factor(toTitleCase(data$interest_r))

full_subgroup_analysis(data,
                       formula=formula_outcome,
                       estimator="mm",
                       y_labels=y_labels_plots,
                       subdir,
                       leftlim = 0.3,
                       rightlim = 0.7,
                       subgroup_variable = "interest_r",
                       subgroup_name = "Political Interest",
                       subgroup1 = "Low",
                       subgroup2 = "High" #the name of the second subgroup (variable level)
)

full_subgroup_analysis(data,
                       formula=formula_outcome,
                       estimator="mm_differences",
                       y_labels=y_labels_plots,
                       subdir,
                       subgroup_variable = "interest_r",
                       subgroup_name = "Political Interest",
                       subgroup1 = "Low",
                       subgroup2 = "High"  #the name of the second subgroup (variable level)
)



data$exposure_r = ifelse(data$exposure_dummy =="less10min", "Low", "High")

data$exposure_r = factor(toTitleCase(data$exposure_r))

full_subgroup_analysis(data,
                       formula=formula_outcome,
                       estimator="mm",
                       y_labels=y_labels_plots,
                       subdir,
                       leftlim = 0.3,
                       rightlim = 0.7,
                       subgroup_variable = "exposure_r",
                       subgroup_name = "News media Exposure",
                       subgroup1 = "Low",
                       subgroup2 = "High" #the name of the second subgroup (variable level)
)

full_subgroup_analysis(data,
                       formula=formula_outcome,
                       estimator="mm_differences",
                       y_labels=y_labels_plots,
                       subdir,
                       subgroup_variable = "exposure_r",
                       subgroup_name = "News media Exposure",
                       subgroup1 = "Low",
                       subgroup2 = "High"  #the name of the second subgroup (variable level)
)



### altre da aggiungere
######### 
######### Ideology
######### 

data$temp_subgroup = factor(data[, "ideology_r"])

plot(cj(data, formula_outcome,
        id = ~respid, estimate = "mm", by = ~temp_subgroup))







estimator = "mm"
  
    effects_pooled <- data |>
      cj(formula_outcome, 
         id = ~respid,
         by = ~temp_subgroup,
         estimate = estimator)
    
    effects_pooled = set_categories_and_levels(effects_pooled,
                                               attributes = attributes)
    
    
    effects_pooled = effects_pooled[effects_pooled$level != "Non-binary", ] #not enough power!
    
    effects_subgroup1 = effects_pooled |>
      filter(temp_subgroup == "Left-wing")
    
    effects_subgroup2 = effects_pooled |>
      filter(temp_subgroup == "Center")
    
    effects_subgroup3 = effects_pooled |>
      filter(temp_subgroup == "Right-wing")
    
    effects_subgroup4 = effects_pooled |>
      filter(temp_subgroup == "Not collocated")
    
    leftlim=0.3
    rightlim=0.7
    intercept=0.5
    
    v=list()
    
    for(attribute in unique(attributes))
    {
      
      p = ggplot(effects_subgroup1[effects_subgroup1$feature==attribute, ])+
        geom_vline(aes(xintercept=intercept), 
                   col="black", 
                   alpha=1/4)+
        geom_pointrange(aes(x=estimate, 
                            xmin=lower, 
                            xmax=upper,
                            y=level), 
                        col=wesanderson::wes_palettes$Darjeeling1[1],
                        shape=19,
                        position = position_nudge(y = +3/16),
                        show.legend = T)+
        geom_pointrange(data=effects_subgroup2[effects_subgroup2$feature==attribute, ],
                        aes(x=estimate,
                            xmin=lower,
                            xmax=upper,
                            y=level), 
                        col=wesanderson::wes_palettes$Darjeeling1[2],
                        shape=17,
                        position = position_nudge(y = 1/16),
                        show.legend = T)+
        geom_pointrange(data=effects_subgroup3[effects_subgroup3$feature==attribute, ],
                        aes(x=estimate,
                            xmin=lower,
                            xmax=upper,
                            y=level), 
                        col=wesanderson::wes_palettes$Darjeeling1[3],
                        shape=18,
                        position = position_nudge(y = -1/16),
                        show.legend = T)+
        geom_pointrange(data=effects_subgroup2[effects_subgroup4$feature==attribute, ],
                        aes(x=estimate,
                            xmin=lower,
                            xmax=upper,
                            y=level), 
                        col=wesanderson::wes_palettes$Darjeeling1[4],
                        shape=15,
                        position = position_nudge(y = -3/16),
                        show.legend = T)+
        ylab(attribute)+
        xlab("\n")+
        xlim(leftlim,rightlim)+
        #scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
        theme(legend.position = "right",
              axis.text.y = element_text(size=10),
              axis.title.y = element_text(size=12))
      
      v[[attribute]] = p
    }
    
    p1 = (v[["Gender"]]/v[["Age"]]/v[["Religion"]]/v[["Citysize"]]/(v[["Job"]]+xlab("Effect size")))+plot_layout(heights = c(3,3,3,3,4))
    p2 = (v[["Conscientiousness"]]/v[["Openness"]]/v[["Neuroticism"]]/v[["Restaurant"]]/v[["Transport"]]/(v[["Animal"]]+xlab("Effect size")))+plot_layout(heights = c(2,2,2,4,3,4))
    
    p=p1|p2
    
    p = p+patchwork::plot_annotation(caption= paste0("Circle = Left-wing, Triangle = Center\n Diamond = Right-wing, Square = Not collocated"))
    

    ggsave(paste0(output_wd, subdir,"ideology_singlecountry.png"), 
           p, 
           height = 12, 
           width = 10, create.dir = T)

    
    ### test of significance differences between left and right

    data1 = data |>
      filter(ideology_r == "Right-wing" | ideology_r == "Left-wing")
    
   
    data1$ideology_r = factor(data1$ideology_r, levels =c("Left-wing", "Right-wing"))
    
    full_subgroup_analysis(data1,
                           formula=formula_outcome,
                           estimator="mm_differences",
                           y_labels=y_labels_plots,
                           subdir,
                           leftlim=-0.15,
                           rightlim=0.15,
                           subgroup_variable = "ideology_r",
                           subgroup_name = "Political Ideology",
                           subgroup1 = "Left",
                           subgroup2 = "Right"  #the name of the second subgroup (variable level)
    )
    
