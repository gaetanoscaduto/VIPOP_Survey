# DA RICONTROLLARE


###############################################################################
#this script is for the analyses related to the CLASSIC conjoint design, 
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
                             x_intercept=999
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
  else #continuous outcome 0-10s
  {
    intercept = 5
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
      scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
      theme(legend.position = "none",
            axis.text.y = element_text(size=10),
            axis.title.y = element_text(size=12))
    
    v[[attribute]] = p
  }
  
  p = (v[["Gender"]]/v[["Age"]]/v[["Religion"]]/v[["Citysize"]]/(v[["Job"]]+xlab("Effect size")))|(v[["Conscientiousness"]]/v[["Openness"]]/v[["Neuroticism"]]/v[["Restaurant"]]/v[["Transport"]]/(v[["Animal"]]+xlab("Effect size")))
  return(p)
}


full_interaction_effects = function(data, 
                                    formula,
                                    type_of_interaction){
  
  effects <- data |>
    cj(formula, 
       id = ~respid,
       estimate = "mm")
  
  p=ggplot(effects)+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                        y=fct_reorder(level, desc(estimate)), col=feature))+
    labs(y="",x="Marginal Mean")+
    xlim(-0.01,1.01)+
    theme(legend.position = "none",
          axis.text.y = element_text(size=10),
          axis.title.y = element_text(size=12))
  
  ggsave(paste0(output_wd, subdir,"interacted_", type_of_interaction, ".png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir,"interacted_", type_of_interaction, ".rds"))
  
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
        xlim(leftlim,rightlim)+
        scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
        theme(legend.position = "right",
              axis.text.y = element_text(size=10),
              axis.title.y = element_text(size=12))
      
      v[[attribute]] = p
    }
    
    p = (v[["Gender"]]/v[["Age"]]/v[["Religion"]]/v[["Citysize"]]/(v[["Job"]]+xlab("Effect size")))|(v[["Conscientiousness"]]/v[["Openness"]]/v[["Neuroticism"]]/v[["Restaurant"]]/v[["Transport"]]/(v[["Animal"]]+xlab("Effect size")))
    
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
        xlim(leftlim,rightlim)+
        scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
        theme(legend.position = "right",
              axis.text.y = element_text(size=10),
              axis.title.y = element_text(size=12))
      
      v[[attribute]] = p
    }
    
    p = (v[["Gender"]]/v[["Age"]]/v[["Religion"]]/v[["Citysize"]]/(v[["Job"]]+xlab("Effect size")))|(v[["Conscientiousness"]]/v[["Openness"]]/v[["Neuroticism"]]/v[["Restaurant"]]/v[["Transport"]]/(v[["Animal"]]+xlab("Effect size")))
    
    p = p+patchwork::plot_annotation(caption= paste0("Differences ", unique(effects_pooled$BY)))
                                                     
    
  }
  
  ggsave(paste0(output_wd, subdir, subgroup_name, estimator, ".png"), 
         p, 
         height = 12, 
         width = 12, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir, subgroup_name, estimator, ".rds"))
}







full_analysis = function(data,
                         formula, #the conjoint formula
                         estimator=c("mm","amce"), #marginal means and amces
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
                        y_labels=y_labels_plots)
  }
  else
  {
    p = draw_plot_effects(effects_pooled,
                          estimator=estimator,
                          y_labels=y_labels_plots,
                          leftlim = 0, 
                          rightlim = 10)
  }
  
  p=p+patchwork::plot_annotation(title = paste("Effects of the attributes Classic Conjoint Experiment"),
                                 caption= toupper(estimator))
  
  ggsave(paste0(output_wd, subdir,"singlecountry.png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir,"singlecountry.rds"))
  
  return(p)
  
}

#Our levels regarding match and mismatches (for labeling)


y_labels_plots = list(gender=c("Female", "Male", "Non-binary"),
                      age=c("25","45","65"),
                      religion=c("Practitioner","Non practitioner", "Non believer"),
                      citysize=c("Big", "Small", "Medium"), #ricorda di correggere l'ordine di sti factor
                      job=c("Entrepreneur", "Teacher", "Waiter", "Lawyer"),
                      consc=c("Reliable", "Disorganized"),
                      ope=c("Open", "Rigid"),
                      neu=c("Calm", "Anxious"),
                      restaurant=c("Traditional", "Vegan","Asian","Steakhouse"),
                      transport=c("Bycicle","Public Transport","SUV"),
                      animal=c("Large dog","Small dog","Cat", "No pets")
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


formula_rw = ccd_chosen_rw ~ ccd_gender+
  ccd_age+ccd_religion+ccd_citysize+ccd_job+
  ccd_consc+ccd_ope+ccd_neu+
  ccd_restaurant+ccd_transport+ccd_animal

formula_continuous = ccd_continuous ~ ccd_gender+
  ccd_age+ccd_religion+ccd_citysize+ccd_job+
  ccd_consc+ccd_ope+ ccd_neu+
  ccd_restaurant+ccd_transport+ccd_animal


#############################################################

#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context
#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"


# gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"
# dataset_rep = paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/")

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/classic_conjoint_design/singlecountry/", context, "/")
data = readRDS(paste0(dataset_rep, "cjdata_ccd_", context, ".RDS"))


#############################################################

######################################
############ EFFECTS ################# 
######################################

################# MAIN EFFECTS #####################


subdir = "MMs/"

full_analysis(data,
              formula_rw,
              "mm",
              subdir)


### Same as before, but with AMCEs (for appendix)

subdir = "AMCEs/"

full_analysis(data,
              formula_rw,
              "amce",
              subdir)

######### Continuous outcome, mm

subdir = "Continuous/"

full_analysis(data,
              formula_continuous,
              "mm",
              subdir,
              continuous = T)



################# ACIEs (interaction effects) #####################

subdir = "Interactions/"

###sociodemos

#gender and age
data$interacted_sociodemos = interaction(data$ccd_age, data$ccd_gender, sep =" ")

formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos

full_interaction_effects(data, formula_interaction_sociodemos, "sociodemos_genderage")

#job and gender

data$interacted_sociodemos = interaction(data$ccd_gender, data$ccd_job, sep =" ")

formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos

full_interaction_effects(data, formula_interaction_sociodemos, "sociodemos_jobgender")

#psycho

data$interacted_psycho = interaction(data$ccd_consc, data$ccd_ope, data$ccd_neu, sep =" ")

formula_interaction_psycho = ccd_chosen_rw ~ interacted_psycho

full_interaction_effects(data, formula_interaction_psycho, "psycho")

#cultural

data$interacted_cultural = interaction(data$ccd_restaurant, data$ccd_transport, sep =" ")

formula_interaction_cultural = ccd_chosen_rw ~ interacted_cultural

full_interaction_effects(data, formula_interaction_cultural, "cultural")




################################################################
################ SUBGROUP ANALYSES #############################
################################################################

subdir = "Subgroup Analyses/"

data$gender_r = factor(ifelse(data$gender == "nonbinary", NA, toTitleCase(data$gender)))

full_subgroup_analysis(data,
                       formula=formula_rw,
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
                       formula=formula_rw,
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
                       formula=formula_rw,
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
                       formula=formula_rw,
                       estimator="mm_differences",
                       y_labels=y_labels_plots,
                       subdir,
                       subgroup_variable = "educ_r",
                       subgroup_name = "Education level",
                       subgroup1 = "College",
                       subgroup2 = "No College" #the name of the second subgroup (variable level)
)



### altre da aggiungere





