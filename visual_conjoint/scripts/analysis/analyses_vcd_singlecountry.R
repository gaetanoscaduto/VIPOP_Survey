#visual_cnjoint_analyses_singlecountry

###############################################################################
#this script is for the analyses related to the visual conjoint design, 
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
#   wesanderson
# )

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
      #scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
      theme(legend.position = "none",
            axis.text.y = element_text(size=10),
            axis.title.y = element_text(size=12))
   
   v[[attribute]] = p
  }
  
  p1 = (v[["Ethnicity"]]/v[["Gender"]]/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+xlab("Effect size")))+plot_layout(heights = c(2,2,2,5,4))
  p2 = (v[["Nostalgia"]]/v[["Valence"]]/v[["Food"]]/v[["Animal"]]/(v[["Crowd"]]+xlab("Effect size")))
  p=p1|p2
  
  return_list= list(plot = p, 
                    effects_data=effects)
  return(return_list)
}



full_subgroup_analysis = function(data,
                                 formula=formula_outcome,
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
    
    effects_pooled = set_categories_and_levels_visual(effects_pooled,
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
        #scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
        theme(legend.position = "right",
              axis.text.y = element_text(size=10),
              axis.title.y = element_text(size=12))
      
      v[[attribute]] = p
    }
    
    p1 = (v[["Ethnicity"]]/v[["Gender"]]/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+xlab("Effect size")))+plot_layout(heights = c(2,2,2,5,4))
    p2 = (v[["Nostalgia"]]/v[["Valence"]]/v[["Food"]]/v[["Animal"]]/(v[["Crowd"]]+xlab("Effect size")))
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
    
    effects_pooled = set_categories_and_levels_visual(effects_pooled,
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
        #scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
        theme(legend.position = "right",
              axis.text.y = element_text(size=10),
              axis.title.y = element_text(size=12))
      
      v[[attribute]] = p
    }
    
    p1 = (v[["Ethnicity"]]/v[["Gender"]]/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+xlab("Effect size")))+plot_layout(heights = c(2,2,2,5,4))
    p2 = (v[["Nostalgia"]]/v[["Valence"]]/v[["Food"]]/v[["Animal"]]/(v[["Crowd"]]+xlab("Effect size")))
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





full_interaction_effects = function(data,
                                    formula, 
                                    estimator = c("mm", "amce"),
                                    leftlim = 999,
                                    rightlim = 999,
                                    x_intercept = 999){
  
  estimator = match.arg(estimator)
  
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
  
  
  effects <- data |>
    cj(formula, 
       id = ~respid,
       estimate = estimator)
  
  p=ggplot(effects)+
    geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                        y=fct_reorder(level, desc(estimate)), col=feature))+
    labs(y="",x="Marginal Mean")+
    xlim(leftlim,rightlim)+
    theme(legend.position = "none",
          axis.text.y = element_text(size=10),
          axis.title.y = element_text(size=12))
  
  
  return_list = list(plot=p, 
                     effects_data = effects)
  
  return(return_list)
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
  result_list = draw_plot_effects(effects_pooled,
                        estimator=estimator,
                        y_labels=y_labels_plots,
                        leftlim,
                        rightlim)
  
  p=result_list$plot
  p=p+patchwork::plot_annotation(title = paste("Effects of the attributes Visual Conjoint Experiment"),
                                 caption= toupper(estimator))
  

  ggsave(paste0(output_wd, subdir,"singlecountry.png"), 
         p, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir,"singlecountry.rds"))
  
  saveRDS(result_list$effects_data, file = paste0(output_wd, subdir,"singlecountry_data.rds"))
  
}

#Our levels regarding match and mismatches (for labeling)


y_labels_plots = list(ethnicity=c("Black","White"),
                      gender=c("Female","Male"),
                      age=c("35", "70"),
                      job=c("Entrepreneur","Lawyer","Politician","Teacher","Waiter"),
                      issue=c("Leftneg","Leftpos","Rightneg","Rightpos"),
                      nostalgia=c("Future1","Future2","Past1","Past2"),
                      valence=c("Corruption1","Corruption2", "Honesty1", "Honesty2"),
                      food=c("Ethnic","Meatpoor","Meatrich", "Vegan"),
                      animal=c("Catpoor","Catrich","Dogpoor","Dogrich"),
                      crowd=c("Mixedelite","Mixedpeople", "Whiteelite", "Whitepeople")
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


#############################################################


#If you launch this script from the master script, make sure to have the context 
# and the outcome fixed. Otherwise, uncomment desired context

#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"

#outcome = "ideology"
#outcome = "trust"
#outcome = "populism"

# gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"
# dataset_rep = paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/")

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/", outcome, "/", context, "/")
data = readRDS(paste0(dataset_rep, "cjdata_vcd_", context, ".RDS"))


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
if(outcome == "populism")
{
  formula_outcome = vcd_chosen_pop ~ vcd_ethnicity + 
    vcd_gender + vcd_age + vcd_job + 
    vcd_issue + vcd_nostalgia + vcd_valence +
    vcd_animal + vcd_food + vcd_crowd
}

######################################
############ EFFECTS ################# 
######################################


######### MAIN EFFECTS ###########


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

################## ACIEs #####################


##### ACIEs


data$interacted_sociodemos = interaction(data$vcd_age, data$vcd_ethnicity, data$vcd_gender, sep ="\n")

data$interacted_cultural = interaction(data$vcd_food, data$vcd_animal, sep ="\n")

data$interacted_political = interaction(data$vcd_issue, data$vcd_valence, sep ="\n")




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

######## ACIEs with MMs

subdir = "Interactions/MMs/"
estimator="mm"


return_list = full_interaction_effects(data, 
                             formula_interaction_sociodemos,
                             estimator,
                             leftlim=0.2,
                             rightlim=0.8)

plot(cj(data, formula_interaction_sociodemos, 
        id = ~respid,
        estimate = estimator), vline = 0.5)

p=return_list$plot

ggsave(paste0(output_wd, subdir,"interacted_sociodemos.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

saveRDS(p, file = paste0(output_wd, subdir,"interacted_sociodemos.rds"))

saveRDS(return_list$effects_data, file = paste0(output_wd, subdir,"interacted_sociodemos_data.rds"))



##### ACIE of the cultural dimensions

return_list = full_interaction_effects(data, 
                             formula_interaction_cultural,
                             estimator,
                             leftlim=0.2,
                             rightlim=0.8)

p=return_list$plot

ggsave(paste0(output_wd, subdir,"interacted_cultural.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

saveRDS(p, file = paste0(output_wd, subdir,"interacted_cultural.rds"))

saveRDS(return_list$effects_data, file = paste0(output_wd, subdir,"interacted_cultural_data.rds"))



#####  ACIE of the political dimensions


return_list = full_interaction_effects(data, 
                             formula_interaction_political,
                             estimator,
                             leftlim=0.2,
                             rightlim=0.8)

p=return_list$plot

ggsave(paste0(output_wd, subdir,"interacted_political.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

saveRDS(p, file = paste0(output_wd, subdir,"interacted_political.rds"))

saveRDS(return_list$effects_data, file = paste0(output_wd, subdir,"interacted_political_data.rds"))



######## ACIEs with AMCEs

subdir = "Interactions/AMCEs/"
estimator="amce"


return_list = full_interaction_effects(data, 
                                       formula_interaction_sociodemos,
                                       estimator)

p=return_list$plot

ggsave(paste0(output_wd, subdir,"interacted_sociodemos.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

saveRDS(p, file = paste0(output_wd, subdir,"interacted_sociodemos.rds"))

saveRDS(return_list$effects_data, file = paste0(output_wd, subdir,"interacted_sociodemos_data.rds"))



##### ACIE of the cultural dimensions

return_list = full_interaction_effects(data, 
                                       formula_interaction_cultural,
                                       estimator)

p=return_list$plot

ggsave(paste0(output_wd, subdir,"interacted_cultural.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

saveRDS(p, file = paste0(output_wd, subdir,"interacted_cultural.rds"))

saveRDS(return_list$effects_data, file = paste0(output_wd, subdir,"interacted_cultural_data.rds"))



#####  ACIE of the political dimensions


return_list = full_interaction_effects(data, 
                             formula_interaction_political,
                             estimator)

p=return_list$plot

ggsave(paste0(output_wd, subdir,"interacted_political.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

saveRDS(p, file = paste0(output_wd, subdir,"interacted_political.rds"))

saveRDS(return_list$effects_data, file = paste0(output_wd, subdir,"interacted_political_data.rds"))


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
                       leftlim = -0.2,
                       rightlim = 0.2,
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
                       leftlim = -0.2,
                       rightlim = 0.2,
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
                       leftlim = -0.2,
                       rightlim = 0.2,
                       subgroup1 = "Low",
                       subgroup2 = "High"    #the name of the second subgroup (variable level)
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
                       leftlim = -0.2,
                       rightlim = 0.2,
                       subgroup1 = "Low",
                       subgroup2 = "High" #the name of the second subgroup (variable level)
)



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

effects_pooled = set_categories_and_levels_visual(effects_pooled,
                                           attributes = attributes)


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

p1 = (v[["Ethnicity"]]/v[["Gender"]]/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+xlab("Effect size")))+plot_layout(heights = c(2,2,2,5,4))
p2 = (v[["Nostalgia"]]/v[["Valence"]]/v[["Food"]]/v[["Animal"]]/(v[["Crowd"]]+xlab("Effect size")))
p=p1|p2

p = p+patchwork::plot_annotation(caption= paste0("Circle = Left-wing, Triangle = Center\n Diamond = Right-wing, Square = Not collocated"))


ggsave(paste0(output_wd, subdir,"Political ideology", estimator, ".png"), 
       p, 
       height = 12, 
       width = 10, create.dir = T)


### test of significance differences between left and right

data1 = data |>
  filter(ideology_r == "Right-wing" | ideology_r == "Left-wing")

# data1 = data1 |>
#   filter(ccd_gender != "Non-Binary")
# 
# data1$ccd_gender = factor(data1$ccd_gender, levels = c("Female", "Male"))
data1$ideology_r = factor(data1$ideology_r, levels =c("Left-wing", "Right-wing"))

full_subgroup_analysis(data1,
                       formula=formula_outcome,
                       estimator="mm_differences",
                       y_labels=y_labels_plots,
                       subdir,
                       leftlim=-0.2,
                       rightlim=0.2,
                       subgroup_variable = "ideology_r",
                       subgroup_name = "Political Ideology",
                       subgroup1 = "Left",
                       subgroup2 = "Right"  #the name of the second subgroup (variable level)
)


