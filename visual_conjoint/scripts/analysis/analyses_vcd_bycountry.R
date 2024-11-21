#visual_conjoint_analyses_bycountry

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
#   patchwork, rio, texreg, tools
# )

#############################################################
# DEFINING FUNCTIONS
#############################################################

###SET CATEGORIES AND LEVELS

#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio

set_categories_and_levels_visual_bycountry = function(effects, 
                                                      attributes=attributes){
  # effects=effects_pooled
  # attributes=attributes
  effects$feature = factor(attributes, levels = unique(attributes))
  effects$level=factor(levels_vector, levels = levels_vector)
  
  return(effects)
}



##Function to draw plots for the effects

draw_plot_effects_bycountry = function(effects_pooled,
                                       effects_bycountry,
                                       estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                                       y_labels=y_labels_plots,
                                       leftlim=999, #the left limit of the plot
                                       rightlim=999,#the right limit of the plot
                                       x_intercept=999 #the vertical line to signal the difference from the insignificance
){
  # 
  # y_labels=y_labels_plots
  # leftlim=999 #the left limit of the plot
  # rightlim=999#the right limit of the plot
  # x_intercept=999 #th
  
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
  
  effects_IT= effects_bycountry |> filter(vcd_country=="IT")
  effects_FR= effects_bycountry |> filter(vcd_country=="FR")
  effects_SW= effects_bycountry |> filter(vcd_country=="SW")
  effects_CZ= effects_bycountry |> filter(vcd_country=="CZ")
  
  v=list()
  for(attribute in unique(attributes))
  {
    these_labels = rev(y_labels_plots[[tolower(attribute)]])
    p = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(data=effects_IT[effects_IT$feature == attribute, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "IT", shape = "IT"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/5),
                      show.legend = T)+
      geom_pointrange(data=effects_FR[effects_FR$feature == attribute, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "FR", shape = "FR"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/10),
                      show.legend = T)+
      geom_pointrange(data=effects_SW[effects_SW$feature == attribute, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "SW", shape = "SW"),
                      alpha = 1,
                      #size=1.3,
                      show.legend = T)+
      geom_pointrange(data=effects_CZ[effects_CZ$feature == attribute, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "CZ", shape = "CZ"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = -1/10),
                      show.legend = T)+
      geom_pointrange(data=effects_pooled[effects_pooled$feature == attribute, ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "POOL", shape = "POOL"),
                      alpha = 1,
                      position = position_nudge(y = -1/5),
                      #size=1.3,
                      show.legend = T)+
      ylab(attribute)+
      xlab("Effect size")+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = these_labels)+
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
    
    
    
    v[[attribute]] = p
  }
  
  
  return_list = list(plot=v, effects = effects_bycountry)
  
  return(return_list)
}

full_interaction_effects_bycountry = function(data,
                                              formula,
                                              type_of_interaction){
  
  effects <- data |>
    cj(formula, 
       id = ~respid,
       estimate = "mm",
       by=~vcd_country,
       h0=0.5)
  
  effects_pooled <- data |>
    cj(formula, 
       id = ~respid,
       estimate = "mm",
       h0=0.5)
  
  effects_pooled$vcd_country = "POOL"
  effects_pooled$BY = "POOL"
  
  effects=rbind(effects, effects_pooled)
  
  effects_IT= effects |> filter(vcd_country=="IT")
  effects_FR= effects |> filter(vcd_country=="FR")
  effects_SW= effects |> filter(vcd_country=="SW")
  effects_CZ= effects |> filter(vcd_country=="CZ")
  effects_POOL= effects |> filter(vcd_country=="POOL")
  
  
  
  p=ggplot()+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(data=effects_IT, aes(x=estimate, xmin=lower, xmax=upper,
                                         y=level, col="IT", shape="IT"),
                    alpha = 1,
                    position = position_nudge(y = 1/5),
                    show.legend = T)+
    geom_pointrange(data=effects_FR, aes(x=estimate, xmin=lower, xmax=upper,
                                         y=level, col="FR", shape="FR"),
                    alpha = 1,
                    position = position_nudge(y = 1/10),
                    show.legend = T)+
    geom_pointrange(data=effects_SW, aes(x=estimate, xmin=lower, xmax=upper,
                                         y=level, col="SW", shape="SW"),
                    alpha = 1,
                    position = position_nudge(y =0),
                    show.legend = T)+
    geom_pointrange(data=effects_CZ, aes(x=estimate, xmin=lower, xmax=upper,
                                         y=level, col="CZ", shape="CZ"),
                    alpha = 1,
                    position = position_nudge(y = -1/10),
                    show.legend = T)+
    geom_pointrange(data=effects_POOL, aes(x=estimate, xmin=lower, xmax=upper,
                                           y=level, col="POOL", shape="POOL"),
                    alpha = 1,
                    position = position_nudge(y = -1/5),
                    show.legend = T)+
    labs(y="",x="Marginal Mean")+
    xlim(-0.1,1.1)+
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
  
  
  ggsave(paste0(output_wd, subdir,"interacted_", type_of_interaction, ".png"), 
         p, 
         height = 12, 
         width = 8,
         create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir, "interacted_", type_of_interaction, ".rds"))
  
  saveRDS(effects, file = paste0(output_wd, subdir, "interacted_", type_of_interaction, "_data.rds"))
  
  
  }




full_analysis_bycountry = function(data,
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
  
  
  h_0 = ifelse(estimator == "mm", 0.5, 0)
  
  effects_pooled <- data |>
    cj(formula, 
       id = ~respid,
       estimate = estimator,
       h0=h_0
       )
  
  effects_bycountry <- data |>
    cj(formula, 
       id = ~respid, 
       by = ~vcd_country,
       estimate = estimator,
       h0=h_0
       )
  

  
  effects_pooled = set_categories_and_levels_visual_bycountry(effects_pooled,
                                                    attributes = attributes)
  
  
  effects_bycountry = set_categories_and_levels_visual_bycountry(effects_bycountry,
                                                    attributes = attributes)
  
  v = draw_plot_effects_bycountry(effects_pooled,
                                  effects_bycountry,
                                  estimator=estimator,
                                  y_labels=y_labels_plots,
                                  leftlim=leftlim,
                                  rightlim=rightlim)
  
  
  #saveRDS(v$effects, file = paste0(output_wd, subdir, "bycountry_data.rds"))
  
  
  return(v$plot)
}

#Our levels regarding match and mismatches (for labeling)


y_labels_plots = list(ethnicity=c("Black","White"),
                      gender=c("Female","Male"),
                      age=c("35", "70"),
                      job=c("Entrepreneur","Lawyer","Politician","Teacher","Waiter"),
                      issue=c("Leftneg","Leftpos","Rightneg","Rightpos"),
                      nostalgia=c("Future1","Future2","Past1","Past2"),
                      valence=c("Corruption1","Corruption2", "Honesty1", "Honesty2"),
                      animal=c("Catpoor","Catrich","Dogpoor","Dogrich"),
                      food=c("Ethnic","Meatpoor","Meatrich", "Vegan"),
                      crowd=c("Mixedelite","Mixedpeople", "Whiteelite","Whitepeople")
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
if(outcome == "populism")
{
  formula_outcome = vcd_chosen_pop ~ vcd_ethnicity + 
    vcd_gender + vcd_age + vcd_job + 
    vcd_issue + vcd_nostalgia + vcd_valence +
    vcd_animal + vcd_food + vcd_crowd
}





#############################################################

#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/bycountry/", outcome, "/")

data = readRDS(paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_vcd_POOL.RDS"))

#############################################################

######################################
############ EFFECTS ################# 
######################################


subdir = "MMs/"

v = full_analysis_bycountry(data,
              formula_outcome,
              "mm",
              subdir,
              leftlim=0.3,
              rightlim=0.7)


for(attribute in unique(attributes))
{
  p=v[[attribute]]+patchwork::plot_annotation(title = paste("Effects of the attributes of the Visual Conjoint Experiment, by country"),
                                 caption= paste("Marginal means on", outcome, "perceptions"))
  
  ggsave(paste0(output_wd, subdir, attribute,"_bycountry.png"), 
         p, 
         height = 6, 
         width = 6,
         create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir, attribute,"_bycountry.rds"))
  
  
}

### Same as before, but with AMCes (for appendix)

subdir = "AMCEs/"

v= full_analysis_bycountry(data,
              formula_outcome,
              "amce",
              subdir)

for(attribute in unique(attributes))
{
  p=v[[attribute]]+patchwork::plot_annotation(title = paste("Effects of the attributes of the Visual Conjoint Experiment, by country"),
                                              caption= paste("Average marginal component effects on", outcome, "perceptions"))
  
  ggsave(paste0(output_wd, subdir, attribute,"_bycountry.png"), 
         p, 
         height = 6, 
         width = 6,
         create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir, attribute,"_bycountry.rds") )
  
}




#######################
##### ACIES
#####################

subdir = "Interactions/"

##### ACIE of Sociodemos

data$interacted_sociodemos = interaction(data$vcd_age, data$vcd_ethnicity, data$vcd_gender, sep =" ")

formula_interaction_sociodemos = vcd_chosen_rw ~ interacted_sociodemos

full_interaction_effects_bycountry(data,
                                   formula_interaction_sociodemos,
                                       "sociodemos")


##### ACIE of the cultural dimensions


data$interacted_cultural = interaction(data$vcd_food, data$vcd_animal, sep =" ")

formula_interaction_cultural = vcd_chosen_rw ~ interacted_cultural

full_interaction_effects_bycountry(data,
                                   formula_interaction_cultural,
                                   "cultural")


#####  ACIE of the political dimensions


data$interacted_political = interaction(data$vcd_issue, data$vcd_valence, sep =" ")

formula_interaction_political = vcd_chosen_rw ~ interacted_political

full_interaction_effects_bycountry(data,
                                   formula_interaction_political,
                                   "political")



