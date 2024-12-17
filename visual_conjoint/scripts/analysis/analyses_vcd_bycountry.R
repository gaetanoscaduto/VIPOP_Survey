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

####################################
###SET CATEGORIES AND LEVELS
####################################

#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio

set_categories_and_levels_visual_bycountry = function(effects){

  effects <- effects %>%
    mutate(feature = gsub("vcd_", "", feature) %>% # Remove "vcd_"
             tools::toTitleCase())   
  
  return(effects)
}


####################################
##DRAW PLOTS EFFECT BY COUNTRY
####################################

draw_plot_effects_bycountry = function(effects_pooled,
                                       effects_bycountry,
                                       estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
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
  
  effects_IT= effects_bycountry |> filter(vcd_country=="IT")
  effects_FR= effects_bycountry |> filter(vcd_country=="FR")
  effects_SW= effects_bycountry |> filter(vcd_country=="SW")
  effects_CZ= effects_bycountry |> filter(vcd_country=="CZ")
  
  v=list()
  for(attribute in unique(effects_pooled$feature))
  {
    these_labels = rev(unique(effects_pooled[effects_pooled$feature==attribute, ]$level))

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
      scale_x_continuous(limits = c(leftlim, rightlim), 
                         breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
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
  
  return(v)
}


####################################
##DRAW INTERACTION EFFECTS BY COUNTRY
####################################


full_interaction_effects_bycountry = function(data,
                                              formula,
                                              type_of_interaction,
                                              estimator="mm",
                                              leftlim=999,
                                              rightlim=999, 
                                              x_intercept = 999){
  
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
       estimate = estimator,
       by=~vcd_country#,
       #h0=0.5
       )
  
  effects_pooled <- data |>
    cj(formula, 
       id = ~respid,
       estimate = estimator#,
       #h0=0.5
       )
  
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
    scale_x_continuous(limits = c(leftlim, rightlim), 
                       breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
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


####################################
##FULL ANALYSIS BY COUNTRY
####################################

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

  estimator=match.arg(estimator)
  
  h_0 = ifelse(estimator == "mm", 0.5, 0)
  
  effects_pooled <- data |>
    cj(formula, 
       id = ~respid,
       estimate = estimator#,
       #h0=h_0
       )
  
  effects_bycountry <- data |>
    cj(formula, 
       id = ~respid, 
       by = ~vcd_country,
       estimate = estimator#,
       #h0=h_0
       )
  
  effects_pooled = set_categories_and_levels_visual_bycountry(effects_pooled)
  
  effects_bycountry = set_categories_and_levels_visual_bycountry(effects_bycountry)
  
  v = draw_plot_effects_bycountry(effects_pooled,
                                  effects_bycountry,
                                  estimator=estimator,
                                  leftlim=leftlim,
                                  rightlim=rightlim)
  
  
  return_list = list(plot=v, effects = effects_bycountry)
  
  #saveRDS(return_list$effects, file = paste0(output_wd, subdir, "bycountry_data.rds"))

  return(return_list)
}


#############################################################
##### PRELIMINARY STUFF
#############################################################

#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/bycountry/", outcome, "/")

data = readRDS(paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_vcd_POOL.RDS"))


#recoding_functional_equivalents = T

if(recoding_functional_equivalents == T)
{
  output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/bycountry/",
                     "FE/", 
                     outcome, "/")
}

if(recoding_functional_equivalents == F)
{
  output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/bycountry/",
                     "NFE/", 
                     outcome, "/")
}


### fedra suggested to treat  corruption, valence and crowd as functional equivalents
#therefore, here I recode them 
#If you don't want them recoded anymore, just comment the following lines


### recode vcd_valence


if(recoding_functional_equivalents == T)
{
  data <- data |>
    mutate(vcd_valence = case_when(
      vcd_valence == "Corruption1" ~ "Corruption",
      vcd_valence == "Corruption2" ~ "Corruption",
      vcd_valence == "Honesty1" ~ "Honesty",
      vcd_valence == "Honesty2" ~ "Honesty",
      TRUE ~ as.character(vcd_valence)  # Keeps any values not in the list as they are
    )
    )
  
  data$vcd_valence = factor(data$vcd_valence, levels=c("Corruption", "Honesty"))
  
  ### recode time
  
  data <- data |>
    mutate(vcd_time = case_when(
      vcd_time == "Future1" ~ "Future",
      vcd_time == "Future2" ~ "Future",
      vcd_time == "Past1" ~ "Past",
      vcd_time == "Past2" ~ "Past",
      TRUE ~ as.character(vcd_time)  # Keeps any values not in the list as they are
    )
    )
  
  data$vcd_time = factor(data$vcd_time, levels=c("Future", "Past"))
  
  ### recode pet
  
  data <- data |>
    mutate(vcd_pet = case_when(
      vcd_pet == "Catpoor" ~ "Cat",
      vcd_pet == "Catrich" ~ "Cat",
      vcd_pet == "Dogpoor" ~ "Dog",
      vcd_pet == "Dogrich" ~ "Dog",
      TRUE ~ as.character(vcd_pet)  # Keeps any values not in the list as they are
    )
    )
  
  data$vcd_pet = factor(data$vcd_pet, levels=c("Cat", "Dog"))
  
  
}

# outcome = "ideology"
# outcome = "trust"
# outcome = "populism"

if(outcome == "ideology")
{
  formula_outcome = vcd_chosen_rw ~  vcd_ethnicity + 
    vcd_gender + vcd_age + vcd_job + 
    vcd_issue + vcd_time + vcd_valence +
    vcd_food + vcd_pet + vcd_crowd
}

if(outcome == "trust")
{
  formula_outcome = vcd_chosen_trust ~ vcd_ethnicity + 
    vcd_gender + vcd_age + vcd_job + 
    vcd_issue + vcd_time + vcd_valence +
    vcd_food + vcd_pet+ vcd_crowd
}
if(outcome == "populism")
{
  formula_outcome = vcd_chosen_pop ~ vcd_ethnicity + 
    vcd_gender + vcd_age + vcd_job + 
    vcd_issue + vcd_time + vcd_valence +
    vcd_food + vcd_pet+ vcd_crowd
}
######################################
############ EFFECTS ################# 
######################################


subdir = "MMs/"

result = full_analysis_bycountry(data,
              formula_outcome,
              "mm",
              subdir,
              leftlim=0.3,
              rightlim=0.7)


for(attribute in unique(result$effects$feature))
{
  p=result$plot[[attribute]]+patchwork::plot_annotation(title = paste("Effects of the attributes of the Visual Conjoint Experiment, by country"),
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

result = full_analysis_bycountry(data,
              formula_outcome,
              "amce",
              subdir)

for(attribute in unique(result$effects$feature))
{
  p=result$plot[[attribute]]+patchwork::plot_annotation(title = paste("Effects of the attributes of the Visual Conjoint Experiment, by country"),
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


data$interacted_cultural = interaction(data$vcd_food, data$vcd_pet, sep =" ")

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



