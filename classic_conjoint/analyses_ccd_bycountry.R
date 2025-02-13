#classic_conjoint_analyses_bycountry


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

#############################################################
# SET CATEGORIES AND LEVELS
#############################################################

#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio

set_categories_and_levels_visual_bycountry = function(effects){
  effects <- effects %>%
    mutate(feature = gsub("ccd_", "", feature) %>% # Remove "vcd_"
             tools::toTitleCase())   
  
  return(effects)
}



#############################################################
# DRAW PLOT EFFECTS
#############################################################

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
    
    leftlim=ifelse(estimator!="mm", -0.3, 0.2)
    rightlim=ifelse(estimator!="mm", 0.3, 0.7)
  }
  
  if(x_intercept == 999)
  {
    intercept = ifelse(estimator!="mm", 0, 0.5)
  }
  
  effects_IT= effects_bycountry |> filter(ccd_country=="IT")
  effects_FR= effects_bycountry |> filter(ccd_country=="FR")
  effects_SW= effects_bycountry |> filter(ccd_country=="SW")
  effects_CZ= effects_bycountry |> filter(ccd_country=="CZ")
  
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
                      show.legend = F
      )+
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
                      size=1,
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
  
  if(estimator =="mm")
  {
    leftlim_gender=0.1
    rightlim_gender=0.9
  }
  
  if(estimator =="amce")
  {
    leftlim_gender=-0.4
    rightlim_gender=0.4
  }
  
  v[["Gender"]] = v[["Gender"]] + scale_x_continuous(limits = c(leftlim_gender, rightlim_gender), 
                                                     breaks = round(seq(leftlim_gender, 
                                                                        rightlim_gender,
                                                                        length.out = 7), 
                                                                    digits=3))
  
  if(estimator =="mm")
  {
    leftlim_transport=0.3
    rightlim_transport=0.7
  }
  
  if(estimator =="amce")
  {
    leftlim_transport=-0.2
    rightlim_transport=0.2
  }
  
  v[["Transport"]] = v[["Transport"]] + scale_x_continuous(limits = c(leftlim_transport, rightlim_transport), 
                                                     breaks = round(seq(leftlim_transport, 
                                                                        rightlim_transport,
                                                                        length.out = 7), 
                                                                    digits=3))
  
  return(v)
}


#############################################################
# FULL INTERACTION EFFECTS
#############################################################


full_interaction_effects_bycountry = function(data,
                                              formula,
                                              type_of_interaction,
                                              leftlim=0.35,
                                              rightlim=0.65){
  
  effects <- data |>
    cj(formula, 
       id = ~respid,
       estimate = "mm",
       by=~ccd_country)
  
  effects_pooled <- data |>
    cj(formula, 
       id = ~respid,
       estimate = "mm")
  
  effects_pooled$ccd_country = "POOL"
  effects_pooled$BY = "POOL"
  
  effects=rbind(effects, effects_pooled)
  
  effects_IT= effects |> filter(ccd_country=="IT")
  effects_FR= effects |> filter(ccd_country=="FR")
  effects_SW= effects |> filter(ccd_country=="SW")
  effects_CZ= effects |> filter(ccd_country=="CZ")
  effects_POOL= effects |> filter(ccd_country=="POOL")
  
  
  
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
                       breaks = round(seq(leftlim, rightlim, length.out = 7), 
                                      digits=3))+
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
         height = 10, 
         width = 10,
         create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir,"interacted_", type_of_interaction, ".rds"))
  
  #saveRDS(effects, file = paste0(output_wd, subdir,"interacted_", type_of_interaction, "_data.rds"))
  
  
}


#############################################################
# FULL ANALYSIS
#############################################################

full_analysis_bycountry = function(data,
                                   formula, #the conjoint formula
                                   estimator=c("mm","amce"), #marginal means and amces
                                   subdir, #the subdirectory where the plots will be saved
                                   continuous=F,#to change if we are dealing with continuous outcome
                                   leftlim=999,
                                   rightlim=999
){
  

  ###### This function performs the whole analysis, draws the graphs and saves
  #them in the appropriate repositories. 
  #It calls the other functions previously defined plus the functions in cjregg and
  #patchwork
  


  estimator=match.arg(estimator)
  
  effects_pooled <- data |>
    cj(formula, 
       id = ~respid,
       estimate = estimator
    )
  
  effects_bycountry <- data |>
    cj(formula, 
       id = ~respid, 
       by = ~ccd_country,
       estimate = estimator
    )
  
  effects_pooled = set_categories_and_levels_visual_bycountry(effects_pooled)
  
  
  effects_bycountry = set_categories_and_levels_visual_bycountry(effects_bycountry)
  

  if(continuous==F)
  {
    v = draw_plot_effects_bycountry(effects_pooled,
                                    effects_bycountry,
                                    estimator=estimator,
                                    leftlim = leftlim,
                                    rightlim = rightlim)
  }
  else
  {
    v = draw_plot_effects_bycountry(effects_pooled,
                                    effects_bycountry,
                                    estimator=estimator,
                                    leftlim = 0,
                                    rightlim = 10)
  }
  
  effects_pooled$BY = "POOL"
  
  effects_pooled$ccd_country = "POOL"
  
  return_list = list(plot = v, effects = effects_bycountry)
  
  return(return_list)
}


#############################################################

#outcome="ideology"
#outcome="populism"

#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/classic_conjoint_design/bycountry/")

data = readRDS(paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_ccd_POOL.RDS"))


if(outcome=="ideology")
{
  formula_outcome = ccd_chosen_rw ~ ccd_gender+
    ccd_age+ccd_religion+ccd_citysize+ccd_profession+
    ccd_consc+ccd_openness+ ccd_neuroticism+
    ccd_restaurant+ccd_transport+ccd_pet
  
  formula_continuous = ccd_continuous ~ ccd_gender+
    ccd_age+ccd_religion+ccd_citysize+ccd_profession+
    ccd_consc+ccd_openness+ ccd_neuroticism+
    ccd_restaurant+ccd_transport+ccd_pet
}


if(outcome=="populism")
{
  formula_outcome = ccd_populism ~ ccd_gender +
    ccd_age+ccd_religion+ccd_citysize+ccd_job+
    ccd_consc+ccd_openness+ ccd_neuroticism+
    ccd_restaurant+ccd_transport+ccd_pet

}


#############################################################

######################################
############ EFFECTS ################# 
######################################



######################################
########### MAIN EFFECTS ################# 
######################################

subdir = "MMs/"

result = full_analysis_bycountry(data,
                            formula_outcome,
                            "mm",
                            subdir,
                            leftlim=0.4,
                            rightlim = 0.6)


for(attribute in unique(result$effects$feature))
{
  p=result$plot[[attribute]]#+patchwork::plot_annotation(title = paste("Effects of the attributes of the Classic Conjoint Experiment, by country"),
                             #                 caption= "Marginal means")
  
  ggsave(paste0(output_wd, subdir, attribute,"_bycountry.png"), 
         p, 
         height = 6, 
         width = 9, 
         create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir, attribute,"_bycountry.rds"))
  
  
}

### Same as before, but with AMCes (for appendix)

subdir = "AMCEs/"

result = full_analysis_bycountry(data,
                           formula_outcome,
                           "amce",
                           subdir,
                           leftlim=-0.1,
                           rightlim = 0.1)

for(attribute in unique(result$effects$feature))
{
  p=result$plot[[attribute]]#+patchwork::plot_annotation(title = paste("Effects of the attributes of the Classic Conjoint Experiment, by country"),
                             #                 caption= "Average marginal component effects")
  
  ggsave(paste0(output_wd, subdir, attribute,"_bycountry.png"), 
         p, 
         height = 6, 
         width = 9, 
         create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir, attribute,"_bycountry.rds"))
  
}


### continuous outcome

# if(outcome == "ideology")
# {
#   subdir = "Continuous/"
#   
#   v= full_analysis_bycountry(data,
#                              formula_continuous,
#                              "mm",
#                              subdir,
#                              continuous = T)
#   
#   for(attribute in unique(attributes))
#   {
#     p=v[[attribute]]+patchwork::plot_annotation(title = paste("Effects of the attributes of the Classic Conjoint Experiment, by country"),
#                                                 caption= "Average marginal component effects")
#     
#     ggsave(paste0(output_wd, subdir, attribute,"_bycountry.png"), 
#            p, 
#            height = 8, 
#            width = 8, 
#            create.dir = T)
#     
#     saveRDS(p, file = paste0(output_wd, subdir, attribute,"_bycountry.rds"))
#     
#     
#   }
#   
# }


######################################
########### ACIEs ################# 
######################################

################# ACIEs (interaction effects) #####################

subdir = "Interactions/"

##############
#sociodemos
################

#Age and gender

data$interacted_sociodemos = interaction(data$ccd_age, data$ccd_gender, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
}

full_interaction_effects_bycountry(data, formula_interaction_sociodemos, "sociodemos_agegender",
                                   leftlim = 0.1,
                                   rightlim = 0.7)

#Age and religion

data$interacted_sociodemos = interaction(data$ccd_age, data$ccd_religion, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
  
}

full_interaction_effects_bycountry(data, formula_interaction_sociodemos, "sociodemos_agereligion")

#Age and job

data$interacted_sociodemos = interaction(data$ccd_age, data$ccd_profession, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
}

full_interaction_effects_bycountry(data, formula_interaction_sociodemos, "sociodemos_agejob")

#Job and religion

data$interacted_sociodemos = interaction(data$ccd_profession, data$ccd_religion, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_sociodemos = ccd_chosen_rw ~ interacted_sociodemos
}

if(outcome == "populism")
{
  formula_interaction_sociodemos = ccd_populism ~ interacted_sociodemos
}

full_interaction_effects_bycountry(data, formula_interaction_sociodemos, "sociodemos_jobreligion")


################
#psychological variables
##############

data$interacted_psycho = interaction(data$ccd_consc, data$ccd_openness, data$ccd_neuroticism, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_psycho = ccd_chosen_rw ~ interacted_psycho
}

if(outcome == "populism")
{
  formula_interaction_psycho = ccd_populism ~ interacted_psycho
}

full_interaction_effects_bycountry(data, formula_interaction_psycho, "psycho")

#################
#lifestyle variables
##############

data$interacted_cultural = interaction(data$ccd_restaurant, data$ccd_transport, sep =" ")

if(outcome == "ideology")
{
  formula_interaction_cultural = ccd_chosen_rw ~ interacted_cultural
}

if(outcome == "populism")
{
  formula_interaction_cultural = ccd_populism ~ interacted_cultural
}

p = full_interaction_effects_bycountry(data, formula_interaction_cultural, "cultural")




