###############################################################################
#this script is for the analyses related to the parallel conjoint design
###############################################################################

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)


#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio
set_categories_and_levels = function(mm, type=c("match","nominal"), features){
  
  mm$category="Sociodemographics"
  
  mm$category=ifelse(grepl("ope",mm$feature) | grepl("consc",mm$feature), 
                     "Psychological",
                     ifelse(grepl("diet",mm$feature) | grepl("animal",mm$feature) | grepl("holiday",mm$feature),
                            "Lifestyle",
                            "Sociodemographics"))
  
  if(type=="match")
  {
    mm$variable = as.character(mm$level)
  
    for(i in 1:nrow(mm))
    {
      mm$variable[i] = toTitleCase(paste(strsplit(as.character(mm$level[i]), "_")[[1]], collapse=" "))
    }
  
    mm$variable
    mm$level=factor(mm$variable, levels = mm$variable)
  
  }
  if(type=="nominal")
  {
    mm$feature = factor(features, levels = unique(features))
    mm$level=factor(levels_vector, levels = levels_vector)
  }
  
  return(mm)
}



##Function to draw plots for match variables

draw_plot_effects = function(mm, type, #"match" or "nominal" 
                             categories=categories, 
                             estimator, #either amce or mm
                             y_labels=y_labels_plots
                           ){
  
  v = list()
  
  leftlim=ifelse(estimator=="amce", -1, 0)
  rightlim=1
  
  intercept = ifelse(estimator=="amce", 0, 0.5)
  
  for(category in categories[1:3])
  {
    p = ggplot(mm[mm$category == category, ])+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level, col=feature))+
      ylab("")+
      xlab(category)+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = rev(y_labels[[type]][[category]])) +
      theme(legend.position = "none")
    
    v[[category]] = p
    
  }
  
  p = patchwork::wrap_plots(v[["Sociodemographics"]],v[["Psychological"]],v[["Lifestyle"]], ncol=1)
    
  return(p)
}




setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/parallel_conjoint/")

output_wd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/conjoint_parallel_design/"
data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_cpd.RDS")

names(data)


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

#Our nominal attributes (here called features)

features= c("Gender", "Gender",
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

####################################
############ ATEs (MATCH/MISMATCH)
######################################

# Estimation: The marginal mean associated with S_i^k=1 for respondents 
#in the natural mediation arm

# Interpretation: The total effect that observing similarity in attribute k 
#has on the willingness to engage in political conversation when no political 
#information is given to the respondent, whether through political inferences or not.


mm <- cj(data[data$cpd_exparm == "natural", ],
         formula_match,
         id = ~respid,
         estimate = "mm")

mm=set_categories_and_levels(mm, "match", features=features)

mm


p = draw_plot_effects(mm, "match", categories=categories, estimator="mm",y_labels=y_labels_plots)

p=p+plot_annotation(title = "ATEs of the Parallel Design Conjoint Experiment",
                    caption="Marginal means of the natural mediation arm")

#p
ggsave(paste0(output_wd,"estimations/", "ATEs_mm_general_base_match.png"), p, height = 10, width = 10)



### Same as before, but with AMCes (for appendix)


mm <- cj(data[data$cpd_exparm == "natural", ],
         formula_match,
         id = ~respid,
         estimate = "amce")

mm=set_categories_and_levels(mm, "match", features=features)

mm

p = draw_plot_effects(mm, "match", categories=categories, estimator="mm",y_labels=y_labels_plots)

p=p+plot_annotation(title = "ATEs of the Parallel Design Conjoint Experiment (recoding match/mismatch)",
                    caption="AMCEs of the natural mediation arm")

ggsave(paste0(output_wd,"estimations/","ATEs_amces_general_base_match.png"), p, height = 10, width = 10)



############ ATEs (nominal value)


mm <- cj(data[data$cpd_exparm == "natural", ],
        formula_nominal,
         id = ~respid,
         estimate = "mm")

mm=set_categories_and_levels(mm, "nominal", features=features)

mm

p = draw_plot_effects(mm, "nominal", categories=categories, estimator="mm",y_labels=y_labels_plots)

p=p+plot_annotation(title = "ATEs of the Parallel Design Conjoint Experiment (nominal values)",
                    caption="Marginal means of the natural mediation arm")

ggsave(paste0(output_wd,"estimations/", "ATEs_mm_general_base_nominal.png"), p, height = 10, width = 10)


#same but with amce


mm <- cj(data[data$cpd_exparm == "natural", ],
         formula_nominal,
         id = ~respid,
       estimate = "amce")

mm=set_categories_and_levels(mm, "nominal", features=features)

mm

p = draw_plot_effects(mm, "nominal", categories=categories, estimator="amce",y_labels=y_labels_plots)

p=p+plot_annotation(title = "ATEs of the Parallel Design Conjoint Experiment (nominal values)",
                    caption="AMCEs of the natural mediation arm")

ggsave(paste0(output_wd,"estimations/", "ATEs_amce_general_base_nominal.png"), p, height = 10, width = 10)


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

mm <- data |>
  filter(cpd_match_ideology == "ideology_match" & !is.na(cpd_match_ideology)) |>
  cj(formula_match,
     id = ~respid,
     estimate = "mm")

#mm

mm=set_categories_and_levels(mm, "match", features=features)

mm


p = draw_plot_match(mm, categories=categories, estimator="mm")


p=p+plot_annotation(title = "ACDEs of the Parallel Design Conjoint Experiment, ideological match",
                    caption="Marginal means of the manipulated mediation arm")

#p
ggsave(paste0(output_wd,"estimations/", "ACDEs_mm_general_base_match.png"), p, height = 10, width = 10)


######################################
#### ACDEs for ideological mismatch with MM
###################################### 

mm <- data |>
  filter(cpd_match_ideology == "ideology_mismatch" & !is.na(cpd_match_ideology)) |>
  cj(formula_match,
     id = ~respid,
     estimate = "mm")

mm=set_categories_and_levels(mm, "match", features=features)

mm

p = draw_plot_match(mm, categories=categories, estimator="mm")

p=p+plot_annotation(title = "ACDEs of the Parallel Design Conjoint Experiment, ideological mismatch",
                    caption="Marginal means of the manipulated mediation arm")

#p
ggsave(paste0(output_wd,"estimations/", "ACDEs_mm_general_base_mismatch.png"), p, height = 10, width = 10)


######################################
#### ACDEs for ideological match with AMCE
###################################### 


mm <- data |>
  filter(cpd_match_ideology == "ideology_match" & !is.na(cpd_match_ideology)) |>
  cj(formula_match,
     id = ~respid,
     estimate = "amce")

#mm

mm=set_categories_and_levels(mm, "match", features=features)

mm

p = draw_plot_match(mm, categories=categories, estimator="amce")

p=p+plot_annotation(title = "ACDEs of the Parallel Design Conjoint Experiment, ideological match",
                    caption="Marginal means of the manipulated mediation arm")

#p
ggsave(paste0(output_wd,"estimations/", "ACDEs_mm_general_base_match.png"), p, height = 10, width = 10)


######################################
#### ACDEs for ideological mismatch with AMCE
###################################### 


mm <- data |>
  filter(cpd_match_ideology == "ideology_mismatch" & !is.na(cpd_match_ideology)) |>
  cj(formula_match,
     id = ~respid,
     estimate = "amce")

#mm

mm=set_categories_and_levels(mm, "match", features=features)

mm

p = draw_plot_match(mm, categories=categories, estimator="amce")

p=p+plot_annotation(title = "ACDEs of the Parallel Design Conjoint Experiment, ideological match",
                    caption="Marginal means of the manipulated mediation arm")

#p
ggsave(paste0(output_wd,"estimations/", "ACDEs_mm_general_base_match.png"), p, height = 10, width = 10)


######### TO DO ON MONDAY 
# ACDES WITH AMCE INSTEAD OF MM
# COMPUTE ELIMINATED EFFECTS
# MAKE THE CODE READABLE BECAUSE YOU WILL NEVER UNDERSTAND IT IN TWO WEEKS




