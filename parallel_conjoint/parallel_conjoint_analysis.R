#this script is for the analyses related to the parallel conjoint design

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)

setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/parallel_conjoint/")

output_wd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/conjoint_parallel_design/"
data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_cpd.RDS")
 
names(data)


categories= c("Sociodemographics", "Psychological", "Lifestyle", "Political")

subcategories = list(Sociodemographics=c("Gender Match", "Gender Mismatch",
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



# data = data |>
#   filter(country=="IT")

###################
#### DIAGNOSTICS ####
###################


#### Randomization check with levels not recoded (probability assigned based on similarity!)

plot(cj_freqs(data, cpd_chosen ~ cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
                cpd_consc + cpd_ope +
                cpd_diet + cpd_animal + cpd_holiday+
                cpd_ideology,
                id = ~respid), col="grey")


ggsave(paste0(output_wd,"randomization_checks/", "diagnostic_randomization_nomatch_cj.png"), height = 15, width = 8)


# With ggplot
aus = cj_freqs(data, cpd_chosen ~ cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
                 cpd_consc + cpd_ope +
                 cpd_diet + cpd_animal + cpd_holiday +
                 cpd_ideology, id = ~respid)

v = list()

for(i in unique(aus$feature))
{
  
  p = aus |>
    filter(feature == i) |>
    ggplot(aes(y=level, x=estimate, fill=feature))+
    geom_col()+
    ylab("")+
    xlab("")+
    ggtitle(as.character(i))+
    #scale_x_continuous(breaks = seq(0, round(max(aus$estimate), digits = -1), by = round(max(aus$estimate)/10, digits = -1)),
                      # limits = c(0,max(aus$estimate)+1))+
    # coord_flip()+
    theme(text = element_text(size = 15),
          legend.position = "none",
          plot.title = element_text(size=14))
  
  v[[i]] = p
}
p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]/v[[7]]/v[[8]]/v[[9]]/v[[10]]

p

ggsave(paste0(output_wd,"randomization_checks/", "diagnostic_randomization_nomatch_ggplot.png"),
       p, height = 15, width = 8)


###### randomization checks with match variables


plot(cj_freqs(data, cpd_chosen ~ cpd_match_gender + cpd_match_age + 
                cpd_match_educ + cpd_match_regionfeel +
                cpd_match_consc + cpd_match_ope +
                cpd_match_diet + cpd_match_animal + cpd_match_holiday+
                cpd_match_ideology,
              id = ~respid), col="grey")


ggsave(paste0(output_wd,"randomization_checks/", "diagnostic_randomization_match_cj.png"), height = 15, width = 8)


# With ggplot
aus = cj_freqs(data, cpd_chosen ~ cpd_match_gender + cpd_match_age +
                 cpd_match_educ + cpd_match_regionfeel +
                 cpd_match_consc + cpd_match_ope +
                 cpd_match_diet + cpd_match_animal + cpd_match_holiday +
                 cpd_match_ideology, id = ~respid)

v = list()

for(i in unique(aus$feature))
{
  
  p = aus |>
    filter(feature == i) |>
    ggplot(aes(y=level, x=estimate, fill=feature))+
    geom_col()+
    ylab("")+
    xlab("")+
    ggtitle(as.character(i))+
    #scale_x_continuous(breaks = seq(0, round(max(aus$estimate), digits = -1), by = round(max(aus$estimate)/10, digits = -1)),
    # limits = c(0,max(aus$estimate)+1))+
    # coord_flip()+
    theme(text = element_text(size = 15),
          legend.position = "none",
          plot.title = element_text(size=14))
  
  v[[i]] = p
}
p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]/v[[7]]/v[[8]]/v[[9]]/v[[10]]

p

ggsave(paste0(output_wd,"randomization_checks/", "diagnostic_randomization_match_ggplot.png"),
       p, height = 15, width = 8)




#### Checking whether there is a preference for the right wing profile

data$cpd_profile_number = as.factor(data$cpd_profile_number)

plot(cj(data, 
        cpd_chosen ~ cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
          cpd_consc + cpd_ope +
          cpd_diet + cpd_animal + cpd_holiday+
          cpd_ideology,
        id = ~respid,
        by = ~cpd_profile_number,
        estimate = "mm"),
     group = "cpd_profile_number",
     vline = 0.5)

### checking it with a different approach: if the effect is significant, 
# it means that  the effect of a certain attribute s influenced by whether a
#profile is on the left or on the right

data$cpd_profile_number = as.numeric(data$cpd_profile_number)-1
plot(cj(data, 
        cpd_profile_number ~ cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
          cpd_consc + cpd_ope +
          cpd_diet + cpd_animal + cpd_holiday+
          cpd_ideology,
        id = ~respid,
        estimate = "mm"),
     vline = 0.5)

#se non ci sono differenze significative rispetto alla zero, non c'è preferenza 
#particolare per il profilo a destra


#### same checks as above but with match variables


data$cpd_profile_number = as.factor(data$cpd_profile_number)

plot(cj(data, 
        cpd_chosen ~ cpd_match_gender + cpd_match_age + cpd_match_educ + cpd_match_regionfeel +
          cpd_match_consc + cpd_match_ope +
          cpd_match_diet + cpd_match_animal + cpd_match_holiday+
          cpd_match_ideology,
        id = ~respid,
        by = ~cpd_profile_number,
        estimate = "mm"),
     group = "cpd_profile_number",
     vline = 0.5)

### checking it with a different approach: if the effect is significant, 
# it means that  the effect of a certain attribute s influenced by whether a
#profile is on the left or on the right

data$cpd_profile_number = as.numeric(data$cpd_profile_number)-1
plot(cj(data, 
        cpd_profile_number ~ cpd_match_gender + cpd_match_age + cpd_match_educ + cpd_match_regionfeel +
          cpd_match_consc + cpd_match_ope +
          cpd_match_diet + cpd_match_animal + cpd_match_holiday+
          cpd_match_ideology,
        id = ~respid,
        estimate = "mm"),
     vline = 0.5)

#se non ci sono differenze significative rispetto alla zero, non c'è preferenza 
#particolare per il profilo a destra

#####


##############################
#### ACTUAL ESTIMATIONS ####
##############################

####################################
############ ATEs (MATCH/MISMATCH)
######################################
# Estimation: The marginal mean associated with S_i^k=1 for respondents 
#in the natural mediation arm

# Interpretation: The total effect that observing similarity in attribute k 
#has on the willingness to engage in political conversation when no political 
#information is given to the respondent, whether through political inferences or not.


mm <- cj(data[data$cpd_exparm == "natural", ],
         cpd_chosen ~  cpd_match_gender + cpd_match_age + cpd_match_educ + cpd_match_regionfeel +
         cpd_match_consc + cpd_match_ope +
         cpd_match_diet + cpd_match_animal + cpd_match_holiday,
         id = ~respid,
         estimate = "mm")

mm$variable = as.character(mm$level)

for(i in 1:nrow(mm))
{
  mm$variable[i] = toTitleCase(paste(strsplit(as.character(mm$level[i]), "_")[[1]], collapse=" "))
}

mm$variable
mm$variable=factor(mm$variable, levels = mm$variable)

#mm$level <- fct_reorder(mm$level, desc(mm$estimate))



mm$category="Sociodemographics"

mm$category=ifelse(grepl("ope",mm$feature) | grepl("consc",mm$feature), 
                   "Psychological",
                   ifelse(grepl("diet",mm$feature) | grepl("animal",mm$feature) | grepl("holiday",mm$feature),
                   "Lifestyle",
                   "Sociodemographics"))

mm
v = list()

for(category in categories[1:3])
{
  p = ggplot(mm[mm$category == category, ])+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable, col=feature))+
    ylab("")+
    xlab(category)+
    #scale_x_continuous(breaks = seq(0,1,by=0.2))+
    xlim(0,1)+
    #xlab("Marginal mean")+
    scale_y_discrete(limits = rev(subcategories[[category]])) +
    theme(legend.position = "none")
  
  v[[category]] = p
  
}

p = v[["Sociodemographics"]]/v[["Psychological"]]/v[["Lifestyle"]]

p=p+plot_annotation(title = "ATEs of the Parallel Design Conjoint Experiment",
                  caption="Marginal means of the natural mediation arm")

ggsave(paste0(output_wd,"estimations/", "ATEs_mm_general_base_match.png"), p, height = 10, width = 10)




### Same as before, but with AMCes (for appendix)


mm <- cj(data[data$cpd_exparm == "natural", ],
         cpd_chosen ~  cpd_match_gender + cpd_match_age + cpd_match_educ + cpd_match_regionfeel +
           cpd_match_consc + cpd_match_ope +
           cpd_match_diet + cpd_match_animal + cpd_match_holiday,
         id = ~respid,
         estimate = "amce")

mm$variable = as.character(mm$level)

for(i in 1:nrow(mm))
{
  mm$variable[i] = toTitleCase(paste(strsplit(as.character(mm$level[i]), "_")[[1]], collapse=" "))
}

mm$variable
mm$variable=factor(mm$variable, levels = mm$variable)

#mm$level <- fct_reorder(mm$level, desc(mm$estimate))



mm$category="Sociodemographics"

mm$category=ifelse(grepl("ope",mm$feature) | grepl("consc",mm$feature), 
                   "Psychological",
                   ifelse(grepl("diet",mm$feature) | grepl("animal",mm$feature) | grepl("holiday",mm$feature),
                          "Lifestyle",
                          "Sociodemographics"))

mm
v=list()
for(category in categories[1:3])
{
  p = ggplot(mm[mm$category == category, ])+
    geom_vline(aes(xintercept=0), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable, col=feature))+
    ylab("")+
    xlab(category)+
    #scale_x_continuous(breaks = seq(0,1,by=0.2))+
    xlim(-1,1)+
    #xlab("Marginal mean")+
    scale_y_discrete(limits = rev(subcategories[[category]])) +
    theme(legend.position = "none")
  
  v[[category]] = p
  
}


p = v[["Sociodemographics"]]/v[["Psychological"]]/v[["Lifestyle"]]

p
p=p+plot_annotation(title = "ATEs of the Parallel Design Conjoint Experiment (recoding match/mismatch)",
                  caption="AMCEs of the natural mediation arm")

ggsave(paste0(output_wd,"estimations/","ATEs_amces_general_base_match.png"), p, height = 10, width = 10)



############ ATEs (nominal value)

features= c("Gender", "Gender",
            "Age", "Age","Age",
            "Education","Education",
            "Regionfeel","Regionfeel","Regionfeel",
            "Conscientiousness","Conscientiousness","Conscientiousness",
            "Openness","Openness","Openness",
            "Diet","Diet","Diet",
            "Animal","Animal","Animal",
            "Holiday","Holiday","Holiday")


levels= c("Female", "Male",
            "Under 30", "Between 30 and 59","Over 60",
            "Degree","No degree",
            "Regionfeel1","Regionfeel2","Regionfeel3",
            "High Consc.","Med. Consc.","Low Consc.",
            "High Ope.","Med. Ope.","Low Ope.",
            "Omnivore","Vegetarian","Vegan",
            "Cat","Dog","No pet",
            "City","Outdoor","Relax")

sublevels = list(Sociodemographics = c("Female", "Male",
                                       "Under 30", "Between 30 and 59","Over 60",
                                       "Degree","No degree",
                                       "Regionfeel1","Regionfeel2","Regionfeel3"),
                 Psychological = c("High Consc.","Med. Consc.","Low Consc.",
                                   "High Ope.","Med. Ope.","Low Ope."),
                 Lifestyle = c("Omnivore","Vegetarian","Vegan",
                               "Cat","Dog","No pet",
                               "City","Outdoor","Relax"))


mm <- cj(data[data$cpd_exparm == "natural", ],
         cpd_chosen ~  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
           cpd_consc + cpd_ope +
           cpd_diet + cpd_animal + cpd_holiday,
         id = ~respid,
         estimate = "mm")


mm$category="Sociodemographics"

mm$category=ifelse(grepl("ope",mm$feature) | grepl("consc",mm$feature), 
                   "Psychological",
                   ifelse(grepl("diet",mm$feature) | grepl("animal",mm$feature) | grepl("holiday",mm$feature),
                          "Lifestyle",
                          "Sociodemographics"))

mm

mm$feature = factor(features, levels = unique(features))
mm$level=factor(levels, levels = levels)

mm

v = list()

for(category in categories[1:3])
{
  p = ggplot(mm[mm$category == category, ])+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level, col=feature))+
    ylab("")+
    xlab(category)+
    #scale_x_continuous(breaks = seq(0,1,by=0.2))+
    xlim(0,1)+
    #xlab("Marginal mean")+
    scale_y_discrete(limits = rev(sublevels[[category]]))+
    scale_color_discrete(name="Attribute")
    #theme(legend.text = element_text("Attribute"))
  
  v[[category]] = p
  
}

p = v[["Sociodemographics"]]/v[["Psychological"]]/v[["Lifestyle"]]

p
p=p+plot_annotation(title = "ATEs of the Parallel Design Conjoint Experiment (nominal values)",
                  caption="Marginal means of the natural mediation arm")

ggsave(paste0(output_wd,"estimations/", "ATEs_mm_general_base_nominal.png"), p, height = 10, width = 10)


#same but with amce


mm <- cj(data[data$cpd_exparm == "natural", ],
         cpd_chosen ~  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
           cpd_consc + cpd_ope +
           cpd_diet + cpd_animal + cpd_holiday,
         id = ~respid,
         estimate = "amce")


mm$category="Sociodemographics"

mm$category=ifelse(grepl("ope",mm$feature) | grepl("consc",mm$feature), 
                   "Psychological",
                   ifelse(grepl("diet",mm$feature) | grepl("animal",mm$feature) | grepl("holiday",mm$feature),
                          "Lifestyle",
                          "Sociodemographics"))

mm

mm$feature = factor(features, levels = unique(features))
mm$level=factor(levels, levels = levels)

mm

v = list()

for(category in categories[1:3])
{
  p = ggplot(mm[mm$category == category, ])+
    geom_vline(aes(xintercept=0), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level, col=feature))+
    ylab("")+
    xlab(category)+
    #scale_x_continuous(breaks = seq(0,1,by=0.2))+
    xlim(-1,1)+
    #xlab("Marginal mean")+
    scale_y_discrete(limits = rev(sublevels[[category]]))+
    scale_color_discrete(name="Attribute")
  #theme(legend.position = "none")
  
  v[[category]] = p
  
}

p = v[["Sociodemographics"]]/v[["Psychological"]]/v[["Lifestyle"]]

p
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

mm <- data |>
  filter(cpd_match_ideology == "ideology_match" & !is.na(cpd_match_ideology)) |>
  cj(cpd_chosen ~  cpd_match_gender + cpd_match_age + cpd_match_educ + cpd_match_regionfeel +
           cpd_match_consc + cpd_match_ope +
           cpd_match_diet + cpd_match_animal + cpd_match_holiday,
         id = ~respid,
         estimate = "mm")

mm
mm$variable = as.character(mm$level)

for(i in 1:nrow(mm))
{
  mm$variable[i] = toTitleCase(paste(strsplit(as.character(mm$level[i]), "_")[[1]], collapse=" "))
}

mm$variable
mm$variable=factor(mm$variable, levels = mm$variable)

#mm$level <- fct_reorder(mm$level, desc(mm$estimate))
mm


mm$category="Sociodemographics"

mm$category=ifelse(grepl("ope",mm$feature) | grepl("consc",mm$feature), 
                   "Psychological",
                   ifelse(grepl("diet",mm$feature) | grepl("animal",mm$feature) | grepl("holiday",mm$feature),
                          "Lifestyle",
                          ifelse(grepl("ideology",mm$feature),
                                 "Political",
                                 "Sociodemographics")
                          )
                   )

mm
v = list()

for(category in categories[1:3])
{
  p = ggplot(mm[mm$category == category, ])+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable, col=feature))+
    ylab("")+
    xlab(category)+
    #scale_x_continuous(breaks = seq(0,1,by=0.2))+
    xlim(0,1)+
    #xlab("Marginal mean")+
    scale_y_discrete(limits = rev(subcategories[[category]])) +
    theme(legend.position = "none")
  
  v[[category]] = p
  
}

p = v[["Sociodemographics"]]/v[["Psychological"]]/v[["Lifestyle"]]

p=p+plot_annotation(title = "ACDEs of the Parallel Design Conjoint Experiment, ideological match",
                    caption="Marginal means of the manipulated mediation arm")

p
ggsave(paste0(output_wd,"estimations/", "ACDEs_mm_general_base_match.png"), p, height = 10, width = 10)


#interaction with political inferences.

mm <- data |>
  filter(cpd_match_ideology == "ideology_mismatch" & !is.na(cpd_match_ideology)) |>
  cj(cpd_chosen ~  cpd_match_gender + cpd_match_age + cpd_match_educ + cpd_match_regionfeel +
       cpd_match_consc + cpd_match_ope +
       cpd_match_diet + cpd_match_animal + cpd_match_holiday,
     id = ~respid,
     estimate = "mm")

mm
mm$variable = as.character(mm$level)

for(i in 1:nrow(mm))
{
  mm$variable[i] = toTitleCase(paste(strsplit(as.character(mm$level[i]), "_")[[1]], collapse=" "))
}

mm$variable
mm$variable=factor(mm$variable, levels = mm$variable)

#mm$level <- fct_reorder(mm$level, desc(mm$estimate))
mm


mm$category="Sociodemographics"

mm$category=ifelse(grepl("ope",mm$feature) | grepl("consc",mm$feature), 
                   "Psychological",
                   ifelse(grepl("diet",mm$feature) | grepl("animal",mm$feature) | grepl("holiday",mm$feature),
                          "Lifestyle",
                          ifelse(grepl("ideology",mm$feature),
                                 "Political",
                                 "Sociodemographics")
                   )
)

mm
v = list()

for(category in categories[1:3])
{
  p = ggplot(mm[mm$category == category, ])+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable, col=feature))+
    ylab("")+
    xlab(category)+
    #scale_x_continuous(breaks = seq(0,1,by=0.2))+
    xlim(0,1)+
    #xlab("Marginal mean")+
    scale_y_discrete(limits = rev(subcategories[[category]])) +
    theme(legend.position = "none")
  
  v[[category]] = p
  
}

p = v[["Sociodemographics"]]/v[["Psychological"]]/v[["Lifestyle"]]

p=p+plot_annotation(title = "ACDEs of the Parallel Design Conjoint Experiment, ideological mismatch",
                    caption="Marginal means of the manipulated mediation arm")

p
ggsave(paste0(output_wd,"estimations/", "ACDEs_mm_general_base_mismatch.png"), p, height = 10, width = 10)


######### TO DO ON MONDAY 
# ACDES WITH AMCE INSTEAD OF MM
# COMPUTE ELIMINATED EFFECTS
# MAKE THE CODE READABLE BECAUSE YOU WILL NEVER UNDERSTAND IT IN TWO WEEKS




