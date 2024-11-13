###############################################################################
#this script is for the randomization checks related to the parallel conjoint design
###############################################################################

# pacman::p_load(
#   cregg, dplyr, ggpubr, cowplot, 
#   MASS, cjoint, corrplot, dplyr, 
#   forcats, ggplot2, gt, gtools, 
#   gtsummary, margins, openxlsx, 
#   patchwork, rio, texreg, tools
# )


#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context
#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"

#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"


output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/conjoint_parallel_design/singlecountry/", context, "/randomization_checks/")
data = readRDS(paste0(dataset_rep, "cjdata_cpd_", context, ".RDS"))

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



###################
#### DIAGNOSTICS ####
###################


#### Randomization check with levels not recoded (probability assigned based on similarity!)

plot(cj_freqs(data, cpd_chosen ~ cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
                cpd_consc + cpd_ope +
                cpd_diet + cpd_animal + cpd_holiday+
                cpd_ideology,
              id = ~respid), col="grey")


ggsave(paste0(output_wd,  "diagnostic_randomization_nomatch_cj.png"), 
       height = 15, width = 8, create.dir = T)


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
    theme(text = element_text(size = 15),
          legend.position = "none",
          plot.title = element_text(size=14))
  
  v[[i]] = p
}
p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]/v[[7]]/v[[8]]/v[[9]]/v[[10]]

p

ggsave(paste0(output_wd,  "diagnostic_randomization_nomatch_ggplot.png"),
       p, height = 15, width = 8, create.dir = T)


###### randomization checks with match variables


plot(cj_freqs(data, cpd_chosen ~ cpd_match_gender + cpd_match_age + 
                cpd_match_educ + cpd_match_regionfeel +
                cpd_match_consc + cpd_match_ope +
                cpd_match_diet + cpd_match_animal + cpd_match_holiday+
                cpd_match_ideology,
              id = ~respid), col="grey")


ggsave(paste0(output_wd,  "diagnostic_randomization_match_cj.png"), 
       height = 15, width = 8, create.dir = T)


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
    theme(text = element_text(size = 15),
          legend.position = "none",
          plot.title = element_text(size=14))
  
  v[[i]] = p
}
p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]/v[[7]]/v[[8]]/v[[9]]/v[[10]]

p

ggsave(paste0(output_wd,  "diagnostic_randomization_match_ggplot.png"),
       p, height = 15, width = 8, create.dir = T)


#checking whether being shown to the right influences choice to be chosen

data$cpd_profile_number = as.factor(data$cpd_profile_number)

plot(cj(data, 
        cpd_chosen ~ cpd_profile_number,
        id = ~respid,
        estimate = "amce"),
     vline = 0)


#### Checking whether tthe effects varies on whether the profile is shown 
#to the right (signifiance is when the two ci dn't overlap)

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


#### same checks as above but with match variables


data$cpd_profile_number = as.factor(data$cpd_profile_number)

# plot(cj(data, 
#         cpd_chosen ~ cpd_match_gender + cpd_match_age + cpd_match_educ + cpd_match_regionfeel +
#           cpd_match_consc + cpd_match_ope +
#           cpd_match_diet + cpd_match_animal + cpd_match_holiday+
#           cpd_match_ideology,
#         id = ~respid,
#         by = ~cpd_profile_number,
#         estimate = "mm"),
#      group = "cpd_profile_number",
#      vline = 0.5)



#se non ci sono differenze significative rispetto alla zero, non c'è preferenza 
#particolare per il profilo a destra



### Attribute order effects conjoint
