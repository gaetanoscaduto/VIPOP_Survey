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

plot(cj(data, 
        cpd_chosen ~ cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
          cpd_consc + cpd_ope +
          cpd_diet + cpd_animal + cpd_holiday+
          cpd_ideology,
        id = ~respid,
        by = ~cpd_profile_number,
        group = "profile_fac",
        estimate = "mm"),
     vline = 0.5)

#se non ci sono differenze significative rispetto alla zero, non c'è preferenza 
#particolare per il profilo a destra

#####
