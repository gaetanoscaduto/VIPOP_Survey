###############################################################################
#this script is for the randomization checks related to the visual conjoint design
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

#outcome="ideology"
#outcome="populism"
#outcome="trust"

#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"


#recoding_functional_equivalents = T

if(recoding_functional_equivalents == T)
{
  output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/",
                     "FE",
                     "/singlecountry/", 
                     outcome, "/",
                     context,
                     "/randomization_checks/")
}

if(recoding_functional_equivalents == F)
{
  output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/",
                     "NFE",
                     "/singlecountry/", 
                     outcome, "/",
                     context,
                     "/randomization_checks/")
}


data = readRDS(paste0(dataset_rep, "cjdata_vcd_", context, ".RDS"))


if(outcome == "ideology")
{
  data$vcd_outcome = data$vcd_chosen_rw
}
if(outcome == "trust")
{
  data$vcd_outcome = data$vcd_chosen_trust
}
if(outcome == "populism")
{
  data$vcd_outcome = data$vcd_chosen_pop
}


###################
#### DIAGNOSTICS ####
###################


#### Randomization check with levels not recoded (probability assigned based on similarity!)
#NOTICE THAT When context==POOL, there is a name which is the same of one surname!

  plot(cj_freqs(data, vcd_outcome ~ vcd_ethnicity + 
                  vcd_gender + vcd_age + vcd_job + 
                  vcd_issue + vcd_time + vcd_valence+
                  vcd_pet + vcd_food + vcd_crowd, 
                  feature_labels = list(vcd_gender="Gender",
                                        vcd_age="Age",
                                        vcd_ethnicity="Ethnicity",
                                        vcd_job="Job",
                                        vcd_issue="Positional Issue",
                                        vcd_time = "Time",
                                        vcd_valence= "Valence",
                                        vcd_pet="Pet",
                                        vcd_food="Food",
                                        vcd_crowd="Crowd"
                                        ),
                  id = ~respid), 
       col="grey")+
    theme(legend.position = "none")

ggsave(paste0(output_wd,"diagnostic_randomization_nomatch_cj.png"), 
       height = 10, width = 8, create.dir = T)


# With ggplot
if(context != "POOL")
{
  aus = cj_freqs(data, vcd_outcome ~ vcd_ethnicity + 
                   vcd_gender + vcd_age + vcd_photo +
                   vcd_name + vcd_surname + vcd_job + 
                   vcd_issue + vcd_time + vcd_valence+
                   vcd_pet + vcd_food + vcd_crowd,
                 id = ~respid)
  
}

if(context == "POOL")
{
  aus = cj_freqs(data, vcd_outcome ~ vcd_ethnicity + 
                   vcd_gender + vcd_age + vcd_photo +
                   #vcd_name + vcd_surname + 
                   vcd_job + 
                   vcd_issue + vcd_time + vcd_valence+
                   vcd_pet + vcd_food + vcd_crowd,
                 id = ~respid)
  
}

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

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p

ggsave(paste0(output_wd, "diagnostic_randomization_nomatch_ggplot1.png"),
       p, height = 12, width = 8, create.dir = T)

if(context != "POOL")
{
  p= v[[7]]/v[[8]]/v[[9]]/v[[10]]/v[[11]]/v[[12]]  
}
if(context == "POOL")
{
  p= v[[7]]/v[[8]]/v[[9]]/v[[10]]#/v[[11]]/v[[12]]  
}


p

ggsave(paste0(output_wd, "diagnostic_randomization_nomatch_ggplot2.png"),
       p, height = 12, width = 8, create.dir = T)


#### Checking whether there is a preference for the profile shown to the right

data$vcd_profile_number = as.factor(data$vcd_profile_number)

p = plot(cj(data, 
        vcd_outcome ~ vcd_ethnicity + 
          vcd_gender + vcd_age + vcd_job + 
          vcd_issue + vcd_time + vcd_valence + 
          vcd_pet + vcd_food + vcd_crowd,
        id = ~respid,
        by = ~vcd_profile_number,
        estimate = "mm"),
     group = "vcd_profile_number",
     vline = 0.5)




### checking it with a different approach: if the effect is significant, 
# it means that  the effect of a certain attribute s influenced by whether a
#profile is on the left or on the right

data$vcd_profile_number = as.numeric(data$vcd_profile_number)-1

p = plot(cj(data, 
        vcd_outcome ~ vcd_ethnicity + 
          vcd_gender + vcd_age + vcd_job + 
          vcd_issue + vcd_time + vcd_valence +
          vcd_pet + vcd_food + vcd_crowd,
        id = ~respid,
        estimate = "mm"),
     vline = 0.5)

ggsave(paste0(output_wd, "effect based on whether the profile is on the right.png"),
       p, height = 12, width = 8, create.dir = T)


#se non ci sono differenze significative rispetto alla zero, non c'è preferenza 
#particolare per il profilo a destra



######################################### 
############# Check name effect ########
######################################### 
data$vcd_ethn_gender = interaction(data$vcd_ethnicity, data$vcd_gender, sep=" ")

v=list()

color_to_plot = ifelse(outcome == "ideology", wesanderson::wes_palettes$Darjeeling1[1],
                       wesanderson::wes_palettes$Darjeeling1[2])

shape_to_plot = ifelse(outcome == "ideology", 
                       19, 
                       17)

for(category in unique(data$vcd_ethn_gender))
{
  temp = data |>
    filter(vcd_ethn_gender == category)
  
  temp$vcd_name = factor(temp$vcd_name, levels = unique(temp$vcd_name))
  
  cj = cj(temp, 
          vcd_outcome ~  vcd_name,  
          id = ~respid,
          alpha=0.01,
          h0=mean(temp$vcd_outcome),
          estimate="mm",
          feature_labels = list(vcd_name = "Names"),
          level_order = "ascending")
  
  p = plot(cj,vline=mean(temp$vcd_outcome))+
    labs(title = category)+
    theme_gray()+
    theme(legend.position="none")
  
  p=p+geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                          y=level), 
                      col=color_to_plot,
                      shape=shape_to_plot)
  
  v[[category]] = p
}

p=(v[["Black Male"]]|v[["Black Female"]])/(v[["White Male"]]|v[["White Female"]])

p=p+plot_annotation(caption = paste0("Vertical lines represent the sub-category's mean; Outcome=", 
                                     toTitleCase(outcome), 
                                     "; 99% C.I."))

ggsave(paste0(output_wd, "no_name_effects.png"),
       p, height = 10, width = 8, create.dir = T)


######################################### 
############# Check surname effect ########
######################################### 

v=list()

for(category in unique(data$vcd_ethn_gender))
{
  temp = data |>
    filter(vcd_ethn_gender == category)
  
  temp$vcd_surname = factor(temp$vcd_surname, levels = unique(temp$vcd_surname))
  
  cj = cj(temp, 
          vcd_outcome ~ vcd_surname,  
          id = ~respid,
          alpha=0.01,
          h0=mean(temp$vcd_outcome),
          estimate="mm",
          feature_labels = list(vcd_surname = "Surames"),
          level_order = "ascending")
  
  p = plot(cj,vline=mean(temp$vcd_outcome))+
    labs(title = category)+
    theme_gray()+
    theme(legend.position="none")
  
  p=p+geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                          y=level), 
                      col=color_to_plot,
                      shape=shape_to_plot)
  
  v[[category]] = p
}

p=(v[["Black Male"]]|v[["Black Female"]])/(v[["White Male"]]|v[["White Female"]])

p=p+plot_annotation(caption = paste0("Vertical lines represent the sub-category's mean; Outcome=", 
                                     toTitleCase(outcome), 
                                     "; 99% C.I."))

p

ggsave(paste0(output_wd, "no_surname_effects.png"),
       p, height = 10, width = 8, create.dir = T)



######################################### 
############# Check photo effect ########
######################################### 
data$vcd_sociodemo = interaction(data$vcd_ethnicity,
                                 data$vcd_gender, 
                                 data$vcd_age, 
                                 sep = " ")


v=list()

for(category in unique(data$vcd_sociodemo))
{
  temp = data |>
    filter(vcd_sociodemo == category)
  
  temp$vcd_photo = factor(temp$vcd_photo, levels = c("5","4","3","2","1"))
  
  cj = cj(temp, 
          vcd_outcome ~ vcd_photo,  
          id = ~respid,
          alpha=0.01,
          h0=mean(temp$vcd_outcome),
          estimate="mm",
          feature_labels = list(vcd_photo = "Profile picture"),
          level_order = "ascending")
  
  p = plot(cj,vline=mean(temp$vcd_outcome))+
    labs(title = category)+
    theme_gray()+
    theme(legend.position="none")
  
  p=p+geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                          y=level), 
                      col=color_to_plot,
                      shape=shape_to_plot)
  
  v[[category]] = p
}

p=(v[["Black Male 35"]]|v[["Black Female 35"]])/
  (v[["Black Male 70"]]|v[["Black Female 70"]])/
  (v[["White Male 35"]]|v[["White Female 35"]])/
  (v[["White Male 70"]]|v[["White Female 70"]])

p=p+plot_annotation(caption = paste0("Vertical lines represent the sub-category's mean; Outcome=", 
                                     toTitleCase(outcome), 
                                     "; 99% C.I."))

p

ggsave(paste0(output_wd, "no_photo_effects.png"),
       p, height = 10, width = 8, create.dir = T)
