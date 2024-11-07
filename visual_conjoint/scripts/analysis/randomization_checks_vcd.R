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


#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/", context, "/")

data = readRDS(paste0(dataset_rep, "cjdata_vcd_", context, ".RDS"))

names(data)




# data = data |>
#   filter(country=="IT")

###################
#### DIAGNOSTICS ####
###################


#### Randomization check with levels not recoded (probability assigned based on similarity!)
if(context != "POOL") #When context==POOL, there is a name which is the same of one surname!
{
  plot(cj_freqs(data, vcd_chosen_rw ~ vcd_ethnicity + 
                  vcd_gender + vcd_age + vcd_photo +
                  vcd_name + vcd_surname +
                  vcd_job + vcd_issue + vcd_nostalgia+
                  vcd_animal + vcd_food + vcd_crowd,
                id = ~respid), col="grey")
}
if(context=="POOL")
{
  plot(cj_freqs(data, vcd_chosen_rw ~ vcd_ethnicity + 
                  vcd_gender + vcd_age + vcd_photo +
                  #vcd_name + vcd_surname +
                  vcd_job + vcd_issue + vcd_nostalgia+
                  vcd_animal + vcd_food + vcd_crowd,
                id = ~respid), col="grey")
  
}


ggsave(paste0(output_wd,"randomization_checks/", "diagnostic_randomization_nomatch_cj.png"), 
       height = 15, width = 8, create.dir = T)


# With ggplot
if(context != "POOL")
{
  aus = cj_freqs(data, vcd_chosen_rw ~ vcd_ethnicity + 
                   vcd_gender + vcd_age + vcd_photo +
                   vcd_name + vcd_surname + vcd_job + 
                   vcd_issue + vcd_nostalgia + vcd_valence+
                   vcd_animal + vcd_food + vcd_crowd,
                 id = ~respid)
  
}

if(context == "POOL")
{
  aus = cj_freqs(data, vcd_chosen_rw ~ vcd_ethnicity + 
                   vcd_gender + vcd_age + vcd_photo +
                   #vcd_name + vcd_surname + 
                   vcd_job + 
                   vcd_issue + vcd_nostalgia + vcd_valence+
                   vcd_animal + vcd_food + vcd_crowd,
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

ggsave(paste0(output_wd,"randomization_checks/", "diagnostic_randomization_nomatch_ggplot1.png"),
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

ggsave(paste0(output_wd,"randomization_checks/", "diagnostic_randomization_nomatch_ggplot2.png"),
       p, height = 12, width = 8, create.dir = T)


#### Checking whether there is a preference for the profile shown to the right

data$vcd_profile_number = as.factor(data$vcd_profile_number)

plot(cj(data, 
        vcd_chosen_rw ~ vcd_ethnicity + 
          vcd_gender + vcd_age + vcd_job + 
          vcd_issue + vcd_nostalgia + vcd_valence + 
          vcd_animal + vcd_food + vcd_crowd,
        id = ~respid,
        by = ~vcd_profile_number,
        estimate = "mm"),
     group = "vcd_profile_number",
     vline = 0.5)

### checking it with a different approach: if the effect is significant, 
# it means that  the effect of a certain attribute s influenced by whether a
#profile is on the left or on the right

data$vcd_profile_number = as.numeric(data$vcd_profile_number)-1

plot(cj(data, 
        vcd_chosen_rw ~ vcd_ethnicity + 
          vcd_gender + vcd_age + vcd_job + 
          vcd_issue + vcd_nostalgia + vcd_valence +
          vcd_animal + vcd_food + vcd_crowd,
        id = ~respid,
        estimate = "mm"),
     vline = 0.5)

#se non ci sono differenze significative rispetto alla zero, non c'è preferenza 
#particolare per il profilo a destra
