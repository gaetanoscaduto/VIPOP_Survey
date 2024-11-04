###############################################################################
#this script is for the randomization checks related to the classic conjoint design
###############################################################################

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)


#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context
#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"

# gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"
# dataset_rep = paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/")

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/classic_conjoint_design/", context, "/randomization_checks/")
data = readRDS(paste0(dataset_rep, "cjdata_ccd_", context, ".RDS"))

names(data)



###################
#### DIAGNOSTICS ####
###################

plot(cj_freqs(data, ccd_chosen_rw ~ ccd_gender+
                ccd_age+ccd_religion+ccd_citysize+ccd_job+
                ccd_consc+ccd_ope+ ccd_neu+
                ccd_restaurant+ccd_transport+ccd_animal,
              id = ~respid), col="grey")


ggsave(paste0(output_wd, "diagnostic_randomization.png"), 
       height = 15, width = 8, create.dir = T)


# With ggplot
aus = cj_freqs(data, ccd_chosen_rw ~ ccd_gender+
                 ccd_age+ccd_religion+ccd_citysize+ccd_job+
                 ccd_consc+ccd_ope+ ccd_neu+
                 ccd_restaurant+ccd_transport+ccd_animal,
               id = ~respid)

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

ggsave(paste0(output_wd,  "diagnostic_randomization_ggplot1.png"),
       p, height = 12, width = 8, create.dir = T)

p= v[[7]]/v[[8]]/v[[9]]/v[[10]]/v[[11]]

p

ggsave(paste0(output_wd,  "diagnostic_randomization_ggplot2.png"),
       p, height = 12, width = 8, create.dir = T)


#### Checking whether there is a preference for the profile shown to the right

data$ccd_profile_number = as.factor(data$ccd_profile_number)

plot(cj(data, 
        ccd_chosen_rw ~ ccd_gender+
        ccd_age+ccd_religion+ccd_citysize+ccd_job+
        ccd_consc+ccd_ope+ ccd_neu+
        ccd_restaurant+ccd_transport+ccd_animal,
        id = ~respid,
        by = ~ccd_profile_number,
        estimate = "mm_differences"),
     group = "ccd_profile_number",
     vline = 0)

### checking it with a different approach: if the effect is significant, 
# it means that  the effect of a certain attribute varies by whether a
#profile is on the left or on the right

#DA RICONTROLLARE
 
#data$ccd_profile_number = as.numeric(data$ccd_profile_number)

plot(cj(data, 
        ccd_chosen ~ ccd_profile_number,
        id = ~respid,
        estimate = "amce"),
     vline = 0)

