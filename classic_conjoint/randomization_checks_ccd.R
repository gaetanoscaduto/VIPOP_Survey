###############################################################################
#this script is for the randomization checks related to the classic conjoint design
###############################################################################
# 
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

# gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"
# dataset_rep = paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/")

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/classic_conjoint_design/singlecountry/", context, "/randomization_checks/")
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
        ccd_chosen_rw ~ ccd_profile_number,
        id = ~respid,
        estimate = "amce"),
     vline = 0)



### Attribute order effects conjoint

# Function to clean and split attribute positions into a list
#filter for the data not from the pilot
data = data[data$pilot==0, ]

remove_left_square = gsub("\\[", "", data$C3_ATT_ORDER)
remove_right_square = gsub("\\]", "", remove_left_square)

positions_list = strsplit(remove_right_square, "-")

# create null variables for the position

number_of_attributes = 11

for(i in 1:number_of_attributes)
{
  data[,  paste0("attribute_",i,"_position")] = NA
}

for(attribute_number in 1:number_of_attributes)
{
  for(i in 1:nrow(data))
  {
    data[i, paste0("attribute_",attribute_number,"_position")] = which(positions_list[[i]]==attribute_number)
  }
}

#make the attribute position variables factors
for(i in 1:number_of_attributes)
{
  data[,  paste0("attribute_",i,"_position")] = factor(data[,  paste0("attribute_",i,"_position")],
                                                       levels = c("1","2","3",
                                                                  "4","5","6",
                                                                  "7","8","9",
                                                                  "10", "11"))
}

#Check if everything is good

# data$C3_ATT_ORDER[1]
# data$attribute_1_position[1]
# data$attribute_10_position[1]
# data$attribute_2_position[1]
# data$attribute_5_position[1]

p1 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_gender, 
                    id = ~respid,
                    by = ~attribute_1_position,
                    estimate = "mm"), 
          group = "attribute_1_position", 
          vline = 0.5)+ 
  xlab("")+
  theme(legend.position = "none")

p2 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_age, 
                    id = ~respid,
                    by = ~attribute_2_position,
                    estimate = "mm"), 
          group = "attribute_2_position", 
          vline = 0.5)+ 
  xlab("")+
  theme(legend.position = "none")

p3 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_religion, 
                    id = ~respid,
                    by = ~attribute_3_position,
                    estimate = "mm"), 
          group = "attribute_3_position", 
          vline = 0.5)+ 
  xlab("")+
  theme(legend.position = "none")

p4 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_citysize, 
                    id = ~respid,
                    by = ~attribute_4_position,
                    estimate = "mm"), 
          group = "attribute_4_position", 
          vline = 0.5)+ 
  xlab("")+
  theme(legend.position = "none")

p5 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_job, 
                    id = ~respid,
                    by = ~attribute_5_position,
                    estimate = "mm"), 
          group = "attribute_5_position", 
          vline = 0.5)+ 
  xlab("")+
  theme(legend.position = "none")

p6 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_consc, 
                    id = ~respid,
                    by = ~attribute_6_position,
                    estimate = "mm"), 
          group = "attribute_6_position", 
          vline = 0.5)+ 
  xlab("")+
  theme(legend.position = "none")

p7 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_ope, 
                    id = ~respid,
                    by = ~attribute_7_position,
                    estimate = "mm"), 
          group = "attribute_7_position", 
          vline = 0.5)+ 
  xlab("")+
  theme(legend.position = "none")

p8 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_neu, 
                    id = ~respid,
                    by = ~attribute_8_position,
                    estimate = "mm"), 
          group = "attribute_8_position", 
          vline = 0.5)+
  xlab("")+
  theme(legend.position = "none")


p9 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_restaurant, 
                    id = ~respid,
                    by = ~attribute_8_position,
                    estimate = "mm"), 
          group = "attribute_8_position", 
          vline = 0.5)+
  xlab("")+
  theme(legend.position = "none")


p10 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_transport, 
                    id = ~respid,
                    by = ~attribute_8_position,
                    estimate = "mm"), 
          group = "attribute_8_position", 
          vline = 0.5)+
  theme(text = element_text(size=10),
        legend.title = element_blank())


p11 = plot(cregg::cj(data, ccd_chosen_rw ~ ccd_animal, 
                    id = ~respid,
                    by = ~attribute_9_position,
                    estimate = "mm"), 
          group = "attribute_9_position", 
          vline = 0.5)+ 
  xlab("")+
  labs(caption = "Colors represent the position of the attribute")+
  theme(legend.position = "none")


p=(p1|p2|p3)/(p4|p5|p6)/(p7|p8|p9)/(p10|p11)

ggsave(paste0(output_wd,  "attribute_order_check.png"),
       p, height = 12, width = 12, create.dir = T)
