# pacman::p_load(
#   cregg, dplyr, ggpubr, cowplot,
#   MASS, cjoint, corrplot, dplyr,
#   forcats, ggplot2, gt, gtools,
#   gtsummary, margins, openxlsx,
#   patchwork, rio, texreg, tools
# )

#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"


#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

#recoding_functional_equivalents=T


########################
#### VISUAL
########################



if(recoding_functional_equivalents == T)
{
  output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/for_thesis_gaetano/FE/")
  
  main_path_vcd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/FE/"
}

if(recoding_functional_equivalents == F)
{
  output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/NFE/for_thesis_gaetano/")
  
  main_path_vcd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/NFE/"
}


########################
#### Marginal Means, general
########################



id_data_mm_it = readRDS(paste0(main_path_vcd, "ideology/IT/MMs/singlecountry_data.rds"))
pop_data_mm_it = readRDS(paste0(main_path_vcd,"populism/IT/MMs/singlecountry_data.rds"))


# View(data.frame(attribute=id_data_mm_it$feature, level=id_data_mm_it$level,
#                 id_est=id_data_mm_it$estimate, pop_est=pop_data_mm_it$estimate))

data = id_data_mm_it 

intercept = 0.5
leftlim = 0.4
rightlim = 0.6

v=list()

for(attribute in unique(data$feature))
{
  p = ggplot()+
    geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
    geom_pointrange(data=id_data_mm_it[id_data_mm_it$feature==attribute, ],
                    aes(x=estimate, xmin=lower, xmax=upper,
                        y=level, col="Right-wing", shape = "Right-wing"),
                        position = position_nudge(y = 1/10),
                        show.legend = F)+
    geom_pointrange(data=pop_data_mm_it[pop_data_mm_it$feature==attribute, ],
                    aes(x=estimate, xmin=lower, xmax=upper,
                        y=level, col="Populism", shape = "Populism"),
                        position = position_nudge(y = -1/10),
                        show.legend = F)+
    ylab(attribute)+
    xlab("\n")+
    scale_x_continuous(limits = c(leftlim, rightlim), 
                       breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
    scale_y_discrete(limits = rev(unique(data[data$feature==attribute, ]$level)))+
    scale_color_manual(
      values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
                 "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
      name = "Inference on",
      limits = c("Right-wing", "Populism")
    ) +
    scale_shape_manual(
      values = c("Right-wing" = 19, 
                 "Populism" = 17),
      name = "Inference on",
      limits = c("Right-wing", "Populism")
    ) +
    theme(
      legend.position = "right",  # You can change this to "top", "bottom", etc.
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12)
    )
  
  
  v[[attribute]] = p
}

p1 = (v[["Gender"]]/(v[["Ethnicity"]]+scale_x_continuous(limits = c(0.3, 0.7),
                                                         breaks = round(seq(0.3, 0.7, length.out = 7), digits=3)))/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+xlab("Effect size")))+plot_layout(heights = c(1,1,1,2,2))
p2= ((v[["Time"]]+ylab("Time"))/v[["Valence"]]/v[["Crowd"]]/v[["Food"]]/(v[["Pet"]]+xlab("Effect size")))+plot_layout(heights = c(1,1,2,2,1))

p=p1|p2

p = p+patchwork::plot_annotation(caption= "Circle = Right-wing, Triangle=Populism; Marginal means; 95% C.I.")

ggsave(paste0(output_wd,"mms_main.png"), p, 
       height = 10, 
       width = 8,
       create.dir = T)




########################
#### AMCE, general
########################

id_data_amce_it = readRDS(paste0(main_path_vcd, "ideology/IT/AMCEs/singlecountry_data.rds"))
pop_data_amce_it = readRDS(paste0(main_path_vcd, "populism/IT/AMCEs/singlecountry_data.rds"))
data=id_data_amce_it 

intercept = 0
leftlim = -0.1
rightlim = 0.1

v=list()

for(attribute in unique(data$feature))
{
  p = ggplot()+
    geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
    geom_pointrange(data=id_data_amce_it[id_data_amce_it$feature==attribute, ],
                    aes(x=estimate, xmin=lower, xmax=upper,
                        y=level, col="Right-wing", shape = "Right-wing"),
                    position = position_nudge(y = 1/10),
                    show.legend = F)+
    geom_pointrange(data=pop_data_amce_it[pop_data_amce_it$feature==attribute, ],
                    aes(x=estimate, xmin=lower, xmax=upper,
                        y=level, col="Populism", shape = "Populism"),
                    position = position_nudge(y = -1/10),
                    show.legend = F)+
    ylab(attribute)+
    xlab("\n")+
    scale_x_continuous(limits = c(leftlim, rightlim), 
                       breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
    scale_y_discrete(limits = rev(unique(data[data$feature==attribute, ]$level)))+
    scale_color_manual(
      values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
                 "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
      name = "Inference on",
      limits = c("Right-wing", "Populism")
    ) +
    scale_shape_manual(
      values = c("Right-wing" = 19, 
                 "Populism" = 17),
      name = "Inference on",
      limits = c("Right-wing", "Populism")
    ) +
    theme(
      legend.position = "right",  # You can change this to "top", "bottom", etc.
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12)
    )
  
  
  v[[attribute]] = p
}

p1 = (v[["Gender"]]/(v[["Ethnicity"]]+scale_x_continuous(limits = c(-0.2, 0.2),
                                                             breaks = round(seq(-0.2, 0.2, length.out = 7), digits=3)))/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+scale_x_continuous(limits = c(-0.2, 0.2),
                                                                                                                                                                               breaks = round(seq(-0.2, 0.2, length.out = 7), digits=3))+xlab("Effect size")))

p2= ((v[["Time"]]+ylab("Time"))/v[["Valence"]]/v[["Crowd"]]/v[["Food"]]/(v[["Pet"]]+xlab("Effect size")))+plot_layout(heights = c(1,1,2,2,1))

p= (p1+plot_layout(heights = c(1,1,1,2,2)))|p2
p = p+patchwork::plot_annotation(caption= "Circle = Right-wing; Triangle=Populism; AMCEs, 95% C.I.")

ggsave(paste0(output_wd,"amces_main.png"), p, 
       height = 10, 
       width = 8,
       create.dir = T)



##########
#ACIEs with politics
##########

#ideology acies marginal means italian political
id_data_politics = readRDS(paste0(main_path_vcd, "ideology/IT/Interactions/MMs/interacted_political_data.rds"))
#populism acies marginal means italian political
pop_data_politics =readRDS(paste0(main_path_vcd, "populism/IT/Interactions/MMs/interacted_political_data.rds"))


id_data_politics$level = gsub("\n"," ", id_data_politics$level)
pop_data_politics$level = gsub("\n"," ", pop_data_politics$level)


intercept = 0.5
leftlim = 0.35
rightlim = 0.65

p_politics = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data_politics,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data_politics,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data_politics$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
  scale_color_manual(
    values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
               "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  scale_shape_manual(
    values = c("Right-wing" = 19, 
               "Populism" = 17),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  theme(legend.position = "none",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

ggsave(paste0(output_wd,"ACIEs_interacted_politics_mm.png"), 
       p_politics,
       height = 10,
       width = 8,
       create.dir = T)


#############
# 
# ggsave(paste0(output_wd,"mms_interacted_politics.png"), p_politics, 
#        height = 12, 
#        width = 8,
#        create.dir = T)
# 
# saveRDS(p_politics, paste0(output_wd,"mms_interacted_politics.rds"))



############ 
#### same but with amce
############

# #ideology acies amces italian political
# id_acie_AMCE_it_pol = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/Interactions/AMCEs/interacted_political_data.rds")
# #populism acies amces italian political
# pop_acie_AMCE_it_pol = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/Interactions/AMCEs/interacted_political_data.rds")
# 
# 
# intercept = 0
# leftlim = -0.25
# rightlim = 0.25
# 
# p = ggplot()+
#   geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
#   geom_pointrange(data=id_data,
#                   aes(x=estimate, xmin=lower, xmax=upper,
#                       y=fct_reorder(level, desc(estimate)),
#                       col="Right-wing", shape = "Right-wing"),
#                   position = position_nudge(y = 1/10),
#                   show.legend = T)+
#   geom_pointrange(data=pop_data,
#                   aes(x=estimate, xmin=lower, xmax=upper,
#                       y=fct_reorder(level, desc(id_data$estimate)),
#                       col="Populism", shape = "Populism"),
#                   position = position_nudge(y = -1/10),
#                   show.legend = T)+
#   labs(y="",x="Marginal Mean")+
#   xlim(leftlim,rightlim)+
#   scale_color_manual(
#     values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
#                "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
#     name = "Inference on",
#     limits = c("Right-wing", "Populism")
#   ) +
#   scale_shape_manual(
#     values = c("Right-wing" = 19, 
#                "Populism" = 17),
#     name = "Inference on",
#     limits = c("Right-wing", "Populism")
#   ) +
#   theme(legend.position = "right",
#         axis.text.y = element_text(size=10),
#         axis.title.y = element_text(size=12))
# 
# 
# ggsave(paste0(output_wd,"amces_interacted_politics.png"), p, 
#        height = 12, 
#        width = 8,
#        create.dir = T)
# 

############################## 
############### ACIEs with sociodemos  1
############################## 
#ideology acies marginal means italian sociodemos
id_data_sociodemo = readRDS(paste0(main_path_vcd, "ideology/IT/Interactions/MMs/interacted_sociodemos_data.rds"))
#populism acies marginal means italian sociodemos
pop_data_sociodemo = readRDS(paste0(main_path_vcd, "populism/IT/Interactions/MMs/interacted_sociodemos_data.rds"))


### axis y labels langer rather than higher
id_data_sociodemo$level = gsub("\n"," ", id_data_sociodemo$level)
pop_data_sociodemo$level = gsub("\n"," ", pop_data_sociodemo$level)


intercept = 0.5
leftlim = 0.2
rightlim = 0.8

p_sociodemo = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data_sociodemo,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data_sociodemo,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data_sociodemo$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
  scale_color_manual(
    values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
               "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  scale_shape_manual(
    values = c("Right-wing" = 19, 
               "Populism" = 17),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  theme(legend.position = "none",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))



ggsave(paste0(output_wd,"ACIEs_interacted_sociodemo_mm.png"), 
       p_sociodemo,
       height = 10,
       width = 8,
       create.dir = T)


#############
# ggsave(paste0(output_wd,"mms_interacted_sociodemos.png"), p_sociodemo, 
#        height = 12, 
#        width = 8,
#        create.dir = T)
# 
# saveRDS(p_sociodemo, paste0(output_wd,"mms_interacted_sociodemos.rds"))

######## 
#### same but with amce
######## 
# 
# #ideology acies amces italian sociodemos
# id_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/Interactions/AMCEs/interacted_sociodemos_data.rds")
# #ideology acies mms italian sociodemos
# pop_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/Interactions/AMCEs/interacted_sociodemos_data.rds")
# 
# 
# intercept = 0
# leftlim = -0.25
# rightlim = 0.25
# 
# p = ggplot()+
#   geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
#   geom_pointrange(data=id_data,
#                   aes(x=estimate, xmin=lower, xmax=upper,
#                       y=fct_reorder(level, desc(estimate)),
#                       col="Right-wing", shape = "Right-wing"),
#                   position = position_nudge(y = 1/10),
#                   show.legend = T)+
#   geom_pointrange(data=pop_data,
#                   aes(x=estimate, xmin=lower, xmax=upper,
#                       y=fct_reorder(level, desc(id_data$estimate)),
#                       col="Populism", shape = "Populism"),
#                   position = position_nudge(y = -1/10),
#                   show.legend = T)+
#   labs(y="",x="Marginal Mean")+
#   xlim(leftlim,rightlim)+
#   scale_color_manual(
#     values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
#                "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
#     name = "Inference on",
#     limits = c("Right-wing", "Populism")
#   ) +
#   scale_shape_manual(
#     values = c("Right-wing" = 19, 
#                "Populism" = 17),
#     name = "Inference on",
#     limits = c("Right-wing", "Populism")
#   ) +
#   theme(legend.position = "right",
#         axis.text.y = element_text(size=10),
#         axis.title.y = element_text(size=12))
# 
# p
# 
# ggsave(paste0(output_wd,"amces_interacted_sociodemos.png"), p, 
#        height = 12, 
#        width = 8,
#        create.dir = T)
# 




####### 
#ACIEs with cultural 
####### 

#ideology acies marginal means italian cultural
id_data_cultural = readRDS(paste0(main_path_vcd, "ideology/IT/Interactions/MMs/interacted_cultural_data.rds"))
#populism acies marginal means italian cultural
pop_data_cultural = readRDS(paste0(main_path_vcd, "populism/IT/Interactions/MMs/interacted_cultural_data.rds"))

id_data_cultural$level = gsub("\n"," ", id_data_cultural$level)
pop_data_cultural$level = gsub("\n"," ", pop_data_cultural$level)



intercept = 0.5
leftlim = 0.4
rightlim = 0.6

p_cultural = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data_cultural,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data_cultural,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data_cultural$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="Marginal Mean")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
  scale_color_manual(
    values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
               "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  scale_shape_manual(
    values = c("Right-wing" = 19, 
               "Populism" = 17),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  theme(legend.position = "none",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

ggsave(paste0(output_wd,"ACIEs_interacted_cultural_mm.png"), 
       p_cultural,
       height = 10,
       width = 8,
       create.dir = T)


#############

# ggsave(paste0(output_wd,"mms_interacted_cultural.png"), p_cultural, 
#        height = 12, 
#        width = 8,
#        create.dir = T)
# 
# saveRDS(p_cultural, paste0(output_wd,"mms_interacted_cultural.rds"))

#### same but with amce

# #ideology acies amces italian cultural
# id_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/Interactions/AMCEs/interacted_cultural_data.rds")
# #ideology acies mms italian cultural
# pop_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/Interactions/AMCEs/interacted_cultural_data.rds")
# 
# 
# intercept = 0
# leftlim = -0.1
# rightlim = 0.1
# 
# p = ggplot()+
#   geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
#   geom_pointrange(data=id_data,
#                   aes(x=estimate, xmin=lower, xmax=upper,
#                       y=fct_reorder(level, desc(estimate)),
#                       col="Right-wing", shape = "Right-wing"),
#                   position = position_nudge(y = 1/10),
#                   show.legend = T)+
#   geom_pointrange(data=pop_data,
#                   aes(x=estimate, xmin=lower, xmax=upper,
#                       y=fct_reorder(level, desc(id_data$estimate)),
#                       col="Populism", shape = "Populism"),
#                   position = position_nudge(y = -1/10),
#                   show.legend = T)+
#   labs(y="",x="Marginal Mean")+
#   xlim(leftlim,rightlim)+
#   scale_color_manual(
#     values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
#                "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
#     name = "Inference on",
#     limits = c("Right-wing", "Populism")
#   ) +
#   scale_shape_manual(
#     values = c("Right-wing" = 19, 
#                "Populism" = 17),
#     name = "Inference on",
#     limits = c("Right-wing", "Populism")
#   ) +
#   theme(legend.position = "right",
#         axis.text.y = element_text(size=10),
#         axis.title.y = element_text(size=12))
# 
# p
# 
# ggsave(paste0(output_wd,"amces_interacted_cultural.png"), p, 
#        height = 12, 
#        width = 8,
#        create.dir = T)




############################## 
############### ACIEs with sociodemos  full
############################## 
#ideology acies marginal means italian sociodemos
id_data_sociodemo_full = readRDS(paste0(main_path_vcd, "ideology/IT/Interactions/MMs/interacted_sociodemos_full_data.rds"))
#populism acies marginal means italian sociodemos
pop_data_sociodemo_full = readRDS(paste0(main_path_vcd, "populism/IT/Interactions/MMs/interacted_sociodemos_full_data.rds"))


### axis y labels langer rather than higher


intercept = 0.5
leftlim = 0
rightlim = 1

p_sociodemo_full = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data_sociodemo_full,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data_sociodemo_full,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data_sociodemo_full$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 11), digits=3))+
  scale_color_manual(
    values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
               "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  scale_shape_manual(
    values = c("Right-wing" = 19, 
               "Populism" = 17),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  theme(legend.position = "none",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

p_sociodemo_full = p_sociodemo_full +plot_annotation(caption = "Circle=Right-wing, Triangle=Populism; Marginal means, 95% C.I.")


ggsave(paste0(output_wd,"ACIEs_Visual_Conjoint_Sociodemo_full.png"), 
       p_sociodemo_full, 
       height = 10, 
       width = 8,
       create.dir = T)



######## Composite graphs



p= p_politics/p_sociodemo/p_cultural+plot_layout(heights = c(2, 1, 2))

p=p+plot_annotation(caption = "Circle=Right-wing, Triangle=Populism; Marginal means, 95% C.I.")

ggsave(paste0(output_wd,"mms_ACIEs_full.png"), p,
       height = 10,
       width = 8,
       create.dir = T)


p= p_politics/p_cultural+plot_layout(heights = c(1, 1))

p=p+plot_annotation(caption = "Circle=Right-wing, Triangle=Populism; Marginal means, 95% C.I.")

ggsave(paste0(output_wd,"mms_ACIEs_polcult.png"), p,
       height = 10,
       width = 8,
       create.dir = T)





############################## 
############### ACIEs with sociodemos politics full
############################## 
#ideology acies marginal means italian sociodemos
id_data_sociodemo_full = readRDS(paste0(main_path_vcd, "ideology/IT/Interactions/MMs/interacted_political_sociodemo_data.rds"))
#populism acies marginal means italian sociodemos
pop_data_sociodemo_full = readRDS(paste0(main_path_vcd, "populism/IT/Interactions/MMs/interacted_political_sociodemos_full_data.rds"))


### axis y labels langer rather than higher


intercept = 0.5
leftlim = 0
rightlim = 1

p_sociodemo_full = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data_sociodemo_full,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data_sociodemo_full,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data_sociodemo_full$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 11), digits=3))+
  scale_color_manual(
    values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
               "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  scale_shape_manual(
    values = c("Right-wing" = 19, 
               "Populism" = 17),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  theme(legend.position = "none",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

p_sociodemo_full = p_sociodemo_full +plot_annotation(caption = "Circle=Right-wing, Triangle=Populism; Marginal means, 95% C.I.")


ggsave(paste0(output_wd,"ACIEs_Visual_Conjoint_Sociodemo_full.png"), 
       p_sociodemo_full, 
       height = 10, 
       width = 8,
       create.dir = T)


############################## 
############### ACIEs with sociodemos politics full (only interaction non-exploratory)
##############################

id_data_mm_it = readRDS(paste0(main_path_vcd, "ideology/IT/Interactions/MMs/interacted_political_sociodemo_data.rds"))

id_data_mm_it$ideology = ifelse(grepl("Right", id_data_mm_it$level), "Right-wing issue", "Left-wing issue")

intercept = 0.5
leftlim = 0.33
rightlim = 0.67

p = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data_mm_it,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col=ideology, shape=ideology),
                  show.legend = T)+
  labs(y="",x="")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 11), digits=3))+
  scale_color_manual(
    values = c("Left-wing issue" = wesanderson::wes_palettes$Darjeeling1[1],
               "Right-wing issue" = wesanderson::wes_palettes$Darjeeling1[2]),
    name = "Inference on",
    limits = c("Left-wing issue", "Right-wing issue")
  ) +
  theme(legend.position = "none",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

p

p = p +plot_annotation(caption = "Circle=Left-wing issue, Triangle=Right-wing issue")

ggsave(paste0(output_wd,"ACIEs_Visual_Conjoint_Politics_with_Sociodemo.png"), 
       p, 
       height = 10, 
       width = 8,
       create.dir = T)



############################## 
############### ACIEs with cultural with politics full (only interaction non-exploratory)
##############################

id_data_mm_it = readRDS(paste0(main_path_vcd, "ideology/IT/Interactions/MMs/interacted_political_cultural_data.rds"))

id_data_mm_it$ideology = ifelse(grepl("Right", id_data_mm_it$level), "Right-wing issue", "Left-wing issue")

intercept = 0.5
leftlim = 0.38
rightlim = 0.62

p = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data_mm_it,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col=ideology, shape=ideology),
                  show.legend = T)+
  labs(y="",x="")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 11), digits=3))+
  scale_color_manual(
    values = c("Left-wing issue" = wesanderson::wes_palettes$Darjeeling1[1],
               "Right-wing issue" = wesanderson::wes_palettes$Darjeeling1[2]),
    name = "Inference on",
    limits = c("Left-wing issue", "Right-wing issue")
  ) +
  theme(legend.position = "none",
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(size=12))

p

p = p +plot_annotation(caption = "Circle=Left-wing issue, Triangle=Right-wing issue")

ggsave(paste0(output_wd,"ACIEs_Visual_Conjoint_Culture_with_Sociodemo.png"), 
       p, 
       height = 10, 
       width = 8,
       create.dir = T)



##################################  ##########################################
################# Ordered overall MM graph for between attribute  ###########
################# confrontation in the VISUAL conjoint #####################  
################# ################# ########################################  



id_data_mm_it = readRDS(paste0(main_path_vcd, "ideology/IT/MMs/singlecountry_data.rds"))
pop_data_mm_it = readRDS(paste0(main_path_vcd, "populism/IT/MMs/singlecountry_data.rds"))


intercept = 0.5
leftlim = 0.3
rightlim = 0.6


p = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data_mm_it,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, estimate, .desc = T), 
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = F)+
  geom_pointrange(data=pop_data_mm_it,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, id_data_mm_it$estimate, .desc = T), 
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = F)+
  ylab("Attribute levels")+
  xlab("\n")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
  scale_color_manual(
    values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
               "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  scale_shape_manual(
    values = c("Right-wing" = 19, 
               "Populism" = 17),
    name = "Inference on",
    limits = c("Right-wing", "Populism")
  ) +
  theme(
    legend.position = "right",  # You can change this to "top", "bottom", etc.
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12)
  )



p = p+patchwork::plot_annotation(caption= "Circle = Right-wing, Triangle=Populism; Marginal means; 95% C.I.")

ggsave(paste0(output_wd,"MMs_vcd_ordered.png"), 
       p, 
       height = 10, 
       width = 8,
       create.dir = T)


p_only_id =  ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data_mm_it,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, estimate, .desc = T), 
                      col="Right-wing", shape = "Right-wing"),
                  show.legend = F)+
  ylab("Attribute levels")+
  xlab("\n")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
  scale_color_manual(
    values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1]),
    name = "Inference on",
    limits = c("Right-wing")
  ) +
  scale_shape_manual(
    values = c("Right-wing" = 19),
    name = "Inference on",
    limits = c("Right-wing")
  ) +
  theme(
    legend.position = "right",  # You can change this to "top", "bottom", etc.
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12)
  )

ggsave(paste0(output_wd,"MMs_vcd_ordered_only_id.png"), 
       p_only_id, 
       height = 10, 
       width = 8,
       create.dir = T)


##################################  ##########################################
################# subgroup analyses differences populism and ideology together
################visual conjoint ##  
################# ################# ########################################  

if(recoding_functional_equivalents == T)
{
  output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/FE/for_thesis_gaetano/")
  
  main_path_vcd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/FE/"
}

if(recoding_functional_equivalents == F)
{
  output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/NFE/for_thesis_gaetano/")
  
  main_path_vcd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/NFE/"
}

subgroups = c("Education level", "News media Exposure", "Gender", "Political Ideology", "Political Interest")

for(subgroup in subgroups)
{
  
  id_data = readRDS(paste0(main_path_vcd, "singlecountry/ideology/IT/Subgroup Analyses/", subgroup, "mm_differences_data.rds"))
  pop_data = readRDS(paste0(main_path_vcd, "singlecountry/populism/IT/Subgroup Analyses/", subgroup, "mm_differences_data.rds"))
  
  
  intercept = 0
  leftlim = -0.2
  rightlim = 0.2
  
  data=id_data
  
  v=list()
  
  for(attribute in unique(data$feature))
  {
    p = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(data=id_data[id_data$feature==attribute, ],
                      aes(x=estimate, xmin=lower, xmax=upper,
                          y=level, col="Right-wing", shape = "Right-wing"),
                      position = position_nudge(y = 1/10),
                      show.legend = F)+
      geom_pointrange(data=pop_data[pop_data$feature==attribute, ],
                      aes(x=estimate, xmin=lower, xmax=upper,
                          y=level, col="Populism", shape = "Populism"),
                      position = position_nudge(y = -1/10),
                      show.legend = F)+
      ylab(attribute)+
      xlab("\n")+
      scale_x_continuous(limits = c(leftlim, rightlim), 
                         breaks = round(seq(leftlim, rightlim, length.out = 7),
                                        digits=3))+
      scale_y_discrete(limits = rev(unique(data[data$feature==attribute, ]$level)))+
      scale_color_manual(
        values = c("Right-wing" = wesanderson::wes_palettes$Darjeeling1[1],
                   "Populism" = wesanderson::wes_palettes$Darjeeling1[2]),
        name = "Inference on",
        limits = c("Right-wing", "Populism")
      ) +
      scale_shape_manual(
        values = c("Right-wing" = 19, 
                   "Populism" = 17),
        name = "Inference on",
        limits = c("Right-wing", "Populism")
      ) +
      theme(
        legend.position = "right",  # You can change this to "top", "bottom", etc.
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12)
      )
    
    
    v[[attribute]] = p
  }
  
  p1 = (v[["Gender"]]/(v[["Ethnicity"]]+scale_x_continuous(limits = c(leftlim-0.3, rightlim+0.3),
                                                           breaks = round(seq(leftlim-0.3, rightlim+0.3, length.out = 7), digits=3)))/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+xlab("Effect size")))+plot_layout(heights = c(1,1,1,2,2))
  p2= ((v[["Time"]])/v[["Valence"]]/v[["Crowd"]]/v[["Food"]]/(v[["Pet"]]+xlab("Effect size")))+plot_layout(heights = c(1,1,2,2,1))
  
  p=p1|p2
  
  p = p+patchwork::plot_annotation(caption= "Circle = Right-wing, Triangle=Populism; Marginal means; 95% C.I.")
  
  
  ggsave(paste0(output_wd, subgroup, "_MMs_differences.png"), 
         p, 
         height = 10, 
         width = 8,
         create.dir = T)
}


####################################################################  
################# CLASSIC CONJOINT ##################################
####################################################################


##################################  
################# ACIES graphs for thesis classic cj
################# #################

################### Rearrange the graph for the ACIEs in the classic conjoint

plot1 = readRDS(paste0(gdrive_code,"VIPOP_SURVEY/analyses/classic_conjoint_design/singlecountry/ideology/IT/Interactions/MMs/interacted_sociodemos_professionagemm.rds"))
plot2 = readRDS(paste0(gdrive_code,"VIPOP_SURVEY/analyses/classic_conjoint_design/singlecountry/ideology/IT/Interactions/MMs/interacted_sociodemos_professionreligionmm.rds"))
plot3 = readRDS(paste0(gdrive_code,"VIPOP_SURVEY/analyses/classic_conjoint_design/singlecountry/ideology/IT/Interactions/MMs/interacted_sociodemos_religionagemm.rds"))

plot1=plot1+labs(y="Interaction between age and profession", x="")
plot2=plot2+labs(y="Interaction between religion and profession")
plot3=plot3+labs(y="Interaction between religion and age", x="")

p = plot1/plot3/plot2 +plot_layout(heights = c(4, 3, 4))


output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/classic_conjoint_design/for_thesis_gaetano/")

ggsave(paste0(output_wd,"ACIEs_Classic_Conjoint_Sociodemos.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)


plot1 = readRDS(paste0(gdrive_code,"VIPOP_SURVEY/analyses/classic_conjoint_design/singlecountry/ideology/IT/Interactions/MMs/interacted_psychomm.rds"))
plot2 = readRDS(paste0(gdrive_code,"VIPOP_SURVEY/analyses/classic_conjoint_design/singlecountry/ideology/IT/Interactions/MMs/interacted_culturalmm.rds"))

plot1=plot1+labs(y="Interaction between psychological attributes", x="")
plot2=plot2+labs(y="Interaction between favorite restaurant and transportation")

p = plot1/plot2+plot_layout(heights = c(3, 4))

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/classic_conjoint_design/for_thesis_gaetano/")

ggsave(paste0(output_wd,"ACIEs_Classic_Conjoint_PsychoandLifestyle.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)




##################################  ##########################################
################# Ordered overall MM graph for between attribute  ###########
################# confrontation in the classic conjoint #####################  
################# ################# ########################################  

data = readRDS(paste0(gdrive_code,"VIPOP_SURVEY/analyses/classic_conjoint_design/singlecountry/ideology/IT/MMs/_data_singlecountry.rds"))


leftlim=0.33
rightlim=0.6
p = ggplot(data)+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, estimate, .desc = T)), 
                  col = wesanderson::wes_palettes$Darjeeling1[1])+
  ylab("Attribute Level")+
  xlab("Effect (MM)")+
  scale_x_continuous(limits = c(leftlim, rightlim), 
                     breaks = round(seq(leftlim, rightlim, length.out = 7), digits=3))+
  theme(legend.position = "none",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

p
ggsave(paste0(output_wd,"MMs_ccd_ordered.png"), 
       p, 
       height = 10, 
       width = 8,
       create.dir = T)





