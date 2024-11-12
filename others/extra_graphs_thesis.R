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

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/visual_conjoint_design/for_thesis_gaetano/")

id_data_mm_it = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/MMs/singlecountry_data.rds")
pop_data_mm_it = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/MMs/singlecountry_data.rds")


attributes= c("Ethnicity", "Ethnicity",
              "Gender", "Gender", 
              "Age","Age",
              "Job","Job","Job","Job","Job",
              "Issue", "Issue", "Issue", "Issue",
              "Nostalgia", "Nostalgia", "Nostalgia", "Nostalgia",
              "Valence","Valence","Valence","Valence",
              "Food","Food","Food","Food",
              "Animal","Animal","Animal","Animal",
              "Crowd","Crowd","Crowd","Crowd")

y_labels_plots = list(ethnicity=c("Black","White"),
                      gender=c("Female","Male"),
                      age=c("35", "70"),
                      job=c("Lawyer","Waiter","Entrepreneur","Teacher","Politician"),
                      issue=c("Leftneg","Leftpos","Rightneg","Rightpos"),
                      nostalgia=c("Past1","Past2","Future1","Future2"),
                      valence=c("Corruption1","Corruption2", "Honesty1", "Honesty2"),
                      food=c("Vegan","Ethnic","Meatpoor","Meatrich"),
                      animal=c("Catpoor","Catrich","Dogpoor","Dogrich"),
                      crowd=c("Mixedpeople","Whitepeople","Mixedelite","Whiteelite")
)

intercept = 0.5
leftlim = 0.4
rightlim = 0.6

v=list()

for(attribute in unique(attributes))
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
    xlim(leftlim,rightlim)+
    scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
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

p = ((v[["Ethnicity"]]+xlim(0.3,0.7))/v[["Gender"]]/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+xlab("Effect size")))|(v[["Nostalgia"]]/v[["Valence"]]/v[["Food"]]/v[["Animal"]]/(v[["Crowd"]]+xlab("Effect size")))
p = p+patchwork::plot_annotation(caption= "Circle = Right-wing\nTriangle=Populism\nMarginal means, 95% C.I.")

p

ggsave(paste0(output_wd,"mms_main.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)


id_data_amce_it = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/AMCEs/singlecountry_data.rds")
pop_data_amce_it = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/AMCEs/singlecountry_data.rds")



intercept = 0
leftlim = -0.1
rightlim = 0.1

v=list()

for(attribute in unique(attributes))
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
    xlim(leftlim,rightlim)+
    scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
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

p = ((v[["Ethnicity"]]+xlim(-0.2,0.2))/v[["Gender"]]/v[["Age"]]/v[["Job"]]/(v[["Issue"]]+xlab("Effect size")))|(v[["Nostalgia"]]/v[["Valence"]]/v[["Food"]]/v[["Animal"]]/(v[["Crowd"]]+xlab("Effect size")))
p = p+patchwork::plot_annotation(caption= "Circle = Right-wing\nTriangle=Populism\nAMCEs, 95% C.I.")

p

ggsave(paste0(output_wd,"amces_main.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)

########## ACIEs with politics

#ideology acies marginal means italian political
id_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/Interactions/MMs/interacted_political_data.rds")
#populism acies marginal means italian political
pop_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/Interactions/MMs/interacted_political_data.rds")


intercept = 0.5
leftlim = 0.37
rightlim = 0.62

p = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="Marginal Mean")+
  xlim(leftlim,rightlim)+
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
  theme(legend.position = "right",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

p

ggsave(paste0(output_wd,"mms_interacted_politics.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)


#### same but with amce

#ideology acies amces italian political
id_acie_AMCE_it_pol = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/Interactions/AMCEs/interacted_political_data.rds")
#populism acies amces italian political
pop_acie_AMCE_it_pol = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/Interactions/AMCEs/interacted_political_data.rds")


intercept = 0
leftlim = -0.25
rightlim = 0.25

p = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="Marginal Mean")+
  xlim(leftlim,rightlim)+
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
  theme(legend.position = "right",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))


ggsave(paste0(output_wd,"amces_interacted_politics.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)



############### ACIEs with sociodemos

#ideology acies marginal means italian sociodemos
id_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/Interactions/MMs/interacted_sociodemos_data.rds")
#populism acies marginal means italian sociodemos
pop_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/Interactions/MMs/interacted_sociodemos_data.rds")


intercept = 0.5
leftlim = 0.25
rightlim = 0.75

p = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="Marginal Mean")+
  xlim(leftlim,rightlim)+
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
  theme(legend.position = "right",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

p

ggsave(paste0(output_wd,"mms_interacted_sociodemos.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)


#### same but with amce

#ideology acies amces italian sociodemos
id_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/Interactions/AMCEs/interacted_sociodemos_data.rds")
#ideology acies mms italian sociodemos
pop_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/Interactions/AMCEs/interacted_sociodemos_data.rds")


intercept = 0
leftlim = -0.25
rightlim = 0.25

p = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="Marginal Mean")+
  xlim(leftlim,rightlim)+
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
  theme(legend.position = "right",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

p

ggsave(paste0(output_wd,"amces_interacted_sociodemos.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)




####### ACIEs with cultural 


#ideology acies marginal means italian cultural
id_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/Interactions/MMs/interacted_cultural_data.rds")
#populism acies marginal means italian cultural
pop_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/Interactions/MMs/interacted_cultural_data.rds")

intercept = 0.5
leftlim = 0.4
rightlim = 0.6

p = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="Marginal Mean")+
  xlim(leftlim,rightlim)+
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
  theme(legend.position = "right",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

p

ggsave(paste0(output_wd,"mms_interacted_cultural.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)


#### same but with amce

#ideology acies amces italian cultural
id_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/ideology/IT/Interactions/AMCEs/interacted_cultural_data.rds")
#ideology acies mms italian cultural
pop_data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/visual_conjoint_design/singlecountry/populism/IT/Interactions/AMCEs/interacted_cultural_data.rds")


intercept = 0
leftlim = -0.1
rightlim = 0.1

p = ggplot()+
  geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
  geom_pointrange(data=id_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(estimate)),
                      col="Right-wing", shape = "Right-wing"),
                  position = position_nudge(y = 1/10),
                  show.legend = T)+
  geom_pointrange(data=pop_data,
                  aes(x=estimate, xmin=lower, xmax=upper,
                      y=fct_reorder(level, desc(id_data$estimate)),
                      col="Populism", shape = "Populism"),
                  position = position_nudge(y = -1/10),
                  show.legend = T)+
  labs(y="",x="Marginal Mean")+
  xlim(leftlim,rightlim)+
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
  theme(legend.position = "right",
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12))

p

ggsave(paste0(output_wd,"amces_interacted_cultural.png"), p, 
       height = 12, 
       width = 8,
       create.dir = T)

