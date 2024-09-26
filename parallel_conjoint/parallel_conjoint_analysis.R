#this script is for the analyses related to the parallel conjoint design

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)
#

data = readRDS("data_recoded.RDS")


##########################
#### RANDOMIZATION DIAGNOSTICS
##########################



#####
# Diagnostics
#####

#### Randomizzazione tutto ok?

plot(cj_freqs(data, chosen ~ Gender + Age + Area + Profession + Ideology + Group, id = ~id), col="grey")

table(data$chosen)
table(data$Gender)
table(data$Age)
table(data$Area)
table(data$Profession)
table(data$Ideology)
table(data$Group)


#oppure con ggplot
aus = cj_freqs(data, chosen ~ Gender + Age + Area + Profession + Ideology + Group, id = ~id)

v = list()

for(i in unique(aus$feature))
{
  
  p = aus |>
    filter(feature == i) |>
    ggplot(aes(y=level, x=estimate))+
    geom_col()+
    ylab("")+
    xlab("")+
    ggtitle(as.character(i))+
    scale_x_continuous(breaks = seq(0, round(max(aus$estimate), digits = -1), by = round(max(aus$estimate)/10, digits = -1)),
                       limits = c(0,max(aus$estimate)+1))+
    # coord_flip()+
    theme(text = element_text(size = 15),
          legend.position = "none",
          plot.title = element_text(size=14))
  
  v[[i]] = p
}
p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p

ggsave(paste0(output_wd,"diagnostic randomization_eng.png"), p, height = 10, width = 8)

