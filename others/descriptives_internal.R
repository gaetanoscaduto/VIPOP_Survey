library(rio)
library(dplyr)
library(ggplot2)

#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context
#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"

#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"

data = readRDS(paste0(dataset_rep, "data_recoded_", context, ".RDS"))

output_dir = paste0(gdrive_code, "VIPOP_SURVEY/risultati_descrittive/", context, "/")
#output_dir_s = "/Users/silviadecadri/Library/CloudStorage/GoogleDrive-silviadecadri@gmail.com/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/risultati_pilot" 



#Silvia's Variables
ggplot(data, aes(x=gender))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "gender.png"),
       width=10, height=10,create.dir = T)


ggplot(data, aes(x=age))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "age.png"), width=10, height=10)


ggplot(data, aes(x=AGE_GROUP))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "agegroup.png"), width=10, height=10)

#nb: not recoded
ggplot(data, aes(x=education))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "education.png"), width=10, height=10)


ggplot(data, aes(x=EDU_LEVEL))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "education_level.png"), width=10, height=10)


#nb: not recoded
ggplot(data, aes(x=region))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "region.png"), width=10, height=10)


#nb: not recoded
ggplot(data, aes(x=region_feel))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "region_feel.png"), width=10, height=10)


ggplot(data, aes(x=citysize))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "citysize.png"), width=10, height=10)


#nb: not recoded
tipi_variables = names(data)[grepl("tipi_", names(data))]
for(i in tipi_variables)
{
  p = ggplot(data, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
    ylab("")
  print(p)
  ggsave(paste0(output_dir,  i, ".png"), width=10, height=10)
}


ggplot(data, aes(x=TIPI_CON_REC))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "tipiconrec.png"), width=10, height=10)


ggplot(data, aes(x=TIPI_OPE_REC))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "tipioperec.png"), width=10, height=10)

ggplot(data, aes(x=diet))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "diet.png"), width=10, height=10)


ggplot(data, aes(x=animal))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "animal.png"), width=10, height=10)


ggplot(data, aes(x=holiday))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "holiday.png"), width=10, height=10)


ggplot(data, aes(x=socialposition))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "socialposition.png"), width=10, height=10)


 data$ideology = factor(data$ideology, levels = c("default","1","2","3", "4","5","6","7","8","9","10","notplaced"))
ggplot(data, aes(x=ideology))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "ideology.png"), width=10, height=10)


ggplot(data, aes(x=IDEOLOGY_REC))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "ideologyrec.png"), width=10, height=10)


ggplot(data, aes(x=nat_med_1))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "natmed1.png"), width=10, height=10)

# #!non esiste man_med! # da Gaetano: si chiama MAT med
# ggplot(data, aes(x=mat_med))+
#   geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
#   scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
# #   ylab("")
# 
# ggsave(paste0(output_dir, "mat_med.png"), width=10, height=10)
# 

#nb: not recoded
ggplot(data, aes(x=sns_use))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "sns_use.png"), width=10, height=10)


#Gaetano's Variables
ggplot(data, aes(x=interest))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "interest.png"), width=10, height=10)


ggplot(data, aes(x=attention_check1))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "attention_check1.png"), width=10, height=10)


ggplot(data, aes(x=exposure))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "exposure.png"), width=10, height=10)


ggplot(data, aes(x=votechoice))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
  ylab("")

ggsave(paste0(output_dir, "votechoice.png"), width=10, height=10)


nethet_variables = names(data)[grepl("nethet_", names(data))]

for(i in nethet_variables)
{
  p = ggplot(data, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
    ylab("")

  ggsave(paste0(output_dir,  i, ".png"), width=10, height=10)
  
}


ft_variables = names(data)[grepl("ft_", names(data))]

for(i in ft_variables)
{
  p = ggplot(data, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
    ylab("")

  ggsave(paste0(output_dir,  i, ".png"), width=10, height=10)
  
}


trust_variables = names(data)[grepl("trust_", names(data))]

for(i in trust_variables)
{
  p = ggplot(data, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
    ylab("")
  
  ggsave(paste0(output_dir,  i, ".png"), width=10, height=10)
  
  
}


populism_variables = names(data)[grepl("populism_", names(data))]

for(i in populism_variables)
{
  p = ggplot(data, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format(), n.breaks=10)+
    ylab("")

  ggsave(paste0(output_dir,  i, ".png"), width=10, height=10)
}

#View(data[, c("start_", "end_")])

data$start_r <- as.POSIXct(data$start_, format = "%Y-%m-%d %H:%M:%S")
data$end_r <- as.POSIXct(data$end_, format = "%Y-%m-%d %H:%M:%S")

# View(data[, c("start_r", "end_r")])

# Calculate the time difference in seconds
data$time_diff_mins <- as.numeric(difftime(data$end_, data$start_, units = "mins"))

mean(data$time_diff_mins)
median(data$time_diff_mins)

ggplot(data[data$time_diff_mins<=35, ], aes(x=time_diff_mins))+
  geom_histogram()+
  scale_y_continuous(n.breaks=10)+
  scale_x_continuous(breaks=seq(0,34, by=1))+
  #xlim(0,31)+
  ylab("")


ggsave(paste0(output_dir, "time_to_complete.png"), width=10, height=10)

