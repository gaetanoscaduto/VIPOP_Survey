library(rio)
library(dplyr)
library(ggplot2)

data = readRDS("data_recoded.RDS")

output_dir = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/risultati_pilot/"

ggplot(data, aes(x=interest))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("")

ggsave(paste0(output_dir,"interest.png"), width=10, height=10)

ggplot(data, aes(x=attention_check1))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("")

ggsave(paste0(output_dir,"attention_check1.png"), width=10, height=10)

ggplot(data, aes(x=exposure))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("")

ggsave(paste0(output_dir,"exposure.png"), width=10, height=10)

ggplot(data, aes(x=votechoice))+
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("")

ggsave(paste0(output_dir,"votechoice.png"), width=10, height=10)

nethet_variables = names(data)[grepl("nethet_", names(data))]

for(i in nethet_variables)
{
  p = ggplot(data, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format())+
    ylab("")

  ggsave(paste0(output_dir, i, ".png"), width=10, height=10)
  
}


ft_variables = names(data)[grepl("ft_", names(data))]

for(i in ft_variables)
{
  p = ggplot(data, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format())+
    ylab("")

  ggsave(paste0(output_dir, i, ".png"), width=10, height=10)
  
}


trust_variables = names(data)[grepl("trust_", names(data))]

for(i in trust_variables)
{
  p = ggplot(data, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format())+
    ylab("")
  
  ggsave(paste0(output_dir, i, ".png"), width=10, height=10)
  
  
}


populism_variables = names(data)[grepl("populism_", names(data))]

for(i in populism_variables)
{
  p = ggplot(data, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format())+
    ylab("")

  ggsave(paste0(output_dir, i, ".png"), width=10, height=10)
}
