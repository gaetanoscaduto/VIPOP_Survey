#randomization check code
#install.packages("ggplot2")
#install.packages("tools")
#install.packages("patchwork")
#install.packages("ggpubr")

library(ggpubr)
library(ggplot2)
library(dplyr)
library(tools)
library(patchwork)

# Function to calculate percentage
calc_percentage <- function(data, variable) {
  data %>%
    group_by(!!sym(variable)) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
}

contexts= c("FR","CZ", "IT", "SW")

dataframes_list = list(IT=data.frame(),
                       FR=data.frame(),
                       CZ=data.frame(),
                       SW=data.frame())


plot_lists = list(IT=list(),
                  FR=list(),
                  CZ=list(),
                  SW=list())

output_dir = "C:/Users/GS_237332609/OneDrive - Università degli Studi di Milano-Bicocca/Desktop/visual_conjoint_experiment-main/visual_conjoint_experiment-main/"
for(context in contexts)
{
  input_dir <- paste0("C:/Users/GS_237332609/OneDrive - Università degli Studi di Milano-Bicocca/Desktop/immagini_def/", context, "/")
  
  files <- list.files(input_dir, full.names = TRUE)
  
  fixed_part = paste0("C:/Users/GS_237332609/OneDrive - Università degli Studi di Milano-Bicocca/Desktop/immagini_def/", context, "/", context, "_")
  
  
  for (file in files) {
    # Remove the fixed part of the file name
    file_name <- gsub(fixed_part, "", file)
    file_name <- gsub(".png", "", file_name)
    
    # Split the file name by underscores
    parts <- unlist(strsplit(file_name, "_"))
    
    # Create a data frame with the parts
    temp_df <- data.frame(
      ethnicity = parts[1],
      gender = parts[2],
      age = as.numeric(parts[3]),
      pic_number = parts[4],
      name = parts[5],
      surname = parts[6],
      job = parts[7],
      issue = parts[8],
      nostalgia = parts[9],
      valence = parts[10],
      food = parts[11],
      animal = parts[12],
      crowd = parts[13]
    )
    
    # Bind the temp data frame to the main data frame
    dataframes_list[[context]] <- bind_rows(dataframes_list[[context]], temp_df)
  }
  
 
  
  dataframes_list[[context]]=na.omit(dataframes_list[[context]])


  # Visualize the distribution of some variables with percentages
  p_ethnicity=ggplot(dataframes_list[[context]], aes(x = ethnicity,fill=ethnicity)) +
    geom_bar() +
    scale_y_continuous(breaks = seq(0,15000, by=1500))+
    labs(x=toTitleCase("ethnicity"), y="Count")+
    theme(legend.position = "none")
  
  p_gender=ggplot(dataframes_list[[context]], aes(x = gender,fill=gender)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("gender"), y="Count")+
    theme(legend.position = "none")
  
  
  p_age=ggplot(dataframes_list[[context]], aes(x = as.character(age), fill=as.character(age))) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("age"), y="Count")+
    theme(legend.position = "none")
  
  
  p_pic_number=ggplot(dataframes_list[[context]], aes(x = pic_number,fill=pic_number)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("pic_number"), y="Count")+
    theme(legend.position = "none")
  
  p_name=ggplot(dataframes_list[[context]], aes(x = name,fill=name)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("name"), y="Count")+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  p_surname=ggplot(dataframes_list[[context]], aes(x = surname,fill=surname)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("surname"), y="Count")+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  p_job=ggplot(dataframes_list[[context]], aes(x = job,fill=job)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("job"), y="Count")+
    theme(legend.position = "none")
  
  p_issue=ggplot(dataframes_list[[context]], aes(x = issue,fill=issue)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("issue"), y="Count")+
    theme(legend.position = "none")
  
  p_nostalgia=ggplot(dataframes_list[[context]], aes(x = nostalgia,fill=nostalgia)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("nostalgia"), y="Count")+
    theme(legend.position = "none")
  
  p_valence=ggplot(dataframes_list[[context]], aes(x = valence,fill=valence)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("valence"), y="Count")+
    theme(legend.position = "none")
  
  p_food=ggplot(dataframes_list[[context]], aes(x = food,fill=food)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("food"), y="Count")+
    theme(legend.position = "none")
  
  p_animal=ggplot(dataframes_list[[context]], aes(x = animal,fill=animal)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("animal"), y="Count")+
    theme(legend.position = "none")
  
  p_crowd=ggplot(dataframes_list[[context]], aes(x = crowd,fill=crowd)) +
    geom_bar() +
    scale_y_continuous(n.breaks=10)+
    labs(x=toTitleCase("crowd"), y="Count")+
    theme(legend.position = "none")
  
  plot_lists[[context]]=list(p_ethnicity, p_gender, p_age, p_pic_number,
                          p_name, p_surname, p_job, p_issue,
                          p_nostalgia, p_valence, p_food, p_animal, p_crowd)
  
  total_plot=ggarrange(p_ethnicity, p_gender, p_age, p_pic_number,
            p_name, p_surname, p_job, p_issue,
            p_nostalgia, p_valence, p_food, p_animal, p_crowd)
    
  ggsave(paste0(output_dir, "randomization checks/", context, ".png"), total_plot, width=25, height=15)
  
}
