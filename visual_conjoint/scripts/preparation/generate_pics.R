 # install.packages("magick")
 # install.packages("fs")
 # install.packages("emo")
 # install.packages("openxlsx")
 # install.packages("dplyr")
 # install.packages("showtext", dependencies=T)

library(magick)
library(fs)
library(openxlsx)
library(dplyr)

#note: if working with google drive, you need to delete the .ini files in the
#"propic/round/ subfolders!

## choose for which country and language to generate the profiles
#IF not debugging, comment the following and uncomment the one after
contexts = c("FR")
#contexts = c("IT","CZ","SW","FR") #national and subnational contexts on which to run the code

#dimensions

job=c("Lawyer","Waiter","Entrepreneur","Teacher","Politician")
issue=c("leftneg","leftpos","rightneg","rightpos")
nostalgia=c("past1","past2","future1","future2")
valence=c("corruption1","corruption2", "honesty1", "honesty2")#,"future1","future2")
food=c("vegan","ethnicrich","meatpoor","meatrich")
animal=c("catpoor","catrich","dogpoor","dogrich")
crowd=c("mixedpeople","whitepeople","mixedelite","whiteelite")


#Select the directory where you want the pictures to be saved
#this directory should contain as many subdirectories as the national contexts are
#and each subdirectory should be named as the contexts (e.g., IT, FR)
output_wd = "C:/Users/GS_237332609/OneDrive - Università degli Studi di Milano-Bicocca/Desktop/immagini_def/" #change if you run on other servers!

# select main directory. This means setting the directory with "immagini esperimenti"
#as the main directory
main_path = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/immagini/"


#Read the excel file containing, for every context, the names of the politicians, 
#and the pieces that compose the info available in their bios (family, job, candidacy statement)
data_name_and_info = read.xlsx("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/Names and infos/Names and infos.xlsx")

#I turn this dataset into a list, more convenient for this task

#A list including dimensions that vary constrained to other dimensions (gender, ethnicity, context)

name_and_info=list()
for(country in contexts)
{
  data_context = data_name_and_info |>
    filter(context==country)
  
  list_black_female_names =   trimws(strsplit(data_context$female_black_name, ",")[[1]])
  list_black_male_names =   trimws(strsplit(data_context$male_black_name, ",")[[1]])
  list_white_male_names = trimws(strsplit(data_context$male_white_name, ",")[[1]])
  list_white_female_names = trimws(strsplit(data_context$female_white_name, ",")[[1]])
  
  list_white_surnames_female = trimws(strsplit(data_context$white_surname_female, ",")[[1]])
  list_white_surnames_male = trimws(strsplit(data_context$white_surname_male, ",")[[1]])
  list_black_surnames_female = trimws(strsplit(data_context$black_surname, ",")[[1]])
  list_black_surnames_male = trimws(strsplit(data_context$black_surname, ",")[[1]])
    
  name_and_info[[country]] = list(Name=list(White=list(Female=list_white_female_names,
                                                     Male=list_white_male_names
                                                     ),
                                          Black=list(Female=list_black_female_names,
                                                     Male=list_black_male_names
                                                     )
                                          ),
                                  Surname=list(White=list(Male=list_white_surnames_male,
                                                          Female=list_white_surnames_female
                                                          ),
                                             Black=list(Male=list_black_surnames_male,
                                                        Female=list_black_surnames_female
                                                        )
                                             ),
                                  Job=list(Female=list(Lawyer=data_context$female_lawyer,
                                                     Waiter=data_context$female_waiter,
                                                     Entrepreneur=data_context$female_entrepreneur,
                                                     Teacher= data_context$female_teacher,
                                                     Politician =data_context$female_politician
                                                     ),
                                           Male=list(Lawyer=data_context$male_lawyer,
                                                   Waiter=data_context$male_waiter,
                                                   Entrepreneur=data_context$male_entrepreneur,
                                                   Teacher= data_context$male_teacher,
                                                   Politician =data_context$male_politician
                                                   )
                                          ),
                                  Candidacy=list(Female=data_context$female_candidacy,
                                                 Male=data_context$male_candidacy
                                                 )
                                )
  
}

 probabilities = c(0.0125,0.0125, 0.0125,0.0125, #probabilities of selecting black profiles, which alphabetically come first in the directory,
                   #we set the probability of generating a black profile overall to 5%
                   0.2375,0.2375,0.2375,0.2375) #probabilities of selecting white profiles


# probabilities = c(0.125,0.125, 0.125,0.125, #probabilities of selecting black profiles, which alphabetically come first in the directory
#                   0.125,0.125, 0.125,0.125) #probabilities of selecting white profiles

# load empty template of an instagram profile 
template = image_read(paste0(main_path,"Template/template_empty1.png"))

for(context in contexts)
{

npics=15000
for(i in 1:npics)
{
 #start building the image name
  namebuild=context
  
  ###########
  #select the propic at random
  #############
  propic_path = paste0(main_path, "Propics/Round")
  
  propic_dirs <- dir_ls(propic_path, type = "directory")
  
  #We have a 95% chance of generating a white profile, a 5% of generating a black one
  
  sel_dir <- sample(propic_dirs, 1, prob=probabilities)
  
  # workaround to solve the problem with accented and encoding. I split the main path
  # and paste it every time and work only on the relevant part of the path
  
  split_strings <- strsplit(sel_dir, "immagini/", fixed = TRUE)[[1]]
  
  propics <- dir_ls(paste0(main_path, split_strings[2]), type = "file")
  
  #Remove the annoying google drive .ini file from the sampling
  propics= propics[grepl("desktop.ini", as.character(propics))==F]
  
  # Choose one file at random from these files
  propic <- sample(propics, 1)
  
  propic <- strsplit(propic, "immagini/", fixed = TRUE)[[1]][2]

  #keep building the name including ethnicity, gender and age which are contained in the file name
  namebuild=gsub(".png", "", paste0(namebuild, "_", gsub("/", "_",strsplit(propic, "Round/", fixed = TRUE)[[1]][2])))
  
  #Read and scale the propic
  propic <- image_read(paste0(main_path,propic)) |>
    image_scale("330")
  
  #############
  #Sample the specific characteristics and build the ic name
  #############
  
  temp_age=ifelse(grepl("35",namebuild)==T, "35", "70") 
  temp_gender=ifelse(grepl("female",namebuild)==T, "Female", "Male")
  temp_ethnicity=ifelse(grepl("black",namebuild)==T, "Black", "White")
  temp_job=sample(job, 1)
  temp_issue=sample(issue, 1)
  temp_nostalgia=sample(nostalgia, 1)
  temp_valence =sample(valence, 1)
  temp_food=sample(food, 1)
  temp_animal=sample(animal, 1)
  temp_crowd=sample(crowd, 1)
  
  temp_name = sample(name_and_info[[context]][["Name"]][[temp_ethnicity]][[temp_gender]], 1)
  temp_surname= sample(name_and_info[[context]][["Surname"]][[temp_ethnicity]][[temp_gender]], 1)
  
  namebuild=paste0(namebuild, "_",
                   temp_name, "_",
                   temp_surname, "_",
                   temp_job, "_",
                   temp_issue, "_",
                   temp_nostalgia, "_",
                   temp_valence, "_",
                   temp_food, "_",
                   temp_animal, "_",
                   temp_crowd, "_",
                   i, #every name is unique even if characteristics were the same 
                   ".png")
  
  #######
  #Put name and bio in the pic, according to gender, ethnicity, and language
  #######
  

    ### annotatate the immage  with bio, acording to gender
    
    #jobs, according to gender
    temp_job_context = name_and_info[[context]][["Job"]][[temp_gender]][[temp_job]]
    
    #the candidacy announcement, according to gender
    
    temp_candidacy_announcement = name_and_info[[context]][["Candidacy"]][[temp_gender]]
    
    #annotating bio
    
    temp_bio = paste0("\n", 
                      temp_job_context,
                      "\n",
                      temp_candidacy_announcement
                      )
                         

  #######
  #Issues
  #######
  
  issue_pic = image_read(paste0(main_path, "Issue/", context, "/", temp_issue, ".png")) |>
      image_scale("480")
  
  ##### 
  #food
  #####

  food_pic = image_read(paste0(main_path, "Food/", temp_food, ".png")) |>
      image_scale("480")

  ##### 
  #nostalgia
  #####
  
  nostalgia_pic = image_read(paste0(main_path,"Nostalgia/", context, "/", temp_nostalgia, ".png")) |>
      image_scale("480")
  
  ##### 
  #crowd
  #####
  
  crowd_pic = image_read(paste0(main_path, "Crowd/", temp_crowd, ".png")) |>
      image_scale("480")
  
  ##### 
  #valence
  #####
    
  valence_pic = image_read(paste0(main_path, "Valence/", context, "/", temp_valence, ".png")) |>
      image_scale("480")
  
  ##### 
  #animal
  #####
  
  animal_pic = image_read(paste0(main_path, "Animal/", temp_animal, ".png")) |>
      image_scale("480")

  ##### 
  # Compose the finale image
  #####
  
  pic = template |> 
    image_annotate(paste(temp_name, temp_surname),
                   size = 60,
                   color = "Black",
                   font = "helvetica",
                   location = "+50+380"
                   ) |>
    image_annotate(temp_bio,
                   size = 50,
                   font= "helvetica",
                   color = "#392C2B",
                   location = "+50+425"
                   ) |>
    image_composite(propic, offset = "+60+30") |>
    image_composite(issue_pic, offset = "+0+1059") |> 
    image_composite(food_pic, offset = "+489+1059") |>
    image_composite(nostalgia_pic, offset = "+978+1059") |>
    image_composite(crowd_pic, offset = "+0+1548") |>
    image_composite(valence_pic, offset = "+489+1548") |>
    image_composite(animal_pic, offset = "+978+1548") |>
      image_scale("x900")
  
  image_write(pic, paste0(output_wd, context, "/", namebuild))
  
  print(paste0(i, " ",context, " ", Sys.time()))
}

}

