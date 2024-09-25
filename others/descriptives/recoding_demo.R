library(rio)
library(dplyr)

data = import("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/demo_data.sav")


 View(data)
 
# - gender (dovrebbe essere categoriale con etichette indicate nel master, non numerica)

 data = data |>
   mutate(gender, 
          gender = case_when(
            gender == "1" ~ "female",
            gender == "2" ~ "male",
            gender == "3" ~ "nonbinary",
            gender == "4" ~ "notsay",
            is.na(gender) ~ NA
          ) 
   )
 
# - education (che è country-dependent, e quindi vorremmo venga lasciata la modalità di risposta senza ricodifica)
# TODo
 
# - region (che è country-dependent, e quindi vorremmo venga lasciata la modalità di risposta senza ricodifica)

# todo
 
# - region_feel (che è country-dependent, e quindi vorremmo venga lasciata la modalità di risposta senza ricodifica)

 #todo
 
# - citysize (dovrebbe essere categoriale con etichette indicate nel master, non numerica)
# 
 
 data = data |>
   mutate(citysize, 
          citysize = case_when(
            citysize == "1" ~ "village",
            citysize == "2" ~ "medium",
            citysize == "3" ~ "large",
            is.na(citysize) ~ NA
          ) 
   )
 
 # - Le variabili "tipi_" hanno nomi diversi rispetto a quanto indicato nel master document (sono "tipi_1", "tipi_2", ..., "tipi_10" invece di "tipi_ext", "tipi_agr_r",..., "tipi_ope_r"
 # 
 


# - diet (dovrebbe essere categoriale con etichette indicate nel master, non numerica)
data = data |>
  mutate(diet = recode(diet,
           "1" = "omnivore",
           "2" = "vegetarian",
           "3" = "vegan",
           .missing = "NA",
           .default = "default"
         ) 
  )


# - animal (dovrebbe essere categoriale con etichette indicate nel master, non numerica)
# 

data = data |>
  mutate(animal = recode(animal,
                       "1" = "cat",
                       "2" = "dog",
                       "3" = "none",
                       .missing = "NA",
                       .default = "default"
  ) 
)
# - holiday (dovrebbe essere categoriale con etichette indicate nel master, non numerica)
#

data = data |>
  mutate(holiday = recode(holiday,
                         "1" = "outdoor",
                         "2" = "city",
                         "3" = "relax",
                         .missing = "NA",
                         .default = "default"
  ) 
)

# - ideology (dovrebbe essere categoriale in quanto ci sono anche modalità non numeriche)

table(data$socialposition)
data = data |>
  mutate(socialposition = recode(socialposition,
                          "0" = "0",
                          "1" = "1",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "5",
                          "6" = "6",
                          "7" = "7",
                          "8" = "8",
                          "9" = "9",
                          "10" = "10",
                          
                          .missing = "NA",
                          .default = "default"
  ) 
)
table(data$socialposition)

table(data$ideology)
data = data |>
  mutate(ideology = recode(ideology,
                                 "0" = "0",
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5",
                                 "6" = "6",
                                 "7" = "7",
                                 "8" = "8",
                                 "9" = "9",
                                 "10" = "10",
                                 "13" = "notplaced",
                                 .missing = "NA",
                                 .default = "default"
  ) 
  )
table(data$ideology)


table(data$sns_use)

# - attention_check_1 (dovrebbe essere categoriale con etichette indicate nel master, non numerica)
# 

table(data$attention_check1)
data = data |>
  mutate(attention_check1 = recode(attention_check1,
                          "1" = "gov",
                          "2" = "par",
                          "3" = "jud",
                          "4" = "med",
                          .missing = "NA",
                          .default = "default"
  ) 
  )

table(data$attention_check1)

# - votechoice (che è country-dependent, e quindi vorremmo venga lasciata la modalità di risposta senza ricodifica)
# 
#todo

# - le variabili "nethet_", come per le "tipi_", sono nominate con numeri invece che con "nethet_right", "nethet_left", "nethet_center", etc. come indicato nel master
#

# - discorso analogo per le variabili "ft_", "trust_", "populism_"
# 
# - attention_check_2 (dovrebbe essere categoriale con etichette indicate nel master, non numerica)
# 


table(data$attention_check2)
data = data |>
  mutate(attention_check2 = recode(attention_check2,
                                   "1" = "euparl",
                                   "2" = "eucomm",
                                   "3" = "russiapres",
                                   "4" = "uspres",
                                   .missing = "NA",
                                   .default = "default"
  ) 
  )

table(data$attention_check2)



### changing variables names

#tipi 
change=paste0(rep("tipi_", 10), c("ext","agr_r","con","neu","ope","ext_r","agr_r","con_r","neu_r","ope_r"))

change

which(grepl("tipi",names(data)))
names(data)[15:24] = change
names(data)[15:24]

#nethet
change=paste0(rep("nethet_", 5), c("right","left","center","notplaced","dontknow"))

change

which(grepl("nethet",names(data)))
names(data)[which(grepl("nethet",names(data)))] = change
names(data)[which(grepl("nethet",names(data)))]

#ft

change=paste0(rep("ft_", 8), c("right","left","center","notplaced","populists","cr","cl","prr"))

change

which(grepl("ft",names(data)))
names(data)[which(grepl("ft",names(data)))] = change
names(data)[which(grepl("ft",names(data)))]


#trust

change=paste0(rep("trust_", 3), c("party","eu","exp"))

change

which(grepl("trust",names(data)))
names(data)[which(grepl("trust",names(data)))] = change
names(data)[which(grepl("trust",names(data)))]


#populism

change=paste0(rep("populism_", 3), c("will","lobby","struggle", "science", 
                                     "ordinary", "corrupt", "compromise",
                                     "expert", "policies"))

change

which(grepl("populism",names(data)))
names(data)[which(grepl("populism",names(data)))] = change
names(data)[which(grepl("populism",names(data)))]
