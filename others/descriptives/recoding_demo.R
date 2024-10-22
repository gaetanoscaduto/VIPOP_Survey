library(rio)
library(dplyr)

#context = "IT"
#context = "FR"
context = "CZ"
#context = "SW"

data = import(paste0("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/demo_data_", context, ".sav"))

#View(data)
 
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
 
 # gender cpd
 
 cpd_gender_names = names(data)[grepl("C1",names(data)) & grepl("A1_", names(data))]
 for(var in cpd_gender_names)
   {
   data = data |>
     mutate(!!var := case_when(
              !!sym(var) == "1" ~ "female",
              !!sym(var) == "2" ~ "male",
              is.na(!!sym(var)) ~ NA
            ) 
     )
   
 } 
 
 
 #Recode age. In the dataset, in the column age we have the difference betwen the year of birth and 1924. I want
 #to convert this into the proper age
 
 data$age = 2024-(as.numeric(data$age)+1924)
 
 # age group
 
 
 data = data |>
   mutate(AGE_GROUP, 
          AGE_GROUP = case_when(
            AGE_GROUP == "1" ~ "under30",
            AGE_GROUP == "2" ~ "between30and59",
            AGE_GROUP == "3" ~ "over60",
            is.na(AGE_GROUP) ~ NA
          ) 
   )
 
 # age cpd
 
 
 cpd_age_names = names(data)[grepl("C1",names(data)) & grepl("A2_", names(data))]
 for(var in cpd_age_names)
 {
   data = data |>
     mutate(!!var := case_when(
       !!sym(var) == "1" ~ "under30",
       !!sym(var) == "2" ~ "between30and59",
       !!sym(var) == "3" ~ "over60",
       is.na(!!sym(var)) ~ NA
     ) 
     )
   
 } 
# - education (che è country-dependent, e quindi vorremmo venga lasciata la modalità di risposta senza ricodifica)
# TODo
 
 data = data |>
   mutate(EDU_LEVEL, 
          EDU_LEVEL = case_when(
            EDU_LEVEL == "1" ~ "nocollege",
            EDU_LEVEL == "2" ~ "college",
            is.na(EDU_LEVEL) ~ NA
          ) 
   )
 
# education cpd
 
 
 cpd_educ_names = names(data)[grepl("C1",names(data)) & grepl("A3_", names(data))]
 for(var in cpd_educ_names)
 {
   data = data |>
     mutate(!!var := case_when(
       !!sym(var) == "1" ~ "nocollege",
       !!sym(var) == "2" ~ "college",
       is.na(!!sym(var)) ~ NA
     ) 
     )
   
 } 
 
# - region (che è country-dependent, e quindi vorremmo venga lasciata la modalità di risposta senza ricodifica)

# todo
 
 
 data = data |>
   mutate(region, 
          macroregion = case_when(
            region %in% c("Lombardia", "Piemonte", "Val d'Aosta", "Liguria") ~ "North-West",
            region %in% c("Veneto", "Trentino-Alto Adige", "Emilia-Romagna", "Friuli-Venezia Giulia") ~ "North-East",
            region %in% c("Lazio", "Marche", "Toscana", "Umbria") ~ "Center",
            region %in% c("Abruzzo", "Basilicata", "Molise", "Calabria", "Campania", "Puglia", "Sardegna", "Sicilia") ~ "South",
            is.na(region) ~ NA
          ) 
   )

 table(data$macroregion)
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

## cpd diet

cpd_diet_names = names(data)[grepl("C1",names(data)) & grepl("A7_", names(data))]
for(var in cpd_diet_names)
{
  data = data |>
    mutate(!!var := case_when(
      !!sym(var) == "1" ~ "omnivore",
      !!sym(var) == "2" ~ "vegetarian",
      !!sym(var) == "3" ~ "vegan",
      is.na(!!sym(var)) ~ NA
    ) 
    )
  
} 

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

# cpd animal


cpd_animal_names = names(data)[grepl("C1",names(data)) & grepl("A8_", names(data))]
for(var in cpd_animal_names)
{
  data = data |>
    mutate(!!var := case_when(
      !!sym(var) == "1" ~ "cat",
      !!sym(var) == "2" ~ "dog",
      !!sym(var) == "3" ~ "none",
      is.na(!!sym(var)) ~ NA
    ) 
    )
  
} 

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

#holiday cpd


cpd_holiday_names = names(data)[grepl("C1",names(data)) & grepl("A9_", names(data))]
for(var in cpd_holiday_names)
{
  data = data |>
    mutate(!!var := case_when(
      !!sym(var) == "1" ~ "outdoor",
      !!sym(var) == "2" ~ "city",
      !!sym(var) == "3" ~ "relax",
      is.na(!!sym(var)) ~ NA
    ) 
    )
  
} 

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

## tipi con for cpd (match)


data = data |>
  mutate(TIPI_CON_REC, 
         TIPI_CON_REC = case_when(
           TIPI_CON_REC == "1" ~ "con_disagree",
           TIPI_CON_REC == "2" ~ "con_neither",
           TIPI_CON_REC == "3" ~ "con_agree",
           is.na(TIPI_CON_REC) ~ NA
         ) 
  )


cpd_con_names = names(data)[grepl("C1",names(data)) & grepl("A5_", names(data))]
for(var in cpd_con_names)
{
  data = data |>
    mutate(!!var := case_when(
      !!sym(var) == "1" ~ "con_disagree",
      !!sym(var) == "2" ~ "con_neither",
      !!sym(var) == "3" ~ "con_agree",
      is.na(!!sym(var)) ~ NA
    ) 
  )
  
} 

### tipi ope for cpd


data = data |>
  mutate(TIPI_OPE_REC, 
         TIPI_OPE_REC = case_when(
           TIPI_OPE_REC == "1" ~ "ope_disagree",
           TIPI_OPE_REC == "2" ~ "ope_neither",
           TIPI_OPE_REC == "3" ~ "ope_agree",
           is.na(TIPI_OPE_REC) ~ NA
         ) 
  )


cpd_ope_names = names(data)[grepl("C1",names(data)) & grepl("A6_", names(data))]
for(var in cpd_ope_names)
{
  data = data |>
    mutate(!!var := case_when(
      !!sym(var) == "1" ~ "ope_disagree",
      !!sym(var) == "2" ~ "ope_neither",
      !!sym(var) == "3" ~ "ope_agree",
      is.na(!!sym(var)) ~ NA
    ) 
    )
  
} 



table(data$ideology)
data = data |>
  mutate(ideology = recode(ideology,
                                 "1" = "0",
                                 "2" = "1",
                                 "3" = "2",
                                 "4" = "3",
                                 "5" = "4",
                                 "6" = "5",
                                 "7" = "6",
                                 "8" = "7",
                                 "9" = "8",
                                 "10" = "9",
                                 "11" = "10",
                                 "13" = "notplaced",
                                 .missing = "NA",
                                 .default = "default"
  ) 
  )
table(data$ideology)

# ideology for cpd match


data = data |>
  mutate(IDEOLOGY_REC, 
         IDEOLOGY_REC = case_when(
           IDEOLOGY_REC == "1" ~ "right",
           IDEOLOGY_REC == "2" ~ "left",
           IDEOLOGY_REC == "3" ~ "center",
           IDEOLOGY_REC == "4" ~ "notplaced",
           is.na(IDEOLOGY_REC) ~ NA
         ) 
  )

### cpd


cpd_ideo_names = names(data)[grepl("C1",names(data)) & grepl("A10_", names(data))]
for(var in cpd_ideo_names)
{
  data = data |>
    mutate(!!var := case_when(
      !!sym(var) == "1" ~ "right",
      !!sym(var) == "2" ~ "left",
      !!sym(var) == "3" ~ "center",
      !!sym(var) == "4" ~ "notplaced",
      is.na(!!sym(var)) ~ NA
    ) 
    )
  
} 




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
change=paste0(rep("tipi_", 10), c("ext","agr_r","con","neu","ope","ext_r","agr","con_r","neu_r","ope_r"))

change

which(grepl("tipi_",names(data)))

data <- data %>%
  rename_with(~change, starts_with("tipi_") & !ends_with("REC"))

names(data)[15:24]

#nethet
change=paste0(rep("nethet_", 5), c("right","left","center","notplaced","dontknow"))

change

which(grepl("nethet",names(data)))

data <- data %>%
  rename_with(~ change, starts_with("nethet_") & !ends_with("REC"))

names(data)[which(grepl("nethet",names(data)))]

#ft

change=paste0(rep("ft_", 8), c("right","left","center","notplaced","populists","cr","cl","prr"))

change

which(grepl("ft_",names(data)))
data <- data %>%
  rename_with(~ change, starts_with("ft_") & !ends_with("REC"))

names(data)[which(grepl("ft_",names(data)))]


#trust

change=paste0(rep("trust_", 3), c("party","eu","exp"))

change

which(grepl("trust",names(data)))

data <- data %>%
  rename_with(~ change, starts_with("trust_") & !ends_with("REC"))

names(data)[which(grepl("trust",names(data)))]


#populism

change=paste0(rep("populism_", 3), c("will","lobby","struggle", "science", 
                                     "ordinary", "corrupt", "compromise",
                                     "expert", "policies"))

change

which(grepl("populism",names(data)))

data <- data %>%
  rename_with(~ change, starts_with("populism_") & !ends_with("open"))

names(data)[which(grepl("populism",names(data)))]

export(data, paste0("data_recoded_", context, ".RDS"))
