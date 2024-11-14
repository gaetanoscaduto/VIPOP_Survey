library(rio)
library(dplyr)

#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context

#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"

# dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
# gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"


data = import(paste0(dataset_rep, "demo_data_", context, ".sav"))

if(context !="POOL")
{
  data$country = context
}

table(data$vce_validation, useNA = "always")

#since the variable vce_validation has been added only after 
# the pilot, we use its missing category as the flag for whether the interview
# belongs to the pilot or not

data$pilot = ifelse(is.na(data$vce_validation), 1, 0)

table(data$pilot)

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

if(context == "IT")
{
  data <- data |>
    mutate(education = case_when(
      education == 1 ~ "Scuola primaria",
      education == 2 ~ "Scuola media",
      education == 3 ~ "Diploma di scuola superiore",
      education == 4 ~ "Laurea triennale",
      education == 5 ~ "Laurea magistrale o a ciclo unico (4-5 anni)",
      education == 6 ~ "Scuola post-laurea/dottorato",
      TRUE ~ as.character(education)  # Keeps any values not in the list as they are
    ))
  
}


if(context == "FR")
{
  
  data <- data |>
    mutate(education = case_when(
      education == 1 ~ "École élémentaire",
      education == 2 ~ "Collège",
      education == 3 ~ "Lycée",
      education == 4 ~ "Enseignement supérieur, Université",
      education == 5 ~ "Licence, maîtrise",
      education == 6 ~ "Master",
      education == 7 ~ "Doctorat",
      TRUE ~ as.character(education)  # Keeps any values not in the list as they are
    ))
  
}

if(context == "CZ")
{
  
  data <- data |>
    mutate(education = case_when(
      education == 1 ~ "Základní vzdělání",
      education == 2 ~ "Střední vzdělání",
      education == 3 ~ "Bakalářský stupeň",
      education == 4 ~ "Magisterský stupeň",
      education == 5 ~ "Doktorský stupeň",
      TRUE ~ as.character(education)  # Keeps any values not in the list as they are
    ))
}


if(context == "SW")
{
  
  data <- data |>
    mutate(education = case_when(
      education == 1 ~ "Grundskola eller motsvarande, kortare än 9 år",
      education == 2 ~ "Grundskola eller motsvarande, 9 år eller längre",
      education == 3 ~ "Gymnasium eller motsvarande, kortare än 3 år",
      education == 4 ~ "Gymnasium eller motsvarande, 3 år eller längre",
      education == 5 ~ "Eftergymnasial utbildning, ej högskola/universitet, kortare än 3 år",
      education == 6 ~ "Eftergymnasial utbildning, ej högskola/universitet, 3 år eller längre",
      education == 7 ~ "Högskola/universitet, kortare än 3 år",
      education == 8 ~ "Högskola/universitet, 3 år eller längre men kortare än 4 år",
      education == 9 ~ "Högskola/universitet, 4 år eller längre",
      education == 10 ~ "Forskarutbildninge",
      education == 11 ~ "Inga av dessa",
      TRUE ~ as.character(education)  # Keeps any values not in the list as they are
    ))
  
}


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

if(context == "IT")
{
  data <- data |>
    mutate(region = case_when(
      region == "1" ~ "Abruzzo",
      region == "2" ~ "Basilicata",
      region == "3" ~ "Calabria",
      region == "4" ~ "Campania",
      region == "5" ~ "Emilia-Romagna",
      region == "6" ~ "Friuli Venezia Giulia",
      region == "7" ~ "Lazio",
      region == "8" ~ "Liguria",
      region == "9" ~ "Lombardia",
      region == "10" ~ "Marche",
      region == "11" ~ "Molise",
      region == "12" ~ "Piemonte",
      region == "13" ~ "Puglia",
      region == "14" ~ "Sardegna",
      region == "15" ~ "Sicilia",
      region == "16" ~ "Toscana",
      region == "17" ~ "Trentino-Alto Adige",
      region == "18" ~ "Umbria",
      region == "19" ~ "Valle d'Aosta",
      region == "20" ~ "Veneto",
      TRUE ~ as.character(region)  # Keeps any values not in the list as they are
    ))
  
}

if(context == "FR")
{
  data <- data |>
    mutate(region = case_when(
      region == "1" ~ "Auvergne-Rhône-Alpes",
      region == "2" ~ "Bourgogne-Franche-Comté",
      region == "3" ~ "Bretagne",
      region == "4" ~ "Centre-Val de Loire",
      region == "5" ~ "Corse",
      region == "6" ~ "Grand Est",
      region == "7" ~ "Hauts-de-France",
      region == "8" ~ "Île-de-France",
      region == "9" ~ "Normandie",
      region == "10" ~ "Nouvelle-Aquitaine",
      region == "11" ~ "Occitanie",
      region == "12" ~ "Pays de la Loire",
      region == "13" ~ "Provence-Alpes-Côte d'Azur",
      TRUE ~ as.character(region)  # Keeps any values not in the list as they are
    ))
  
}

if(context == "CZ")
{
  data <- data |>
    mutate(region = case_when(
      region == "1" ~ "Středočeský kraj",
      region == "2" ~ "Královéhradecký kraj",
      region == "3" ~ "Karlovarský kraj",
      region == "4" ~ "Liberecký kraj",
      region == "5" ~ "Moravskoslezský kraj",
      region == "6" ~ "Olomoucký kraj",
      region == "7" ~ "Pardubický kraj",
      region == "8" ~ "Plzeňský kraj",
      region == "9" ~ "Praha",
      region == "10" ~ "Jihočeský kraj",
      region == "11" ~ "Jihomoravský kraj",
      region == "12" ~ "Ústecký kraj",
      region == "13" ~ "Kraj Vysočina",
      region == "14" ~ "Zlínský kraj",
      TRUE ~ as.character(region)  # Keeps any values not in the list as they are
    ))
  
}

if(context == "SW")
{
  data <- data |>
    mutate(region = case_when(
      region == "1" ~ "Stockholms",
      region == "2" ~ "Uppsala",
      region == "3" ~ "Södermanlands",
      region == "4" ~ "Östergötlands",
      region == "5" ~ "Jönköpings",
      region == "6" ~ "Kronobergs",
      region == "7" ~ "Kalmar",
      region == "8" ~ "Gotlands",
      region == "9" ~ "Blekinge",
      region == "10" ~ "Skåne",
      region == "11" ~ "Hallands",
      region == "12" ~ "Västra Götalands",
      region == "13" ~ "Värmlands",
      region == "14" ~ "Örebro",
      region == "15" ~ "Västmanlands",
      region == "16" ~ "Dalarnas",
      region == "17" ~ "Gävleborgs",
      region == "18" ~ "Västernorrlands",
      region == "19" ~ "Jämtlands",
      region == "20" ~ "Västerbottens",
      region == "21" ~ "Norrbottens",
      TRUE ~ as.character(region)  # Keeps any values not in the list as they are
    ))
  
}


# - region_feel (che è country-dependent, e quindi vorremmo venga lasciata la modalità di risposta senza ricodifica)
# 
if(context == "IT")
{
  data <- data |>
    mutate(region_feel = case_when(
      region_feel == 1 ~ "northern italy",
      region_feel == 2 ~ "central italy",
      region_feel == 3 ~ "southern italy",
      TRUE ~ as.character(region_feel)  # Keeps any values not in the list as they are
    )
    )
  
  cpd_regionfeel_names = names(data)[grepl("C1",names(data)) & grepl("A4_", names(data))]
  for(var in cpd_regionfeel_names)
  {
    data = data |>
      mutate(!!var := case_when(
        !!sym(var) == "1" ~ "northern italy",
        !!sym(var) == "2" ~ "central italy",
        !!sym(var) == "3" ~ "southern italy",
        is.na(!!sym(var)) ~ NA
        )
      )
  }

}


if(context == "FR")
{

  data <- data |>
    mutate(region_feel = case_when(
      region_feel == 1 ~ "paris",
      region_feel == 2 ~ "noparis",
      TRUE ~ as.character(region_feel)  # Keeps any values not in the list as they are
    )
    )
  
  cpd_regionfeel_names = names(data)[grepl("C1",names(data)) & grepl("A4_", names(data))]
  for(var in cpd_regionfeel_names)
  {
    data = data |>
      mutate(!!var := case_when(
        !!sym(var) == "1" ~ "paris",
        !!sym(var) == "2" ~ "noparis",
        is.na(!!sym(var)) ~ NA
      )
      )
  }

}

if(context == "CZ")
{

  data <- data |>
    mutate(region_feel = case_when(
      region_feel == 1 ~ "cechia",
      region_feel == 2 ~ "moravia",
      region_feel == 3 ~ "prague",
      TRUE ~ as.character(region_feel)  # Keeps any values not in the list as they are
    ))
  
  cpd_regionfeel_names = names(data)[grepl("C1",names(data)) & grepl("A4_", names(data))]
  for(var in cpd_regionfeel_names)
  {
    data = data |>
      mutate(!!var := case_when(
        !!sym(var) == "1" ~ "cechia",
        !!sym(var) == "2" ~ "moravia",
        !!sym(var) == "3" ~ "prague",
        is.na(!!sym(var)) ~ NA
      )
      )
  }
}


if(context == "SW")
{

  data <- data |>
    mutate(region_feel = case_when(
      region_feel == 1 ~ "gotland",
      region_feel == 2 ~ "svealand",
      region_feel == 3 ~ "norrland",
      TRUE ~ as.character(region_feel)  # Keeps any values not in the list as they are
    ))
  
  cpd_regionfeel_names = names(data)[grepl("C1",names(data)) & grepl("A4_", names(data))]
  for(var in cpd_regionfeel_names)
  {
    data = data |>
      mutate(!!var := case_when(
        !!sym(var) == "1" ~ "gotland",
        !!sym(var) == "2" ~ "svealand",
        !!sym(var) == "3" ~ "norrland",
        is.na(!!sym(var)) ~ NA
      )
      )
  }

}


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

# - socialposition

table(data$socialposition)
data = data |>
  mutate(socialposition = recode(socialposition,
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



#table(data$ideology)

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

#table(data$ideology)

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

### cpd idelogy

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


# - interest/exposure variables (recodifica levels nomi abbreviati e aggiunta dicotomiche)

#sns_use - recode
# table(data$sns_use)
# 
# 
# data = data |>
#   mutate(sns_use_rec = recode(sns_use,
#                               "1" = "nev_hardev",
#                               "2" = "lessthan10",
#                               "3" = "between10and30",
#                               "4" = "between30and60",
#                               "5" = "morethan60"
#                               ) 
#          )
# 
# data = data |>
#   filter(data$pilot == 1) |>
#   mutate(sns_use_rec = recode(sns_use,
#                               "1" = "nev_hardev",
#                               "2" = "at_least_once_a_month",
#                               "3" = "at_least_once_a_week",
#                               "4" = "at_least_once_a_day",
#                               "5" = "more_than_once_per_day",
#   ) 
#   )


#table(data$sns_use_rec)

#sns_use - dicotomica

data = data |>
  mutate(sns_use_dummy = recode(sns_use,
                                "1" = "rarely",
                                "2" = "rarely",
                                "3" = "rarely",
                                "4" = "often",
                                "5" = "often",
                                ) 
         )


#table(data$sns_use_dummy)
##

#interest - recode

#table(data$interest)

data = data |>
  mutate(interest_rec = recode(interest,
                               "1" = "not_all",
                               "2" = "little",
                               "3" = "average",
                               "4" = "quite",
                               "5" = "very",
                               ) 
         )

#table(data$interest_rec)

#interest - dicotomica
data = data |>
  mutate(interest_dummy = recode(interest,
                                 "1" = "no_interest",
                                 "2" = "no_interest",
                                 "3" = "no_interest",
                                 "4" = "yes_interest",
                                 "5" = "yes_interest",
                                 ) 
         )

#exposure - recode

#table(data$exposure)

data = data |>
  mutate(exposure_rec = recode(exposure,
                               "1" = "never",
                               "2" = "rarely",
                               "3" = "less10min",
                               "4" = "between10and30",
                               "5" = "between30and60",
                               "6" = "between60and120",
                               "7" = "above120")
         
  )

#table(data$exposure_rec)

#exposure - dicotomica

data = data |>
  mutate(exposure_dummy = recode(exposure,
                                 "1" = "less10min",
                                 "2" = "less10min",
                                 "3" = "less10min",
                                 "4" = "more10min",
                                 "5" = "more10min",
                                 "6" = "more10min",
                                 "7" = "more10min")
         )
##


#ricodificare le variabili del classic conjoint design

conjattr_full = c("gender",
                  "age",
                  "religion",
                  "citysize",
                  "job",
                  "consc",
                  "ope",
                  "neu",
                  "restaurant",
                  "transport",
                  "animal")

#conjoint classic gender

ccd_varnames=list(gender=c(""),
                  age=c(""),
                  religion=c(""),
                  citysize=c(""),
                  job=c(""),
                  consc=c(""),
                  ope=c(""),
                  neu=c(""),
                  restaurant=c(""),
                  transport=c(""),
                  animal=c("")
                  )


# i put in ccd_varnames[attribute] the names of the variables that in the dataset
#correspond to a that conjoint attribute

for(attribute in conjattr_full)
{
  A_name = paste0("A", which(conjattr_full==attribute), "_")
  ccd_varnames[[attribute]]= names(data)[grepl("C3",names(data)) & grepl(A_name, names(data))]
}


# Define the list with label mappings for each variable
label_list <- list(gender=c("Female", "Male", "Non-binary"),
                   age=c("25 years old","45 years old","65 years old"),
                   religion=c("Practitioner","Non practitioner", "Non believer"),
                   citysize=c("Big", "Small", "Medium"), #ricorda di correggere l'ordine di sti factor
                   job=c("Entrepreneur", "Teacher", "Waiter", "Lawyer"),
                   consc=c("Reliable", "Disorganized"),
                   ope=c("Open", "Rigid"),
                   neu=c("Calm", "Anxious"),
                   restaurant=c("Traditional", "Vegan","Asian","Steakhouse"),
                   transport=c("Bicycle","Public Transport","SUV"),
                   animal=c("Large dog","Small dog","Cat", "No pets")
)


# Function to recode old variable based on label list
recode_variable <- function(old_var, label_var) {
  recoded_var <- sapply(old_var, function(x)  {
    if (is.na(x)) {
      return(NA)  # Return NA if the value is missing
    } else if (is.numeric(x) && x <= length(label_var) && x > 0) {
      return(label_var[x])
    } else {
      return(NA)  # Return NA for out-of-range or invalid values
    }
  })
  return(recoded_var)
}


for(attribute in conjattr_full)
{
  for(var_to_recode in ccd_varnames[[attribute]])
    
    data[, var_to_recode] = recode_variable( data[, var_to_recode], label_list[[attribute]])
} 


# - attention_check_1 (dovrebbe essere categoriale con etichette indicate nel master, non numerica)


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

#table(data$attention_check1)

# - votechoice (che è country-dependent, e quindi vorremmo venga lasciata la modalità di risposta senza ricodifica)

if(context=="IT")
{
  data <- data |>
    mutate(votechoice = case_when(
      votechoice == 1 ~ "Fratelli d'Italia",
      votechoice == 2 ~ "Partito Democratico",
      votechoice == 3 ~ "Movimento 5 Stelle",
      votechoice == 4 ~ "Lega",
      votechoice == 5 ~ "Forza Italia - Noi Moderati",
      votechoice == 6 ~ "Azione - Siamo europei",
      votechoice == 7 ~ "Stati Uniti d'Europa (Italia Viva - +Europa, altre liste)",
      votechoice == 8 ~ "Alleanza Verde-Sinistra",
      votechoice == 9 ~ "Altra lista",
      votechoice == 10 ~ "Non ho votato/Mi sono astenuto",
      votechoice == 11 ~ "Scheda bianca/nulla",
      votechoice == 12 ~ "Preferisco non rispondere",
      TRUE ~ as.character(votechoice)  # Keeps any values not in the list as they are
    ))
  
}
if(context=="FR")
{
  data <- data |>
    mutate(votechoice = case_when(
      votechoice == 1 ~ "RN - Rassemblement national",
      votechoice == 2 ~ "Coalition Besoin d'Europe (Renaissance, Modem, Horizons, Parti Radical, Union des démocrates et indépendants)",
      votechoice == 3 ~ "Coalition Réveiller l'Europe (Parti socialiste, Place publique)",
      votechoice == 4 ~ "LFI - La France Insoumise",
      votechoice == 5 ~ "LR - Les Républicains",
      votechoice == 6 ~ "LE - EELV - Les Écologistes - Europe Ecologie Les Verts",
      votechoice == 7 ~ "Coalition La France fière (Reconquête!, Centre national des indépendants et paysans)",
      votechoice == 8 ~ "Autre",
      votechoice == 9 ~ "Je n'ai pas voté/je me suis abstenu(e)",
      votechoice == 10 ~ "Bulletin blanc/nul",
      votechoice == 11 ~ "Je préfère ne pas répondre",
      TRUE ~ as.character(votechoice)  # Keeps any values not in the list as they are
    ))
}
if(context=="CZ")
{
  data <- data |>
    mutate(votechoice = case_when(
      votechoice == 1 ~ "ANO 2011",
      votechoice == 2 ~ "SPOLU (Občanská demokratická strana, Křesťanská a demokratická unie – Československá strana lidová, Tradice Odpovědnost Prosperita 09)",
      votechoice == 3 ~ "Přísaha a Motoristé (Přísaha – občanské hnutí Roberta Šlachty, AUTO - Motoristé sobě)",
      votechoice == 4 ~ "Stačilo! - Koalice Stačilo! (Komunistická strana Čech a Moravy, Česká strana národně sociální, Spojení demokraté – Sdružení nezávislých)",
      votechoice == 5 ~ "STAN - Starostové a osobnosti pro Evropu",
      votechoice == 6 ~ "Piráti - Česká pirátská strana",
      votechoice == 7 ~ "SPD a Trikolora (Svoboda a přímá demokracie, Trikolóra hnutí občanů)",
      votechoice == 8 ~ "PRO - Právo Respekt Odbornost Jindřicha Rajchla",
      votechoice == 9 ~ "SOCDEM - Sociální demokracie",
      votechoice == 10 ~ "Svobodní - Strana svobodných občanů",
      votechoice == 11 ~ "Zelení - Strana zelených",
      votechoice == 12 ~ "Jinou stranu",
      votechoice == 13 ~ "Nehlasoval jsem/zdržel jsem se hlasování",
      votechoice == 14 ~ "Prázdný/neplatný hlasovací lístek",
      votechoice == 15 ~ "Raději jsem neodpověděl(a)",
      TRUE ~ as.character(votechoice)  # Keeps any values not in the list as they are
    ))
  
}
if(context=="SW")
{
  data <- data |>
    mutate(votechoice = case_when(
      votechoice == 1 ~ "S - Socialdemokraterna",
      votechoice == 2 ~ "M - Moderaterna",
      votechoice == 3 ~ "MP - Miljöpartiet de gröna",
      votechoice == 4 ~ "SD - Sverigedemokraterna",
      votechoice == 5 ~ "V - Vänsterpartiet",
      votechoice == 6 ~ "C - Centerpartiet",
      votechoice == 7 ~ "KD - Kristdemokraterna",
      votechoice == 8 ~ "L - Liberalerna",
      votechoice == 9 ~ "Övriga parter",
      votechoice == 10 ~ "Jag röstade inte/avstod från att rösta",
      votechoice == 11 ~ "Blank/Null röstsedel",
      votechoice == 12 ~ "Jag föredrar att inte svara ",
      TRUE ~ as.character(votechoice)  # Keeps any values not in the list as they are
    ))
  
}



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

#table(data$attention_check2)

### changing variables names

#tipi 

change=paste0(rep("tipi_", 10), c("ext","agr_r","con","neu","ope","ext_r","agr","con_r","neu_r","ope_r"))

change

which(grepl("tipi_",names(data)))

data <- data %>%
  rename_with(~ change, starts_with("tipi_") & !ends_with("REC"))

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
  rename_with(~ change, starts_with("populism_") & (!ends_with("REC") & !ends_with("open")))

names(data)[which(grepl("populism",names(data)))]


# I create more usable time variables
data$start_r <- as.POSIXct(data$start_, format = "%Y-%m-%d %H:%M:%S")
data$end_r <- as.POSIXct(data$end_, format = "%Y-%m-%d %H:%M:%S")

# Calculate the time difference in minutes
data$time_diff_mins <- as.numeric(difftime(data$end_, data$start_, units = "mins"))

################################
#Cleaning  the data!
###############################
#clean = T
if(clean == T)
{
  #Remove those that did attention_check 2 wrong
  data = data[data$attention_check2 == "eucomm", ]
  
  #Remove speeders
  data = data[data$time_diff_mins>5.2, ]
  
  #Remove laggards
  data = data[data$time_diff_mins<35, ]
  
}

export(data, paste0(dataset_rep, "data_recoded_", context, ".RDS"))
