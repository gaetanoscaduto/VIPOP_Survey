#### Script to transform the dataset in the conjoint dataset

# In this script I take the dataset in the form given by the survey compay (1 row equals 1 respondent)
# and I transform it in order to have a dataset that can be analyzed by the conjoint estimators


#################################################################
################################
# Make it a conjoint dataset
#########################


# load a bunch of packages

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)

setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey")
# import the dataset with row=respondent

data = readRDS("data_recoded.RDS")

N=1500  #number of respondents

ntask = 5 #number of conjoint task for the parallel conjoint design

nprofiles = 2 #number of profiles seen by the respondent for each task (it is a forced-choice design)

profiles_per_resp = ntask*nprofiles #total number of profiles each respondent sees 

#create the empty dataframe with placeholders

cjdata = data.frame("respid" = rep(NA, N*ntask*nprofiles)) #respondent's id (for merging and clustering)

cjdata$cpd_task_number = NA#"task_number" #sequential number of the task
cjdata$cpd_profile_number = NA#"profile_number" #sequential number of the profile

#the conjoint attribute as seen by the respondent (for now with placeholders)
# We use MISTAKE as a placeholder because if they values are not changed at the end of the script tù
#then it means there has been a mistake
cjdata$cpd_gender = NA#"MISTAKE c_gender"
cjdata$cpd_age = NA#"MISTAKE c_age"
cjdata$cpd_educ = NA# "MISTAKE c_educ"
cjdata$cpd_regionfeel = NA#"MISTAKE c_regionfeel"
cjdata$cpd_consc = NA#"MISTAKE c_consc"
cjdata$cpd_ope = NA#"MISTAKE c_ope"
cjdata$cpd_diet = NA#"MISTAKE c_diet"
cjdata$cpd_animal = NA#"MISTAKE c_animal"
cjdata$cpd_holiday = NA#"MISTAKE c_holiday"
cjdata$cpd_ideology = NA#"MISTAKE c_ideology"

#the conjoint attribute recoded as to whether they match or not the respondent's characteristics
cjdata$cpd_match_gender = NA#"MISTAKE cpd_match_gender"
cjdata$cpd_match_age = NA#"MISTAKE cpd_match_age"
cjdata$cpd_match_educ = NA#"MISTAKE cpd_match_educ"
cjdata$cpd_match_regionfeel = NA#"MISTAKE cpd_match_regionfeel"
cjdata$cpd_match_consc = NA#"MISTAKE cpd_match_consc"
cjdata$cpd_match_ope = NA#"MISTAKE cpd_match_ope"
cjdata$cpd_match_diet = NA#"MISTAKE cpd_match_diet"
cjdata$cpd_match_animal = NA#"MISTAKE cpd_match_animal"
cjdata$cpd_match_holiday = NA#"MISTAKE cpd_match_holiday"
cjdata$cpd_match_ideology = NA#"MISTAKE cpd_match_ideology" #the ideology variable is missing for those
#that are in the natural mediation arm



#the experimental arm where the respondent is allocated (manipulated or natural)
cjdata$cpd_exparm = NA#"cpd_exparm"


#the profile the respondent has chosen
cjdata$cpd_chosen = NA#"cpd_chosen"


##### check if everything is okay with the variable name

names(cjdata)


# now we actually create the conjoint dataset 

for(i in 1:nrow(data)) #for every row in data
{
  for(k in 1:ntask) #for each task 
  {
    for(j in 1:nprofiles) # for (both of) the profiles visualized in each task
    {
      
    this_row = profiles_per_resp*(i-1)+(k-1)*2+j # the row of the cjdata that we are going to change the values of
    
    cjdata[this_row, "respid"] = data[i, "id__"] #the respondent id
    
    cjdata[this_row, "cpd_task_number"] = k #the task number
    
    cjdata[this_row, "cpd_profile_number"] = j #the profile number

    
    #for each of the conjoint attributes, we fill the dataset accordingly
    
    cjdata[this_row, "cpd_gender"] = data[i, paste0("C1_","P",j,"_A1__",k)] 
    
    cjdata[this_row, "cpd_age"] = data[i, paste0("C1_","P",j,"_A2__",k)]
    
    cjdata[this_row, "cpd_educ"] = data[i, paste0("C1_","P",j,"_A3__",k)]
    
    cjdata[this_row, "cpd_regionfeel"] = data[i, paste0("C1_","P",j,"_A4__",k)]
    
    cjdata[this_row, "cpd_consc"] = data[i, paste0("C1_","P",j,"_A5__",k)]
    
    cjdata[this_row, "cpd_ope"] = data[i, paste0("C1_","P",j,"_A6__",k)]
    
    cjdata[this_row, "cpd_diet"] = data[i, paste0("C1_","P",j,"_A7__",k)]
    
    cjdata[this_row, "cpd_animal"] = data[i, paste0("C1_","P",j,"_A8__",k)]
    
    cjdata[this_row, "cpd_holiday"] = data[i, paste0("C1_","P",j,"_A9__",k)]

    cjdata[this_row, "cpd_ideology"] = data[i, paste0("C1_","P",j,"_A10__",k)]
    
    
    #we set the value for the variable indicating whether we are in the manipulated or natural mediation arm
    cjdata[this_row, "cpd_exparm"] = ifelse(is.na(data[i, "mat_med_5"]) & !is.na(data[i, "nat_med_5"]),
                                                 "natural", 
                                                 ifelse(!is.na(data[i, "mat_med_5"]) & is.na(data[i, "nat_med_5"]),
                                                        "mediated", "MISSING"
                                                        )
                                            )
    
    #we set the value for the variable indicating whether the profile was chosen or not 
    #it is a dummy variable: 0 = not chosen, 1=chosen
    cjdata[this_row, "cpd_chosen"] = ifelse(cjdata[this_row, "cpd_exparm"]=="natural", 
                                                 data[i, paste0("nat_med_", (j*k+1)%/%2)],
                                                 ifelse(cjdata[this_row, "cpd_exparm"]=="mediated", 
                                                        data[i, paste0("mat_med_", (j*k+1)%/%2)],
                                                        "ERROR - missing"))
      
    # if the profile that has been chosen is the one that in this moment is in the
    #cell we set it at 1, else we set it at 0
    cjdata[this_row, "cpd_chosen"] = ifelse(cjdata[this_row, "cpd_chosen"] == j,
                                            1, 0)
    }
      
  }
}

table(cjdata$cpd_chosen)
#make them all factors so that they work with cj functions

###################
### CREATION OF THE "MATCH" VARIABLES
###################

cjdata1= merge(cjdata, data, by.x = "respid", by.y = "id__")

cjdata=cjdata1

rm(cjdata1)

variables_to_set=c("gender","age","educ","regionfeel",
                   "consc","ope",
                   "diet", "animal","holiday",
                   "ideology")

#creation of mock variables to make names uniform

cjdata$match_gender = cjdata$gender
cjdata$match_age = cjdata$AGE_GROUP
cjdata$match_educ = cjdata$EDU_LEVEL
cjdata$match_regionfeel = cjdata$region_feel
cjdata$match_consc = cjdata$TIPI_CON_REC
cjdata$match_ope = cjdata$TIPI_OPE_REC
cjdata$match_diet = cjdata$diet
cjdata$match_animal = cjdata$animal
cjdata$match_holiday = cjdata$holiday
cjdata$match_ideology = cjdata$IDEOLOGY_REC #PROBABILMENTE DA RICODIFICARE ANCORA QUESTA


#YOU NEED TO CHECK THAT THE LEVELS OF THE VARIABLES IN THE CONJOINT AND AT THE RESPONDENT
# LEVEL ARE EQUIVALENT BEFORE! CHECK WHEN YOU HAVE CINT'S FINAL DATA!



# The following loop sets the values of the match variables. These can be either "match" or "mismatch"
# but are initialized as "mistakes" so we know if everything went well
for(variable in variables_to_set)
{
  cjdata[, paste0("cpd_match_", variable)] = ifelse(cjdata[, paste0("cpd_", variable)] == cjdata[, paste0("match_", variable)],
                                                  paste0(variable, "_match"),
                                                  paste0(variable, "_mismatch"))
}

#remove these now useless variables
cjdata$match_gender = NULL
cjdata$match_age = NULL
cjdata$match_educ = NULL
cjdata$match_regionfeel = NULL
cjdata$match_consc = NULL
cjdata$match_ope = NULL
cjdata$match_diet = NULL
cjdata$match_animal = NULL
cjdata$match_holiday = NULL
cjdata$match_ideology = NULL



for(i in 1:ncol(cjdata))
{
  cjdata[, i] = factor(cjdata[, i]) 
  #levels = unique(cjdata[, i])[1:length(unique(cjdata[, i]))])
}

#the following factor needs to be reordered
cjdata[, "cpd_age"] = factor(cjdata[, "cpd_age"], levels = c("under30",
                                                             "between30and59",
                                                             "over60"))

#l'unico che non voglio factor è chosen!

cjdata[, "cpd_chosen"] = as.numeric(cjdata[, "cpd_chosen"])-1



#check if everything is okay with making them all factors
for(i in 1:ncol(cjdata))
{
  print(is.factor(cjdata[, i]))
}


#very rough visual randomization check
# 
# for (i in 4:ncol(data)) {
#   
#   print(names(cjdata)[i])
#   
#   print(table(cjdata[, i], useNA = "always"))
#   
#   # Pause and wait for user input to proceed
#   readline(prompt = "Press [Enter] to proceed to the next variable:")
#   
#   # Continue to the next iteration
# }


# rough check regarding the probability of seeing a match or not 
# 
# for (variable in variables_to_set)
# {
#   
#   print(paste0("match ", variable))
#   
#   print(table(cjdata[, paste0("cpd_match_", variable)], useNA = "always"))
#   
#   # Pause and wait for user input to proceed
#   readline(prompt = "Press [Enter] to proceed to the next variable:")
#   
#   # Continue to the next iteration
# }


#at this point we can export the dataset for further analyses in other scripts 

export(cjdata, paste0("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/","cjdata_cpd.RDS"))

#end