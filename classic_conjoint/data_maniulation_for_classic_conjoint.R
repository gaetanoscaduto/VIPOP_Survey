#### Script to transform the dataset in the conjoint dataset for the classic conjoint design

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

cjdata$ccd_task_number = NA#"task_number" #sequential number of the task
cjdata$ccd_profile_number = NA#"profile_number" #sequential number of the profile

#the conjoint attribute as seen by the respondent (for now with placeholders)
# We use MISTAKE as a placeholder because if they values are not changed at the end of the script tù
#then it means there has been a mistake

conjattr_full = c("gender",
                  "age",
                  "citysize",
                  "job",
                  "consc",
                  "ope",
                  "neu",
                  "restaurant",
                  "transport",
                  "animal")

cjdata$ccd_gender = NA
cjdata$ccd_age = NA
cjdata$ccd_citysize = NA
cjdata$ccd_job = NA
cjdata$ccd_consc = NA
cjdata$ccd_ope = NA
cjdata$ccd_neu = NA
cjdata$ccd_restaurant = NA
cjdata$ccd_transport = NA
cjdata$ccd_animal = NA


#the experimental arm where the respondent is allocated (manipulated or natural)

#the profile the respondent has chosen
cjdata$ccd_chosen = NA
cjdata$ccd_continuous = NA
cjdata$ccd_populism = NA


##### check if everything is okay with the variable name

names(cjdata)


for(i in 1:nrow(data)) #for every row in data
{
  for(k in 1:ntask) #for each task 
  {
    for(j in 1:nprofiles) # for (both of) the profiles visualized in each task
    {
      
      this_row = profiles_per_resp*(i-1)+(k-1)*2+j # the row of the cjdata that we are going to change the values of
      
      cjdata[this_row, "respid"] = data[i, "id__"] #the respondent id
      
      cjdata[this_row, "ccd_task_number"] = k #the task number
      
      cjdata[this_row, "ccd_profile_number"] = j #the profile number
      
      
      #for each of the conjoint attributes, we fill the dataset accordingly
      
      for(colname in conjattr_full)
      {
        cjdata[this_row, paste0("ccd_",colname)] = data[i, paste0("C3_P",
                                                                  j,
                                                                  "_A",
                                                                  which(conjattr_full==colname),
                                                                  "__",
                                                                  k)]   
      }
      
      #we set the value for the variable indicating whether the profile was chosen or not 
      #it is a dummy variable: 0 = not chosen, 1=chosen
      cjdata[this_row, "ccd_chosen"] = data[i, paste0("cjcl_",k,"_ideo_dic")]
      
      cjdata[this_row, "ccd_continuous"] = data[i, paste0("cjcl_",k,"_ideo__1")]
      
      cjdata[this_row, "ccd_populism"] = data[i, paste0("cjcl_",k,"_pop")]
      
      # if the profile that has been chosen is the one that in this moment is in the
      #cell we set it at 1, else we set it at 0
      cjdata[this_row, "ccd_chosen"] = ifelse(cjdata[this_row, "ccd_chosen"] == j,
                                                 1, 0)
      
      cjdata[this_row, "ccd_populism"] = ifelse(cjdata[this_row, "ccd_populism"] == j,
                                                  1, 0)
    }
    
  }
}


########################################################
#DA QUA IN POI TUTTO DA RIADATTARE
# cerca di capire perché CI SONO DEGLI STRANI NA NEL CJDATA$CCD_CONTINUOUS
########################################################





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

cjdata$cpd_exparm2 = ifelse(cjdata$cpd_exparm=="natural", "natural",
                            ifelse(cjdata$cpd_match_ideology=="ideology_match", 
                                   "ideology_match",
                                   "ideology_mismatch"))


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