#### Script to transform the dataset in the conjoint dataset for the classic conjoint design

# In this script I take the dataset in the form given by the survey compay (1 row equals 1 respondent)
# and I transform it in order to have a dataset that can be analyzed by the conjoint estimators


#################################################################
################################
# Make it a conjoint dataset
#########################


# load a bunch of packages, the more the merrier

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
                  "religion",
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
cjdata$ccd_religion = NA
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
cjdata$ccd_chosen_rw = NA
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
      cjdata[this_row, "ccd_chosen_rw"] = data[i, paste0("cjcl_",k,"_ideo_dic")]
      
      cjdata[this_row, "ccd_continuous"] = data[i, paste0("cjcl_",k,"_ideo__1")]
      
      cjdata[this_row, "ccd_populism"] = data[i, paste0("cjcl_",k,"_pop")]
      
      # if the profile that has been chosen is the one that in this moment is in the
      #cell we set it at 1, else we set it at 0
      cjdata[this_row, "ccd_chosen_rw"] = ifelse(cjdata[this_row, "ccd_chosen_rw"] == j,
                                                 1, 0)
      
      cjdata[this_row, "ccd_populism"] = ifelse(cjdata[this_row, "ccd_populism"] == j,
                                                  1, 0)
    }
    
  }
}


########################################################
#DA QUA IN POI TUTTO DA RIADATTARE
# cerca di capire perché CI SONO DEGLI STRANI NA NEL CJDATA$CCD_CONTINUOUS
# quando arriva il pilota devi vedere se quella variabile ha dei missing
########################################################



#make them all factors so that they work with cj functions

for(i in 1:ncol(cjdata))
{
  cjdata[, i] = factor(cjdata[, i]) 
  #levels = unique(cjdata[, i])[1:length(unique(cjdata[, i]))])
}

cjdata[, "ccd_chosen_rw"] = as.numeric(cjdata[, "ccd_chosen_rw"])-1
cjdata[, "ccd_populism"] = as.numeric(cjdata[, "ccd_populism"])-1
cjdata[, "ccd_continuous"] = as.numeric(cjdata[, "ccd_continuous"])
#check if everything is okay with making them all factors but the outcomes
for(i in 1:ncol(cjdata))
{
  print(is.factor(cjdata[, i]))
}

#merge

cjdata1= merge(cjdata, data, by.x = "respid", by.y = "id__")

cjdata=cjdata1

rm(cjdata1)



export(cjdata, paste0("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/","cjdata_ccd.RDS"))

#end