#Data manipulation for visual conjoint design

#### Script to transform the dataset in the conjoint dataset

# In this script I take the dataset in the form given by the survey company (1 row equals 1 respondent)
# and I transform it in order to have a dataset that can be analyzed by the conjoint estimators
#Notice that the information regarding the conjoint attributes is given
#in the form of a filename containing the characteristics in a fixed order. 
# therefore, the script accounts for this. 

#Un esempio di come è il file è il seguente
# IT_white_male_35_2_Giuseppe_Colombo_Teacher_rightneg_future1_honesty1_meatrich_catpoor_whiteelite_90.jpg


#Therefore the variables are, in order:
# country
# ethnicity
# gender
# age
# photo
# name
# surname
# job
# issue
# nostalgia
# valence
# food
# animal
# crowd
# identifier

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

#If you launch this script from the master script, make sure to have the context fixed
#otherwise, uncomment desired context
#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"


#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

# import the dataset with row=respondent

data = readRDS(paste0(dataset_rep, "data_recoded_", context, ".RDS"))

N=1500  #number of respondents

ntask = 5 #number of conjoint task for the parallel conjoint design

nprofiles = 2 #number of profiles seen by the respondent for each task (it is a forced-choice design)

profiles_per_resp = ntask*nprofiles #total number of profiles each respondent sees 

visconj_varnames=NULL

conjattr_full = c("country",
  "ethnicity",
  "gender",
  "age",
  "photo",
  "name",
  "surname",
  "job",
  "issue",
  "nostalgia",
  "valence",
  "food",
  "animal",
  "crowd",
  "identifier")

for(i in 1:ntask)
{
  for(j in 1:nprofiles)
  {
    visconj_varnames = c(visconj_varnames, paste0("vcd_t",
                                                  i,#task number
                                                  "_p",
                                                  j,#profile number
                                                  "_",
                                                  c("country",
                                                    "ethnicity",
                                                    "gender",
                                                    "age",
                                                    "photo",
                                                    "name",
                                                    "surname",
                                                    "job",
                                                    "issue",
                                                    "nostalgia",
                                                    "valence",
                                                    "food",
                                                    "animal",
                                                    "crowd",
                                                    "identifier")
    )
    )
    
  }
}

visconj_varnames

#initialize the variables
for(varname in visconj_varnames)
{
  data[, varname] = NA
}


#in the following megaloop we parse the information
for(i in 1:ntask) #for each task
{
  for(j in 1:nprofiles) #for each profile
  {
    #put in a temporary variable the strsplit of what its in the variable named
    #C2P[PROFILE]__[TASK]__open
    temp = strsplit(data[, paste0("C2_P",
                                  j, #profile number
                                  "__",
                                  i, #task numebr
                                  "__open")], "_")
    
    
    for(k in 1:nrow(data)) #for each row in the dataset
    {
      if(length(temp[[k]]) <15) #if the vector null (the respondent likely did not
        #reach that part of the experiment)
      {
        print("error or empty data")
        next #skip to the next part of the loop
      }
      
      for(l in 1:15) #for each of the 15 variables in the visual conjoint 
        #you put the value in the right cell
      {
        data[k, paste0("vcd_t",
                       i,
                       "_p",
                       j,
                       "_",
                       conjattr_full[l])] = temp[[k]][l]
      }
      
    }
  }
}



############ NOW WE HAVE PARSED THE DATA AS THE CLASSIC DATASET WE
#WOULD RECEIVE FROM A SURVEY COMPANY AFTER RUNNING A CONJOINT. 
#WE NEED TO TRANSFORM THIS IN A PROPER CONJOINT DATASET

#create the empty dataframe with placeholders

cjdata = data.frame("respid" = rep(NA, N*ntask*nprofiles)) #respondent's id (for merging and clustering)

cjdata$vcd_country = NA
cjdata$vcd_task_number = NA#"task_number" #sequential number of the task
cjdata$vcd_profile_number = NA#"profile_number" #sequential number of the profile

#the conjoint attribute as seen by the respondent (for now with placeholders)
# We use MISTAKE as a placeholder because if they values are not changed at the end of the script tù
#then it means there has been a mistake


#generate the variables and initialize with NA
for(varname in conjattr_full)
{
  cjdata[, paste0("vcd_", varname)] = NA
}



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
      
      cjdata[this_row, "vcd_country"] = data[i, "country"]
      
      cjdata[this_row, "respid"] = data[i, "id__"] #the respondent id
      
      cjdata[this_row, "vcd_task_number"] = k #the task number
      
      cjdata[this_row, "vcd_profile_number"] = j #the profile number
      
      
      #for each of the conjoint attributes, we fill the dataset accordingly
      
      for(colname in conjattr_full)
      {
        cjdata[this_row, paste0("vcd_",colname)] = data[i, paste0("vcd_t",
                                                                  k,
                                                                  "_p"
                                                                  ,j,
                                                                  "_",
                                                                  colname)]   
      }
      
      
      
      #we set the value for the variable indicating whether the profile was chosen or not 
      #it is a dummy variable: 0 = not chosen, 1=chosen
      cjdata[this_row, "vcd_chosen_rw"] = data[i, paste0("vce_",k,"_rightwing")]
      
      cjdata[this_row, "vcd_chosen_trust"] = data[i, paste0("vce_",k,"_trustworth")]
      
      cjdata[this_row, "vcd_chosen_pop"] = data[i, paste0("vce_",k,"_populist")]
      
      # if the profile that has been chosen is the one that in this moment is in the
      #cell we set it at 1, else we set it at 0
      cjdata[this_row, "vcd_chosen_rw"] = ifelse(cjdata[this_row, "vcd_chosen_rw"] == j,
                                                 1, 0)
      
      cjdata[this_row, "vcd_chosen_trust"] = ifelse(cjdata[this_row, "vcd_chosen_trust"] == j,
                                                    1, 0)
      
      cjdata[this_row, "vcd_chosen_pop"] = ifelse(cjdata[this_row, "vcd_chosen_pop"] == j,
                                                  1, 0)
    }
    
  }
}

table(cjdata$vcd_chosen_rw)


# I want to adjust the content of some cells to a more readable/correct format

cjdata = cjdata |>
  mutate(vcd_food, 
         vcd_food = case_when(
           vcd_food == "ethnicrich" ~ "Ethnic",
           vcd_food == "meatpoor" ~ "Meatpoor",
           vcd_food == "meatrich" ~ "Meatrich",
           vcd_food == "vegan" ~ "Vegan",
           is.na(vcd_food) ~ NA
         ) 
  )




### merge with the original dataset

cjdata1= merge(cjdata, data, by.x = "respid", by.y = "id__", sort = F)

if(context != "POOL") #then respid is the same of numeric variable
{
  cjdata1=cjdata1 |>
    arrange(as.numeric(respid), vcd_task_number, vcd_profile_number)
}

if(context == "POOL")
{
  cjdata1=cjdata1 |>
    arrange(respid, vcd_task_number, vcd_profile_number)
}

cjdata_prev = cjdata
cjdata=cjdata1

rm(cjdata1)




## everything becomes now a factor
for(i in 1:ncol(cjdata_prev))
{
  cjdata[, i] = factor(toTitleCase(as.character(cjdata[, i]))) 
}

#quelli che non voglio factor sono i chosen oltre alle prime (per ora)!

cjdata[, "vcd_chosen_rw"] = as.numeric(cjdata[, "vcd_chosen_rw"])-1
cjdata[, "vcd_chosen_trust"] = as.numeric(cjdata[, "vcd_chosen_trust"])-1
cjdata[, "vcd_chosen_pop"] = as.numeric(cjdata[, "vcd_chosen_pop"])-1



#check if everything is okay with making them all factors
# for(i in 1:ncol(cjdata))
# {
#   print(is.factor(cjdata[, i]))
# }


#at this point we can export the dataset for further analyses in other scripts 

export(cjdata, paste0(dataset_rep,"cjdata_vcd_", context, ".RDS"))

#end
