#### Script to transform the dataset in the conjoint dataset

#################################################################
################################
# Make it a conjoint dataset
#########################

#this script is for the analyses related to the parallel conjoint design

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)
#

data = readRDS("data_recoded.RDS")

N=1500 

ntask = 5

nprofiles = 2

profiles_per_resp = ntask*nprofiles

cjdata = data.frame("respid" = rep("respid", N*ntask*nprofiles))
cjdata$cpd_task_number = "task_number"
cjdata$cpd_profile_number = "profile_number"

cjdata$cpd_gender = "c_gender"
cjdata$cpd_age = "c_age"
cjdata$cpd_educ = "c_educ"
cjdata$cpd_regionfeel = "c_regionfeel"
cjdata$cpd_consc = "c_consc"
cjdata$cpd_ope = "c_ope"
cjdata$cpd_diet = "c_diet"
cjdata$cpd_animal = "c_animal"
cjdata$cpd_holiday = "c_holiday"

cjdata$cpd_exparm = "cpd_exparm"

cjdata$cpd_chosen = "cpd_chosen"

####################ALLUNGA IL DATASET DI MODO DA ESSERE 15000 RIGHE!

names(cjdata)

for(i in 1:nrow(data))
{
  for(k in 1:ntask)
  {
    for(j in 1:nprofiles)
    {
      
    this_row = profiles_per_resp*(i-1)+(k-1)*2+j 
    
    cjdata[this_row, "respid"] = data[i, "id__"]
    
    cjdata[this_row, "cpd_task_number"] = k
    
    cjdata[this_row, "cpd_profile_number"] = j
    
    cjdata[this_row, "cpd_gender"] = data[i, paste0("C1_","P",j,"_A1__",k)] 
    
    cjdata[this_row, "cpd_age"] = data[i, paste0("C1_","P",j,"_A2__",k)]
    
    cjdata[this_row, "cpd_educ"] = data[i, paste0("C1_","P",j,"_A3__",k)]
    
    cjdata[this_row, "cpd_regionfeel"] = data[i, paste0("C1_","P",j,"_A4__",k)]
    
    cjdata[this_row, "cpd_consc"] = data[i, paste0("C1_","P",j,"_A5__",k)]
    
    cjdata[this_row, "cpd_ope"] = data[i, paste0("C1_","P",j,"_A6__",k)]
    
    cjdata[this_row, "cpd_diet"] = data[i, paste0("C1_","P",j,"_A7__",k)]
    
    cjdata[this_row, "cpd_animal"] = data[i, paste0("C1_","P",j,"_A8__",k)]
    
    cjdata[this_row, "cpd_holiday"] = data[i, paste0("C1_","P",j,"_A9__",k)]
    
    cjdata[this_row, "cpd_exparm"] = ifelse(is.na(data[i, "mat_med_5"]) & !is.na(is.na(data[i, "nat_med_5"])),
                                                 "natural", 
                                                 ifelse(!is.na(data[i, "mat_med_5"]) & is.na(is.na(data[i, "nat_med_5"])),
                                                        "mediated", "MISSING"))
    
    cjdata[this_row, "cpd_chosen"] = ifelse(cjdata[this_row, "cpd_exparm"]=="natural", 
                                                 data[i, paste0("nat_med_", (j*k+1)%/%2)],
                                                 ifelse(cjdata[this_row, "cpd_exparm"]=="mediated", 
                                                        data[i, paste0("mat_med_", (j*k+1)%/%2)],
                                                        "ERROR - missing"))
      
    
    }
      
  }
}

########## TUTTA LA PARTE CHE SEGUE è ANCORA DA MODIFICARE



#setting ingroup/outgroup


cjdata$chosen = as.numeric(cjdata$chosen)

cjdata$c_gender_r = ifelse(cjdata$c_gender==1, "F", "M")

cjdata$c_gender_r = factor(cjdata$c_gender_r)

table(cjdata$c_gender, cjdata$c_gender_r)


cjdata$c_age_r = ifelse(cjdata$c_age==1, "37",
                        ifelse(cjdata$c_age==2, "52", "67"))

cjdata$c_age_r = factor(cjdata$c_age_r)

table(cjdata$c_age_r)


cjdata$c_prov_r = ifelse(cjdata$c_prov==1, "Centro Italia",
                         ifelse(cjdata$c_prov==2, "Nord Italia", "Sud Italia"))

cjdata$c_prov_r = factor(cjdata$c_prov_r)

table(cjdata$c_prov_r)

cjdata$c_job_r = ifelse(cjdata$c_job==1, "Economista",
                        ifelse(cjdata$c_job==2, "Filosofo/a", 
                               ifelse(cjdata$c_job==3, "Giornalista",
                                      ifelse(cjdata$c_job==4, "Giurista", "Politologo/a"))))

cjdata$c_job_r = factor(cjdata$c_job_r, levels = c("Giornalista", "Economista", "Filosofo/a",
                                                   "Giurista", "Politologo/a"))

table(cjdata$c_job_r, cjdata$c_job)

cjdata$c_ideo_r = ifelse(cjdata$c_ideo==1, "Non note",
                         ifelse(cjdata$c_ideo==2, "Centro", 
                                ifelse(cjdata$c_ideo==3, "Destra", "Sinistra")))

cjdata$c_ideo_r = factor(cjdata$c_ideo_r, levels = c("Non note", "Centro", "Destra", "Sinistra"))

table(cjdata$c_ideo_r, cjdata$c_ideo)

cjdata = merge(cjdata, data)
cjdata$c_inoutgr = "c_inoutgr"

for(i in 1:nrow(cjdata))
{
  cjdata$c_inoutgr[i] = ifelse(grepl("Non note", cjdata$c_ideo_r[i]), "Non note", 
                               ifelse(grepl(cjdata$c_ideo_r[i], cjdata$outgroup[i], ignore.case = T), "Outgroup", "Ingroup"))
}

table(cjdata$c_inoutgr)

export(cjdata, paste0(getwd(), "/data/survey/","cjdata_ready.RDS"))

