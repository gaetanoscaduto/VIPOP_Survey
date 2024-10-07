# DA RICONTROLLARE


###############################################################################
#this script is for the analyses related to the CLASSIC conjoint design, 
#when considering a country at a time or pooled all together
###############################################################################


#############################################################
#LIBRARY CALLS
#############################################################

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)

#############################################################
# DEFINING FUNCTIONS
#############################################################

###SET CATEGORIES AND LEVELS

#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio

set_categories_and_levels_visual = function(effects, 
                                            attributes=attributes){
  # effects=effects_pooled
  # attributes=attributes
  effects$feature = factor(attributes, levels = unique(attributes))
  effects$level=factor(levels_vector, levels = levels_vector)
  
  return(effects)
}



##Function to draw plots for the effects

draw_plot_effects = function(effects, 
                             estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                             y_labels=y_labels_plots,
                             leftlim=999, #the left limit of the plot
                             rightlim=999,#the right limit of the plot
                             x_intercept=999
){
  
  estimator=match.arg(estimator)
  
  v = list()
  
  if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
    #with [-1; 1] for amces and [0, 1] for mm
  {
    leftlim=ifelse(estimator!="mm", -1, 0)
    rightlim=1
    intercept = ifelse(estimator!="mm", 0, 0.5)
  }
  else #continuous outcome 0-10s
  {
    intercept = 5
  }
  
  v=list()
  for(attribute in unique(attributes))
  {
    p = ggplot(effects[effects$feature==attribute, ])+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper,
                          y=level, col=feature))+
      ylab(attribute)+
      xlab("\n")+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = rev(y_labels_plots[[tolower(attribute)]])) +
      theme(legend.position = "none",
            axis.text.y = element_text(size=10),
            axis.title.y = element_text(size=12))
    
    v[[attribute]] = p
  }
  
  p = (v[["Gender"]]/v[["Age"]]/v[["Religion"]]/v[["Citysize"]]/(v[["Job"]]+xlab("Effect size")))|(v[["Conscientiousness"]]/v[["Openness"]]/v[["Neuroticism"]]/v[["Restaurant"]]/v[["Transport"]]/(v[["Animal"]]+xlab("Effect size")))
  return(p)
}




full_analysis = function(data,
                         formula, #the conjoint formula
                         estimator=c("mm","amce"), #marginal means and amces
                         subdir,#the subdirectory where the plots will be saved
                         continuous=F #to change if we are dealing with continuous outcome
){
  
  
  ###### This function performs the whole analysis, draws the graphs and saves
  #them in the appropriate repositories. 
  #It calls the other functions previously defined plus the functions in cjregg and
  #patchwork
  
  # formula=formula_continuous
  #  estimator="mm"
  
  estimator=match.arg(estimator)
  
  effects_pooled <- data |>
    cj(formula, 
       id = ~respid,
       estimate = estimator)
  
  effects_pooled = set_categories_and_levels_visual(effects_pooled,
                                                    attributes = attributes)
  
  if(continuous==F)
  {
  p = draw_plot_effects(effects_pooled,
                        estimator=estimator,
                        y_labels=y_labels_plots)
  }
  else
  {
    p = draw_plot_effects(effects_pooled,
                          estimator=estimator,
                          y_labels=y_labels_plots,
                          leftlim = 0, 
                          rightlim = 10)
  }
  
  p=p+patchwork::plot_annotation(title = paste("Effects of the attributes Classic Conjoint Experiment"),
                                 caption= toupper(estimator))
  
  ggsave(paste0(output_wd,"estimations/", subdir,"singlecountry.png"), 
         p, 
         height = 10, 
         width = 10)
  
  return(p)
  
}

#Our levels regarding match and mismatches (for labeling)


y_labels_plots = list(gender=c("Female", "Male", "Non-binary"),
                      age=c("25","45","65"),
                      religion=c("Practitioner","Non practitioner", "Non believer"),
                      citysize=c("Big", "Small", "Medium"), #ricorda di correggere l'ordine di sti factor
                      job=c("Entrepreneur", "Teacher", "Waiter", "Lawyer"),
                      consc=c("Reliable", "Disorganized"),
                      ope=c("Open", "Rigid"),
                      neu=c("Calm", "Anxious"),
                      restaurant=c("Traditional", "Vegan","Asian","Steakhouse"),
                      transport=c("Bycicle","Public Transport","SUV"),
                      animal=c("Large dog","Small dog","Cat", "No pets")
                      )


levels_vector= unlist(y_labels_plots, use.names = F)

attributes= c("Gender", "Gender", "Gender",
              "Age","Age","Age",
              "Religion","Religion","Religion",
              "Citysize","Citysize","Citysize",
              "Job","Job","Job","Job",
              "Conscientiousness","Conscientiousness",
              "Openness","Openness",
              "Neuroticism","Neuroticism",
              "Restaurant","Restaurant","Restaurant","Restaurant",
              "Transport","Transport","Transport",
              "Animal","Animal","Animal","Animal"
              )


formula_rw = ccd_chosen_rw ~ ccd_gender+
  ccd_age+ccd_religion+ccd_citysize+ccd_job+
  ccd_consc+ccd_ope+ ccd_neu+
  ccd_restaurant+ccd_transport+ccd_animal

formula_continuous = ccd_continuous ~ ccd_gender+
  ccd_age+ccd_religion+ccd_citysize+ccd_job+
  ccd_consc+ccd_ope+ ccd_neu+
  ccd_restaurant+ccd_transport+ccd_animal


#############################################################


setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/VIPOP/VIPOP_Survey/classic_conjoint/")

output_wd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/analyses/classic_conjoint_design/"
data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_ccd.RDS")

data=rbind(data, data, data, data)
data=rbind(data, data, data, data)

context = "IT"
# context = "FR"
# context = "SW"
# context = "CZ"
# 
# data = data |>
#   filter(country == "IT")

#############################################################

######################################
############ EFFECTS ################# 
######################################


subdir = "MMs/"

full_analysis(data,
              formula_rw,
              "mm",
              subdir)


### Same as before, but with AMCes (for appendix)

subdir = "AMCEs/"

full_analysis(data,
              formula_rw,
              "amce",
              subdir)

######### Continuous outcome, mm

subdir = "Continuous/"

full_analysis(data,
              formula_continuous,
              "mm",
              subdir,
              continuous = T)
