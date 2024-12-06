###############################################################################
#this script is for the analyses related to the parallel conjoint design,
# considering all countries at the same time
###############################################################################


#############################################################
#LIBRARY CALLS
#############################################################

# pacman::p_load(
#   cregg, dplyr, ggpubr, cowplot, 
#   MASS, cjoint, corrplot, dplyr, 
#   forcats, ggplot2, gt, gtools, 
#   gtsummary, margins, openxlsx, 
#   patchwork, rio, texreg, tools, 
#   lme4, ggeffects, wesanderson
# )

#############################################################
# DEFINING FUNCTIONS
#############################################################

###SET CATEGORIES AND LEVELS

#Here I define a function to set categories and levels in a neat and presentable 
#fashion in the mm dataset resulting from the cj function. The
#functio

set_categories_and_levels_bycountry = function(effects, 
                                               type=c("match","nominal"), 
                                               nominal_attributes=nominal_attributes){
  
  type=match.arg(type)
  
  effects$category="Sociodemographics"
  
  effects$category=ifelse(grepl("ope",effects$feature) | grepl("consc",effects$feature), 
                          "Psychological",
                          ifelse(grepl("diet",effects$feature) | grepl("animal",effects$feature) | grepl("holiday",effects$feature),
                                 "Lifestyle",
                                 "Sociodemographics"))
  
  if(type=="match")
  {
    effects$variable = as.character(effects$level)
    
    for(i in 1:nrow(effects))
    {
      effects$variable[i] = toTitleCase(paste(strsplit(as.character(effects$level[i]), "_")[[1]], collapse=" "))
    }
    
    effects$level=factor(effects$variable)
    
  }
  if(type=="nominal")
  {
    effects$feature = factor(nominal_attributes, levels = unique(nominal_attributes))
    effects$level=factor(levels_vector, levels = levels_vector)
  }
  
  return(effects)
}



##Function to draw plots for the effects

draw_plot_effects_bycountry = function(effects, 
                                       type=c("match", "nominal"), #"match" or "nominal" 
                                       categories=c("Sociodemographics", "Psychological", "Lifestyle", "Political"), #vector of thee categories 
                                       #("sociodemo", "psycho", "lifestyle")
                                       estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                                       y_labels=y_labels_plots,
                                       leftlim=999, #the left limit of the plot
                                       rightlim=999,#the right limit of the plot
                                       x_intercept=999 #the vertical line to signal the difference from the insignificance
){
  
  estimator=match.arg(estimator)
  type=match.arg(type)
  
  v = list()
  
  if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
    #with [-1; 1] for amces and [0, 1] for mm
  {
    
    leftlim=ifelse(estimator!="mm", -1, 0)
    rightlim=1
  }
  if(x_intercept==999)
  {
    intercept = ifelse(estimator!="mm", 0, 0.5)
  }
  
  
  for(category in categories[1:3])
  {
    
    these_labels = y_labels[[type]][[category]]
    p = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(data=effects[effects$category == category & effects$cpd_country == "IT", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "IT", shape = "IT"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/5),
                      show.legend = T)+
      geom_pointrange(data=effects[effects$category == category & effects$cpd_country == "FR", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "FR", shape = "FR"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/15),
                      show.legend = T)+
      geom_pointrange(data=effects[effects$category == category & effects$cpd_country == "SW", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "SW", shape = "SW"),
                      alpha = 1,
                      position = position_nudge(y = -1/15),
                      #size=1.3,
                      show.legend = T)+
      geom_pointrange(data=effects[effects$category == category & effects$cpd_country == "CZ", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "CZ", shape = "CZ"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = -1/5),
                      show.legend = T)+
      # geom_pointrange(data=effects[effects$category == category & effects$cpd_country == "POOL", ],
      #                 aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "POOL", shape = "POOL"),
      #                 alpha = 1,
      #                 position = position_nudge(y = -1/5),
      #                 #size=1.3,
      #                 show.legend = T)+
      ylab("")+
      xlab(category)+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = rev(these_labels))+
      scale_color_manual(
        values = c("IT" = wesanderson::wes_palettes$Darjeeling1[1],
                   "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                   "SW" = wesanderson::wes_palettes$Darjeeling1[3],
                   "CZ" = wesanderson::wes_palettes$Darjeeling1[4]#,
                   #"POOL" = 'black'
                   ),
        name = "Country",
        limits = c("IT", "FR", "SW", "CZ"#,
                   #"POOL"
                   )
      ) +
      scale_shape_manual(
        values = c("IT" = 19, 
                   "FR" = 17, 
                   "SW" = 15, 
                   "CZ" = 18#, 
                   #"POOL" = 1
                   ),
        name = "Country",
        limits = c("IT", "FR", "SW", "CZ"#, 
                   #"POOL"
                   )
      ) +
      theme(
        legend.position = "right",  # You can change this to "top", "bottom", etc.
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12)
      )
    
    
    
    v[[category]] = p
    
  }
  
  return(v)
}


full_analysis_bycountry = function(data,
                                   formula, #the conjoint formula
                                   effect=c("ATEs", "ACDEs", "EEs"), #the three possible effects to compute
                                   type=c("match", "nominal"), #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                                   estimator=c("mm","amce", "mm_differences", "amce_differences"), #marginal means and amces
                                   arm=c("natural", "ideology_match", "ideology_mismatch"), #natural mediation arm, or manipulated mediation arm with ideological match, 
                                   #or manipulated mediation arm with ideological mismatch
                                   subdir,
                                   leftlim=999,
                                   rightlim=999#the subdirectory where the plots will be saved
){
  
  #browser()
  ###### This function performs the whole analysis, draws the graphs and saves
  #them in the appropriate repositories. 
  #It calls the other functions previously defined plus the functions in cjregg and
  #patchwork
  
  #Notice that the function behaves slightly differently for EEs, that need to be
  #treated a bit differntly due to the complications
  
  effect=match.arg(effect)
  type=match.arg(type)
  estimator=match.arg(estimator)
  arm=match.arg(arm)
  
  
  
  if(effect!= "EEs")
  {
    # effects_pooled <- data |>
    #   filter(cpd_exparm2 == arm) |>
    #   cj(formula, 
    #      id = ~respid,
    #      estimate = estimator)
    # 
    # effects_pooled$cpd_country = "POOL"
    # effects_pooled$BY = "POOL"
    
    effects_bycountry <- data |>
      filter(cpd_exparm2 == arm) |>
      cj(formula, 
         id = ~respid, 
         by= ~cpd_country,
         estimate = estimator)
  }
  if(effect== "EEs")
  {
    estimator= paste0(estimator, "_differences")
    
    # effects_pooled <- data |>
    #   filter(cpd_exparm2 == "natural" | cpd_exparm2 == arm) |>
    #   cj(formula_match,
    #      id = ~respid,
    #      estimate = estimator,
    #      by = ~cpd_exparm)
    # 
    # effects_pooled$cpd_country = "POOL"
    
    effects_bycountry =data.frame()
    
    for(context in c("IT", "FR", "SW", "CZ"))
    {
      temp_effects_bycountry <- data |>
        filter((cpd_exparm2 == "natural" | cpd_exparm2 == arm) & cpd_country == context) |>
        cj(formula_match,
           id = ~respid,
           estimate = estimator,
           by = ~cpd_exparm)
      
      temp_effects_bycountry$cpd_country = context
      
      effects_bycountry=rbind(effects_bycountry, temp_effects_bycountry)
    }
    
  }
  
  #browser()
  effects = effects_bycountry
  
  
  effects=set_categories_and_levels_bycountry(effects,
                                              type,
                                              nominal_attributes=nominal_attributes)
  
  
  #browser()
  v = draw_plot_effects_bycountry(effects, 
                                  type = type, 
                                  categories=categories, 
                                  estimator=estimator, 
                                  y_labels=y_labels_plots,
                                  leftlim = leftlim,
                                  rightlim= rightlim)
  
  for(category in categories[1:3])
  {
    # v[[category]]=v[[category]]+patchwork::plot_annotation(title = paste(effect, "of the Parallel Design Conjoint Experiment, ", type),
    #                                                        caption= paste0(toupper(estimator), "s of the", arm, " mediation arm"))
    
    ggsave(paste0(output_wd,"estimations/", subdir, category, "_bycountry.png"), 
           v[[category]], 
           height = 8, 
           width = 8, create.dir = T)
    
    saveRDS(v[[category]], file = paste0(output_wd,"estimations/", subdir, category, "_bycountry.rds"))
    
  }
  
}



############
### Function to draw lots comparing the effects of ATEs, ACDEs, and EEs.
# It funtion similarly to draw_plot_effects_bycountry
# in the single country script it doesnt exist since there it's only a variation 
#of the draw_plot_effects function with the argument for_comparison = T. 
#Here it is a function in its own right


draw_compared_effects_bycountry = function(ates, #the dataset with the ates
                                           #already disentangled by country+pooled
                                           acdes, #the dataset with the acdes
                                           #already disentangled by country+pooled
                                           ees, #the dataset with the ees
                                           #already disentangled by country+pooled
                                           type=c("match", "nominal"), #"match" or "nominal" 
                                           categories=c("Sociodemographics", "Psychological", "Lifestyle", "Political"), #vector of thee categories 
                                           #("sociodemo", "psycho", "lifestyle")
                                           estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                                           y_labels=y_labels_plots,
                                           leftlim=999, #the left limit of the plot
                                           rightlim=999,#the right limit of the plot
                                           x_intercept=999 #the vertical line to signal the difference from the insignificance
){
  
  #####browser()
  
  estimator=match.arg(estimator)
  type=match.arg(type)
  
  intercept = ifelse(estimator=="mm", 0.5, 0)
  
  ates_list = list()
  acdes_list = list()
  ees_list = list()
  
  #draw the ATEs plots
  for(category in categories[1:3])
  {
    these_labels = y_labels[[type]][[category]]
    ates_plot = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(data=ates[ates$category == category & ates$cpd_country == "IT", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "IT", shape = "IT"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/5),
                      show.legend = T)+
      geom_pointrange(data=ates[ates$category == category & ates$cpd_country == "FR", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "FR", shape = "FR"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/15),
                      show.legend = T)+
      geom_pointrange(data=ates[ates$category == category & ates$cpd_country == "SW", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "SW", shape = "SW"),
                      alpha = 1,
                      position = position_nudge(y = -1/15),
                      #size=1.3,
                      show.legend = T)+
      geom_pointrange(data=ates[ates$category == category & ates$cpd_country == "CZ", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "CZ", shape = "CZ"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = -1/5),
                      show.legend = T)+
      # geom_pointrange(data=ates[ates$category == category & ates$cpd_country == "POOL", ],
      #                 aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "POOL", shape = "POOL"),
      #                 alpha = 1,
      #                 position = position_nudge(y = -1/5),
      #                 #size=1.3,
      #                 show.legend = T)+
      ylab("")+
      xlab(category)+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = rev(these_labels))+
      scale_color_manual(
        values = c("IT" = wesanderson::wes_palettes$Darjeeling1[1],
                   "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                   "SW" = wesanderson::wes_palettes$Darjeeling1[3],
                   "CZ" = wesanderson::wes_palettes$Darjeeling1[4]#,
                   #"POOL" = 'black'
                   ),
        name = "Country",
        limits = c("IT", "FR", "SW", "CZ"#, "POOL"
                   )
      ) +
      scale_shape_manual(
        values = c("IT" = 19, 
                   "FR" = 17, 
                   "SW" = 15, 
                   "CZ" = 18#, 
                   #"POOL" = 1
                   ),
        name = "Country",
        limits = c("IT", "FR", "SW", "CZ"#, #"POOL"
                   )
      ) +
      theme(
        legend.position = "none",  # You can change this to "top", "bottom", etc.
        axis.text.y = element_text(size = 10, #angle = 90, 
                                   hjust = 0.5, vjust=0.5),
        axis.title.y = element_text(size = 12)
      )
    
    
    
    ates_list[[category]] = ates_plot
    
  }
  
  
  #draw the acdes plots
  for(category in categories[1:3])
  {
    these_labels = y_labels[[type]][[category]]
    acdes_plot = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_pointrange(data=acdes[acdes$category == category & acdes$cpd_country == "IT", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "IT", shape = "IT"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/5),
                      show.legend = T)+
      geom_pointrange(data=acdes[acdes$category == category & acdes$cpd_country == "FR", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "FR", shape = "FR"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/15),
                      show.legend = T)+
      geom_pointrange(data=acdes[acdes$category == category & acdes$cpd_country == "SW", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "SW", shape = "SW"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = -1/15),
                      show.legend = T)+
      geom_pointrange(data=acdes[acdes$category == category & acdes$cpd_country == "CZ", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "CZ", shape = "CZ"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = -1/5),
                      show.legend = T)+
      # geom_pointrange(data=acdes[acdes$category == category & acdes$cpd_country == "POOL", ],
      #                 aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "POOL", shape = "POOL"),
      #                 alpha = 1,
      #                 position = position_nudge(y = -1/5),
      #                 #size=1.3,
      #                 show.legend = T)+
      ylab("")+
      xlab(category)+
      xlim(leftlim,rightlim)+
      scale_y_discrete(limits = rev(these_labels))+
      scale_color_manual(
        values = c("IT" = wesanderson::wes_palettes$Darjeeling1[1],
                   "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                   "SW" = wesanderson::wes_palettes$Darjeeling1[3],
                   "CZ" = wesanderson::wes_palettes$Darjeeling1[4]#,
                   #"POOL" = 'black'
                   ),
                   name = "Country",
                   limits = c("IT", 
                              "FR",
                              "SW",
                              "CZ"#, "POOL"
                   )
        ) +
          scale_shape_manual(
            values = c("IT" = 19, 
                       "FR" = 17, 
                       "SW" = 15, 
                       "CZ" = 18#, 
                       #"POOL" = 1
            ),
            name = "Country",
            limits = c("IT", 
                       "FR", 
                       "SW",
                       "CZ"#, "POOL"
            )
          ) +
          theme(
            legend.position = "none",  # You can change this to "top", "bottom", etc.
            axis.text.y = element_text(size = 10, #angle = 90,
                                       hjust = 0.5, vjust=0.5),
            axis.title.y = element_text(size = 12)
          )
        
        
        
        acdes_list[[category]] = acdes_plot
        
  }
  
  
  #draw the ees plots
  for(category in categories[1:3])
  {
    these_labels = y_labels[[type]][[category]]
    ees_plot = ggplot()+
      geom_vline(aes(xintercept=0), col="black", alpha=1/4)+
      geom_pointrange(data=ees[ees$category == category & ees$cpd_country == "IT", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "IT", shape = "IT"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/5),
                      show.legend = T)+
      geom_pointrange(data=ees[ees$category == category & ees$cpd_country == "FR", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "FR", shape = "FR"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = 1/15),
                      show.legend = T)+
      geom_pointrange(data=ees[ees$category == category & ees$cpd_country == "SW", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "SW", shape = "SW"),
                      alpha = 1,
                      position = position_nudge(y = -1/15),
                      #size=1.3,
                      show.legend = T)+
      geom_pointrange(data=ees[ees$category == category & ees$cpd_country == "CZ", ],
                      aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "CZ", shape = "CZ"),
                      alpha = 1,
                      #size=1.3,
                      position = position_nudge(y = -1/5),
                      show.legend = T)+
      # geom_pointrange(data=ees[ees$category == category & ees$cpd_country == "POOL", ],
      #                 aes(x=estimate, xmin=lower, xmax=upper, y=level, col = "POOL", shape = "POOL"),
      #                 alpha = 1,
      #                 position = position_nudge(y = -1/5),
      #                 #size=1.3,
      #                 show.legend = T)+
      ylab("")+
      xlab(category)+
      xlim(-0.3,0.3)+
      scale_y_discrete(limits = rev(these_labels))+
      scale_color_manual(
        values = c("IT" = wesanderson::wes_palettes$Darjeeling1[1],
                   "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                   "SW" = wesanderson::wes_palettes$Darjeeling1[3],
                   "CZ" = wesanderson::wes_palettes$Darjeeling1[4]#,
                   #"POOL" = 'black'
        ),
        name = "Country",
        limits = c("IT", "FR", "SW", "CZ"#, "POOL"
                   )
      ) +
      scale_shape_manual(
        values = c("IT" = 19, 
                   "FR" = 17, 
                   "SW" = 15, 
                   "CZ" = 18#, 
                   #"POOL" = 1
        ),
        name = "Country",
        limits = c("IT",
                   "FR",
                   "SW",
                   "CZ"#, "POOL"
        )
      ) +
      theme(
        legend.position = "none",  # You can change this to "top", "bottom", etc.
        axis.text.y = element_text(size = 10, #angle = 90, 
                                   hjust = 0.5, vjust=0.5),
        axis.title.y = element_text(size = 12)
      )
    
    
    
    ees_list[[category]] = ees_plot
    
  }
  
  
  
  #Now I make a list containing for each kind of effect the (three) plots
  #related to that effect (the different attribute category)
  #This list is the argument that then gets returned to the compare_effects_bycountry
  #function 
  
  full_plot_list = list(ates_plots = ates_list,
                        acdes_plots=acdes_list,
                        ees_plots=ees_list
  )
  
  return(full_plot_list)
} 
#function to draw and save the plots related to the effect of the number of matches
# (regardless of the actual attribute displayed) into the probaility of 
#selecting someone as their conversation partners (with and without politics,
#depending on the experimental arm selected)



compare_effects_bycountry = function(data,
                                     formula, 
                                     type=c("match", "nominal"), #whether we are considering 
                                     #the nominal attributes or the recoding match vs mismatch with the respondent
                                     estimator=c("mm","amce"), #marginal means and amces
                                     arm=c("ideology_match", "ideology_mismatch"), #manipulated mediation arm with ideological match, 
                                     #or manipulated mediation arm with ideological mismatch
                                     subdir,
                                     leftlim=999,
                                     rightlim=999,
                                     x_intercept=999#the subdirectory where the plots will be saved
){
  
  
  ###### This function ends up drawing the graphs with the three effects compared like
  #Acharya et al
  
  
  type=match.arg(type)
  estimator=match.arg(estimator)
  arm=match.arg(arm)
  
  ### Compute the ATEs
  ates <- data |>
    filter(cpd_exparm2 == "natural") |>
    cj(formula, id = ~respid,
       estimate = estimator,
       by= ~cpd_country)
  
  # ates_pooled = data |>
  #   filter(cpd_exparm2 == "natural") |>
  #   cj(formula, id = ~respid,
  #      estimate = estimator)
  # 
  # ates_pooled$cpd_country = "POOL"
  # ates_pooled$BY = "POOL"
  
  # ates=rbind(ates, ates_pooled)
  
  ###Compute the ACDEs
  acdes <- data |>
    filter(cpd_exparm2 == arm) |>
    cj(formula, 
       id = ~respid,
       estimate = estimator,
       by = ~cpd_country)
  
  ###browser()
  
  # acdes_pooled <- data |>
  #   filter(cpd_exparm2 == arm) |>
  #   cj(formula, 
  #      id = ~respid,
  #      estimate = estimator)
  # 
  # 
  # acdes_pooled$cpd_country = "POOL"
  # acdes_pooled$BY = "POOL"
  # 
  # acdes=rbind(acdes, acdes_pooled)
  
  
  ### Compute the EEs
  ####browser()
  ees = data.frame()
  for(context in c("IT","FR","SW","CZ"))
  {
    ees_country = data |>
      filter(cpd_country == context & (cpd_exparm2 == "natural" | cpd_exparm2 == arm)) |>
      cj(formula,
         id = ~respid,
         estimate = paste0(estimator, "_differences"),
         by = ~cpd_exparm)
    
    ees_country$cpd_country = context
    
    ees=rbind(ees, ees_country)
  }
  
  # ees_pooled = data |>
  #   filter(cpd_exparm2 == "natural" | cpd_exparm2 == arm) |>
  #   cj(formula,
  #      id = ~respid,
  #      estimate = paste0(estimator, "_differences"),
  #      by = ~cpd_exparm)
  # 
  # ees_pooled$cpd_country = "POOL"
  # 
  # ######browser()
  # 
  # ees = rbind(ees, ees_pooled)
  
  
  #browser()
  
  temp_ref_cat = ates |> filter(is.na(std.error))
  
  temp_ref_cat$cpd_exparm = "natural"

  ees=rbind(ees, temp_ref_cat)
  ##Set the categories and levels for the three datasets
  
  ates = set_categories_and_levels_bycountry(ates,
                                             type,
                                             nominal_attributes=nominal_attributes)
  
  acdes = set_categories_and_levels_bycountry(acdes,
                                              type,
                                              nominal_attributes=nominal_attributes)
  
  ees = set_categories_and_levels_bycountry(ees,
                                            type,
                                            nominal_attributes=nominal_attributes)
  
  #Call draw effects with the for_comparison argument ==T, which means that it will return
  #the vector separately, not the already assembled immage
  

  x_intercept = ifelse(estimator!="mm_differences", 0, 0.5)
  
  #browser()
  plots = draw_compared_effects_bycountry(ates,
                                          acdes,
                                          ees,
                                          type = type,
                                          categories=categories,
                                          estimator=estimator,
                                          y_labels=y_labels_plots,
                                          leftlim=leftlim,
                                          rightlim=rightlim,
                                          x_intercept = x_intercept
  )
  pates=plots$ates_plots
  
  pacdes=plots$acdes_plots
  
  pees=plots$ees_plots
  
  #Now I assemble three plots (for each category) so that they are easy to compare
  
  p_socio = (pates[["Sociodemographics"]]+labs(title = "Average Treatment Effects (ATEs)"))/(pacdes[["Sociodemographics"]]+labs(title = "Average Conditional Direct Effects (ACDEs)"))/(pees[["Sociodemographics"]]+labs(title = "Eliminated Effects (EEs)"))
  
  p_psycho = (pates[["Psychological"]]+labs(title = "Average Treatment Effects (ATEs)"))/(pacdes[["Psychological"]]+labs(title = "Average Conditional Direct Effects (ACDEs)"))/(pees[["Psychological"]]+labs(title = "Eliminated Effects (EEs)"))
  
  p_lifestyle = (pates[["Lifestyle"]]+labs(title = "Average Treatment Effects (ATEs)"))/(pacdes[["Lifestyle"]]+labs(title = "Average Conditional Direct Effects (ACDEs)"))/(pees[["Lifestyle"]]+labs(title = "Eliminated Effects (EEs)"))
  
  
  
  #I save the three plots
  
  
  ggsave(paste0(output_wd,"estimations/", subdir,"socio_bycountry.png"), 
         p_socio, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir,"socio_bycountry.rds"))
  
  ggsave(paste0(output_wd,"estimations/", subdir,"psycho_bycountry.png"), 
         p_psycho, 
         height = 10, 
         width = 10)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir,"psycho_bycountry.rds"))
  
  
  ggsave(paste0(output_wd,"estimations/", subdir,"lifestyle_bycountry.png"), 
         p_lifestyle, 
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", subdir,"lifestyle_bycountry.rds"))
  
  
  return(plots)
}








#Our categories of apolitical traits
categories= c("Sociodemographics", "Psychological", "Lifestyle", "Political")

#Our levels regarding match and mismatches (for labeling)
y_labels_match = list(Sociodemographics=c("Gender Mismatch", "Gender Match",
                                          "Age Mismatch", "Age Match",
                                          "Educ Mismatch", "Educ Match",
                                          "Regionfeel Mismatch", "Regionfeel Match"),
                      Psychological = c("Consc Mismatch", "Consc Match", 
                                        "Ope Mismatch", "Ope Match"),
                      Lifestyle =c("Diet Mismatch", "Diet Match",
                                   "Animal Mismatch", "Animal Match",
                                   "Holiday Mismatch", "Holiday Match"
                      ),
                      Political = c("Ideology Mismatch",
                                    "Ideology Match"))


nominal_attributes= c("Gender", "Gender",
                      "Age", "Age","Age",
                      "Education","Education",
                      "Regionfeel","Regionfeel","Regionfeel","Regionfeel",
                      "Regionfeel","Regionfeel","Regionfeel","Regionfeel",
                      "Regionfeel","Regionfeel","Regionfeel",
                      "Conscientiousness","Conscientiousness","Conscientiousness",
                      "Openness","Openness","Openness",
                      "Diet","Diet","Diet",
                      "Animal","Animal","Animal",
                      "Holiday","Holiday","Holiday")

levels_vector= c("Female", "Male",
                 "Under 35", "Between 35 and 59","Over 60",
                 "Degree","No degree",
                 "Cechia (CZ)","Center (IT)", "Gotland (SW)",
                 "Moravia (CZ)", "No Paris (FR)", "North (IT)",
                 "Norrland (SW)", "Paris (FR)", "Prague (CZ)",
                 "South (IT)", "Svealand (SW)",
                 "High Consc.","Med. Consc.","Low Consc.",
                 "High Ope.","Med. Ope.","Low Ope.",
                 "Omnivore","Vegetarian","Vegan",
                 "Cat","Dog","No pet",
                 "City","Outdoor","Relax")

y_labels_nominal = list(Sociodemographics = c("Female", "Male",
                                              "Under 35", "Between 35 and 59","Over 60",
                                              "Degree","No degree",
                                              "Cechia (CZ)","Center (IT)", "Gotland (SW)",
                                              "Moravia (CZ)", "No Paris (FR)", "North (IT)",
                                              "Norrland (SW)", "Paris (FR)", "Prague (CZ)",
                                              "South (IT)", "Svealand (SW)"),
                        Psychological = c("High Consc.","Med. Consc.","Low Consc.",
                                          "High Ope.","Med. Ope.","Low Ope."),
                        Lifestyle = c("Omnivore","Vegetarian","Vegan",
                                      "Cat","Dog","No pet",
                                      "City","Outdoor","Relax"),
                        Political = c("Right-wing",
                                      "Left-wing",
                                      "Center",
                                      "Not collocated"))



y_labels_plots=list(match=y_labels_match, 
                    nominal=y_labels_nominal)

#Our nominal attributes (here called nominal_attributes)




formula_match = cpd_chosen ~  cpd_match_gender + cpd_match_age + 
  cpd_match_educ + cpd_match_regionfeel +
  cpd_match_consc + cpd_match_ope +
  cpd_match_diet + cpd_match_animal + cpd_match_holiday

formula_nominal = cpd_chosen ~  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday

#############################################################


#dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
#gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = paste0(gdrive_code, "VIPOP_SURVEY/analyses/conjoint_parallel_design/")

data = readRDS(paste0(gdrive_code, "VIPOP_SURVEY/dataset_finali_per_analisi/cjdata_cpd_POOL.RDS"))


#############################################################

####################################
############ ATEs (MATCH/MISMATCH)
######################################

# Estimation: The marginal mean associated with S_i^k=1 for respondents 
#in the natural mediation arm

# Interpretation: The total effect that observing similarity in attribute k 
#has on the willingness to engage in political conversation when no political 
#information is given to the respondent, whether through political inferences or not.

subdir = "ATEs/match/MMs/"


full_analysis_bycountry(data, 
                        formula_match,
                        "ATEs",
                        "match",
                        "mm",
                        "natural",
                        leftlim=0.35,
                        rightlim=0.65,
                        subdir)


### Same as before, but with AMCes (for appendix)
subdir = "ATEs/match/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "ATEs",
                        "match",
                        "amce",
                        "natural",
                        leftlim=-0.15,
                        rightlim=0.15,
                        subdir)



############ ATEs (nominal value)

subdir = "ATEs/nominal/MMs/"


full_analysis_bycountry(data, 
                        formula_nominal,
                        "ATEs",
                        "nominal",
                        "mm",
                        "natural",
                        leftlim=0.35,
                        rightlim=0.65,
                        subdir)


#same but with amce

subdir = "ATEs/nominal/AMCEs/"

full_analysis_bycountry(data, 
                        formula_nominal,
                        "ATEs",
                        "nominal",
                        "amce",
                        "natural",
                        leftlim=-0.15,
                        rightlim=0.15,
                        subdir = subdir)


########################################
############ ADCEs (MATCH/MISMATCH)#####
########################################

#ESTIMATION
# The marginal mean associated with S_i^k=1 for respondents 
# in the maniulated mediation arm with ideological similarity condition

# The marginal mean associated with S_i^k=1 for respondents in the 
#maniulated mediation arm with ideological dissimilarity condition

# INTERPRETATION
#The effects that similarity in each attribute k has on the willingness to
#engage in political conversations that is due neither to mediation nor to 
#interaction with political inferences.

######################################
#### ACDEs for ideological match with MM
######################################

subdir = "ACDEs/match/MMs/"


full_analysis_bycountry(data, 
                        formula_match,
                        "ACDEs",
                        "match",
                        "mm",
                        "ideology_match",
                        leftlim=0.4,
                        rightlim=0.6,
                        subdir)



######################################
#### ACDEs for ideological mismatch with MM
###################################### 

subdir = "ACDEs/mismatch/MMs/"


full_analysis_bycountry(data, 
                        formula_match,
                        "ACDEs",
                        "match",
                        "mm",
                        "ideology_mismatch",
                        leftlim=0.4,
                        rightlim=0.6,
                        subdir)

######################################
#### ACDEs for ideological match with AMCE
###################################### 

subdir = "ACDEs/match/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "ACDEs",
                        "match",
                        "amce",
                        "ideology_match",
                        leftlim=-0.1,
                        rightlim=0.1,
                        subdir)

############################################################################
################ ACDEs for ideological mismatch with AMCE################### 
############################################################################

subdir = "ACDEs/mismatch/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "ACDEs",
                        "match",
                        "amce",
                        "ideology_mismatch",
                        leftlim=-0.1,
                        rightlim=0.1,
                        subdir)


############################################################################
########################## ELIMINATED EFFECTS ##############################
############################################################################

#ESTIMATION
# Point estimation: the difference between ATE and ACDE. 
#for standard errors see Acharya et al. (2018) Or Lòpez-Ortega 2023

#INTERPRETATION
# The portion of the ATE explained by political inferences, either through 
#mediation or through interaction between S_i^k and the specific ideology inferred.


##### ELIMINATED EFFECTS WITH MM FOR IDEOLOGICAL MATCH

subdir = "EEs/match/MMs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "EEs",
                        "match",
                        "mm",
                        "ideology_match",
                        leftlim=-0.2,
                        rightlim=0.2,
                        subdir)


##### ELIMINATED EFFECTS WITH MM FOR IDEOLOGICAL MISMATCH

subdir = "EEs/mismatch/MMs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "EEs",
                        "match",
                        "mm",
                        "ideology_mismatch",
                        leftlim=-0.2,
                        rightlim=0.2,
                        subdir)


##### ELIMINATED EFFECTS WITH AMCE FOR IDEOLOGICAL MATCH

subdir = "EEs/match/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "EEs",
                        "match",
                        "amce",
                        "ideology_match",
                        leftlim=-0.2,
                        rightlim=0.2,
                        subdir)

##### ELIMINATED EFFECTS WITH AMCE FOR IDEOLOGICAL MISMATCH

subdir = "EEs/mismatch/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "EEs",
                        "match",
                        "amce",
                        "ideology_mismatch",
                        leftlim=-0.2,
                        rightlim=0.2,
                        subdir)



#########################################
### Now I want to have the three effects close to each other. How do I do that?

subdir="CompareEffects/Ideology_match/MMs"


plots_match = compare_effects_bycountry(data,
                                        formula_match,
                                        type="match", #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                                        estimator="mm", #marginal means and amces
                                        arm="ideology_match", #manipulated mediation arm with ideological match, 
                                        #or manipulated mediation arm with ideological mismatch
                                        subdir,#the subdirectory where the plots will be saved
                                        leftlim=0.35,
                                        rightlim=0.65#,
                                        #x_intercept=0.5
)


subdir="CompareEffects/Ideology_match/AMCEs/"

plots_match = compare_effects_bycountry(data,
                                        formula_match,
                                        type="match", #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                                        estimator="amce", #marginal means and amces
                                        arm="ideology_match", #manipulated mediation arm with ideological match, 
                                        #or manipulated mediation arm with ideological mismatch
                                        subdir,#the subdirectory where the plots will be saved
                                        leftlim=-0.2,
                                        rightlim=0.2#,
                                        #x_intercept=0.5
)



subdir="CompareEffects/Ideology_mismatch/"


#####browser()
plots_mismatch = compare_effects_bycountry(data,
                                           formula_match,
                                           type="match", #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                                           estimator="mm", #marginal means and amces
                                           arm="ideology_mismatch", #manipulated mediation arm with ideological match, 
                                           #or manipulated mediation arm with ideological mismatch
                                           subdir,#the subdirectory where the plots will be saved
                                           leftlim=0.35,
                                           rightlim=0.65#,
                                           #x_intercept=0.5
)


#Now I also draw the plots like Achara et al figure 1, namely by comparing EEs on the
#two manipulated mediation arms
subdir="CompareEffects/ATES_vs_EEs/"

plots_match$ates_plots$Sociodemographics = plots_match$ates_plots$Sociodemographics + labs(title = "ATEs (natural mediation arm)")

plots_match$ees_plots$Sociodemographics  = (plots_match$ees_plots$Sociodemographics + labs(title = "EEs (ideological similarity)"))

plots_mismatch$ees_plots$Sociodemographics = (plots_mismatch$ees_plots$Sociodemographics  + labs(title = "EEs (ideological dissimilarity)"))

p = plots_match$ates_plots$Sociodemographics | plots_match$ees_plots$Sociodemographics  | plots_mismatch$ees_plots$Sociodemographics                   


ggsave(paste0(output_wd,"estimations/", subdir, "sociodemographics_bycountry.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

saveRDS(p, file = paste0(output_wd,"estimations/", subdir, "sociodemographics_bycountry.rds"))


plots_match$ates_plots$Psychological = (plots_match$ates_plots$Psychological + labs(title = "ATEs (natural mediation arm)"))

plots_match$ees_plots$Psychological  = (plots_match$ees_plots$Psychological + labs(title = "EEs (ideological similarity)"))

plots_mismatch$ees_plots$Psychological = (plots_mismatch$ees_plots$Psychological  + labs(title = "EEs (ideological dissimilarity)"))

p = plots_match$ates_plots$Psychological | plots_match$ees_plots$Psychological  | plots_mismatch$ees_plots$Psychological                   


ggsave(paste0(output_wd,"estimations/", subdir, "psychological_bycountry.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

saveRDS(p, file = paste0(output_wd,"estimations/", subdir, "psychological_bycountry.rds"))



plots_match$ates_plots$Lifestyle = (plots_match$ates_plots$Lifestyle + labs(title = "ATEs (natural mediation arm)"))

plots_match$ees_plots$Lifestyle  = (plots_match$ees_plots$Lifestyle + labs(title = "EEs (ideological similarity)"))

plots_mismatch$ees_plots$Lifestyle = (plots_mismatch$ees_plots$Lifestyle  + labs(title = "EEs (ideological dissimilarity)"))

p = plots_match$ates_plots$Lifestyle | plots_match$ees_plots$Lifestyle  | plots_mismatch$ees_plots$Lifestyle                   


ggsave(paste0(output_wd,"estimations/", subdir, "Lifestyle_bycountry.png"), 
       p, 
       height = 10, 
       width = 10, create.dir = T)

saveRDS(p, file = paste0(output_wd,"estimations/", subdir, "Lifestyle_bycountry.rds"))
