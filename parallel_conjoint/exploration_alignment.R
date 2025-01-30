 pacman::p_load(
   cregg, dplyr, ggpubr, cowplot,
   MASS, cjoint, corrplot, dplyr,
   forcats, ggplot2, gt, gtools,
   gtsummary, margins, openxlsx,
   patchwork, rio, texreg, tools
 )

#context = "IT"
#context = "FR"
#context = "CZ"
#context = "SW"
#context = "POOL"


dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"

output_wd = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/explorations/alignment"

data = readRDS("G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/data_recoded_POOL.rds")


traits = c("gender", "AGE_GROUP", "EDU_LEVEL", "region_feel", 
                       "TIPI_CON_REC", "TIPI_OPE_REC",
                       "diet", "animal", "holiday"
                       )


data = data |>
  mutate(IDEOLOGY_REC, 
         IDEOLOGY_REC = case_when(
           IDEOLOGY_REC == "right" ~ "Right (7-10)",
           IDEOLOGY_REC == "left" ~ "Left (0-3)",
           IDEOLOGY_REC == "center" ~ "Moderate (4-6)",
           IDEOLOGY_REC == "notplaced" ~ "Prefers not to collocate",
           is.na(IDEOLOGY_REC) ~ NA
         ) 
  )


# Ensure the directory exists
if (!dir.exists(output_wd)) {
  dir.create(output_wd, recursive = TRUE)
}


for(trait in traits)
{
  data_percent <- data %>%
    group_by(country, IDEOLOGY_REC, !!sym(trait)) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(country, IDEOLOGY_REC) %>%
    mutate(percentage = count / sum(count))
  
  # Generate the ggplot for the current trait
  p <- ggplot(data_percent, aes_string(x = "IDEOLOGY_REC", y = "percentage", fill = trait)) +
    geom_bar(stat = "identity", position = "dodge") +  # Separate bars by color
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),  # Format y-axis as percentages
      breaks = seq(0, max(data_percent$percentage), length.out = 5)  # Add 5 evenly spaced ticks
    ) +
    labs(
      title = paste("Percentage by of", trait, "by Ideology (Faceted by Country)"),
      x = "Ideology",
      y = "Percentage",
      fill = trait
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels if needed
      legend.position = "top"
    ) +
    facet_wrap(~country)  # Create a facet for each country
  
  # Define the output file name
  output_file <- file.path(output_wd, paste0(trait, "_percentage_by_ideology_faceted_bar_graph.png"))
  
  # Save the plot
  ggsave(output_file, plot = p, width = 12, height = 8, dpi = 300)
}



data$notplaced= ifelse(data$IDEOLOGY_REC=="Prefers not to collocate", 1, 0)

table(data$notplaced)

model = glm(data=data, notplaced ~ gender+ AGE_GROUP+ country+ EDU_LEVEL+ 
    citysize+ as.numeric(socialposition)+ interest+ exposure, 
    family = binomial)

summary(model)

library(mediation) #trying the mediation model 

data$socialposition1 = ifelse(data$socialposition>5,1,0)

mediator1_model <- glm(socialposition1 ~ gender + AGE_GROUP + country + citysize + interest + exposure, 
                       data = data, family = gaussian)  # Use appropriate family (gaussian for continuous mediator)

data$college = ifelse(data$EDU_LEVEL=="college", 1, 0)
mediator2_model <- glm(college ~ gender + AGE_GROUP + country + citysize + interest + exposure, 
                       data = data, family = gaussian)  # Use appropriate family (or ordinal regression for ordered data)

# Outcome model (including mediators)
outcome_model <- glm(notplaced ~ gender + socialposition1 + EDU_LEVEL + AGE_GROUP + country + citysize + interest + exposure, 
                     data = data, family = binomial)

mediation_socialposition <- mediate(mediator1_model, outcome_model, 
                                    treat = "gender", mediator = "socialposition1",
                                    boot = TRUE, sims = 1000)  # Bootstrapping for confidence intervals

# Mediation for EDU_LEVEL
mediation_edu_level <- mediate(mediator2_model, outcome_model, 
                               treat = "gender", mediator = "EDU_LEVEL",
                               boot = TRUE, sims = 1000)  # Bootstrapping for confidence intervals


# Summary for socialposition mediation
summary(mediation_socialposition)

# Summary for EDU_LEVEL mediation
summary(mediation_edu_level)