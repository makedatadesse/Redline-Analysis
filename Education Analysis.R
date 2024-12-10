library(moments)
library(data.table)
library(dplyr)
library(DescTools)
library(corrplot)
library(tidycensus)
library(tidyverse)
library(lsr)
library(sf)
library(ggplot2)    
library(tigris) 
library(ggpattern)
library(viridis)
library(RColorBrewer)
library(nnet)

## API key: 3c9e13d499f9ed0c1d8f1c848dd6919fc443afad
census_api_key("3c9e13d499f9ed0c1d8f1c848dd6919fc443afad")

## hennepin county educational attainment dataframe
educ <- c(
  noHSdegree = "S1501_C02_008E",
  HSgrad = "S2301_C04_001E",
  bach = "S1501_C02_012E",
  gradu = "S1501_C02_013E")

data <- get_acs(geography = "tract", 
                variables = educ,
                state = "Minnesota",
                county = "Hennepin",
                output = "wide",
                year = 2022)
ls(data)
data <- data %>% select(-c(S1501_C02_008M, S1501_C02_012M, S1501_C02_013M, S2301_C04_001M))

## import redlining csv
setwd("~/Desktop/Redlining")
redline <- read.csv("2020TractRedlining.csv")

## bind
data$GEOID <- as.numeric(data$GEOID)
redline$GEOID <- as.numeric(redline$GEOID)

data2 <- data %>%
  inner_join(redline, by = "GEOID")

## Create new columns based on 'grade'
data2 <- data2 %>%
  mutate(Agrade = ifelse(grade == "A", 1, 0))

data2 <- data2 %>%
  mutate(Bgrade = ifelse(grade == "B", 1, 0))

data2 <- data2 %>%
  mutate(Cgrade = ifelse(grade == "C", 1, 0))

data2 <- data2 %>%
  mutate(Dgrade = ifelse(grade == "D", 1, 0))

## New dataframe to remove duplicates
combined_data <- data %>%
  left_join(data2, by = "GEOID")

educ_red <- combined_data %>%
  group_by(GEOID) %>%
  summarise(
    NAME.x = first(NAME.x),
    noHSdegree.x = first(noHSdegree.x),
    HSgrad.x = first(HSgrad.x),
    bach = first(bach.x),
    gradu = first(gradu.x),
    city = first(city),
    state = first(state),
    # Aggregate redlining columns
    Agrade = max(Agrade, na.rm = TRUE),
    Bgrade = max(Bgrade, na.rm = TRUE),
    Cgrade = max(Cgrade, na.rm = TRUE),
    Dgrade = max(Dgrade, na.rm = TRUE)
  )

colnames(educ_red)[3] ="noHSdegree.D"
colnames(educ_red)[4] ="HSgrad.C"
colnames(educ_red)[5] ="bach.B"
colnames(educ_red)[6] ="gradu.A"

## highest educational attainment proportion
educ_red <- educ_red %>% 
  drop_na(noHSdegree.D, HSgrad.C, bach.B, gradu.A)

educ_red <- educ_red %>%
  mutate(
    city = ifelse(is.na(city), "Minneapolis", city),
    state = ifelse(is.na(state), "MN", state)
  )

educ_red <- educ_red %>%
  mutate(
    highest_prop = case_when(
      noHSdegree.D == pmax(noHSdegree.D, HSgrad.C, bach.B, gradu.A) ~ "D",
      HSgrad.C == pmax(noHSdegree.D, HSgrad.C, bach.B, gradu.A) ~ "C",
      bach.B == pmax(noHSdegree.D, HSgrad.C, bach.B, gradu.A) ~ "B",
      gradu.A == pmax(noHSdegree.D, HSgrad.C, bach.B, gradu.A) ~ "A"
    )
  )

colSums(is.na(educ_red))
class(educ_red)

educ_red <- educ_red[!apply(educ_red[, c("Agrade", "Bgrade", "Cgrade", "Dgrade")] == -Inf, 1, any), ]

## redlining score
educ_red <- educ_red %>%
  mutate(redlining_score = (Cgrade + Dgrade) / (Agrade + Bgrade + Cgrade + Dgrade))


# outcome variable:
educ_red$highest_prop <- factor(educ_red$highest_prop, 
                                levels = c("D", "C", "B", "A"),
                                ordered = TRUE)


## Category D (no highschool diploma) as baseline:
logit_model <- multinom(highest_prop ~ redlining_score, data = educ_red)
summary(logit_model)
exp(coefficients(logit_model))

## B, or bachelors degree, shows up the most often
table(educ_red$highest_prop)
## Making Category B (most frequent category), the reference
educ_red$highest_prop <- factor(educ_red$highest_prop, ordered = FALSE)
educ_red$highest_prop <- relevel(educ_red$highest_prop, ref = "B")

logit_model <- multinom(formula = highest_prop ~ redlining_score, data = educ_red)
summary(logit_model)
exp(coefficients(logit_model))

##

# redline binary
educ_red$Redlined <- ifelse(educ_red$Cgrade == 1 | educ_red$Dgrade == 1, 1, 0)
educ_red$NonRedlined <- ifelse(educ_red$Agrade == 1 | educ_red$Bgrade == 1, 1, 0)

############

## linear model:
lm_educ_red <- educ_red %>%
  pivot_longer(cols = c(noHSdegree.D, HSgrad.C, bach.B, gradu.A), 
               names_to = "EducationLevel",
               values_to = "Rate"
  )

lm_educ_red <- lm_educ_red %>%
  mutate(EducationLevel = factor(EducationLevel,
                                 levels = c("noHSdegree.D", "HSgrad.C", "bach.B", "gradu.A"),
                                 labels = c("NoHS", "HSGrad", "Bachelors", "Graduate")),
  Redlined = as.factor(Redlined)
  )

lm_model <- lm(Rate ~ Redlined * EducationLevel, data = lm_educ_red)
summary(lm_model)


#### Visualization:
tracts <- tracts(state = "MN", year = 2020)
educ_red$GEOID <- as.character(educ_red$GEOID)
educ_red_geoids <- educ_red$GEOID

minneapolis_eductracts <- tracts %>%
  filter(GEOID %in% educ_red_geoids)

minneapolis_eductracts <- minneapolis_eductracts %>%
  left_join(educ_red, by = "GEOID")

educ_tracts <- tracts %>%
  left_join(educ_red, by = "GEOID")
educ_hennepin_tracts <- educ_tracts %>%
  filter(COUNTYFP == "053")

minneapolis_eductracts <- minneapolis_eductracts %>%
  left_join(educ_red, by = "GEOID") 

minneapolis_eductracts <- minneapolis_eductracts %>%
  mutate(redlining_score_cat = case_when(
    redlining_score.x <= 0.3333333 ~ "Low",
    redlining_score.x > 0.3333333 & redlining_score.x <= 0.5000000 ~ "Medium",
    redlining_score.x > 0.5000000 & redlining_score.x<= 0.6666667 ~ "High",
    redlining_score.x > 0.6666667 ~ "Very High"
  ))

#################### Maps
minneapolis_eductracts <- minneapolis_eductracts %>%
  mutate(education_cat = case_when(
    highest_prop.x == "D" ~ "No High School",
    highest_prop.x == "C" ~ "High School Graduate",
    highest_prop.x == "B" ~ "Bachelors Degree",
    highest_prop.x == "A" ~ "Professional Degree",
    TRUE ~ "Unknown"
  ))

ggplot(minneapolis_eductracts) +
  geom_sf_pattern(aes(fill = redlining_score_cat, pattern = education_cat),
                  color = "black", size = 0.5, 
                  pattern_density = 0.2,
                  pattern_spacing = 0.03) + 
  scale_fill_manual(values = c("Low" = "#2944ed", 
                               "Medium" = "#7EB77F", 
                               "High" = "#F6AE2D", 
                               "Very High" = "#e71d36")) +
  scale_pattern_manual(values = c("No High School" = "stripe", 
                                  "High School Graduate" = "crosshatch", 
                                  "Bachelors Degree" = "none", 
                                  "Professional Degree" = "circle")) +
  scale_pattern_fill_manual(values = c()) +
  theme_minimal() +
  labs(title = "Redlining Score and Highest Education Level in Minneapolis",
       subtitle = "Census Tracts in Minneapolis, Minnesota",
       fill = "Redlining Score",
       pattern = "Highest Education Level") +
  theme(legend.position = "right")


################
