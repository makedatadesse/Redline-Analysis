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

## API key: 3c9e13d499f9ed0c1d8f1c848dd6919fc443afad
census_api_key("3c9e13d499f9ed0c1d8f1c848dd6919fc443afad")


## hennepin county poverty status dataframe
pov <- c(
  Poverty = "S1701_C03_001E",
  TotalPopulation = "S1701_C01_001E")

povdata <- get_acs(geography = "tract", 
                         variables = pov,
                         state = "Minnesota",
                         county = "Hennepin",
                         output = "wide",
                         year = 2022)

ls(povdata)
povdata <- povdata %>% select(-c(S1701_C01_001M, S1701_C03_001M))

## import redlining csv
setwd("~/Desktop/Redlining")
redline <- read.csv("2020TractRedlining.csv")

minneapolis_data <- redline %>%
  filter(city == "Minneapolis")

minneapolis_data <- minneapolis_data %>%
  filter(grade != "")

povdata$GEOID <- as.numeric(povdata$GEOID)
minneapolis_data$GEOID <- as.numeric(minneapolis_data$GEOID)

pov_minneapolis <- povdata %>%
  inner_join(minneapolis_data, by = "GEOID")

## Create new columns based on 'grade'
pov_minneapolis <- pov_minneapolis %>%
  mutate(Agrade = ifelse(grade == "A", 1, 0))

pov_minneapolis <- pov_minneapolis %>%
  mutate(Bgrade = ifelse(grade == "B", 1, 0))

pov_minneapolis <- pov_minneapolis %>%
  mutate(Cgrade = ifelse(grade == "C", 1, 0))

pov_minneapolis <- pov_minneapolis %>%
  mutate(Dgrade = ifelse(grade == "D", 1, 0))

## New dataframe to remove duplicates and NA values
pov_minneapolis <- pov_minneapolis %>%
  drop_na(Poverty, TotalPopulation)

redline_pov <- povdata %>%
  left_join(pov_minneapolis, by = "GEOID")

colnames(redline_pov)[2] ="NAME"
colnames(redline_pov)[3] ="Poverty"
colnames(redline_pov)[4] ="TotalPop"

cleaned_pov <- pov_minneapolis %>%
  group_by(GEOID) %>%
  summarise(
    NAME = first(NAME),
    Poverty = first(Poverty),
    TotalPop = first(Poverty),
    city = first(city),
    state = first(state),
    # Aggregate redlining columns
    Agrade = max(Agrade),
    Bgrade = max(Bgrade),
    Cgrade = max(Cgrade),
    Dgrade = max(Dgrade)
  )

## Remove NA in Pov and TotalPop
cleaned_pov <- cleaned_pov %>% 
  drop_na(Poverty, TotalPop)

cleaned_pov <- cleaned_pov %>%
  mutate(
    city = ifelse(is.na(city), "Minneapolis", city),
    state = ifelse(is.na(state), "MN", state)
  )

## The updated dataframe to run analysis on is cleaned_pov
## The dataframe with duplicate census tracts but proper join is pov_minneapolis
## high poverty == 20%
cleaned_pov <- cleaned_pov[, -which(names(cleaned_pov) == "TotalPop")]
## redlining score was created because of the nuance of certain tracts having 
##  a history of covering more than one grade, especially since cleaned_pov aggregates the duplicates
##  so as to avoid redundency issues and treat each census tract as a unique observation.
## The logistical regression here is a linear model (model5) that 
##
redline_pov <- cleaned_pov %>%
  mutate(redlining_score = (Cgrade + Dgrade) / (Agrade + Bgrade + Cgrade + Dgrade))
model <- lm(formula = Poverty ~ redlining_score, data = redline_pov)
summary(model)


### Visualization
tracts <- tracts(state = "MN", year = 2020)
redline_pov$GEOID <- as.character(redline_pov$GEOID)
redline_geoids <- redline_pov$GEOID

minneapolis_tracts <- tracts %>%
  filter(GEOID %in% redline_geoids)

minneapolis_tracts <- minneapolis_tracts %>%
  left_join(redline_pov, by = "GEOID")

tracts <- tracts %>%
  left_join(redline_pov, by = "GEOID")
hennepin_tracts <- tracts %>%
  filter(COUNTYFP == "053")

#################
ggplot(minneapolis_tracts) +
  geom_sf(aes(fill = redlining_score), color = "black", size = 0.1) +  # Color by redlining score
  scale_fill_viridis_c(option = "D", direction = -1) +  # Customize color scale for redlining score
  theme_minimal() +
  labs(title = "Correlation of Redline Score and Poverty Rate",
       subtitle = "Census Tracts in Minnesota",
       fill = "Redline Score") +
  theme(legend.position = "right")

minneapolis_tracts <- minneapolis_tracts %>%
  mutate(poverty_rate_cat = ifelse(Poverty > 20, "High", "Low"))

## 2
minneapolis_tracts <- minneapolis_tracts %>%
  mutate(redlining_score_cat = case_when(
    redlining_score <= 0.3333333 ~ "Low",
    redlining_score > 0.3333333 & redlining_score <= 0.5000000 ~ "Medium",
    redlining_score > 0.5000000 & redlining_score<= 0.6666667 ~ "High",
    redlining_score > 0.6666667 ~ "Very High"
  ))

ggplot(minneapolis_tracts) +
  geom_sf_pattern(aes(fill = redlining_score_cat, pattern = poverty_rate_cat),
                  color = "black", size = .5, 
                  pattern_density = 0.01,
                  pattern_spacing = 0.02) +
  scale_fill_manual(values = c("Low" = "#2944ed", 
                               "Medium" = "#7EB77F", 
                               "High" = "#F6AE2D", 
                               "Very High" = "#e71d36")) + 
  scale_pattern_manual(values = c("Low" = "none", "High" = "stripe")) +  
  theme_minimal() +
  labs(title = "Redlining Score and Poverty Rate in Minneapolis",
       subtitle = "Census Tracts in Minneapolis, Minnesota",
       fill = "Redline Score",
       pattern = "Poverty Rate") +
  theme(legend.position = "right")


##############################

## binary for high poverty
cleaned_pov$high_poverty <- ifelse(cleaned_pov$Poverty > 20, 1, 0)
# binary for redlined or not
cleaned_pov$redlined <- ifelse(cleaned_pov$Cgrade == 1 | cleaned_pov$Dgrade == 1, 1, 0)
cleaned_pov$non_redlined <- ifelse(cleaned_pov$Agrade == 1 | cleaned_pov$Bgrade == 1, 1, 0)
## logistical regression
model <- glm(high_poverty ~ redlined + non_redlined, data = cleaned_pov, family = "binomial")
summary(model)
### 
write.csv(pov_minneapolis, "pov_minneapolis.csv")
## binary for high poverty
pov_minneapolis$high_poverty <- ifelse(pov_minneapolis$Poverty > 20, 1, 0)
# binary for redlined or not
pov_minneapolis <- pov_minneapolis %>%
  mutate(
    redlined = ifelse(Cgrade == 1 | Dgrade == 1, 1, 0),
    non_redlined = ifelse(Agrade == 1 | Bgrade == 1, 1, 0),
    mixed_history = ifelse(redlined == 1 & non_redlined == 1, 1, 0)
  )

## logistical regression
model2 <- glm(high_poverty ~ redlined + mixed_history + non_redlined, data = pov_minneapolis, family = "binomial")
summary(model2)


pov <- pov_minneapolis %>%
  mutate(redlining_score = (Cgrade + Dgrade) / (Agrade + Bgrade + Cgrade + Dgrade))
model3 <- glm(high_poverty ~ redlining_score, data = pov, family = "binomial")
summary(model3)

