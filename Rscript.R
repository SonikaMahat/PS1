library(tidyverse)
library(haven)

#Import dataset
South_Africa_Round_9_Data_Afrobarometer <- read_sav("South Africa_Round 9_Data_Afrobarometer.sav")
View(South_Africa_Round_9_Data_Afrobarometer)

#Dataset Description
str(South_Africa_Round_9_Data_Afrobarometer)
summary(South_Africa_Round_9_Data_Afrobarometer)

#Question: 1--------------------------------------------------------------------
#Number of respondents
nrow(South_Africa_Round_9_Data_Afrobarometer)

#Date of Interview
summary(South_Africa_Round_9_Data_Afrobarometer$DATEINTR)

#Question: 2------------------------------------------------------------------
Demo_data <- South_Africa_Round_9_Data_Afrobarometer %>% 
  select(Q1, Q2, Q100, URBRUR)

Demo_data <- Demo_data %>%
  rename(
    Gender = Q100,
    Age = Q1,
    Language = Q2,
    Location = URBRUR
  )

Demo_data <- Demo_data %>%
  mutate(Gender = case_when(
      Gender == 1 ~ "Male",
      Gender == 2 ~ "Female",
      TRUE ~ as.character(Gender)  # Keep other values as they are
    ))

Demo_data <- Demo_data %>%
  mutate(Location = case_when(
  Location == 1 ~ "Urban",
  Location == 2 ~ "Rural",
  TRUE ~ as.character(Location)))


Demo_data <- Demo_data %>%
  mutate(Language = case_when(
    Language == 1 ~ "English",
    Language == 700 ~ "Afrikaans",
    Language == 701 ~ "Ndebele",
    Language == 702 ~ "Xhosa",
    Language == 703 ~ "Pedi",
    Language == 705 ~ "Tswana",
    Language == 706 ~ "Swazi",
    Language == 707 ~ "Venda",
    Language == 708 ~ "Zulu",
    Language == 709 ~ "Shangaan/Tsonga",
    Language == 9995 ~ "Other",
    Language == 9998 ~ "Refused",
    Language == 9999 ~ "Don't Know",
    Language == -1 ~ "Missing",
    TRUE ~ as.character(Language)))

final_summary <- Demo_data %>%
  group_by(Gender) %>%
  summarise(
    Total_Respondents = n(),  # Ensures Male = 794, Female = 786
    Avg_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Min_Age = min(Age, na.rm = TRUE),
    Max_Age = max(Age, na.rm = TRUE),
    Age_Respondents = sum(!is.na(Age)),  # Count of respondents with Age data
    Unique_Languages = n_distinct(Language),  # Number of unique languages spoken
    Urban_Count = sum(Location == "Urban", na.rm = TRUE),
    Rural_Count = sum(Location == "Rural", na.rm = TRUE)
  ) %>%
  left_join(
    Demo_data %>%
      group_by(Gender, Language) %>%
      summarise(Language_Count = n(), .groups = "drop") %>%
      pivot_wider(names_from = Language, values_from = Language_Count, values_fill = list(Language_Count = 0)),
    by = "Gender"
  )

#Question: 3---------------------------------------------------------------
China_Influence <- South_Africa_Round_9_Data_Afrobarometer %>% 
  select(Q78A)

China_Influence <- China_Influence %>%
  mutate(
    Q78A = case_when(
      Q78A == 1 ~ "Very Negative",
      Q78A == 2 ~ "Somewhat Negative",
      Q78A == 3 ~ "Neither Positive nor Negative",
      Q78A == 4 ~ "Somewhat Positive",
      Q78A == 5 ~ "Very Positive",
      TRUE ~ "Don't Know / No Response"))

China_Influence <- China_Influence %>%
  count(Q78A, name = "Count")

China_Influence %>%
  knitr::kable(caption = "Economic and Political Influence of China in South Africa")

#Question 4---------------------------------------------------------------------------
USA_Influence <- South_Africa_Round_9_Data_Afrobarometer %>% 
  select(Q78B)

USA_Influence <- USA_Influence %>%
  mutate(
    Q78B = case_when(
      Q78B == 1 ~ "Very Negative",
      Q78B == 2 ~ "Somewhat Negative",
      Q78B == 3 ~ "Neither Positive nor Negative",
      Q78B == 4 ~ "Somewhat Positive",
      Q78B == 5 ~ "Very Positive",
      TRUE ~ "Don't Know / No Response"))

USA_Influence<- USA_Influence %>%
  count(Q78B, name = "Count")

USA_Influence %>%
  knitr::kable(caption = "Economic and Political Influence of USA in South Africa")

#Question 5---------------------------------------------------------------------------
Perception_USA_China <- South_Africa_Round_9_Data_Afrobarometer %>%
  select(Q78A, Q78B)

Perception_USA_China <- Perception_USA_China %>%
  mutate(
    across(
      Q78A:Q78B,  # Variables to clean
      ~ if_else(.x %in% 1:5, .x, NA_real_)))  # Keep only valid responses, replace others with NA

t.test(Perception_USA_China$Q78A, Perception_USA_China$Q78B, paired = TRUE)      
