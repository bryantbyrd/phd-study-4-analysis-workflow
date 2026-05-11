install.packages("car")

library(readxl)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)

# Load files
bio <- read_excel("Biomarkers.xlsx")
bf <- read_excel("Body Fat.xlsx")

# Clean body fat file
bf_clean <- bf %>%
  rename(
    ID = `Code Name post`,
    Body_Fat = `Body Fat`
  ) %>%
  mutate(ID = as.character(ID))

# Clean biomarker file and merge body fat
bio_clean <- bio %>%
  mutate(
    ID = as.character(ID),
    Group = factor(Group),
    Timepoint = factor(Timepoint, levels = c("Before", "Immediate"))
  ) %>%
  left_join(bf_clean, by = "ID")

# Check that body fat merged correctly
bio_clean %>%
  select(ID, Group, Timepoint, Body_Fat, CRP, IL_6, IL_10, `TNF_α`, TAC) %>%
  head()