library(readxl)
library(dplyr)
library(tidyr)

# 1. Read the matched-pair section from your Excel file
# Adjust the file path if needed

# 2. Clean names manually
matched_long <- performance_outcome %>%
  rename(
    pair_id = ID,
    participant = Participants,
    race = Race,
    trial = Trial,
    finish_time_excel = `Finsished Time`,
    finish_sec = Finished_Sec,
    pct_diff_excel = `%diff`,
    place = Place
  ) %>%
  filter(!is.na(pair_id), trial %in% c("IPC", "Sham"))

# 3. Convert to one row per matched pair
matched_wide <- matched_long %>%
  select(pair_id, participant, race, trial, finish_sec, place) %>%
  pivot_wider(
    names_from = trial,
    values_from = c(participant, race, finish_sec, place),
    names_sep = "_"
  ) %>%
  mutate(
    pct_diff_decimal = (finish_sec_IPC - finish_sec_Sham) / finish_sec_Sham,
    pct_diff_percent = pct_diff_decimal * 100
  )

# Check the matched pairs
matched_wide

################################################################################

# 4. Normality test on the paired percentage differences
shapiro.test(matched_wide$pct_diff_decimal)

# 5. One-sample t-test against zero
performance_ttest <- t.test(matched_wide$pct_diff_decimal, mu = 0)

performance_ttest

