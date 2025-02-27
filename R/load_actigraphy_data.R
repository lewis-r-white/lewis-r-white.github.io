library(tidyverse)
library(lubridate)
library(here)

multinight_y4_final <- read_csv(here("data", "actigraphy", "multinight_y4_final.csv")) 

multinight_y8_final <- read_csv(here("data", "actigraphy", "multinight_y8_final.csv"))

singlenight_y4_final <- read_csv(here("data", "actigraphy", "singlenight_y4_final.csv"))

singlenight_y8_final <- read_csv(here("data", "actigraphy", "singlenight_y8_final.csv"))


multinight_y4_final_simple <- multinight_y4_final %>%
  select(Patient.Studyid:End.Date, Total.Time..mins.:Wake.Time..mins., Sleep.Ratio, Number.of.Awakenings) %>%
  mutate(study = "multinight_y4") %>%
  rename(mstudyid = Patient.Studyid)

multinight_y8_final_simple <- multinight_y8_final %>%
  select(record_id, vname, Start.Date, End.Date, Total.Time..mins.:Wake.Time..mins., Sleep.Ratio, Number.of.Awakenings) %>%
  mutate(study = "multinight_y8") %>%
  rename(mstudyid = record_id)

singlenight_y4_final_simple <- singlenight_y4_final %>%
  select(Patient.Studyid, vname, Start.Date, End.Date, Total.Time..mins.:Wake.Time..mins., Sleep.Ratio, Number.of.Awakenings) %>%
  mutate(study = "singlenight_y4") %>%
  rename(mstudyid = Patient.Studyid)

singlenight_y8_final_simple <- singlenight_y8_final %>%
  select(record_id, vname, Start.Date, End.Date, Total.Time..mins.:Wake.Time..mins., Sleep.Ratio, Number.of.Awakenings) %>%
  mutate(study = "singlenight_y8") %>%
  rename(mstudyid = record_id)

actigraphy_simple_full <- rbind(multinight_y4_final_simple, 
                                multinight_y8_final_simple, 
                                singlenight_y4_final_simple,
                                singlenight_y8_final_simple) %>%
  mutate(
    Start.Date = mdy(Start.Date), 
    End.Date = mdy(End.Date)
  )

y4 <- rbind(multinight_y4_final_simple, singlenight_y4_final_simple)
y8 <- rbind(multinight_y8_final_simple, singlenight_y8_final_simple)
multinight <- rbind(multinight_y4_final_simple, multinight_y8_final_simple)
singlenight <- rbind(singlenight_y4_final_simple, singlenight_y8_final_simple)
