## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_main_functions_v2.1.R")
db_dial_ccBGSSSE025_AdvDSGS.esm <- read.csv(".\\dbs\\db_DIAL_ccBGSSSE025_AdvDSGS.esm_v1.csv", sep = ";")


db_dial_ccBGSSSE025_AdvDSGS.esm_merged <- shaper(db_dial_ccBGSSSE025_AdvDSGS.esm)


## db with massively classified QNAM

db_dial_ccBGSSSE025_AdvDSGS.esm_massclass <- db_dial_ccBGSSSE025_AdvDSGS.esm_merged %>%
      mutate( ## clasify type of quest
        QNAM_type = case_when(
          str_detect(QNAM, "QuestA|QuestB") ~ "MQ_SS", ## Main Quest
          str_detect(QNAM, "MiscQuest|ElytraPetAcquisition|StaadaQuest") ~ "misc_SS" ## Miscellaneous
        )
      )
      


## ready for json db (filtering and adding tags)

db_dial_ccBGSSSE025_AdvDSGS.esm_json_ready <- db_dial_ccBGSSSE025_AdvDSGS.esm_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type)
      ) %>%
        # filter(
        #   # Remove entries with rejection phrases because those might or might not contain scriptname
        #   !str_detect(FULL, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather") |
        #   !str_detect(RNAM, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not")
        # ) %>%
          filter(
            # Ensure at least one of RNAM or FULL has a value
            !is.na(RNAM) | !is.na(FULL)
          ) %>% 
            mutate(
              # Replace any RNAM containing "TIF_" with "NA (Quest)"
              RNAM = if_else(str_detect(RNAM, "TIF_"), "NA", RNAM),
              FULL_trans = paste0(FULL, " (Quest)"), ## Add "(Quest)"
              RNAM_trans = paste0(RNAM, " (Quest)")
            )


################################################################################



## Json generation:


json_ccBGSSSE025_AdvDSGS.esm <- json_gen(db_dial_ccBGSSSE025_AdvDSGS.esm_json_ready,"ccBGSSSE025-AdvDSGS.esm", "NA (Quest)")

write(json_ccBGSSSE025_AdvDSGS.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\ccBGSSSE025-AdvDSGS.esm\\QuestDialogueTagsccBGSSSE025_AdvDSGS.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_ccBGSSSE025_AdvDSGS.esm.RData")