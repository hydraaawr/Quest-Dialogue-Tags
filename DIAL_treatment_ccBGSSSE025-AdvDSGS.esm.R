## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_ccBGSSSE025_AdvDSGS.esm <- read.csv(".\\dbs\\db_DIAL_ccBGSSSE025_AdvDSGS.esm.csv", sep = ";")


db_dial_ccBGSSSE025_AdvDSGS.esm_merged <- shaper(db_dial_ccBGSSSE025_AdvDSGS.esm)


## db with massively classified QNAM

massclasser_ccBGSSSE025_AdvDSGS.esm <- function(db_dial_merged){

  db_dial_massclass <- db_dial_merged %>%
        mutate( ## clasify type of quest
          QNAM_type = case_when(
          str_detect(QNAM, "QuestA|QuestB") ~ "MQ_SS", ## Main Quest
          str_detect(QNAM, "MiscQuest|ElytraPetAcquisition|StaadaQuest") ~ "misc_SS" ## Miscellaneous
          )
        ) 

  return(db_dial_massclass)

}


db_dial_ccBGSSSE025_AdvDSGS.esm_massclass <- db_dial_ccBGSSSE025_AdvDSGS.esm_merged %>%
  massclasser_ccBGSSSE025_AdvDSGS.esm()
      


## ready for json db (filtering and adding tags)

db_dial_ccBGSSSE025_AdvDSGS.esm_json_ready <- db_dial_ccBGSSSE025_AdvDSGS.esm_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type)
      ) %>%
        rm_na_renamer_full_rnam()


################################################################################



## Json generation:


json_ccBGSSSE025_AdvDSGS.esm <- json_gen(db_dial_ccBGSSSE025_AdvDSGS.esm_json_ready,"ccBGSSSE025-AdvDSGS.esm", "NA (Quest)")

write(json_ccBGSSSE025_AdvDSGS.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\ccBGSSSE025-AdvDSGS.esm\\QuestDialogueTagsccBGSSSE025_AdvDSGS.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_ccBGSSSE025_AdvDSGS.esm.RData")