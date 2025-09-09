## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## treatment #######################################################

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_ayop_main <- read.csv(".\\dbs\\db_DIAL_AYOP_main.csv", sep = ";")

## Procedure

db_dial_ayop_main_merged <- shaper(db_dial_ayop_main)

## db with massively classified QNAM
massclasser_AYOP_main <- function(db_dial_merged){

  db_dial_massclass <- db_dial_merged %>%
        mutate( ## clasify type of quest
          QNAM_type = case_when(
          str_detect(QNAM, "^MQ") ~ "MQ", ## Main Quest
          str_detect(QNAM, "^AYOP") ~ "AYOP_main" ## AYOP_main added Quest
          )
        ) 

  return(db_dial_massclass)

}


db_dial_ayop_main_massclass <- db_dial_ayop_main_merged %>%
  massclasser_AYOP_main()
      






db_dial_ayop_main_json_ready <- db_dial_ayop_main_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type) 
      ) %>%
        rm_na_renamer_full_rnam()


#### base db #####################################################
## Loading

load(".\\Resources\\DIAL_treatment_skyrim.esm.RData")




## Patching #############################################################################


db_dial_skyrim.esm_ayop_main_json_ready <- patcher_exclude(db_dial_skyrim.esm_json_ready, db_dial_ayop_main_json_ready)[[1]]
db_dial_ayop_main_new_json_ready <- patcher_exclude(db_dial_skyrim.esm_json_ready, db_dial_ayop_main_json_ready)[[2]]

## Json generation:


json_skyrim.esm_ayop_main <- json_gen(db_dial_skyrim.esm_ayop_main_json_ready,"Skyrim.esm", "NA (Quest)")

json_ayop_main_new <- json_gen(db_dial_ayop_main_new_json_ready, "At Your Own Pace - Main Quest.esp", "NA (Quest)")


## bind them (NOT NECESSARY BECAUSE THERE'S NO MODIFICATIONS OF SKYRIM PLUGIN)

# json_main <- paste0(
#   '[', 
#   gsub('\\[|\\]', '', json_skyrim.esm_ayop_main),
#   ',',
#   gsub('\\[|\\]', '', json_ayop_main_new), 
#   ']'
# )

## Export

write(json_ayop_main_new, ".\\SKSE\\Plugins\\DynamicStringDistributor\\At Your Own Pace - Main Quest.esp\\QuestDialogueTagsAt_Your_Own_Pace_Main_Quest.esp.json")


