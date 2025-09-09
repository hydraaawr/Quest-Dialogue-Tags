## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133
## combines base cc fishing + sfo changes

library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## sfo treatment #######################################################

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_ayop_su <- read.csv(".\\dbs\\db_DIAL_AYOP_SU.csv", sep = ";")

## Procedure

db_dial_ayop_su_merged <- shaper(db_dial_ayop_su)

## db with massively classified QNAM
massclasser_AYOP_SU <- function(db_dial_merged){

  db_dial_massclass <- db_dial_merged %>%
        mutate( ## clasify type of quest
          QNAM_type = case_when(
          str_detect(QNAM, "^MQ") ~ "MQ", ## Main Quest
          str_detect(QNAM, "^AYOP") ~ "AYOP_SU" ## AYOP_SU added Quest
          )
        ) 

  return(db_dial_massclass)

}


db_dial_ayop_su_massclass <- db_dial_ayop_su_merged %>%
  massclasser_AYOP_SU()
      






db_dial_ayop_su_json_ready <- db_dial_ayop_su_massclass %>%
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


db_dial_skyrim.esm_ayop_su_json_ready <- patcher_exclude(db_dial_skyrim.esm_json_ready, db_dial_ayop_su_json_ready)[[1]]
db_dial_ayop_su_new_json_ready <- patcher_exclude(db_dial_skyrim.esm_json_ready, db_dial_ayop_su_json_ready)[[2]]

## Json generation:


json_skyrim.esm_ayop_su <- json_gen(db_dial_skyrim.esm_ayop_su_json_ready,"Skyrim.esm", "NA (Quest)")

json_ayop_su_new <- json_gen(db_dial_ayop_su_new_json_ready, "At Your Own Pace - Skyrim Unbound.esp", "NA (Quest)")


## bind them (NOT NECESSARY BECAUSE THERE'S NO MODIFICATIONS OF SKYRIM PLUGIN)

# json_main <- paste0(
#   '[', 
#   gsub('\\[|\\]', '', json_skyrim.esm_ayop_su),
#   ',',
#   gsub('\\[|\\]', '', json_ayop_su_new), 
#   ']'
# )

## Export

write(json_ayop_su_new, ".\\SKSE\\Plugins\\DynamicStringDistributor\\At Your Own Pace - Skyrim Unbound.esp\\QuestDialogueTagsAt_Your_Own_Pace_Skyrim_Unbound.esp.json")


