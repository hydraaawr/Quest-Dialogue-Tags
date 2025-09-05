## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133
## combines base cc fishing + sfo changes

library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## sfo treatment #######################################################

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_sti <- read.csv(".\\dbs\\db_DIAL_STI.csv", sep = ";")

## Procedure

db_dial_sti_merged <- shaper(db_dial_sti)

## db with massively classified QNAM
massclasser_STI <- function(db_dial_merged){

  db_dial_massclass <- db_dial_merged %>%
        mutate( ## clasify type of quest
          QNAM_type = case_when(
          str_detect(QNAM, "^MS") ~ "MS", ## Side quests
          str_detect(QNAM, "STI") ~ "STI" ## STI added Quest
          )
        ) 

  return(db_dial_massclass)

}


db_dial_sti_massclass <- db_dial_sti_merged %>%
  massclasser_STI()
      


## ready for json db (filtering and adding tags)

db_dial_sti_json_ready <- db_dial_sti_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type),
        ## Exclude without scripts
        !is.na(Scriptname) | str_detect(FULL,"is up to something") 
      ) %>%
          rm_na_renamer_full_rnam()


#### base db #####################################################
## Loading

load(".\\Resources\\DIAL_treatment_skyrim.esm.RData")




## Patching #############################################################################


db_dial_skyrim.esm_sti_json_ready <- patcher_exclude(db_dial_skyrim.esm_json_ready, db_dial_sti_json_ready)[[1]]
db_dial_sti_new_json_ready <- patcher_exclude(db_dial_skyrim.esm_json_ready, db_dial_sti_json_ready)[[2]]

## Json generation:


json_skyrim.esm_sti <- json_gen(db_dial_skyrim.esm_sti_json_ready,"Skyrim.esm", "NA (Quest)")

json_sti_new <- json_gen(db_dial_sti_new_json_ready, "SaveTheIcerunner.esp", "NA (Quest)")


## bind them

json_main <- paste0(
  '[', 
  gsub('\\[|\\]', '', json_skyrim.esm_sti),
  ',',
  gsub('\\[|\\]', '', json_sti_new), 
  ']'
)

## Export

write(json_main, ".\\SKSE\\Plugins\\DynamicStringDistributor\\SaveTheIcerunner.esp\\QuestDialogueTagsSaveTheIcerunner.esp.json")


