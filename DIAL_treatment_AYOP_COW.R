## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## treatment #######################################################

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_ayop_cow <- read.csv(".\\dbs\\db_DIAL_AYOP_COW.csv", sep = ";")

## Procedure

db_dial_ayop_cow_merged <- shaper(db_dial_ayop_cow)

## db with massively classified QNAM
massclasser_AYOP_COW <- function(db_dial_merged){

  db_dial_massclass <- db_dial_merged %>%
        mutate( ## clasify type of quest
          QNAM_type = case_when(
          str_detect(QNAM, "^MG") ~ "MG", ## Mages Guild
          str_detect(QNAM, "AYOP") ~ "AYOP_COW" ## AYOP_COW added Quest
          )
        ) 

  return(db_dial_massclass)

}



## ready for json db (filtering and adding tags)

db_dial_ayop_cow_massclass <- db_dial_ayop_cow_merged %>%
  massclasser_AYOP_COW()
      


custom_filter_AYOP_COW <- function(db_dial_massclass){ ## custom manual filters

  db_dial_massclass <- db_dial_massclass %>%
    filter(

      !str_detect(FULL,"AYOPMGQuestTolfdirShared1Saarthal")
    )
    return(db_dial_massclass)
}




db_dial_ayop_cow_json_ready <- db_dial_ayop_cow_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type) 
      ) %>%
        custom_filter_AYOP_COW() %>% 
          rm_na_renamer_full_rnam()


#### base db #####################################################
## Loading

load(".\\Resources\\DIAL_treatment_skyrim.esm.RData")




## Patching #############################################################################


db_dial_skyrim.esm_ayop_cow_json_ready <- patcher_exclude(db_dial_skyrim.esm_json_ready, db_dial_ayop_cow_json_ready)[[1]]
db_dial_ayop_cow_new_json_ready <- patcher_exclude(db_dial_skyrim.esm_json_ready, db_dial_ayop_cow_json_ready)[[2]]

## Json generation:


json_skyrim.esm_ayop_cow <- json_gen(db_dial_skyrim.esm_ayop_cow_json_ready,"Skyrim.esm", "NA (Quest)")

json_ayop_cow_new <- json_gen(db_dial_ayop_cow_new_json_ready, "At Your Own Pace - Main Quest.esp", "NA (Quest)")


## bind them

json_main <- paste0(
  '[', 
  gsub('\\[|\\]', '', json_skyrim.esm_ayop_cow),
  ',',
  gsub('\\[|\\]', '', json_ayop_cow_new), 
  ']'
)

## Export

write(json_main, ".\\SKSE\\Plugins\\DynamicStringDistributor\\At Your Own Pace - College of Winterhold.esp\\QuestDialogueTagsAt_Your_Own_Pace_College_of_Winterhold.esp.json")


