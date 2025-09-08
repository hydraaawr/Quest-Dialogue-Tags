## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133
## combines base game + 2 dlcs with ussep changes

library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## USSEP treatment #######################################################

## Loading

load(".\\Resources\\DIAL_treatment_skyrim.esm.RData")
load(".\\Resources\\DIAL_treatment_dawnguard.esm.RData")
load(".\\Resources\\DIAL_treatment_dragonborn.esm.RData")
load(".\\Resources\\DIAL_treatment_hearthfires.esm.RData")


source(".\\DIAL_treatment_functions_v4.0.R")

db_dial_ussep <- read.csv(".\\dbs\\db_DIAL_USSEP.csv", sep = ";")

## Procedure

db_dial_ussep_merged <- shaper(db_dial_ussep)

## db with massively classified QNAM

db_dial_ussep_massclass <- db_dial_ussep_merged %>%
  mutate( ## work independently
    QNAM_type_skyrim = massclasser_skyrim.esm(.)$QNAM_type,
    QNAM_type_dawnguard = massclasser_dawnguard.esm(.)$QNAM_type,
    QNAM_type_dragonborn = massclasser_dragonborn.esm(.)$QNAM_type,
    QNAM_type_hearthfires = massclasser_hearthfires.esm(.)$QNAM_type
  ) %>%
  mutate( # to then merge, so all massclasser work as if they worked simultaneously
    QNAM_type = coalesce(QNAM_type_skyrim, QNAM_type_dawnguard, QNAM_type_dragonborn, QNAM_type_hearthfires)
  ) %>%
  select(-QNAM_type_skyrim, -QNAM_type_dawnguard, -QNAM_type_dragonborn, -QNAM_type_hearthfires)
      


## ready for json db (filtering and adding tags)

rejection_vector_ussep <- paste(
  rejection_vector_skyrim.esm,
  rejection_vector_dawnguard.esm,
  rejection_vector_dragonborn.esm,
  rejection_vector_hearthfires.esm,
  sep = "|"
)





db_dial_ussep_json_ready <- db_dial_ussep_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type)
      ) %>%
        rumor_gen_skyrim.esm() %>%
          rumor_gen_dawnguard.esm() %>%
            rumor_gen_dragonborn.esm() %>%
                custom_filter_skyrim.esm() %>%
                  custom_filter_dawnguard.esm() %>%
                      custom_filter_hearthfires.esm() %>%
                        filter_rejection_phrases(rejection_vector_ussep) %>%
                          rm_na_renamer_full_rnam()





## Patching #############################################################################


db_dial_vanilla_json_ready <- list(
  db_dial_skyrim.esm_update.esm_json_ready,
  db_dial_dawnguard.esm_json_ready,
  db_dial_dragonborn.esm_json_ready,
  db_dial_hearthfires.esm_json_ready
)

db_dial_vanilla_ussep_json_ready <- patcher_include(db_dial_vanilla_json_ready,db_dial_ussep_json_ready)

## Extracting the individual dbs
db_dial_skyrim.esm_ussep_json_ready <- db_dial_vanilla_ussep_json_ready[[1]][[1]]
db_dial_dawnguard.esm_ussep_json_ready <- db_dial_vanilla_ussep_json_ready[[1]][[2]]
db_dial_dragonborn.esm_ussep_json_ready <- db_dial_vanilla_ussep_json_ready[[1]][[3]]
db_dial_hearthfires.esm_ussep_json_ready <- db_dial_vanilla_ussep_json_ready[[1]][[4]]
db_dial_ussep_new_json_ready <- db_dial_vanilla_ussep_json_ready[[2]]
#############################################################################################

## Json generation:


json_skyrim.esm_ussep <- json_gen(db_dial_skyrim.esm_ussep_json_ready,"Skyrim.esm", "NA (Quest)")

json_dawnguard.esm_ussep <- json_gen(db_dial_dawnguard.esm_ussep_json_ready,"Dawnguard.esm", "NA (Quest)")

json_dragonborn.esm_ussep <- json_gen(db_dial_dragonborn.esm_ussep_json_ready,"Dragonborn.esm", "NA (Quest)")

json_hearthfires.esm_ussep <- json_gen(db_dial_hearthfires.esm_ussep_json_ready, "HearthFires.esm", "NA (Quest)")

json_ussep_new <- json_gen(db_dial_ussep_new_json_ready, "unofficial skyrim special edition patch.esp", "NA (Quest)")


## bind them

json_main <- paste0(
  '[', 
  gsub('\\[|\\]', '', json_skyrim.esm_ussep),
  ',', 
  gsub('\\[|\\]', '', json_dawnguard.esm_ussep), 
  ',', 
  gsub('\\[|\\]', '', json_dragonborn.esm_ussep),
  ',', 
  gsub('\\[|\\]', '', json_hearthfires.esm_ussep),  
  ',',
  gsub('\\[|\\]', '', json_ussep_new), 
  ']'
)

## Export

write(json_main, ".\\SKSE\\Plugins\\DynamicStringDistributor\\unofficial skyrim special edition patch.esp\\QuestDialogueTagsmain.json")


