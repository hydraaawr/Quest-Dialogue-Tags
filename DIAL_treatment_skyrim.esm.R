## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

load(".\\Resources\\DIAL_treatment_update.esm.RData")
source(".\\DIAL_treatment_functions_v4.0.R")

db_dial_skyrim.esm <- read.csv(".\\dbs\\db_DIAL_skyrim.esm.csv", sep = ";")


db_dial_skyrim.esm_merged <- shaper(db_dial_skyrim.esm)


## db with massively classified QNAM

db_dial_skyrim.esm_massclass <- db_dial_skyrim.esm_merged %>%
  massclasser_skyrim.esm()


## ready for json db (filtering and adding tags)

db_dial_skyrim.esm_json_ready <- db_dial_skyrim.esm_massclass %>%
  isolate_ids() %>%
   jsonreadier_skyrim.esm()



## Patching #############################################################################

db_dial_skyrim.esm_update.esm_json_ready <- patcher_include(db_dial_skyrim.esm_json_ready, db_dial_update.esm_json_ready)[[1]]

#############################################################################################


## Json generation:


json_skyrim.esm_update.esm <- json_gen(db_dial_skyrim.esm_update.esm_json_ready,"Skyrim.esm", "NA (Quest)")

## No need to include new update.esm entries; there are none

write(json_skyrim.esm_update.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Skyrim.esm\\QuestDialogueTagsSkyrim.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_skyrim.esm.RData")