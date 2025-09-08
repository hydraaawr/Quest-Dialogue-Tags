## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_update.esm <- read.csv(".\\dbs\\db_DIAL_Update.esm.csv", sep = ";")


db_dial_update.esm_merged <- shaper(db_dial_update.esm)


## db with massively classified QNAM

db_dial_update.esm_massclass <- db_dial_update.esm_merged %>%
  massclasser_skyrim.esm()
      


## ready for json db (filtering and adding tags)

db_dial_update.esm_json_ready <- db_dial_update.esm_massclass %>%
  jsonreadier_skyrim.esm()

################################################################################



## Json generation:


json_update.esm <- json_gen(db_dial_update.esm_json_ready,"Update.esm", "NA (Quest)")

# write(json_update.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Update.esm\\QuestDialogueTagsUpdate.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_update.esm.RData")