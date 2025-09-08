## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133
## combines base cc fishing + sfo changes

library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## sfo treatment #######################################################

## Loading
load(".\\Resources\\DIAL_treatment_ccBGSSSE001_Fish.esm.RData")
source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_sfo <- read.csv(".\\dbs\\db_DIAL_sfo.csv", sep = ";")

## Procedure

db_dial_sfo_merged <- shaper(db_dial_sfo)

## db with massively classified QNAM


db_dial_sfo_massclass <- db_dial_sfo_merged %>%
  massclasser_ccBGSSSE001_Fish.esm()
      


## ready for json db (filtering and adding tags)

db_dial_sfo_json_ready <- db_dial_sfo_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type),
      ) %>%
        custom_filter_ccBGSSSE001_Fish.esm.esm() %>%
          filter_rejection_phrases(rejection_vector_ccBGSSSE001_Fish.esm) %>%
            rm_na_renamer_full_rnam()




## Patching #############################################################################


db_dial_ccBGSSSE001_Fish.esm_sfo_json_ready <- patcher_include(db_dial_ccBGSSSE001_Fish.esm_json_ready, db_dial_sfo_json_ready)[[1]]
db_dial_sfo_new_json_ready <- patcher_include(db_dial_ccBGSSSE001_Fish.esm_json_ready, db_dial_sfo_json_ready)[[2]]

#############################################################################################

## Json generation:


json_ccBGSSSE001_Fish.esm_sfo <- json_gen(db_dial_ccBGSSSE001_Fish.esm_sfo_json_ready,"ccBGSSSE001-Fish.esm", "NA (Quest)")

json_sfo_new <- json_gen(db_dial_sfo_new_json_ready, "Simple Fishing Overhaul.esp", "NA (Quest)")


## bind them (NOT NECESSARY BECAUSE THERE'S NO NEW DIALOGUES IN THIS CASE)

# json_main <- paste0(
#   '[', 
#   gsub('\\[|\\]', '', json_ccBGSSSE001_Fish.esm_sfo),
#   ',',
#   gsub('\\[|\\]', '', json_sfo_new), 
#   ']'
# )

## Export

write(json_ccBGSSSE001_Fish.esm_sfo, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Simple Fishing Overhaul.esp\\QuestDialogueTagsSimple_Fishing_Overhaul.esp.json")


