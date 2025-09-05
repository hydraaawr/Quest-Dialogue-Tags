## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133
## combines base cc fishing + sfo changes

library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## sfo treatment #######################################################

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_sfo <- read.csv(".\\dbs\\db_DIAL_sfo.csv", sep = ";")

## Procedure

db_dial_sfo_merged <- shaper(db_dial_sfo)

## db with massively classified QNAM

db_dial_sfo_massclass <- db_dial_sfo_merged %>%
    mutate( ## clasify type of quest
        QNAM_type = case_when(
        str_detect(QNAM, "MQ") ~ "MQ", ## Main Quest
          str_detect(QNAM, "Radiant") ~ "radiant", ## Radiant
          str_detect(QNAM, "Misc") ~ "misc", ## Miscellaneous

        )
      )
      


## ready for json db (filtering and adding tags)

db_dial_sfo_json_ready <- db_dial_sfo_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type),
        ## Exclude without scripts
        !is.na(Scriptname)
      ) %>%
        filter(
          # Remove entries with rejection phrases because those might or might not contain scriptname
              !(
                # Check FULL (managing NA)
                if_else(is.na(FULL), FALSE, 
                      str_detect(FULL, regex("(?i)I haven't found|I don't have time", ignore_case = TRUE))) |
                # Check RNAM (managing NA)  
                if_else(is.na(RNAM), FALSE,
                      str_detect(RNAM, regex("(?i)I haven't found|I don't have time", ignore_case = TRUE)))
                  )) %>%
                    filter(
                      # Ensure at least one of RNAM or FULL has a value
                      !is.na(RNAM) | !is.na(FULL)
                    ) %>% 
                      mutate(
                        FULL_trans = paste0(FULL, " (Quest)"), ## Add "(Quest)"
                        RNAM_trans = paste0(RNAM, " (Quest)")
                      )


#### cc fishing base db #####################################################
## Loading

load(".\\Resources\\DIAL_treatment_ccBGSSSE001_Fish.esm.RData")




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


