## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133
## combines base cc fishing + sfo changes

library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## sfo treatment #######################################################

## Loading

source(".\\DIAL_treatment_main_functions_v2.1.R")
db_dial_sfo.esp <- read.csv(".\\dbs\\db_DIAL_sfo.esp_v1.csv", sep = ";")

## Procedure

db_dial_sfo.esp_merged <- shaper(db_dial_sfo.esp)

## db with massively classified QNAM

db_dial_sfo.esp_massclass <- db_dial_sfo.esp_merged %>%
    mutate( ## clasify type of quest
        QNAM_type = case_when(
        str_detect(QNAM, "MQ") ~ "MQ", ## Main Quest
          str_detect(QNAM, "Radiant") ~ "radiant", ## Radiant
          str_detect(QNAM, "Misc") ~ "misc", ## Miscellaneous

        )
      )
      


## ready for json db (filtering and adding tags)

db_dial_sfo.esp_json_ready <- db_dial_sfo.esp_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type),
        ## Exclude without scripts
        !is.na(Scriptname)
      ) %>%
        filter(
          # Remove entries with rejection phrases because those might or might not contain scriptname
          !str_detect(FULL, "(?i)I haven't found|I don't have time") |
          !str_detect(RNAM, "(?i)I haven't found|I don't have time")
        ) %>%
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




## Matching #############################################################################
## These include untouched plugin records + sfoezied ones

db_dial_ccBGSSSE001_Fish.esm_sfo_json_ready <- rows_update(db_dial_ccBGSSSE001_Fish.esm_json_ready,db_dial_sfo.esp_json_ready, by = c("Formid_DIAL_isolated","Formid_INFO_isolated"), unmatched = "ignore")


####

## generate the extra ones added by sfo

db_dial_sfo.esp_new_json_ready <- anti_join(db_dial_sfo.esp_json_ready,db_dial_ccBGSSSE001_Fish.esm_sfo_json_ready)

## Failsafe for some special cases that had same fomid dial but different info (and generate repeated entries)
Formid_DIAL_isolated_ccBGSSSE001_Fish.esm_sfo <- db_dial_ccBGSSSE001_Fish.esm_sfo_json_ready$Formid_DIAL_isolated

db_dial_sfo.esp_new_json_ready <- db_dial_sfo.esp_new_json_ready %>%
  filter(!Formid_DIAL_isolated %in% Formid_DIAL_isolated_ccBGSSSE001_Fish.esm_sfo)

#############################################################################################

## Json generation:


json_ccBGSSSE001_Fish.esm_sfo <- json_gen(db_dial_ccBGSSSE001_Fish.esm_sfo_json_ready,"ccBGSSSE001_Fish.esm", "NA (Quest)")

json_sfo.esp_new <- json_gen(db_dial_sfo.esp_new_json_ready, "Simple Fishing Overhaul.esp", "NA (Quest)")


## bind them (NOT NECESSARY BECAUSE THERE'S NO NEW DIALOGUES IN THIS CASE)

# json_main <- paste0(
#   '[', 
#   gsub('\\[|\\]', '', json_ccBGSSSE001_Fish.esm_sfo),
#   ',',
#   gsub('\\[|\\]', '', json_sfo.esp_new), 
#   ']'
# )

## Export

write(json_ccBGSSSE001_Fish.esm_sfo, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Simple Fishing Overhaul.esp\\QuestDialogueTagsSimple_Fishing_Overhaul.esp.json")


