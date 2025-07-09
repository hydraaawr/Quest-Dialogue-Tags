## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133
## combines base cc fishing + sfo changes

library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## sfo treatment #######################################################

## Loading

source(".\\DIAL_treatment_main_functions_v2.1.R")
db_dial_sti <- read.csv(".\\dbs\\db_DIAL_STI_v1.csv", sep = ";")

## Procedure

db_dial_sti_merged <- shaper(db_dial_sti)

## db with massively classified QNAM

db_dial_sti_massclass <- db_dial_sti_merged %>%
  mutate( ## clasify type of quest
        QNAM_type = case_when(
          str_detect(QNAM, "^MS") ~ "MS", ## Side quests
          str_detect(QNAM, "STI") ~ "STI" ## STI added Quest
        )
      )
      


## ready for json db (filtering and adding tags)

db_dial_sti_json_ready <- db_dial_sti_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type),
        ## Exclude without scripts
        !is.na(Scriptname) | str_detect(FULL,"is up to something") 
      ) %>%
          filter(
            # Ensure at least one of RNAM or FULL has a value
            !is.na(RNAM) | !is.na(FULL)
          ) %>% 
            mutate(
              FULL_trans = paste0(FULL, " (Quest)"), ## Add "(Quest)"
              RNAM_trans = paste0(RNAM, " (Quest)")
            )


#### base db #####################################################
## Loading

load(".\\Resources\\DIAL_treatment_skyrim.esm.RData")




## Matching #############################################################################
## These include untouched base plugin records + patched ones

db_dial_skyrim.esm_sti_json_ready <- rows_update(db_dial_skyrim.esm_json_ready,db_dial_sti_json_ready, by = c("Formid_DIAL_isolated","Formid_INFO_isolated"), unmatched = "ignore")
db_dial_skyrim.esm_sti_crop_json_ready <- anti_join(db_dial_skyrim.esm_sti_json_ready, db_dial_skyrim.esm_json_ready) ## base patched only

####

## generate extra ones added by patch

db_dial_sti_new_json_ready <- anti_join(db_dial_sti_json_ready,db_dial_skyrim.esm_sti_json_ready)

## Failsafe for some special cases that had same fomid dial but different info (and generate repeated entries)
Formid_DIAL_isolated_skyrim.esm_sti <- db_dial_skyrim.esm_sti_json_ready$Formid_DIAL_isolated

db_dial_sti_new_json_ready <- db_dial_sti_new_json_ready %>%
  filter(!Formid_DIAL_isolated %in% Formid_DIAL_isolated_skyrim.esm_sti)


## And now only leave

#############################################################################################

## Json generation:


json_skyrim.esm_sti <- json_gen(db_dial_skyrim.esm_sti_crop_json_ready,"Skyrim.esm", "NA (Quest)")

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


