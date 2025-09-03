## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_skyrim.esm <- read.csv(".\\dbs\\db_DIAL_skyrim.esm.csv", sep = ";")


db_dial_skyrim.esm_merged <- shaper(db_dial_skyrim.esm)


## db with massively classified QNAM

db_dial_skyrim.esm_massclass <- db_dial_skyrim.esm_merged %>%
      mutate( ## clasify type of quest
        QNAM_type = case_when(
          str_detect(QNAM, "^MQ") ~ "MQ", ## Main Quest
          str_detect(QNAM, "^MG") ~ "MG", ## Mages guild
          str_detect(QNAM, "^C\\d{2}|CR") ~ "C", ## Companions
          str_detect(QNAM, "^DB") ~ "DB", ## Dark brotherhood
          str_detect(QNAM, "^CW") ~ "CW", ## Civil war(legion + stormcloacks)
          str_detect(QNAM, "^TG") ~ "TG", ## Thieves guild
          str_detect(QNAM, "^DA\\d{2}") ~ "DA", ## Daedric
          str_detect(QNAM, "^MS\\d{2}|^VC\\d{2}|^dun|^NN\\d{2}|^[Tt]\\d{2}") ~ "MS", ## Side quests
          str_detect(QNAM, "Favor|Freeform|^Tutorial|BQ|Farm|City Dialogue") ~ "misc_skyrim", ## Miscellaneous
          str_detect(Formid_DIAL,"Heard any rumors lately?|What's the word around town?") ~ "rumor_skyrim" ## rumors
        )
      )
      


## ready for json db (filtering and adding tags)

db_dial_skyrim.esm_json_ready <- db_dial_skyrim.esm_massclass %>%
  isolate_ids() %>%
   jsonreadier_skyrim.esm()

## Apply Update.esm ##########################################################################
## Loading

load(".\\Resources\\DIAL_treatment_update.esm.RData")
## Matching #############################################################################
## These include untouched plugin records + ussepezied ones

db_dial_skyrim.esm_update.esm_json_ready <- rows_update(db_dial_skyrim.esm_json_ready,db_dial_update.esm_json_ready, by = c("Formid_DIAL_isolated","Formid_INFO_isolated"), unmatched = "ignore")
####

## generate the extra ones added by Update.esm

db_dial_update.esm_new_json_ready <- anti_join(db_dial_update.esm_json_ready,db_dial_skyrim.esm_update.esm_json_ready)

## Failsafe for some special cases that had same fomid dial but different info (and generate repeated entries)
Formid_DIAL_isolated_skyrim.esm_update.esm <- db_dial_skyrim.esm_update.esm_json_ready$Formid_DIAL_isolated

db_dial_update.esm_new_json_ready <- db_dial_update.esm_new_json_ready %>%
  filter(!Formid_DIAL_isolated %in% Formid_DIAL_isolated_skyrim.esm_update.esm) ## no new update.esm records

#############################################################################################


## Json generation:


json_skyrim.esm_update.esm <- json_gen(db_dial_skyrim.esm_update.esm_json_ready,"Skyrim.esm", "NA (Quest)")

write(json_skyrim.esm_update.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Skyrim.esm\\QuestDialogueTagsSkyrim.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_skyrim.esm.RData")