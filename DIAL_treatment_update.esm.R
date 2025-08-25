## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_main_functions_v3.0.R")
db_dial_update.esm <- read.csv(".\\dbs\\db_DIAL_Update.esm.csv", sep = ";")


db_dial_update.esm_merged <- shaper(db_dial_update.esm)


## db with massively classified QNAM

db_dial_update.esm_massclass <- db_dial_update.esm_merged %>%
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

db_dial_update.esm_json_ready <- db_dial_update.esm_massclass %>%
  isolate_ids() %>%
    jsonreadier_skyrim.esm()

################################################################################



## Json generation:


json_update.esm <- json_gen(db_dial_update.esm_json_ready,"Update.esm", "NA (Quest)")

# write(json_update.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Update.esm\\QuestDialogueTagsUpdate.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_update.esm.RData")