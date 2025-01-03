## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_main_functions_v1.R")
db_dial_skyrim.esm <- read.csv(".\\dbs\\db_DIAL_skyrim.esm_v1.csv", sep = ";")


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
          str_detect(QNAM, "Favor|Freeform|Tutorial|BQ|Farm|City Dialogue") ~ "misc" ## Miscellaneous INCLUDE CITY DIALOGUE MANUALLY?
        )
      ) 
      


## ready for json db (filtering and adding tags)

db_dial_skyrim.esm_json_ready <- db_dial_skyrim.esm_massclass %>%
   mutate(
      Formid_DIAL_isolated = as.character(str_extract_all(Formid_DIAL, "(?<=DIAL:)[^\\]]*")), ## get only Formid_DIAL
      Formid_INFO_isolated = as.character(str_extract_all(INFO, "(?<=INFO:)[^\\]]*"))
    ) %>%
  filter(
    ## No classified out
    !is.na(QNAM_type),
    # Exclude "City Dialogue" without Scriptname
    !(str_detect(QNAM, "City Dialogue") & is.na(Scriptname))
  ) %>%
  filter(
    # Remove entries with rejection phrases because those might or might not contain scriptname
     !str_detect(FULL, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather") |
     !str_detect(RNAM, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not")
  ) %>%
  filter(
    # Ensure at least one of RNAM or FULL has a value
    !is.na(RNAM) | !is.na(FULL)
  ) %>% 
            mutate(
              FULL_trans = paste0(FULL, " (Quest)"), ## Add "(Quest)"
              RNAM_trans = paste0(RNAM, " (Quest)")
            )


################################################################################



## Json generation:


json_skyrim.esm <- json_gen(db_dial_skyrim.esm_json_ready,"Skyrim.esm", "NA (Quest)")

write(json_skyrim.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Skyrim.esm\\QuestDialogueTagsSkyrim.esm.json")


