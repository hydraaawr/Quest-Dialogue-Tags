## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_main_functions_v2.1.R")
db_dial_dawnguard.esm <- read.csv(".\\dbs\\db_DIAL_dawnguard.esm_v1.csv", sep = ";")


db_dial_dawnguard.esm_merged <- shaper(db_dial_dawnguard.esm)


## db with massively classified QNAM

db_dial_dawnguard.esm_massclass <- db_dial_dawnguard.esm_merged %>%
      mutate( ## clasify type of quest
        QNAM_type = case_when(
          str_detect(QNAM, "DLC1VQ01MiscObjective|DLC1VQ01|DLC1VQ02|DLC1HunterBaseIntro|DLC1VQ03Hunter|DLC1VampireBaseIntro|DLC1VQ03Vampire|DLC1VQElder|DLC1VQElderHandler|DLC1VQ04|DLC1VQ05|DLC1VQ06|DLC1VQ07|DLC1VQ08") ~ "MQ", ## Main Quest
          str_detect(QNAM, "DLC1RH") ~ "DG", ## dawnguard radiants
          str_detect(QNAM, "DLC1RV") ~ "VP", ## vampire radiants
          str_detect(QNAM, "DLC1VQSaint|DLC1LD") ~ "SQ", ## Side Quests
          str_detect(QNAM, "DLC1VQDragon|DLC1VQFVBooks|DLC01SoulCairnHorseQuest2|DLC1Surgery") ~ "misc_dawnguard" ## Misc
        )
      ) 
      


## ready for json db (filtering and adding tags)

db_dial_dawnguard.esm_json_ready <- db_dial_dawnguard.esm_massclass %>%
  isolate_ids() %>%
   mutate(
      FULL = case_when(
        str_detect(Formid_DIAL, "Rumor") & is.na(RNAM) & is.na(FULL) ~ "Heard any rumors lately?", ## generate "Rumor"
        TRUE ~ FULL
      ) 
    ) %>% 
      filter(
        ## No classified out
        !is.na(QNAM_type),
        # Exclude "DLC1VQ01 Awakening"
        !(str_detect(QNAM, "DLC1VQ01 Awakening"))

      ) %>%
        filter(
          # Remove entries with rejection phrases because those might or might not contain scriptname
          !str_detect(FULL, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather|think about it") |
          !str_detect(RNAM, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not|think about it")
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


json_dawnguard.esm <- json_gen(db_dial_dawnguard.esm_json_ready,"Dawnguard.esm","NA (Quest)")

write(json_dawnguard.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Dawnguard.esm\\QuestDialogueTagsDawnguard.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_dawnguard.esm.RData")
