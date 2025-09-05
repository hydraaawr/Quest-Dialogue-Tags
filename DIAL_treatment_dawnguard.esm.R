## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_dawnguard.esm <- read.csv(".\\dbs\\db_DIAL_dawnguard.esm.csv", sep = ";")


db_dial_dawnguard.esm_merged <- shaper(db_dial_dawnguard.esm)


## db with massively classified QNAM

massclasser_dawnguard.esm <- function(db_dial_merged){

  db_dial_massclass <- db_dial_merged %>%
        mutate( ## clasify type of quest
          QNAM_type = case_when(
          str_detect(QNAM, "DLC1VQ01MiscObjective|DLC1VQ01|DLC1VQ02|DLC1HunterBaseIntro|DLC1VQ03Hunter|DLC1VampireBaseIntro|DLC1VQ03Vampire|DLC1VQElder|DLC1VQElderHandler|DLC1VQ04|DLC1VQ05|DLC1VQ06|DLC1VQ07|DLC1VQ08") ~ "MQ", ## Main Quest
          str_detect(QNAM, "DLC1RH") ~ "DG", ## dawnguard radiants
          str_detect(QNAM, "DLC1RV") ~ "VP", ## vampire radiants
          str_detect(QNAM, "DLC1VQSaint|DLC1LD") ~ "SQ", ## Side Quests
          str_detect(QNAM, "DLC1VQDragon|DLC1VQFVBooks|DLC01SoulCairnHorseQuest2|DLC1Surgery") ~ "misc_dawnguard", ## Misc
          str_detect(QNAM, "DLC1VampireTutorial") ~ "VT"
          )
        ) 

  return(db_dial_massclass)

}



db_dial_dawnguard.esm_massclass <- db_dial_dawnguard.esm_merged %>%
      massclasser_dawnguard.esm()
      


## ready for json db (filtering and adding tags)
rejection_vector_dawnguard.esm <- paste(
  "another time",
  "sorry, i can't",
  "sorry to",
  "can't help",
  "not interested",
  "I'd rather",
  "think about it",
  sep = "|"
)



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
        !(str_detect(QNAM, "DLC1VQ01 Awakening")),
        # Exclude vampire tutorials without scriptname
        !(str_detect(QNAM_type, "VT") & is.na(Scriptname))

      ) %>%
        filter_rejection_phrases(rejection_vector_dawnguard.esm) %>%
          rm_na_renamer_full_rnam()
          

################################################################################



## Json generation:


json_dawnguard.esm <- json_gen(db_dial_dawnguard.esm_json_ready,"Dawnguard.esm","NA (Quest)")

write(json_dawnguard.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Dawnguard.esm\\QuestDialogueTagsDawnguard.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_dawnguard.esm.RData")
