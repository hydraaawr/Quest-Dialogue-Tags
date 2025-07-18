## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_main_functions_v2.1.R")
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
          str_detect(QNAM, "Favor|Freeform|^Tutorial|BQ|Farm|City Dialogue") ~ "misc_skyrim", ## Miscellaneous
          str_detect(Formid_DIAL,"Heard any rumors lately?|What's the word around town?") ~ "rumor_skyrim" ## rumors
        )
      )
      


## ready for json db (filtering and adding tags)

db_dial_skyrim.esm_json_ready <- db_dial_skyrim.esm_massclass %>%
  isolate_ids() %>%
   mutate(   ## generate "rumor"
      FULL = case_when(
        str_detect(Formid_DIAL, "Rumor|T01Innkeeper") & is.na(RNAM) & is.na(FULL) ~ "Heard any rumors lately?", ## generate "Rumor"
        TRUE ~ FULL 
        ),
      RNAM = case_when(
        str_detect(FULL, "What's the word around town?") & is.na(RNAM) ~ "What's the word around town?", ## special case where we have to generate RNAM to trigger json generation INFO generation
        TRUE ~ RNAM
      )
    ) %>%
      filter(
        ## No classified out
        !is.na(QNAM_type),
        # Exclude "City Dialogue" without Scriptname
        !(str_detect(QNAM, "City Dialogue") & is.na(Scriptname)),
        ## Exclude rumors without Scriptname
        !(str_detect(QNAM_type, "rumor") & is.na(Scriptname)),
        ## Exclude some CW dials without scriptname
        !(str_detect(Formid_DIAL, "CW00JoinAboutFactionTopic|CW00AboutTopic|CWAbout|CWWhatsEmpireDoingTopic|CWWhatWillItTakeTopic") & is.na(Scriptname)),
        ## Exclude MS dials without scriptname
        !(str_detect(Formid_DIAL, "^MS") & is.na(Scriptname))
      ) %>% 
        filter(
          # Remove entries with rejection phrases because those might or might not contain scriptname
          !str_detect(FULL, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not|I'd rather be|not right now|Good luck with that") |
          !str_detect(RNAM, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not|I'd rather be|not right now|Good luck with that")
        ) %>%
          filter(
            # Ensure at least one of RNAM or FULL has a value
            !is.na(RNAM) | !is.na(FULL)
          ) %>% 
            mutate(
              # Replace any RNAM containing "TIF_" with "NA (Quest)"
              RNAM = if_else(str_detect(RNAM, "TIF_"), "NA", RNAM),
              FULL_trans = paste0(FULL, " (Quest)"), ## Add "(Quest)"
              RNAM_trans = paste0(RNAM, " (Quest)")
            )


################################################################################



## Json generation:


json_skyrim.esm <- json_gen(db_dial_skyrim.esm_json_ready,"Skyrim.esm", "NA (Quest)")

write(json_skyrim.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Skyrim.esm\\QuestDialogueTagsSkyrim.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_skyrim.esm.RData")