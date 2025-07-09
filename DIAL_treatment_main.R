## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133
## combines base game + 2 dlcs with ussep changes

library(dplyr)
library(jsonlite)
library(stringr)

rm(list = ls())


## USSEP treatment #######################################################

## Loading

source(".\\DIAL_treatment_main_functions_v2.1.R")
db_dial_ussep <- read.csv(".\\dbs\\db_DIAL_USSEP_v1.csv", sep = ";")

## Procedure

db_dial_ussep_merged <- shaper(db_dial_ussep)

## db with massively classified QNAM

db_dial_ussep_massclass <- db_dial_ussep_merged %>%
    mutate( ## clasify type of quest
        QNAM_type = case_when(
        ## skyrim.esm
          str_detect(QNAM, "^MQ") ~ "MQ", ## Main Quest
          str_detect(QNAM, "^MG") ~ "MG", ## Mages guild
          str_detect(QNAM, "^C\\d{2}|CR") ~ "C", ## Companions
          str_detect(QNAM, "^DB") ~ "DB", ## Dark brotherhood
          str_detect(QNAM, "^CW") ~ "CW", ## Civil war(legion + stormcloacks)
          str_detect(QNAM, "^TG") ~ "TG", ## Thieves guild
          str_detect(QNAM, "^DA\\d{2}") ~ "DA", ## Daedric
          str_detect(QNAM, "^MS\\d{2}|^VC\\d{2}|^dun|^NN\\d{2}|^[Tt]\\d{2}") ~ "MS", ## Side quests
          str_detect(QNAM, "Favor|Freeform|^Tutorial|BQ|Farm|City Dialogue") ~ "misc", ## Miscellaneous
          str_detect(Formid_DIAL,"Heard any rumors lately?|What's the word around town?") ~ "rumor_skyrim", ## rumors
        ## dawnguard.esm
          str_detect(QNAM, "DLC1VQ01MiscObjective|DLC1VQ01|DLC1VQ02|DLC1HunterBaseIntro|DLC1VQ03Hunter|DLC1VampireBaseIntro|DLC1VQ03Vampire|DLC1VQElder|DLC1VQElderHandler|DLC1VQ04|DLC1VQ05|DLC1VQ06|DLC1VQ07|DLC1VQ08") ~ "MQ", ## Main Quest
          str_detect(QNAM, "DLC1RH") ~ "DG", ## dawnguard radiants
          str_detect(QNAM, "DLC1RV") ~ "VP", ## vampire radiants
          str_detect(QNAM, "DLC1VQSaint|DLC1LD") ~ "SQ", ## Side Quests
          str_detect(QNAM, "DLC1VQDragon|DLC1VQFVBooks|DLC01SoulCairnHorseQuest2|DLC1Surgery") ~ "misc", ## Misc
          str_detect(QNAM, "DLC1VampireTutorial") ~ "VT",
        ## dragonborn.esm
          str_detect(QNAM, "DLC2MQ01|DLC2WE09|DLC2MQ02|DLC2MQ03|DLC2MQ03B|DLC2MQ04|DLC2MQ05|DLC2MQ06") ~ "MQ", ## Main Quest
          str_detect(QNAM, "DLC2BlackBook04Quest|DLC2RR03|DLC2RR01|DLC2TT1b|DLC2RR02") ~ "RRSD", ## Raven Rock Side Quests
          str_detect(QNAM, "DLC2RR03Intro|DLC2RRFavor03|DLC2RRFavor06|DLC2RRFavor02|DLC2RRFavor07|DLC2RRFavor04|DLC2TGQuest|DLC2RRFavor05|DLC2RRFavor01") ~ "RRmisc", ## Raven Rock miscellaneous
          str_detect(QNAM, "DLC2SV01|DLC2SV02|DLC2SV02Misc|DLC2WB01") ~ "RRSV", ## Skaal Village Side Quests
          str_detect(QNAM, "DLC2SkaalVillageFreeform2|Favor104|DLC2SkaalVillageFreeform1") ~ "SVmisc", ## Skaal Village miscellaneous
          str_detect(QNAM, "DLC2TTR7|DLC2BlackBook07Quest|DLC2TTR5|DLC2TTR4a|DLC2TTF1|DLC2TTF2|DLC2TTR2|DLC2TTR1|DLC2TT2|DLC2TT1|DLC2TTR3a|DLC2TTR3b") ~ "TMSD", ## Tel Mithryn Side quests
          str_detect(QNAM, "DLC2TTR4b|DLC2TTR8") ~ "TMmisc", ## Tel Mithryn miscellaneous
          str_detect(QNAM, "DLC2MH02|DLC2MH01") ~ "TMHSD", ## Thirsk Mead Hall Side quests
          str_detect(QNAM, "DLC2ThirskFFElmusBack|DLC2ThirskFFElmus|DLC2ThirskFFHalbarn|DLC2ThirskFFHilund") ~ "TMHmisc", ## Thirsk Mead Hall miscellaneous
          str_detect(QNAM, "DLC2BlackBook06Quest|DLC2BlackBook05Quest|DLC2BlackBook03Quest|DLC2dunHaknirTreasureQST|DLC2dunHaknirTreasureQSTMisc|DLC2EbonyWarriorQuest|DLC2dunKolbjornQST|DLC2dunKolbjornMiscQST") ~ "OS", ## Other Side
          str_detect(QNAM, "DLC2WE06|DLC2dunHrodulf|DLC2KagrumezQST|DLC2SV02AncarionMerchant|DLC2dunKolbjornMiscQST|DLC2dunKarstaagQST|DLC2dunFrostmoonQSTMisc") ~ "misc", ## other miscellaneous quests
          str_detect(Formid_DIAL,"Rumor") ~ "rumor_dragonborn" ## rumors

        )
      )
      


## ready for json db (filtering and adding tags)

db_dial_ussep_json_ready <- db_dial_ussep_massclass %>%
  isolate_ids() %>%
  ## skyrim.esm + dawnguard.esm
   mutate(   ## generate "rumor"
      FULL = case_when(
        str_detect(Formid_DIAL, "Rumor|T01Innkeeper") & is.na(RNAM) & is.na(FULL) & QNAM_type == "rumor_skyrim|MS" ~ "Heard any rumors lately?", ## generate "Rumor"
        TRUE ~ FULL 
        ),
      RNAM = case_when(
        str_detect(FULL, "What's the word around town?") & is.na(RNAM) ~ "What's the word around town?", ## special case where we have to generate RNAM to trigger json generation INFO generation
        TRUE ~ RNAM
      )) %>%
    ## dragonborn.esm
    ## Here rumors work differently: You have to force generating multiple INFO RNAMS by filling both
      mutate(
        FULL = case_when(
          str_detect(Formid_DIAL, "Rumors") & is.na(RNAM) & is.na(FULL) & !is.na(Scriptname) & QNAM_type == "rumor_dragonborn" ~ "Heard any rumors lately?", ## generate "Rumor"
          TRUE ~ FULL
        ),
        RNAM = case_when(
          str_detect(Formid_DIAL, "Rumors") & is.na(RNAM) & !is.na(Scriptname) & QNAM_type == "rumor_dragonborn" ~ "Heard any rumors lately?", ## generate "Rumor"
          TRUE ~ RNAM
        )
          ) %>%
            filter(
              ## No classified out
              !is.na(QNAM_type),
              ## skyrim.esm
              # Exclude "City Dialogue" without Scriptname
              !(str_detect(QNAM, "City Dialogue") & is.na(Scriptname)),
              ## Exclude rumors without Scriptname
              !(str_detect(QNAM_type, "rumor") & is.na(Scriptname)),
              ## Exclude some CW dials without scriptname
              !(str_detect(Formid_DIAL, "CW00JoinAboutFactionTopic|CW00AboutTopic|CWAbout|CWWhatsEmpireDoingTopic|CWWhatWillItTakeTopic") & is.na(Scriptname)),
              ## Exclude MS dials without scriptname
              !(str_detect(Formid_DIAL, "^MS") & is.na(Scriptname)),
              ## dawnguard.esm
              # Exclude "DLC1VQ01 Awakening"
              !(str_detect(QNAM, "DLC1VQ01 Awakening")),
              # Exclude vampire tutorials without scriptname
              !(str_detect(QNAM_type, "VT") & is.na(Scriptname))
            ) %>%
              filter(
                # Remove entries with rejection phrases because those might or might not contain scriptname
                !str_detect(FULL, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not|I'd rather be|think about it|good luck with that|show up eventually|not right now|Good luck with that") |
                !str_detect(RNAM, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not|I'd rather be|think about it|good luck with that|show up eventually|not right now|Good luck with that")
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


#### Vanilla + dlcs dbs #####################################################
## Loading

load(".\\Resources\\DIAL_treatment_skyrim.esm.RData")
load(".\\Resources\\DIAL_treatment_dawnguard.esm.RData")
load(".\\Resources\\DIAL_treatment_dragonborn.esm.RData")



## Matching #############################################################################
## These include untouched plugin records + ussepezied ones

db_dial_skyrim.esm_ussep_json_ready <- rows_update(db_dial_skyrim.esm_json_ready,db_dial_ussep_json_ready, by = c("Formid_DIAL_isolated","Formid_INFO_isolated"), unmatched = "ignore")

db_dial_dawnguard.esm_ussep_json_ready <- rows_update(db_dial_dawnguard.esm_json_ready,db_dial_ussep_json_ready, by = c("Formid_DIAL_isolated","Formid_INFO_isolated"), unmatched = "ignore")

db_dial_dragonborn.esm_ussep_json_ready <- rows_update(db_dial_dragonborn.esm_json_ready,db_dial_ussep_json_ready, by = c("Formid_DIAL_isolated","Formid_INFO_isolated"), unmatched = "ignore")

####

## generate the extra ones added by ussep

db_dial_vanilla_ussep_json_ready <- bind_rows(db_dial_skyrim.esm_ussep_json_ready, db_dial_dawnguard.esm_ussep_json_ready, db_dial_dragonborn.esm_ussep_json_ready) ## join the three that have all records + ussep modifications
db_dial_ussep_new_json_ready <- anti_join(db_dial_ussep_json_ready,db_dial_vanilla_ussep_json_ready)

## Failsafe for some special cases that had same fomid dial but different info (and generate repeated entries)
Formid_DIAL_isolated_vanilla_ussep <- db_dial_vanilla_ussep_json_ready$Formid_DIAL_isolated

db_dial_ussep_new_json_ready <- db_dial_ussep_new_json_ready %>%
  filter(!Formid_DIAL_isolated %in% Formid_DIAL_isolated_vanilla_ussep)

#############################################################################################

## Json generation:


json_skyrim.esm_ussep <- json_gen(db_dial_skyrim.esm_ussep_json_ready,"Skyrim.esm", "NA (Quest)")

json_dawnguard.esm_ussep <- json_gen(db_dial_dawnguard.esm_ussep_json_ready,"Dawnguard.esm", "NA (Quest)")

json_dragonborn.esm_ussep <- json_gen(db_dial_dragonborn.esm_ussep_json_ready,"Dragonborn.esm", "NA (Quest)")

json_ussep_new <- json_gen(db_dial_ussep_new_json_ready, "unofficial skyrim special edition patch.esp", "NA (Quest)")


## bind them

json_main <- paste0(
  '[', 
  gsub('\\[|\\]', '', json_skyrim.esm_ussep),
  ',', 
  gsub('\\[|\\]', '', json_dawnguard.esm_ussep), 
  ',', 
  gsub('\\[|\\]', '', json_dragonborn.esm_ussep), 
  ',',
  gsub('\\[|\\]', '', json_ussep_new), 
  ']'
)

## Export

write(json_main, ".\\SKSE\\Plugins\\DynamicStringDistributor\\unofficial skyrim special edition patch.esp\\QuestDialogueTagsmain.json")


