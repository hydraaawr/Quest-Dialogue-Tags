## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_main_functions_v2.1.R")
db_dial_dragonborn.esm <- read.csv(".\\dbs\\db_DIAL_dragonborn.esm_v1.csv", sep = ";")


db_dial_dragonborn.esm_merged <- shaper(db_dial_dragonborn.esm)


## db with massively classified QNAM

db_dial_dragonborn.esm_massclass <- db_dial_dragonborn.esm_merged %>%
      mutate( ## clasify type of quest
        QNAM_type = case_when(
          str_detect(QNAM, "DLC2MQ01|DLC2WE09|DLC2MQ02|DLC2MQ03|DLC2MQ03B|DLC2MQ04|DLC2MQ05|DLC2MQ06") ~ "MQ", ## Main Quest
          str_detect(QNAM, "DLC2BlackBook04Quest|DLC2RR03|DLC2RR01|DLC2TT1b|DLC2RR02") ~ "RRSD", ## Raven Rock Side Quests
          str_detect(QNAM, "DLC2RR03Intro|DLC2RRFavor03|DLC2RRFavor06|DLC2RRFavor02|DLC2RRFavor07|DLC2RRFavor04|DLC2TGQuest|DLC2RRFavor05|DLC2RRFavor01") ~ "RRmisc", ## Raven Rock miscellaneous
          str_detect(QNAM, "DLC2SV01|DLC2SV02|DLC2SV02Misc|DLC2WB01") ~ "RRSV", ## Skaal Village Side Quests
          str_detect(QNAM, "DLC2SkaalVillageFreeform2|Favor104DLC2SkaalVillageFreeform1") ~ "SVmisc", ## Skaal Village miscellaneous
          str_detect(QNAM, "DLC2TTR7|DLC2BlackBook07Quest|DLC2TTR5|DLC2TTR4a|DLC2TTF1|DLC2TTF2|DLC2TTR2|DLC2TTR1|DLC2TT2|DLC2TT1|DLC2TTR3a|DLC2TTR3b") ~ "TMSD", ## Tel Mithryn Side quests
          str_detect(QNAM, "DLC2TTR4b|DLC2TTR8") ~ "TMmisc", ## Tel Mithryn miscellaneous
          str_detect(QNAM, "DLC2MH02|DLC2MH01") ~ "TMHSD", ## Thirsk Mead Hall Side quests
          str_detect(QNAM, "DLC2ThirskFFElmusBack|DLC2ThirskFFElmus|DLC2ThirskFFHalbarn|DLC2ThirskFFHilund") ~ "TMHmisc", ## Thirsk Mead Hall miscellaneous
          str_detect(QNAM, "DLC2BlackBook06Quest|DLC2BlackBook05Quest|DLC2BlackBook03Quest|DLC2dunHaknirTreasureQST|DLC2dunHaknirTreasureQSTMisc|DLC2EbonyWarriorQuest|DLC2dunKolbjornQST|DLC2dunKolbjornMiscQST") ~ "OS", ## Other Side
          str_detect(QNAM, "DLC2WE06|DLC2dunHrodulf|DLC2KagrumezQST|DLC2SV02AncarionMerchant|DLC2dunKolbjornMiscQST|DLC2dunKarstaagQST|DLC2dunFrostmoonQSTMisc") ~ "misc", ## other miscellaneous quests
          str_detect(Formid_DIAL,"Rumor") ~ "rumor" ## rumors
        )
      ) 
      


## ready for json db (filtering and adding tags)

db_dial_dragonborn.esm_json_ready <- db_dial_dragonborn.esm_massclass %>%
  isolate_ids() %>%
   mutate(
    ## Here rumors work differently: You have to force generating multiple INFO RNAMS by filling both
      FULL = case_when(
        str_detect(Formid_DIAL, "Rumors") & is.na(RNAM) & is.na(FULL) & !is.na(Scriptname) ~ "Heard any rumors lately?", ## generate "Rumor"
        TRUE ~ FULL
      ),
      RNAM = case_when(
        str_detect(Formid_DIAL, "Rumors") & is.na(RNAM) & !is.na(Scriptname) ~ "Heard any rumors lately?", ## generate "Rumor"
        TRUE ~ RNAM
      ),

    ) %>% 
      filter(
        ## No classified out
        !is.na(QNAM_type),
        # Exclude (in case you need it)
        # !(str_detect(QNAM, ""))

      ) %>%
        filter(
          # Remove entries with rejection phrases because those might or might not contain scriptname
          !str_detect(FULL, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather|think about it|good luck with that|show up eventually") |
          !str_detect(RNAM, "(?i)another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not|think about it|good luck with that|show up eventually")
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


json_dragonborn.esm <- json_gen(db_dial_dragonborn.esm_json_ready,"Dragonborn.esm","NA (Quest)")

write(json_dragonborn.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Dragonborn.esm\\QuestDialogueTagsDragonborn.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_dragonborn.esm.RData")
