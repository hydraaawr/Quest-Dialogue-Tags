## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_main_functions_v2.1.R")
db_dial_hearthfires.esm <- read.csv(".\\dbs\\db_DIAL_hearthfires.esm.csv", sep = ";")


db_dial_hearthfires.esm_merged <- shaper(db_dial_hearthfires.esm)


## db with massively classified QNAM

db_dial_hearthfires.esm_massclass <- db_dial_hearthfires.esm_merged %>%
      mutate( ## clasify type of quest
        QNAM_type = case_when(
          str_detect(QNAM, "^BYOHHouse") ~ "house_hearthfires", ## Main Quest

        )
      )
      


## ready for json db (filtering and adding tags)

db_dial_hearthfires.esm_json_ready <- db_dial_hearthfires.esm_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type),
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



## Json generation:


json_hearthfires.esm <- json_gen(db_dial_hearthfires.esm_json_ready,"Hearthfires.esm", "NA (Quest)")

write(json_hearthfires.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Hearthfires.esm\\QuestDialogueTagsHearthfires.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_hearthfires.esm.RData")