## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_functions_v4.0.R")
db_dial_hearthfires.esm <- read.csv(".\\dbs\\db_DIAL_hearthfires.esm.csv", sep = ";")


db_dial_hearthfires.esm_merged <- shaper(db_dial_hearthfires.esm)


## db with massively classified QNAM


massclasser_hearthfires.esm <- function(db_dial_merged){

  db_dial_massclass <- db_dial_merged %>%
        mutate( ## clasify type of quest
          QNAM_type = case_when(
          str_detect(QNAM, "^BYOHHouse") ~ "house_hearthfires", ## Main Quest
          )
        ) 

  return(db_dial_massclass)

}



db_dial_hearthfires.esm_massclass <- db_dial_hearthfires.esm_merged %>%
 massclasser_hearthfires.esm()
      


## ready for json db (filtering and adding tags)


custom_filter_hearthfires.esm <- function(db_dial_massclass){ ## custom manual filters

  db_dial_massclass <- db_dial_massclass %>%
    filter(
      # Ensure at least one of RNAM or FULL has a value
      !is.na(RNAM) | !is.na(FULL)
    )
    return(db_dial_massclass)
}







rejection_vector_hearthfires.esm <- paste(
  "never mind",
  "i'll have to think about it",
  sep = "|"
)





db_dial_hearthfires.esm_json_ready <- db_dial_hearthfires.esm_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type),
      ) %>% 
        custom_filter_hearthfires.esm() %>%
            filter_rejection_phrases(rejection_vector_hearthfires.esm) %>%
              rm_na_renamer_full_rnam()



## Json generation:


json_hearthfires.esm <- json_gen(db_dial_hearthfires.esm_json_ready,"HearthFires.esm", "NA (Quest)")

write(json_hearthfires.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\HearthFires.esm\\QuestDialogueTagsHearthFires.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_hearthfires.esm.RData")