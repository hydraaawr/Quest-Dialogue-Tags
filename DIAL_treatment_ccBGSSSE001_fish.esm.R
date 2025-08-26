## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(stringr)

rm(list = ls())

## Loading

source(".\\DIAL_treatment_functions_v3.0.R")
db_dial_ccBGSSSE001_Fish.esm <- read.csv(".\\dbs\\db_DIAL_ccBGSSSE001_Fish.esm.csv", sep = ";")


db_dial_ccBGSSSE001_Fish.esm_merged <- shaper(db_dial_ccBGSSSE001_Fish.esm)


## db with massively classified QNAM

db_dial_ccBGSSSE001_Fish.esm_massclass <- db_dial_ccBGSSSE001_Fish.esm_merged %>%
      mutate( ## clasify type of quest
        QNAM_type = case_when(
          str_detect(QNAM, "MQ") ~ "MQ", ## Main Quest
          str_detect(QNAM, "Radiant") ~ "radiant", ## Radiant
          str_detect(QNAM, "Misc") ~ "misc", ## Miscellaneous
        )
      )
      


## ready for json db (filtering and adding tags)

db_dial_ccBGSSSE001_Fish.esm_json_ready <- db_dial_ccBGSSSE001_Fish.esm_massclass %>%
  isolate_ids() %>%
      filter(
        ## No classified out
        !is.na(QNAM_type),
        ## Exclude without scripts
        !is.na(Scriptname)
      ) %>%
        filter(
          # Remove entries with rejection phrases because those might or might not contain scriptname
              !(
                # Check FULL (managing NA)
                if_else(is.na(FULL), FALSE, 
                      str_detect(FULL, regex("(?i)I haven't found|I don't have time", ignore_case = TRUE))) |
                # Check RNAM (managing NA)  
                if_else(is.na(RNAM), FALSE,
                      str_detect(RNAM, regex("(?i)I haven't found|I don't have time", ignore_case = TRUE)))
                  )) %>%
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


json_ccBGSSSE001_Fish.esm <- json_gen(db_dial_ccBGSSSE001_Fish.esm_json_ready,"ccBGSSSE001-Fish.esm", "NA (Quest)")

write(json_ccBGSSSE001_Fish.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\ccBGSSSE001-Fish.esm\\QuestDialogueTagsccBGSSSE001_Fish.esm.json")


## Export env

save.image(".\\Resources\\DIAL_treatment_ccBGSSSE001_Fish.esm.RData")