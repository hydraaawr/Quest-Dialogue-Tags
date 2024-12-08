library(tidyr)
library(stringr)
library(dplyr)
library(jsonlite)


rm(list = ls())
db_dial_skyrim.esm <- read.csv(".\\dbs\\db_DIAL_skyrim.esm.csv")


## DB SHAPING #######################################################################

colnames(db_dial_skyrim.esm)[2] <- "Formid"

## NA tagging

db_dial_skyrim.esm <- db_dial_skyrim.esm %>%
  mutate_all(~ na_if(., "")) ## replace empties with NAs


## Coalesce and clean the product

db_dial_skyrim.esm_coalesFormid <- db_dial_skyrim.esm %>% 
  mutate(Formid = coalesce(Formid,X.0...3.)) %>% ## unify both formid cols
    filter(!is.na(Formid)) %>% ## further cleaning
        select(-X.0...3.) ## remove extra formid col





## db with transformed FULL, and isolated formid cols (ready for extraction)

db_dial_skyrim.esm_json_ready <- db_dial_skyrim.esm_coalesFormid %>%
    mutate(
      Formid_isolated = as.character(str_extract_all(Formid, "(?<=DIAL:)[^\\]]*")) ## get only formid
    ) %>%
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
          str_detect(QNAM, "Favor|Freeform|Tutorial|BQ|Farm") ~ "misc" ## Miscellaneous INCLUDE CITY DIALOGUE MANUALLY?
        )
      ) %>% ## filter those that didnt have a clear dialogue formid, NA quest (probably cutted content) and match any type of quest
        filter(!is.na(Formid_isolated) & !is.na(QNAM) & !is.na(QNAM_type)) %>% 
            mutate(
              FULL_trans = paste0(FULL, " (Quest)") ## Add "(Quest)"
            ) %>%
              select(Formid_isolated,FULL_trans,QNAM, QNAM_type) ## select cols of interest





################################################################################

## Json generation:

json_gen <- function(db_dial_json_ready,plugin){


  json_obj_list <- vector("list", length(nrow(db_dial_json_ready)))



  for(i in seq_len(nrow(db_dial_json_ready))){

    json_obj_list[[i]] <- { 
     list(
       form_id = paste0(db_dial_json_ready[i,"Formid_isolated"],"|",plugin), 
       type = "DIAL FULL",     
       string = db_dial_json_ready[i,"FULL_trans"]    
     )
   }

   
  }
  
  json_output <- toJSON(json_obj_list, pretty = TRUE, auto_unbox = TRUE)



  return(json_output)

}

json_skyrim.esm <- json_gen(db_dial_skyrim.esm_json_ready,"Skyrim.esm")


#write(json_skyrim.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Skyrim.esm\\QuestDialogueTagsSkyrim.esm.json")





## Testing #####################################################################


