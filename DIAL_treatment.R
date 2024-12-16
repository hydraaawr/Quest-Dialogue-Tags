library(tidyr)
library(stringr)
library(dplyr)
library(jsonlite)


rm(list = ls())
## Loading

db_dial_skyrim.esm <- read.csv(".\\dbs\\db_DIAL_skyrim.esm.csv", sep = ";")


## DB SHAPING #######################################################################


colnames(db_dial_skyrim.esm)[c(4,7)] <- c("INFO","Scriptname")

## NA tagging

db_dial_skyrim.esm <- db_dial_skyrim.esm %>%
  mutate_all(~ na_if(., "")) ## replace empties with NAs

##  separate into 2 dbs because of record subrecord structure

db_dial_qnam_skyrim.esm <- db_dial_skyrim.esm %>%
  filter(!is.na(X.0...3.)) %>% ## contains formid 2, therefore contains FULL and QNAM
    rename(Formid = X.0...3.) %>%
      select(Record,Formid,FULL,QNAM)

db_dial_info_skyrim.esm <- db_dial_skyrim.esm %>%
  filter(!is.na(X.0.)) %>% ## contains formid 1, therefore contains INFO,Scriptname, RNAM. FULL COLUMN IS A SEPARATOR aberration
    rename(Formid = X.0.) %>%
      select(Record,Formid,INFO,Scriptname,RNAM)


#########################


## join both

db_dial_skyrim.esm_merged <- full_join(db_dial_qnam_skyrim.esm,db_dial_info_skyrim.esm, by = "Formid") #relationship = "many-to-many")



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
      


## ready for json db

db_dial_skyrim.esm_json_ready <- db_dial_skyrim.esm_massclass %>%
   mutate(
      Formid_isolated = as.character(str_extract_all(Formid, "(?<=DIAL:)[^\\]]*")) ## get only formid
    ) %>%
          filter(!is.na(QNAM_type) & (!is.na(Scriptname) | !str_detect(QNAM,"City Dialogue")) & (!is.na(RNAM) | (!is.na(FULL)))) %>% 
              mutate(
                FULL_trans = paste0(FULL, " (Quest)"), ## Add "(Quest)"
                RNAM_trans = paste0(RNAM, " (Quest)")
              )


################################################################################



## Json generation:

json_gen <- function(db_dial_json_ready,plugin){


  json_obj_list <- vector("list", length(nrow(db_dial_json_ready)))
  json_extra_obj_list <- vector("list") ## in case we need to save the FULL and RNAM of the same



  for(i in seq_len(nrow(db_dial_json_ready))){

    if(!identical(db_dial_json_ready[i,"FULL_trans"],"NA (Quest)") && identical(db_dial_json_ready[i,"RNAM_trans"],"NA (Quest)")){ ## If FULL_trans is not empty and RNAM is, include FULL

      json_obj_list[[i]] <- { 
        list(
          form_id = paste0(db_dial_json_ready[i,"Formid_isolated"],"|",plugin), 
          type = "DIAL FULL",     
          string = db_dial_json_ready[i,"FULL_trans"]    
        )
      }

    } else if(identical(db_dial_json_ready[i,"FULL_trans"],"NA (Quest)") && !identical(db_dial_json_ready[i,"RNAM_trans"],"NA (Quest)")) { ## opposite of previous
      json_obj_list[[i]] <- { 
        list(
          form_id = paste0(db_dial_json_ready[i,"Formid_isolated"],"|",plugin), 
          type = "DIAL FULL",     
          string = db_dial_json_ready[i,"RNAM_trans"]    
        )

      }
   
    } else { ## both filled (no NA (Quest) on neither)

      json_obj_list[[i]] <- { 
        list(
          form_id = paste0(db_dial_json_ready[i,"Formid_isolated"],"|",plugin), 
          type = "DIAL FULL",     
          string = db_dial_json_ready[i,"FULL_trans"]    
        )
      }

      json_extra_obj_list[[i]] <- { 
        list(
          form_id = paste0(db_dial_json_ready[i,"Formid_isolated"],"|",plugin), 
          type = "DIAL FULL",     
          string = db_dial_json_ready[i,"RNAM_trans"]    
        )
      }
    }
  }

  json_extra_obj_list <- json_extra_obj_list[lengths(json_extra_obj_list) != 0] ## clean NULL objects
  ## print(length(json_extra_obj_list))
  json_obj_list <- append(json_obj_list,json_extra_obj_list) ## join both
  json_obj_list <- json_obj_list[!duplicated(json_obj_list)] ## cleaning duplicates 
  json_output <- toJSON(json_obj_list, pretty = TRUE, auto_unbox = TRUE)

  message(sprintf("GENERATED JSON ENTRIES: %d",
    length(json_obj_list)))

  return(json_output)

}

json_skyrim.esm <- json_gen(db_dial_skyrim.esm_json_ready,"Skyrim.esm")


write(json_skyrim.esm, ".\\SKSE\\Plugins\\DynamicStringDistributor\\Skyrim.esm\\QuestDialogueTagsSkyrim.esm.json")


