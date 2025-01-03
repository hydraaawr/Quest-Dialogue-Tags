## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


library(tidyr)
library(dplyr)
library(jsonlite)


rm(list = ls())

shaper <- function(db_dial){

    colnames(db_dial)[c(4,7)] <- c("INFO","Scriptname")

    ## NA tagging

    db_dial <- db_dial %>%
    mutate_all(~ na_if(as.character(.), "")) ## replace empties with NAs

    ##  separate into 2 dbs because of record subrecord structure

    db_dial_qnam_skyrim.esm <- db_dial %>%
    filter(!is.na(X.0...3.)) %>% ## contains Formid_DIAL 2, therefore contains FULL and QNAM
        rename(Formid_DIAL = X.0...3.) %>%
        select(Record,Formid_DIAL,FULL,QNAM)

    db_dial_info_skyrim.esm <- db_dial %>%
    filter(!is.na(X.0.)) %>% ## contains Formid_DIAL 1, therefore contains INFO,Scriptname, RNAM. FULL COLUMN IS A SEPARATOR aberration
        rename(Formid_DIAL = X.0.) %>%
        select(Record,Formid_DIAL,INFO,Scriptname,RNAM)

    #########################

    ## join both

    db_dial_merged <- full_join(db_dial_qnam_skyrim.esm,db_dial_info_skyrim.esm, by = "Formid_DIAL") #relationship = "many-to-many")

    return(db_dial_merged)
}




json_gen <- function(db_dial_json_ready,plugin,target_NA){


  json_obj_list <- vector("list", length(nrow(db_dial_json_ready)))



  for(i in seq_len(nrow(db_dial_json_ready))){

    if(!identical(db_dial_json_ready[i,"FULL_trans"],target_NA) && identical(db_dial_json_ready[i,"RNAM_trans"],target_NA)){ ## If FULL_trans is not empty and RNAM is, include FULL

      json_obj_list[[i]] <- { 
        list(
          form_id = paste0(db_dial_json_ready[i,"Formid_DIAL_isolated"],"|",plugin), 
          type = "DIAL FULL",     
          string = db_dial_json_ready[i,"FULL_trans"]    
        )
      }

    } else if(identical(db_dial_json_ready[i,"FULL_trans"],target_NA) && !identical(db_dial_json_ready[i,"RNAM_trans"],target_NA)) { ## opposite of previous
      json_obj_list[[i]] <- { 
        list(
          form_id = paste0(db_dial_json_ready[i,"Formid_DIAL_isolated"],"|",plugin), 
          type = "DIAL FULL",     
          string = db_dial_json_ready[i,"RNAM_trans"]    
        )

      }
   
    } else { ## both filled (no target_NA on neither)

      json_obj_list[[i]] <- { 
        list(
          form_id = paste0(db_dial_json_ready[i,"Formid_INFO_isolated"],"|",plugin), 
          type = "INFO RNAM",     
          string = db_dial_json_ready[i,"RNAM_trans"]    
        )
      }

    }
  }

  json_obj_list <- json_obj_list[!duplicated(json_obj_list)] ## cleaning duplicates 
  json_output <- toJSON(json_obj_list, pretty = TRUE, auto_unbox = TRUE)

  message(sprintf("GENERATED JSON ENTRIES: %d",
    length(json_obj_list)))

  return(json_output)

}


