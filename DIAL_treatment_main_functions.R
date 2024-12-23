## Author: Hydraaawr https://github.com/hydraaawr; https://www.nexusmods.com/users/83984133


shaper <- function(db_dial){

    colnames(db_dial)[c(4,7)] <- c("INFO","Scriptname")

    ## NA tagging

    db_dial <- db_dial %>%
    mutate_all(~ na_if(., "")) ## replace empties with NAs

    ##  separate into 2 dbs because of record subrecord structure

    db_dial_qnam_skyrim.esm <- db_dial %>%
    filter(!is.na(X.0...3.)) %>% ## contains formid 2, therefore contains FULL and QNAM
        rename(Formid = X.0...3.) %>%
        select(Record,Formid,FULL,QNAM)

    db_dial_info_skyrim.esm <- db_dial %>%
    filter(!is.na(X.0.)) %>% ## contains formid 1, therefore contains INFO,Scriptname, RNAM. FULL COLUMN IS A SEPARATOR aberration
        rename(Formid = X.0.) %>%
        select(Record,Formid,INFO,Scriptname,RNAM)

    #########################

    ## join both

    db_dial_merged <- full_join(db_dial_qnam_skyrim.esm,db_dial_info_skyrim.esm, by = "Formid") #relationship = "many-to-many")

    return(db_dial_merged)
}




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


