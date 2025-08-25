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



isolate_ids <- function(db_dial_massclass){

  db_dial_massclass <- db_dial_massclass %>%
    mutate(
      Formid_DIAL_isolated = as.character(str_extract_all(Formid_DIAL, "(?<=DIAL:)[^\\]]*")), ## get only Formid_DIAL
      Formid_INFO_isolated = as.character(str_extract_all(INFO, "(?<=INFO:)[^\\]]*"))
    )

  return (db_dial_massclass)
}



jsonreadier_skyrim.esm <- function(db_dial_massclass_isolated){
  db_dial_json_ready <- db_dial_massclass_isolated %>%
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
              !(
                # Check FULL (managing NA)
                if_else(is.na(FULL), FALSE, 
                      str_detect(FULL, regex("another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not|I'd rather be|not right now|Good luck with that|i don't have time (for this.|right now.)?$|i don't have time for that(\\.| now\\.)?$|i can't do that right now", ignore_case = TRUE))) |
                # Check RNAM (managing NA)  
                if_else(is.na(RNAM), FALSE,
                      str_detect(RNAM, regex("another time|sorry, i can't|sorry to|can't help|not interested|I'd rather not|I'd rather be|not right now|Good luck with that|i don't have time (for this.|right now.)?$|i don't have time for that(\\.| now\\.)?$|i can't do that right now", ignore_case = TRUE)))
                  )) %>%
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

  return(db_dial_json_ready)



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
          form_id = paste0(db_dial_json_ready[i,"Formid_INFO_isolated"],"|",plugin), 
          type = "INFO RNAM",     
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


