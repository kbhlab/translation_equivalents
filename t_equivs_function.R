

t_equivs <- function(en_data, fr_data, output = "by_baby") {

  case_when(
    ncol(en_data) != 555 & ncol(en_data) != 858 & ncol(fr_data) != 572 & ncol(fr_data) != 815 ~ stop("Warning: your input data must be in the format of 'CSV/ALL' from WebCDI!"),
    TRUE ~ warning("Data format okay")
    )

  if(ncol(en_data) < 600) {
    
    #WORDS AND GESTURES:
  
    #rename problematic french data columns:
    names(fr_data)[120] <- "poisson_animal"
    names(fr_data)[155] <- "eau_beverage"
    names(fr_data)[167] <- "poisson_food"
    names(fr_data)[212] <- "bain_object"
    names(fr_data)[277] <- "eau_not_beverage"
    names(fr_data)[315] <- "bain_routine"
    
    lookup_en_colnames <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/wg_lookup_en_colnames.csv") #this is the lookup table for renaming the English data columns from Web CDI
    
    lookup_fr_colnames <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/wg_lookup_fr_colnames.csv") #this is the lookup table for renaming the French data columns from Web CDI (CAUTION: currently Web CDI French data download column names have duplicates that must be changed manually before this code can work correctly)
    
    TE_IDs_en <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/wg_lookup_TE_IDs_en.csv") #this is the lookup table for merging English data with TE IDs
    TE_IDs_fr <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/wg_lookup_TE_IDs_fr.csv") #this is the lookup table for merging French data with TE IDs
    
    #change column names and select only relevant columns for calculating TE score:
    
    names(en_data) <- lookup_en_colnames$new_names[match(names(en_data), lookup_en_colnames$old_names)] #this code matches all colnames in the lookup table -- if new colnames are added to Web CDI and not in the lookup table, they will become NA
    en_data <- en_data %>% select(-contains("discard"))
    
    names(fr_data) <- lookup_fr_colnames$new_names[match(names(fr_data), lookup_fr_colnames$old_names)] #this code matches all colnames in the lookup table -- if new colnames are added to Web CDI and not in the lookup table, they will become NA
    fr_data <- fr_data %>% select(-contains("discard"))
    
    #make data long:
    
    en_data <- en_data %>% gather(key = "word", value = "knowledge", -study_name, -baby_id, -recording_name)
    fr_data <- fr_data %>% gather(key = "word", value = "knowledge", -study_name, -baby_id, -recording_name)
    
    #merge with TE IDs:
    
    en_data <- left_join(TE_IDs_en, en_data, by = c("en_word" = "word")) #this code merges English data with corresponding TE IDs
    fr_data <- left_join(TE_IDs_fr, fr_data, by = c("fr_word" = "word")) #this code merges French data with corresponding TE IDs
    
    #merge English and French together:
    
    en_fr_together <- inner_join(en_data, fr_data, by = c("TE_ID", "baby_id"), suffix = c("_en", "_fr")) #this code merges the English with the French data, based on TE ID and subject ID
    en_fr_together <- en_fr_together %>% select(baby_id, recording_name_en, TE_ID, en_word, knowledge_en, fr_word, knowledge_fr) #this code reorders the columns for easier reading in View mode
    
    #add TE score columns:
    
    en_fr_together <- en_fr_together %>% mutate(comp_TE_score = case_when(
      knowledge_en == "understands" & knowledge_fr == "understands" ~ 1,
      knowledge_en == "understands" & knowledge_fr == "produces" ~ 1,
      knowledge_en == "produces" & knowledge_fr == "understands" ~ 1,
      TRUE ~ 0
    ))
    
    en_fr_together <- en_fr_together %>% mutate(prod_TE_score = case_when(
      knowledge_en == "produces" & knowledge_fr == "produces" ~ 1,
      TRUE ~ 0
    ))
    
    #calculate summary TE scores for each baby:
    
    comp_TEs <- en_fr_together %>% group_by(baby_id) %>% summarize(comp_TE_score = sum(comp_TE_score, na.rm = TRUE))
    
    prod_TEs <- en_fr_together %>% group_by(baby_id) %>% summarize(prod_TE_score = sum(prod_TE_score, na.rm = TRUE))
    
    total_TEs <- full_join(comp_TEs, prod_TEs, by = "baby_id")
    total_TEs <- total_TEs %>% mutate(total_TE_score = comp_TE_score + prod_TE_score)
    
    #calculate overall average TE scores & summary stats:
    
    avg_TEs <- total_TEs %>% summarize(avg_comp_TE = mean(comp_TE_score),
                                       min_comp_TE = min(comp_TE_score),
                                       max_comp_TE = max(comp_TE_score),
                                       avg_prod_TE = mean(prod_TE_score),
                                       min_prod_TE = min(prod_TE_score),
                                       max_prod_TE = max(prod_TE_score),
                                       avg_total_TE = mean(total_TE_score),
                                       min_total_TE = min(total_TE_score),
                                       max_total_TE = max(total_TE_score)
    )
    
    if(output == "by_baby") {
      return(total_TEs)
      warning("Outputting TE scores by baby ID. If you would like to see summary scores, please set argument output = 'summary'")
    } else if(output == "summary") {
      return(avg_TEs)
      warning("Outputting TE summary scores. If you would like to see TE scores by baby ID, please set argument output = 'by_baby'")
    }
    
    #for checking the TE correspondence between FR & EN to verify correct matching:
    #TEs_matching <- full_join(TE_IDs_en, TE_IDs_fr, by = "TE_ID")
    
  } else if(ncol(en_data) > 600) {
    
    
    #WORDS AND SENTENCES:
    
    #rename problematic data columns:
    names(fr_data)[94] <- "poisson_animal"
    names(fr_data)[158] <- "eau_beverage"
    names(fr_data)[181] <- "poisson_food"
    names(fr_data)[302] <- "bain_object"
    names(fr_data)[344] <- "eau_not_beverage"
    names(fr_data)[419] <- "bain_routine"
    names(fr_data)[96] <- "poulet_animal"
    names(fr_data)[186] <- "poulet_food"
    names(fr_data)[288] <- "pot_jar"
    names(fr_data)[328] <- "pot_potty"
    names(fr_data)[119] <- "baton_bat"
    names(fr_data)[339] <- "baton_stick"
    names(fr_data)[175] <- "orange_food"
    names(fr_data)[593] <- "orange_colour"
    names(fr_data)[723] <- "fin_is_1"
    names(fr_data)[737] <- "fin_is_2"
    names(fr_data)[733] <- "fini2"
    names(en_data)[740] <- "feet_plural"
    
    lookup_en_colnames <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/ws_lookup_en_colnames.csv") #this is the lookup table for renaming the English data columns from Web CDI
    
    lookup_fr_colnames <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/ws_lookup_fr_colnames.csv") #this is the lookup table for renaming the French data columns from Web CDI (CAUTION: currently Web CDI French data download column names have duplicates that must be changed manually before this code can work correctly)
    
    TE_IDs_en <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/ws_lookup_TE_IDs_en.csv") #this is the lookup table for merging English data with TE IDs
    TE_IDs_fr <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/ws_lookup_TE_IDs_fr.csv") #this is the lookup table for merging French data with TE IDs
    
    
    #change column names and select only relevant columns for calculating TE score:
    
    names(en_data) <- lookup_en_colnames$new_names[match(names(en_data), lookup_en_colnames$old_names)] #this code matches all colnames in the lookup table -- if new colnames are added to Web CDI and not in the lookup table, they will become NA
    en_data <- en_data %>% select(-contains("discard"))
    
    names(fr_data) <- lookup_fr_colnames$new_names[match(names(fr_data), lookup_fr_colnames$old_names)] #this code matches all colnames in the lookup table -- if new colnames are added to Web CDI and not in the lookup table, they will become NA
    fr_data <- fr_data %>% select(-contains("discard"))
    
    
    #make data long:
    
    en_data <- en_data %>% gather(key = "word", value = "knowledge", -study_name, -baby_id, -recording_name)
    fr_data <- fr_data %>% gather(key = "word", value = "knowledge", -study_name, -baby_id, -recording_name)
    
    
    #merge with TE IDs:
    
    en_data <- left_join(TE_IDs_en, en_data, by = c("en_word" = "word")) #this code merges English data with corresponding TE IDs
    fr_data <- left_join(TE_IDs_fr, fr_data, by = c("fr_word" = "word")) #this code merges French data with corresponding TE IDs
    
    
    #merge English and French together:
    
    en_fr_together <- inner_join(en_data, fr_data, by = c("TE_ID", "baby_id"), suffix = c("_en", "_fr")) #this code merges the English with the French data, based on TE ID and subject ID
    en_fr_together <- en_fr_together %>% select(baby_id, recording_name_en, TE_ID, en_word, knowledge_en, fr_word, knowledge_fr) #this code reorders the columns for easier reading in View mode
    
    
    #add TE score column:
    
    en_fr_together <- en_fr_together %>% mutate(prod_TE_score = case_when(
      knowledge_en == "produces" & knowledge_fr == "produces" ~ 1,
      TRUE ~ 0
    ))
    
    
    #calculate summary TE scores for each baby:
    
    prod_TEs <- en_fr_together %>% group_by(baby_id) %>% summarize(prod_TE_score = sum(prod_TE_score, na.rm = TRUE))
    
    
    #calculate overall average TE scores & summary stats:
    
    avg_TEs <- prod_TEs %>% summarize(avg_prod_TE = mean(prod_TE_score),
                                      min_prod_TE = min(prod_TE_score),
                                      max_prod_TE = max(prod_TE_score)
    )
    
    if(output == "by_baby") {
      return(prod_TEs)
      warning("Outputting TE scores by baby ID. If you would like to see summary scores, please set argument output = 'summary'")
    } else if(output == "summary") {
      return(avg_TEs)
      warning("Outputting TE summary scores. If you would like to see TE scores by baby ID, please set argument output = 'by_baby'")
    }    
  }
}
