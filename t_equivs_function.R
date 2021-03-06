#function developed by Hilary Killam, August 2019

t_equivs <- function(en_data, fr_data, output = "by_baby") {
  
  # if(!(ncol(en_data) == 555 || ncol(en_data) == 858 )) 
  #   stop("Error: English data input must be in csv from WebCDI download 'CSV/ALL'")
  # if(!(ncol(fr_data) == 572 || ncol(fr_data) == 815 )) 
  #   stop("Error: French data input must be in csv from WebCDI download 'CSV/ALL'")
  # 
  # message_summary <- "\n* * * * * \nOutputting TE summary scores for your whole dataset. If you would like to see TE scores by baby ID, please set argument output = 'by_baby'\n\n* * * * *"
  # message_by_baby <- "\n* * * * * \nOutputting TE scores by baby ID. If you would like to see summary TE scores, please set argument output = 'summary'\n\n* * * * *"
  
  if(ncol(en_data) < 600) {
    
    #WORDS AND GESTURES:
    
    #load lookup tables:
    
    lookup_en_colnames <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/wg_lookup_en_colnames.csv", 
                                   col_types = cols(.default = col_character())) #this is the lookup table for renaming the English data columns from Web CDI
    
    lookup_fr_colnames <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/wg_lookup_fr_colnames.csv",
                                   col_types = cols(.default = col_character())) #this is the lookup table for renaming the French data columns from Web CDI (CAUTION: currently Web CDI French data download column names have duplicates that must be changed manually before this code can work correctly)
    
    TE_IDs_en <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/wg_lookup_TE_IDs.csv",
                          col_types = cols(.default = col_character())) %>% select(en_word, TE_ID) #this is the lookup table for merging English data with TE IDs
    TE_IDs_fr <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/wg_lookup_TE_IDs.csv",
                          col_types = cols(.default = col_character())) %>% select(fr_word, TE_ID) #this is the lookup table for merging French data with TE IDs
    
    #change column names and select only relevant columns for calculating TE score:
    
    names(en_data) <- lookup_en_colnames$new_names[match(names(en_data), lookup_en_colnames$old_names)] #this code matches all colnames in the lookup table -- if new colnames are added to Web CDI and not in the lookup table, they will become NA
    en_data <- en_data %>% select(baby_id, recording_name, en_baa_baa:en_some)
    
    names(fr_data) <- lookup_fr_colnames$new_names[match(names(fr_data), lookup_fr_colnames$old_names)] #this code matches all colnames in the lookup table -- if new colnames are added to Web CDI and not in the lookup table, they will become NA
    fr_data <- fr_data %>% select(baby_id, recording_name, fr_bee_bee:fr_un_peu)
    
    #make data long:
    
    en_data <- en_data %>% gather(key = "word", value = "knowledge", -baby_id, -recording_name)
    fr_data <- fr_data %>% gather(key = "word", value = "knowledge", -baby_id, -recording_name)
    
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
    
    #add lang score columns:
    
    en_fr_together <- en_fr_together %>% mutate(en_score = case_when(
      knowledge_en == "produces" | knowledge_en == "understands" ~ 1,
      TRUE ~ 0
    ))
    
    en_fr_together <- en_fr_together %>% mutate(fr_score = case_when(
      knowledge_fr == "produces" | knowledge_fr == "understands" ~ 1,
      TRUE ~ 0
    ))
    
    #calculate summary TE scores for each baby:
    
    comp_TEs <- en_fr_together %>% group_by(baby_id) %>% summarize(comp_TE_score = sum(comp_TE_score, na.rm = TRUE))
    
    prod_TEs <- en_fr_together %>% group_by(baby_id) %>% summarize(prod_TE_score = sum(prod_TE_score, na.rm = TRUE))
    
    en_words <- en_fr_together %>% group_by(baby_id) %>% summarize(en_words = sum(en_score, na.rm = TRUE))
    
    fr_words <- en_fr_together %>% group_by(baby_id) %>% summarize(fr_words = sum(fr_score, na.rm = TRUE))
    
    lang_scores <- full_join(en_words, fr_words, by = "baby_id")
    
    
    total_TEs <- full_join(comp_TEs, prod_TEs, by = "baby_id") 
    total_TEs <- total_TEs %>% mutate(total_TE_score = comp_TE_score + prod_TE_score)
    
    total_TEs <- full_join(total_TEs, lang_scores, by = "baby_id")
    total_TEs <- total_TEs %>% mutate(total_vocab = en_words + fr_words)
    
    #calculate overall average TE scores & summary stats:
    
    avg_TEs <- total_TEs %>% summarize(avg_comp_TE = mean(comp_TE_score),
                                       min_comp_TE = min(comp_TE_score),
                                       max_comp_TE = max(comp_TE_score),
                                       avg_prod_TE = mean(prod_TE_score),
                                       min_prod_TE = min(prod_TE_score),
                                       max_prod_TE = max(prod_TE_score),
                                       avg_total_TE = mean(total_TE_score),
                                       min_total_TE = min(total_TE_score),
                                       max_total_TE = max(total_TE_score),
                                       avg_en_words = mean(en_words),
                                       min_en_words = min(en_words),
                                       max_en_words = max(en_words),
                                       avg_fr_words = mean(fr_words),
                                       min_fr_words = min(fr_words),
                                       max_fr_words = max(fr_words),
                                       avg_total_vocab = mean(total_vocab),
                                       min_total_vocab = min(total_vocab),
                                       max_total_vocab = max(total_vocab)
    )
    
    if(output == "by_baby") {
      message(message_by_baby)
      return(total_TEs)
    } else if(output == "summary") {
      message(message_summary)
      return(avg_TEs)
    }
    
    #for checking the TE correspondence between FR & EN to verify correct matching:
    #TEs_matching <- full_join(TE_IDs_en, TE_IDs_fr, by = "TE_ID")
    
  } else if(ncol(en_data) > 600) {
    
    
    #WORDS AND SENTENCES:
    
    #load lookup tables:
    
    lookup_en_colnames <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/ws_lookup_en_colnames.csv",
                                   col_types = cols(.default = col_character())) #this is the lookup table for renaming the English data columns from Web CDI
    
    lookup_fr_colnames <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/ws_lookup_fr_colnames.csv",
                                   col_types = cols(.default = col_character())) #this is the lookup table for renaming the French data columns from Web CDI (CAUTION: currently Web CDI French data download column names have duplicates that must be changed manually before this code can work correctly)
    
    TE_IDs_en <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/ws_lookup_TE_IDs.csv",
                          col_types = cols(.default = col_character())) %>% select(en_word, TE_ID) #this is the lookup table for merging English data with TE IDs
    TE_IDs_fr <- read_csv("https://raw.githubusercontent.com/kbhlab/translation_equivalents/master/ws_lookup_TE_IDs.csv",
                          col_types = cols(.default = col_character())) %>% select(fr_word, TE_ID) #this is the lookup table for merging French data with TE IDs
    
    
    #change column names and select only relevant columns for calculating TE score:
    
    names(en_data) <- lookup_en_colnames$new_names[match(names(en_data), lookup_en_colnames$old_names)] #this code matches all colnames in the lookup table -- if new colnames are added to Web CDI and not in the lookup table, they will become NA
    en_data <- en_data %>% select(baby_id, recording_name, en_baa_baa:en_then)
    
    names(fr_data) <- lookup_fr_colnames$new_names[match(names(fr_data), lookup_fr_colnames$old_names)] #this code matches all colnames in the lookup table -- if new colnames are added to Web CDI and not in the lookup table, they will become NA
    fr_data <- fr_data %>% select(baby_id, recording_name, fr_aie:fr_si)
    
    
    #make data long:
    
    en_data <- en_data %>% gather(key = "word", value = "knowledge", -baby_id, -recording_name)
    fr_data <- fr_data %>% gather(key = "word", value = "knowledge", -baby_id, -recording_name)
    
    
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
    
    #add lang score columns:
    
    en_fr_together <- en_fr_together %>% mutate(en_score = case_when(
      knowledge_en == "produces" | knowledge_en == "understands" ~ 1,
      TRUE ~ 0
    ))
    
    en_fr_together <- en_fr_together %>% mutate(fr_score = case_when(
      knowledge_fr == "produces" | knowledge_fr == "understands" ~ 1,
      TRUE ~ 0
    ))
    
    #calculate summary TE scores for each baby:
    
    prod_TEs <- en_fr_together %>% group_by(baby_id) %>% summarize(prod_TE_score = sum(prod_TE_score, na.rm = TRUE))
    
    en_words <- en_fr_together %>% group_by(baby_id) %>% summarize(en_words = sum(en_score, na.rm = TRUE))
    
    fr_words <- en_fr_together %>% group_by(baby_id) %>% summarize(fr_words = sum(fr_score, na.rm = TRUE))
    
    lang_scores <- full_join(en_words, fr_words, by = "baby_id")
    
    prod_TEs <- full_join(prod_TEs, lang_scores, by = "baby_id")
    
    prod_TEs <- prod_TEs %>% mutate(total_vocab = en_words + fr_words)
    
    
    #calculate overall average TE scores & summary stats:
    
    avg_TEs <- prod_TEs %>% summarize(avg_prod_TE = mean(prod_TE_score),
                                      min_prod_TE = min(prod_TE_score),
                                      max_prod_TE = max(prod_TE_score),
                                      avg_en_words = mean(en_words),
                                      min_en_words = min(en_words),
                                      max_en_words = max(en_words),
                                      avg_fr_words = mean(fr_words),
                                      min_fr_words = min(fr_words),
                                      max_fr_words = max(fr_words),
                                      avg_total_vocab = mean(total_vocab),
                                      min_total_vocab = min(total_vocab),
                                      max_total_vocab = max(total_vocab)
    )
    
    if(output == "by_baby") {
      message(message_by_baby)
      return(prod_TEs)
    } else if(output == "summary") {
      message(message_summary)
      return(avg_TEs)
    }    
  }
}