library(tidyverse)
library(foreign)
library(readxl)
library(stringr)
library(naniar)
library(scales)
library(qdapTools)
library(quanteda)
library(quanteda.textstats)



"%notin%" <- function(x, y) !(x %in% y)

trust_levels <- c("Completely distrust", "Mostly distrust", "Somewhat distrust",
                  "Neither trust nor distrust",
                  "Somewhat trust", "Mostly trust", "Completely trust")

dat_raw <- read.csv("/Users/ashleyblum/Downloads/Poland+Media+Project_December+29,+2023_18.27.csv",
                      na.strings=c("-99"))

questions <- as.character(dat_raw[1,])
vars <- colnames(dat_raw)
codebook<- as.data.frame(cbind(vars, questions))

dat_raw <- dat_raw[-(1:2),]


dat_full <- dat_raw %>%
  filter(
    consent %in% "I consent, begin the study",
    screener_captcha %in% 15,
    screener_disagree %in% "Somewhat disagree",
    screener_number %in% "25",
  ) %>%
  rowwise() %>%
  mutate(
    #attention check
    attention1 = ifelse(
      (screener_web_1 == "pudelek.pl" & screener_web_17 == "echodnia.eu" & 
         screener_web_1 == "0" & screener_web_2 == "0" & screener_web_3 == "0" & screener_web_4 == "0" &
         screener_web_5 == "0" & screener_web_6 == "0" & screener_web_7 == "0" & screener_web_8 == "0" &
         screener_web_9 == "0" & screener_web_10 == "0" &
         screener_web_11 == "0" & screener_web_12 == "0" & screener_web_13 == "0" &
         screener_web_14 == "0" & screener_web_15 == "0" & screener_web_16 == "0" &
         screener_web_18 == "0"), 1, 0),
    attention2 = ifelse(
      (screener_color_3 == "Red" & screener_color_5 == "Green" & screener_color_1 == "0" &
         screener_color_2 == "0" & screener_color_4 == "0" & screener_color_6 == "0"), 1, 0),
    attention_score = (attention1 + attention2) / 2,
    
    start_date = as.Date(StartDate),

    #partisanship and political attitudes
    voted = ifelse(voted == "Yes", 1, 0),
    pisvoted = ifelse(party_voted == "pis", 1, 0),
    pissupported = ifelse(party_supported == "pis", 1, 0),
    pro_pis = ifelse(pisvoted == 1 | pissupported == 1, 1, 0 ),
    
    antipisvoted = ifelse(party_voted %in% c("ko", "nl", "td"), 1, 0),
    antipissupported = ifelse(party_supported %in% c("ko", "nl", "td"), 1, 0),
    anti_pis = ifelse(antipisvoted == 1 | antipissupported == 1, 1, 0 ),
    
    
    #demographics
    education = factor(education, levels = c("Primary school or lower", "Vocational secondary school", "General secondary school",
                                 "Bachelor's degree", "Advanced degree (Master's, doctorate etc.")),
    ba_grad = ifelse(education %in% c("Bachelor's degree", "Advanced degree (Master's, doctorate etc."), 1, 0),

    female = ifelse(gender == "Female", 1, 0),
    income = factor(income),
    income = droplevels(income, "Prefer not to say"),
    
    age = as.numeric(age),
    
    #Media Use

    media_pref_tv = ifelse(media_pref == "tv", 1, 0),
    media_pref_online = ifelse(media_pref == "online", 1, 0),
    
    use_tvp = ifelse(news_use_1 == "tvp", 1, 0),
    use_tvn = ifelse(news_use_2 == "tvn", 1, 0),
    
    
    reg_tvp = ifelse(news_use_followup_1 %in% c("A few times per week", "Everyday or almost everyday"), 1, 0),
    reg_tvn = ifelse(news_use_followup_2 %in% c("A few times per week", "Everyday or almost everyday"), 1, 0),
    
    
    pol_interest = factor(pol_interest,
                          levels = c("Not at all interested", "Slightly interested",
                                     "Moderately interested", "Very interested")),
    pol_interest_n = rescale(as.numeric(pol_interest), from = c(1, 4), to = c(0, 1)),
    
    #media 2 trust
    trust_tvp = factor(trust_tvp, levels = trust_levels),
    trust_tvn = factor(trust_tvn, levels = trust_levels),
    trust_polsat = factor(trust_polsat, levels = trust_levels),
    trust_republika = factor(trust_republika, levels = trust_levels),
    trust_trwam = factor(trust_trwam, levels = trust_levels),
    
    
    #outsider outcomes
    threat_traditions = rescale(as.numeric(outsider_threat_1), from = c(0,10), to = c(0,1)),
    threat_organizations = rescale(as.numeric(outsider_threat_2), from = c(0,10), to = c(0,1)),
    threat_values = rescale(as.numeric(outsider_threat_4), from = c(0,10), to = c(0,1)),
    threat_safety = rescale(as.numeric(outsider_threat_5), from = c(0,10), to = c(0,1)),
    
    open_integration = rescale(as.numeric(outsider_openness_m_1), from = c(0,10), to = c(1,0)),
    open_organizations = rescale(as.numeric(outsider_openness_m_2), from = c(0,10), to = c(1,0)),
    open_positive = rescale(as.numeric(outsider_openness_m_4), from = c(0,10), to = c(1,0)),
    open_admit = rescale(as.numeric(outsider_openness_m_5), from = c(0,10), to = c(1,0)),
    
    #Democratic Opposition distrust outcomes
    
    oppo_institutions = rescale(as.numeric(tusk_gov1_1), from = c(0,10), to = c(0,1)),
    oppo_corruption = rescale(as.numeric(tusk_gov1_2), from = c(0,10), to = c(0,1)),
    oppo_discord = rescale(as.numeric(tusk_gov1_3), from = c(0,10), to = c(0,1)),
    
    
    #outsider meta outcomes
    
    #note these are scaled so that 1 = high threat
    m_threat_traditions = rescale(as.numeric(outsider_threat_m_1), from = c(0,10), to = c(0,1)),
    m_threat_organizations = rescale(as.numeric(outsider_threat_m_2), from = c(0,10), to = c(0,1)),
    m_threat_values = rescale(as.numeric(outsider_threat_m_4), from = c(0,10), to = c(0,1)),
    m_threat_safety = rescale(as.numeric(outsider_threat_m_5), from = c(0,10), to = c(0,1)),
    
    #note, these are rescaled so 1 = less open
    m_open_integration = rescale(as.numeric(outsider_openness_m_1), from = c(0,10), to = c(1,0)),
    m_open_organizations = rescale(as.numeric(outsider_openness_m_2), from = c(0,10), to = c(1,0)),
    m_open_positive = rescale(as.numeric(outsider_openness_m_4), from = c(0,10), to = c(1,0)),
    m_open_admit = rescale(as.numeric(outsider_openness_m_5), from = c(0,10), to = c(1,0)),
    
    #Democratic Opposition distrust meta outcomes
    
    m_oppo_institutions = rescale(as.numeric(tusk_gov1_m_1), from = c(0,10), to = c(0,1)),
    m_oppo_corruption = rescale(as.numeric(tusk_gov1_m_2), from = c(0,10), to = c(0,1)),
    m_oppo_discord = rescale(as.numeric(tusk_gov1_m_4), from = c(0,10), to = c(0,1)),
    
    politicians_trust_interests = rescale(as.numeric(politicians_trust_1), from = c(0,10), to = c(0,1)),
    politicians_trust_corrupt = rescale(as.numeric(politicians_trust_2), from = c(0,10), to = c(0,1)),
    
    party_coop = rescale(as.numeric(party_coop_1), from = c(0,10), to = c(0,1)),
    
    Finished = case_when(Finished == "True" ~ 1,
                       Finished == "False" ~ 0)
  ) %>%
  
  rowwise() %>%
  mutate(
    outsider_threat = mean(threat_traditions, threat_organizations, threat_values, threat_safety,
                           open_integration, open_organizations, open_positive, open_admit, na.rm = T),
    oppo_distrust = mean(oppo_institutions, oppo_corruption, oppo_discord, na.rm = T),
    m_outsider_threat = mean(m_threat_traditions, m_threat_organizations, m_threat_values, m_threat_safety,
                             m_open_integration, m_open_organizations, m_open_positive, m_open_admit, na.rm = T),
    m_oppo_distrust = mean(m_oppo_institutions, m_oppo_corruption, m_oppo_discord, na.rm = T)
    ) %>%
  mutate(
    source_rec_tvp = ifelse(source_rec_1 == "tvp", 1, 0),
    source_rec_tvn = ifelse(source_rec_2 == "tvn", 1, 0)
  )

#### TO DO: ADD IN ALL TRIAL LEVEL DATA
#I think we want to create 8 columns for each person for each response (trustworthy, truthful, objective + the trust index (the average of the three responses) i.e. 32 columns total) representing each of their trials and a column representing the average of all 8 trials for the respondent  
  
 
#Main data set for those who finished the survey
dat <- dat_full %>% filter(Finished == 1)




###IGNORE THIS PART

## Long Data - Trial Level
dat_long <- dat %>%
  pivot_longer(cols = c(
    "trust_index_1", "trust_index_2",    #trust_index
    "trust_index_3", "trust_index_4", "trust_index_5",
    "trust_index_6", "trust_index_7", "trust_index_8",
    
    "trustworthy_1",  "trustworthy_2",           #trustworthy outcomes
    "trustworthy_2",  "trustworthy_3", "trustworthy_4","trustworthy_5",
    "trustworthy_6",  "trustworthy_7", "trustworthy_8", 
    
    "truthful_1",  "truthful_2",       #truthful outcomes
    "truthful_3",  "truthful_4", "truthful_5",
    "truthful_6",   "truthful_7", "truthful_8", 
  
    "objective_1",  "objective_2",          #objective outcomes
    "objective_3",  "objective_4", "objective_5",
    "objective_6", "objective_7", "objective_8"
  ),
  names_to = "response_indicator", values_to = "value") %>%
  mutate(
    excerpt = case_when(str_detect(response_indicator, "_fox_1") ~ fox_id_1,
                        str_detect(response_indicator, "_fox_2") ~ fox_id_2,
                        str_detect(response_indicator, "_fox_3") ~ fox_id_3,
                        str_detect(response_indicator, "_cnn_1") ~ cnn_id_1,
                        str_detect(response_indicator, "_cnn_2") ~ cnn_id_2),
    source = case_when(str_detect(excerpt,"fox") ~ "fox",       #excerpt source
                       str_detect(excerpt, "cnn") ~ "cnn",
    question = case_when(
      source == "cnn" ~ str_extract(response_indicator, ".+(?=_cnn?)"), 
      source == "fox" ~ str_extract(response_indicator, ".+(?=_fox?)"))
    ),
    trial = paste(ResponseId, excerpt, sep = "_")
  ) %>%
  relocate(excerpt) %>%
  relocate(ResponseId) %>%
  mutate(trump_support = NA) %>%
  pivot_wider(id_cols = c("trial", "ResponseId", "excerpt",
                          "treated", "source",
                          "voted", "party_voted",
                          "pro_pis", "anti_pis",
                          "political_interest_n",
                          "use_tvp", "use_tvn",
                          "reg_tvp", "reg_tvn",
                          "attention_score",
                          #"start_date",
                          "age", "female", "income",
                          "education", "ba_grad", "employment"),
              
              names_from = question,
              values_from = value) %>%
  mutate(counter_source = 
           case_when(source == "tvp" & anti_pis == 1 ~ 1,
                     source == "tvn" & pro_pis == 1 ~ 1,
                     source == "tvp" & pro_pis == 1 ~ 0,
                     source == "tvn" & anti_pis == 1 ~ 0),
         aligned_source = 
           case_when(source == "tvn" & anti_pis == 1 ~ 1,
                     source == "tvp" & pro_pis == 1 ~ 1,
                     source == "tvn" & pro_pis == 1 ~ 0,
                     source == "tvp" & anti_pis == 1 ~ 0),
         )





dat_long <- data.table::rbindlist(list(dat_long_d, dat_long_r), use.names = TRUE, fill = TRUE)

dat_attitudes <- dat %>%
  dplyr::select(ResponseId, ukr_support, ends_with("ft"), allies, allies_n)

dat_long <- left_join(dat_long, dat_attitudes, by = "ResponseId")

excerpt_topics <- read.csv("/Users/ashleyblum/Downloads/Study 2 Article Database - Sheet1 (1).csv")

excerpt_topics$source <- NULL

dat_long <- left_join(dat_long, excerpt_topics, by = "excerpt")






#dat is the main data set and includes only those who finish the study
#dat full is everyone who consented to participate and passed the initial screens but includes people who dropped out in the middle of the study
#dat long is equivalent to dat, reformatted to trial level
save(dat, file = "/Users/ashleyblum/Documents/Work/Research/Source Branding and Foreign Affairs/dat_branding3.RData")
save(dat_full, file = "/Users/ashleyblum/Documents/Work/Research/Source Branding and Foreign Affairs/dat_full_branding3.RData")
save(dat_long, file = "/Users/ashleyblum/Documents/Work/Research/Source Branding and Foreign Affairs/dat_long_branding3.RData")
