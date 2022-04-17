#### Preamble ####
# Purpose: Clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Mahak Jain
# Data: 3 January 2021
# Contact: mahak.jain@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the GSS data and saved it to inputs/data
# - Don't forget to gitignore it!



#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
library(janitor)



raw_data <- read.csv("inputs/data/raw_data.csv")
dict <- read_lines("inputs/data/codebook.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("inputs/data/Stata.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))

# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

# Adding selected variables 

gss <- raw_data %>% 
  select(recid, 
         agegr10,
         sex,
         marstat,
         hsdsizec,
         ageprgr0,
         ageprgrd,
         prcode,
         icr_10,
         socnet,
         icr_30,
         vcg_310,
         vcg_320,
         vcg_340,
         vbr_10,
         vbr_15,
         vbr_20,
         vbr_25,
         vbr_30,
         vbr_35,
         vbr_40,
         vbr_45,
         rep_05,
         svr_10,
         svr_25,
         svr_30,
         svr_35,
         svr_40,
         svr_45,
         vismin,
         visminpr,
         sbl_100,
         sbl_200,
         sbl_300,
         sbl_500,
         sbl_700,
         sbl_800,
         sbl_820,
         pct_10,
         tip_10,
         tip_15,
         tip_20,
         tip_22,
         tip_25,
         tnp_10,
         rlm_10,
         rlm_15,
         rlm_20,
         srh_110,
         srh_115,
         slm_01,
         incm) %>% 
  mutate_at(vars(agegr10:incm), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(agegr10:incm),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))
view(gss)
# Fixing the names
gss <- gss %>% 
  clean_names() %>% 
  rename(id = recid,
         age_grp = agegr10,
         sex = sex,
         mar_stat = marstat,
         hh_size = hsdsizec,
         age_grppr = ageprgr0,
         age_diffpr = ageprgrd,
         prov = prcode,
         int_use_month = icr_10,
         soc_net = socnet,
         int_use_freq = icr_30,
         vol_avg_month = vcg_310,
         vol_metnewppl = vcg_320,
         donate = vcg_340,
         voted = vbr_10,
         vote_elig = vbr_15,
         no_vote_reason = vbr_20,
         vote_next = vbr_25,
         prov_vote = vbr_30,
         prov_vote_elig = vbr_35,
         mun_vote = vbr_40,
         mun_vote_elig = vbr_45,
         pol_interest = rep_05,
         sv_hr = svr_10,
         sv_law = svr_25,
         sv_gendereq = svr_30,
         sv_lang = svr_35,
         sv_div = svr_40,
         sv_abor = svr_45,
         vis_min = vismin,
         vis_min_pr = visminpr,
         trust_gen = pct_10,
         trust_fam = tip_10,
         trust_nb = tip_15,
         trust_work_school = tip_20,
         trust_lang = tip_22,
         trust_stranger = tip_25,
         trust_np = tnp_10,
         wallet_n = rlm_10,
         wallet_pol = rlm_15,
         wallet_stranger = rlm_20,
         self_gen_health = srh_110,
         self_mental_health = srh_115,
         sub_well = slm_01,
         sbl_com = sbl_100,
         sbl_city = sbl_200,
         sbl_prov = sbl_300,
         sbl_can = sbl_500,
         sbl_origin = sbl_700,
         sbl_ethnic = sbl_800,
         sbl_lang = sbl_820,
         annual_inc = incm)


#### Clean up ####

#Adding civic engagement of individual as a Member or participant of communities:
vol_work <- raw_data %>% 
  mutate(member_part_vol_work = case_when(
    cer_110=="Yes"~ "Union",
    cer_120=="Yes" ~ "Political Party",
    cer_140=="Yes" ~ "Sports or Recreational",
    cer_150=="Yes" ~ "Cultural or Educational",
    cer_160=="Yes" ~ "Religious", 
    cer_170=="Yes" ~ "School or Community",
    cer_180=="Yes" ~ "Service Club",
    cer_190=="Yes" ~ "Seniors Group",
    cer_200=="Yes" ~ "Yputh Organization",
    cer_210=="Yes" ~ "Immigrant or Ethnic",
    cer_230=="Yes" ~ "Other")) %>% 
    #TRUE~ "NA")) %>% 
  select(member_part_vol_work) %>% 
  pull()

#Editing variable for political engagement
pol_int <- raw_data %>% 
  mutate(pol_eng = case_when(
    rep_10=="Yes"~ "Searched for information",
    rep_20=="Yes" ~ "Volunteered for political party",
    rep_30=="Yes" ~ "Expressed views - News/politician",
    rep_35=="Yes" ~ "Expressed views - Internet",
    rep_40=="Yes" ~ "Signed paper petition", 
    rep_45=="Yes" ~ "Signed Internet petition",
    rep_50=="Yes" ~ "Product choice ethical reasons",
    rep_60=="Yes" ~ "SAttended public meeting",
    rep_70=="Yes" ~ "Spoke at public meeting",
    rep_80=="Yes" ~ "Participated in a demonstration",
    rep_85=="Yes" ~ "Visible sign of support", 
    TRUE~ "NA")) %>% 
  select(pol_eng) %>% 
  pull()


gss <- gss %>% mutate(member_part_vol_work = vol_work, pol_eng = pol_int)





#In the end, converting gss into csv
write_csv(gss, "inputs/data/cleaned_gss.csv")
    