
library(dplyr)
library(jsonlite)
library(lubridate)
library(purrr)
library(readr)
library(rvest)
library(stringr)
library(tidyr)

# scrape the https://www.drugs.com/newdrugs-archive/ website for details on all FDA approvals from 2003-2019
# sites are indexed by month and year (will only use data from 2010+ in the end)
# the data isn't always consistent across time so there is a fair bit of manual tweaking incorporated into the function

GetFirstApprovals <- function(month, year){
  
  fda_approvals <- xml2::read_html(paste0("https://www.drugs.com/newdrugs-archive/", month, "-", year, ".html")) %>% 
    html_nodes('.newsItem')
  
  # we can pull out individual html elements that contain drug names and a brief description
  brand_name <- fda_approvals %>%
    html_node('a') %>%
    html_text() %>%
    as.data.frame() %>%
    filter(. != "") %>%
    unlist() %>%
    as.vector()
  
  # for some reason, a few drugs don't have a 'brand name' in the database
  if(month == "march" & year == "2011"){
    brand_name <- append(brand_name, "Adenovirus type 4 and type 7 vaccine" , after=2) 
  }
  
  if(month == "august" & year == "2019"){
    brand_name <- append(brand_name, "pretomanid", after=1)
  }
  
  if(month == "june" & year == "2018"){
    brand_name <- append(brand_name, "moxidectin", after=1)
  }
  
  description <- fda_approvals %>%
    html_nodes("p:not(.drug-subtitle)") %>%
    html_text() %>%
    as.data.frame() %>%
    filter(. != "Marketing Status: Discontinued" & . != "") %>%
    filter((str_detect(., "Tafinlar is also indicated") == FALSE) & 
             (str_detect(., "Mekinist is also indicated") == FALSE) &
             (str_detect(., "CAR T therapy") == FALSE)) %>% # fix for Tafinlar & Mekinist
    filter(. != "The last doses of Gardasil (quadrivalent) expired on May 1, 2017 and it is no longer available for use in the United States." & 
             . != "Gardasil has now been replaced by Gardasil 9." & . != "Gardasil (human papillomavirus vaccine) is a recombinant vaccine indicated in girls and women 9 through 26 years of age for the prevention of:" & 
             . != "Gardasil is indicated  in boys and men 9 through 26 years of age for the prevention of:") %>% 
    # want to avoid repeats (most commonly without parentheses)
    # but also need to manually code in exceptions so we get descriptions for every drug
    filter(str_detect(., "\\(") | 
             str_detect(., "ella|Exforge|Hylenex|Crestor|Oraqix|Restylane|Cyanokit|Radiesse|Zolpimist|Docetaxel|Intermezzo|Qutenza|Captique|Hylaform|Soltamox|LoSeasonique|Metozolv|Azor|Fentora|OraDisc|Riomet|Crestor|Fortical|MoviPrep|Evithrom|ACAM2000|Embeda|Extavia|Zuplenz|Navstel|CaloMist|Duetact|Xolegel|FluMist|Bexxar|Reyataz|Xolair|Gardasil|Juvéderm|Orapred|Extina|Zipsor|Caldolor|Azficel-T|Perlane|Perforomist|Vitrase|HalfLytely|Taclonex|Exubera|Flector|Recothrom|Vectical|Testosterone|Myfortic|Fazaclo|Adenovirus|Soliris|Ceprotin|Janumet|Aplenzin|H5N1|AzaSite|Fosamax|ChiRhoStim")) %>%
    unlist() %>% 
    as.vector()
  
  # and more manual fixes for the couple of drugs without descriptions: 
  if(month == "april" & year == "2013"){
    description <- append(description, "", after=4) # Kcentra
  }
  
  if(month == "april" & year == "2007"){
    description <- append(description, "", after=3) # Veramyst
  }
  
  if(month == "december" & year == "2014"){
    description <- description[-6] 
  }
  
  # the following need a little more help to get from the "drug-subtitle" class (regex to the rescue)
  drug_details <- fda_approvals %>% 
    html_nodes('.drug-subtitle') %>%
    html_text() 
  
  dates <- as.character(str_match(string = drug_details, pattern = ("(?<=Approval: ).*(?=Company)")))
  company <- as.character(str_match(string = drug_details, pattern = ("(?<=Company: ).*(?=Treatment)")))
  condition <- as.character(str_match(string = drug_details, pattern = ("(?<=Treatment for: ).*")))
  
  if(month == "august" & year == "2017"){
    dates <- dates[-11] 
    company <- company[-11] 
    condition <- condition[-11] 
  }
  
  if(month == "december" & year == "2012"){
    dates <- dates[-10] 
    company <- company[-10] 
    condition <- condition[-10] 
  }
  
  # all together now
  approvals <- cbind(brand_name, description, dates, company, condition) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    unique()
}

months <- rep(c("january","february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"), 17)
years <- rep(c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"), 12)

# iterate across all months/years, then combine into a single data frame 
scraped_data <- map2(months, years, GetFirstApprovals) %>%
  do.call(rbind, .) %>%
  mutate(dates_clean = gsub("Company: .*","", dates)) %>%
  mutate(approval_date = as_date(dates_clean, "%B %d, %Y"))

# can't run this within dplyr:
generic_name <- str_match(scraped_data$description, "\\(([^()]*)\\)") %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  mutate(generic_name = V2,
         generic_name_fw = str_extract(generic_name, '\\w*')) %>%
  select(generic_name, generic_name_fw)

## TODO: fix up description for June 2006 Gardsail (important update): In December 2014, the FDA announced the approval of Gardasil 9 for the prevention of cervical, vulvar, vaginal, and anal cancers caused by HPV types 6, 11, 16, 18, 31, 33, 45, 52 and 58. Gardasil 9 adds protection against five additional HPV types - 31, 33, 45, 52 and 58 - which cause approximately 20 percent of cervical cancers.

scraped_data_generic <- cbind(scraped_data, generic_name)

# load in ATC categories for drugs (link on CUI) 
drug_types <- read_csv("ATC_bioportal.csv") %>%
  mutate(atc_class = gsub(".+UATC/(\\w{1}).+$", "\\1", `Class ID`), # pull out ATC class from url
         generic_name = `Preferred Label`) %>%
  select(atc_class, generic_name) 

# combine (using full generic name as well as just first word)
scraped_data_atc <- scraped_data_generic %>% 
  left_join(drug_types, by = "generic_name")

scraped_data_atc_fw <- scraped_data_generic %>% 
  left_join(drug_types %>% mutate(generic_name_fw = generic_name) %>% select(-generic_name), by = "generic_name_fw")

# all together:
scraped_data_all <- left_join(scraped_data_atc, scraped_data_atc_fw, by = c("brand_name", "generic_name", "description", "company", "condition", "approval_date")) %>% 
  select(-dates.x, -dates.y, -dates_clean.x, -dates_clean.y, -generic_name_fw.x, -generic_name_fw.y)

# because some drugs have more than one ATC class (sigh), need to one-hot encode:
OneHot <- function(input_data, ATC_class, class_var){

  one_hot_encoded <- input_data %>%
    group_by(brand_name) %>%
    mutate(!!class_var := ifelse((any(atc_class.x == ATC_class) | any(atc_class.y == ATC_class)), TRUE, FALSE)) %>%
    select(brand_name, class_var) %>%
    unique() %>%
    ungroup()
  
  return(one_hot_encoded)

}

atc_list <- list("A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R", "S", "V")
var_list <- list("ATC_A", "ATC_B", "ATC_C", "ATC_D", "ATC_G", "ATC_H", "ATC_J", "ATC_L", "ATC_M", "ATC_N", "ATC_P", "ATC_R", "ATC_S", "ATC_V")
vars <- list(ATC_class = atc_list, class_var = var_list)
scraped_data_dummy <- pmap(vars, OneHot, scraped_data_all) %>%
  do.call(cbind, .) 
  
scraped_data_dedupe <- scraped_data_dummy[, !duplicated(colnames(scraped_data_dummy))] %>%
  left_join(scraped_data_generic, by = "brand_name")

# save out copy of dataset now (never know when website will change)
write_csv(scraped_data_dedupe, "scraped_approval_data.csv")


# also want to link with FDA database to ensure we're not missing anything 
# and also to filter to just "New Molecular Entities" [NMEs]

fda_submissions <- read_delim("../drugsatfda20200107/Submissions.txt", delim = "\t") %>%
  filter(SubmissionStatusDate >= "2010-01-01") %>% # 2010+ approvals
  filter(SubmissionClassCodeID %in% c(7, 8, 20, 26) | 
           (SubmissionClassCodeID == 19 & ApplNo == 211882)) %>% 
  filter(SubmissionType == "ORIG") # NMEs

fda_products <- read_delim("../drugsatfda20200107/Products.txt", delim = "\t") %>%
  select(ApplNo, DrugName, ActiveIngredient) %>%
  unique() %>%
  inner_join(fda_submissions, by = "ApplNo") %>%
  mutate(brand_name = tolower(DrugName)) %>%
  # manual fixes for join below
  mutate(brand_name = case_when(brand_name == "jevtana kit" ~ "jevtana",
                                brand_name == "arcapta neohaler" ~ "arcapta",
                                brand_name == "omontys preservative free" ~ "omontys",
                                brand_name == "tudorza pressair" ~ "tudorza",
                                brand_name == "gattex kit" ~ "gattex",
                                brand_name == "lymphoseek kit" ~ "lymphoseek",
                                brand_name == "viekira pak (copackaged)" ~ "viekira pak",
                                brand_name == "symdeko (copackaged)" ~ "symdeko",
                                brand_name == "vyleesi (autoinjector)" ~ "vyleesi",
                                brand_name == "trikafta (copackaged)" ~ "trikafta",
                                brand_name == "latanoprost" ~ "netarsudil and latanoprost ophthalmic solution",
                                TRUE~brand_name)) %>%
  inner_join(scraped_data_dedupe %>% mutate(brand_name = tolower(brand_name)), by = "brand_name") %>%
  select(-DrugName, -SubmissionClassCodeID, -ApplNo) %>%
  distinct()

write_csv(fda_products, "fda_approval_data.csv")

# There were 9 drugs missed; 6 are diagnostic agents (which I don't want) and 
# 3 are a little weird (fish oil as a drug?) which I'll manually add in:
# SUCRAID is apparently a NME, but was originally approved in 1998 (discard)
# CHOLINE C-11 is a radioactive diagnostic agent (discard)
# GALLIUM DOTATOC GA 68 is a radioactive diagnostic agent (discard)
# FLUORODOPA F18 is a radioactive diagnostic agent (discard)
# TISSUEBLUE is a staining agent (discard)
# EXEM FOAM KIT is an ultrasound contrast agent (discard)
# Benznidazole is an antiparasitic medication used in the treatment of Chagas disease (manually add)
# Raxibacumab is a human monoclonal antibody intended for the prophylaxis and treatment of inhaled anthrax (manually add)
# Omegaven is a fatty acid emulsion used for total parenteral nutrition ((manually add)

manually_curated_data <- read_csv("manually_curated_patient_numbers.csv") %>%
  mutate(dates_clean = as.Date(approval_date, "%m/%d/%y")) %>%
  select(-1, -`Biosimilar or new formulation?`, -approval_date, -generic_name, -NCTid_1, -NCTid_2, -NCTid_3, -NCTid_4, -NCTid_5, -NCTid_6, -X34, -X35) %>%
  mutate(outer_radius = total_pts_rct,
         middle_radius = placebo_pts_rct + comparator_pts_rct,
         inner_radius = placebo_pts_rct) %>%
  filter(!is.na(brand_name)) %>%
  mutate(category = case_when((rowSums(.[2:15], na.rm = TRUE) > 1) | 
                                (ATC_G == TRUE | ATC_H == TRUE | ATC_P == TRUE | ATC_V == TRUE | ATC_S == TRUE) ~ 10, #groups with <10 or combos
                                (ATC_A == TRUE) ~ 2,
                                (ATC_B == TRUE) ~ 3,
                                (ATC_C == TRUE) ~ 4, 
                                (ATC_D == TRUE) ~ 5,
                                (ATC_J == TRUE) ~ 6,
                                (ATC_L == TRUE) ~ 7,
                                (ATC_M == TRUE) ~ 8,
                                (ATC_N == TRUE) ~ 1,
                                (ATC_R == TRUE) ~ 10,
                                TRUE ~ 11))

data_export <- manually_curated_data %>%
  mutate(year = year(dates_clean),
         month = month(dates_clean),
         x = year + ((month-1)/12),
         r = outer_radius,
         y = 100) %>%
  select(x, y, r, category, company, dates_clean, brand_name, condition, description, middle_radius, inner_radius) 

json_names <- jsonlite::toJSON(data_export, dataframe = 'rows', pretty = TRUE)
write(json_names, "../d3-viz/temp.json")






ggplot(manually_curated_data, aes(x = dates_clean, y = as.factor(color_code))) +
  geom_point(size = manually_curated_data$outer_radius/500,
             color = manually_curated_data$color_code,
             alpha = 0.2) + 
  geom_point(size = manually_curated_data$middle_radius/500,
             color = manually_curated_data$color_code,
             alpha = 0.7) + 
  geom_point(size = manually_curated_data$inner_radius/500,
             color = "white",
             alpha = 1) 



mutate(color_code = case_when((rowSums(.[2:15], na.rm = TRUE) > 1) | 
                                (ATC_G == TRUE | ATC_H == TRUE | ATC_P == TRUE | ATC_V == TRUE | ATC_S == TRUE) ~ "various", #groups with <10 or combos
                              (ATC_A == TRUE) ~ "metabolism",
                              (ATC_B == TRUE) ~ "blood",
                              (ATC_C == TRUE) ~ "cardiovascular", 
                              (ATC_D == TRUE) ~ "dermatologicals",
                              (ATC_J == TRUE) ~ "antiinfectives",
                              (ATC_L == TRUE) ~ "antineoplastic_immunomodulating",
                              (ATC_M == TRUE) ~ "musculoskeletal",
                              (ATC_N == TRUE) ~ "nervous",
                              (ATC_R == TRUE) ~ "respiratory",
                              TRUE ~ "missing"))

p3 <- ggplot(mydata, 
             aes(x = predictor, # we need an aesthetic with _x_
                 y = 1)) # & _y_

p3 + geom_beeswarm(color = "red", 
                   groupOnX = FALSE) +
  labs(title ="Beeswarm Plot of predictor") + # Add title
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank()) # tweak y axis

mutate(color_code = case_when((rowSums(.[2:15], na.rm = TRUE) > 1) | 
                                (ATC_G == TRUE | ATC_H == TRUE | ATC_P == TRUE | ATC_V == TRUE | ATC_S == TRUE) ~ "grey", #groups with <10 or combos
                              (ATC_A == TRUE) ~ "blue",
                              (ATC_B == TRUE) ~ "red",
                              (ATC_C == TRUE) ~ "pink", 
                              (ATC_D == TRUE) ~ "black",
                              (ATC_J == TRUE) ~ "orange",
                              (ATC_L == TRUE) ~ "green",
                              (ATC_M == TRUE) ~ "purple",
                              (ATC_N == TRUE) ~ "yellow",
                              (ATC_R == TRUE) ~ "black",
                              TRUE ~ "white"))

