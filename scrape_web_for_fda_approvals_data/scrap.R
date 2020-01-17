
GetDrugApprovals <- function(year) {
  
  approvals_html <- read_html(paste0("https://www.centerwatch.com/drug-information/fda-approved-drugs/year/", year))
  
  # parse html into text format that R can read
  parsed_html <- approvals_html %>% 
    html_nodes('.col-lg-8.col-sm-9 p') %>%
    html_text() %>%
    tolower()
  
  # now extract specific information we want
  names_step1 <- str_extract(string = parsed_html, pattern = (".*(?=;)"))
  drug_name <- str_extract(string = names_step1, pattern = (".*(?=; )"))
  brand_name <- str_extract(string = drug_name, pattern = (".*(?= \\()"))
  generic_name <- str_extract(string = drug_name, pattern = ("(?<= \\().*(?=\\))"))
  company <- str_extract(string = names_step1, pattern = ("(?<=; ).*"))
  approval_date <- str_extract(string = parsed_html, pattern = ("(?<=approved ).*"))
  indication <- str_extract(string = parsed_html, pattern = ("(?<= for ).*(?=, approved)"))
  
  # combine all data elements we want into a data frame
  approvals <- cbind(brand_name, generic_name, company, approval_date, indication) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    filter(!is.na(drug_name))
  
  return(approvals)
}

# collect info from 2000-2019 
all_approvals <- map(c(2000:2019), GetDrugApprovals) %>%
  do.call(rbind, .) %>%
  unique() %>%
  filter(brand_name != "diovan")

# clean dates
clean_dates <- all_approvals %>%
  mutate(clean_date1 = gsub(", ", "", approval_date),
         clean_date2 = gsub(" of", "", clean_date1),
         clean_date3 = gsub("in ", "", clean_date2),
         clean_date4 = gsub("on ", "", clean_date3),
         clean_date = as.yearmon(clean_date4, "%b %Y"))

# and a bunch of manual ones:
clean_dates$clean_date[clean_dates$brand_name == "tygacil"] <- as.yearmon("June 2005", "%b %Y")

write.csv(clean_dates, file = "~/Desktop/drug_approvals.csv")
# drop incorrect:


clean_dates$clean_date[clean_dates$company == "miacalcin"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "famvir"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "epivir"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "prinivil or zestril"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "leukine"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "androderm"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "fosamax"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "caverject"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "precose"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "invirase"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "ethyol"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "avonex"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "zerit"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "valtrex"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "infanrix"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "miacalcin"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "miacalcin"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "miacalcin"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "miacalcin"] <- as.yearmon("June 2003", "%b %Y")
clean_dates$clean_date[clean_dates$company == "miacalcin"] <- as.yearmon("June 2003", "%b %Y")





clean_dates$brand_name[clean_dates$company == "corixa"] <- "bexxar"
clean_dates$generic_name[clean_dates$company == "corixa"] <- "tositumomab and iodine"

# clean up to do: 
# remove "the treatment of, the management of, the prevention of" text
# to lowercase, then clean up company name; likely group all small comapanies to "other" (<10 drugs?)
# dates --> fix "august of 2010" etc...

## SCRATCH
temp <- clean_dates %>%
  group_by(company) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  mutate(company_cat = ifelse(total<5, "other", company)) %>%
  arrange(desc(total)) %>%
  mutate(ordered_company = factor(company_cat, levels = company_cat[order(total)]))


plot <- ggplot(temp, aes(clean_date, ordered_company)) + geom_point()

# API for collecting drug therapeutic class:

GetDrugClass <- function(drugname){
  path <- "https://rxnav.nlm.nih.gov/REST/rxclass/class/byDrugName.json?"
  request <- GET(url = path, 
                 query = list(
                   drugName = drugname))
  
  response <- content(request, as = "text", encoding = "UTF-8") 
  
  df <- fromJSON(response, flatten = TRUE) %>% 
    data.frame() %>%
    select(drug_name = userInput.drugName, 
           class = rxclassDrugInfoList.rxclassDrugInfo.rxclassMinConceptItem.className) #%>%
  #head(1)
  
  return(df)
  
}

druglist <- clean_dates %>%
  select(generic_name) %>%
  unique() 

all_classes <- map(druglist, GetDrugClass)






