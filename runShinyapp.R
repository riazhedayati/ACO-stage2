library(shiny)
library(tidyverse)
library(magrittr)
library(ggrepel)
library(lubridate)

#############################################################
##### load data

#read in CPT codes/descriptions
if(exists("codes") == FALSE){
  codes <- read_csv("procedure codes/CPT_cms.csv") %>% 
    bind_rows(read_tsv("procedure codes/CPT_aginity.txt")) %>% 
    bind_rows(read_csv("procedure codes/HCPC.csv")) %>% 
    bind_rows(read_csv("procedure codes/HIPPS.csv")) %>% 
    filter(!is.na(Description)) %>% 
    distinct(Code, .keep_all = T) #if duplicates, use CPT first, then HCPC, then HIPPS 
}

#read in Daina's summarization of specialties
if(exists("DD_rollup") == FALSE){
  DD_rollup <- read_csv("DD_specialty_rollup_031318.csv")
}

#read in NPI to name mapping
if(exists("npi_names") == FALSE){
  npi_names <- read_csv("npi_names.csv") %>% 
    filter(SVC_PROV_LAST_NAME != "N/A") %>% 
    filter(is.na(SVC_PROV_LAST_NAME) == FALSE) %>% 
    select(SERVICING_NPI, SVC_PROV_FULL_NAME) %>% 
    rename(Serv_NPI = SERVICING_NPI,
           `Provider Full Name` = SVC_PROV_FULL_NAME)
}
#read in 2017 claim line level ACO data
if(exists("claim_lines") == FALSE){
  claim_lines <- read_tsv("aco_final.txt") %>% 
    left_join(DD_rollup) %>% 
    filter(Paid_Amt != 0) %>% 
    select(-CPT_Desc) %>% 
    left_join(codes, by = c("CPT_Code" = "Code")) %>% 
    rename(CPT_Desc = Description) %>% 
    mutate(Claim_Type = factor(Claim_Type, levels = c("IP", "OP", "PHYSICIAN", "SNF", "HHA", "HOSPICE", "DME"))) %>% 
    left_join(npi_names, by = "Serv_NPI")
}

#read in 2016 claim line data
if(exists("claim_lines_2016") == FALSE){
  claim_lines_2016 <- read_tsv("aco_final_16.txt") %>% 
    left_join(DD_rollup) %>% 
    filter(Paid_Amt != 0)
}



#############################################################
##### run shiny app
runApp("app.R")
