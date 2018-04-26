library(tidyverse)
library(magrittr)
library(ggrepel)
library(knitr)
library(kableExtra)
library(lubridate)


##########################################################################
### Read in raw data from csv/txt

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

#read in claim line level ACO dataset pull from Curry
if(exists("claim_lines") == FALSE){
  claim_lines <- read_tsv("aco_final.txt") %>% 
    left_join(DD_rollup) %>% 
    filter(Paid_Amt != 0) %>% 
    select(-CPT_Desc) %>% 
    left_join(codes, by = c("CPT_Code" = "Code")) %>% 
    rename(CPT_Desc = Description) %>% 
    mutate(Claim_Type = factor(Claim_Type, levels = c("IP", "OP", "PHYSICIAN", "SNF", "HHA", "HOSPICE", "DME")))
}

#determine top spend by specialty
top_spending_specialties <- claim_lines %>% 
  group_by(HICN, specialty) %>% 
  summarize(claim_lines = n(), 
            Paid_Amt = sum(Paid_Amt, na.rm = TRUE),
            HCC = mean(HCC, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(specialty) %>% 
  summarise(paid_sum = sum(Paid_Amt),
            num_patients = n()) %>% 
  arrange(desc(paid_sum)) %>% 
  mutate(avg_spend_per_patient = paid_sum/num_patients) %>% 
  filter(!is.na(specialty)) %>% 
  filter(num_patients > 100) %>% 
  head(20) %>% 
  arrange(desc(avg_spend_per_patient)) %>% 
  print

#define specialty list
specialty_list <- c("Cardio", 
                    "Oncology", 
                    "Surgery", 
                    "Ortho", 
                    "Ophthal",
                    "PMR", 
                    "Pulmonary",
                    "Hospitalist",
                    "Neuro Surg", 
                    "Neurology")

specialty_list <- top_spending_specialties %$%
  as.character(specialty)

spec <- "Neuro Surgery"

#define function to calculate mode, and second most common value, etc
modelist <- function (x) with(rle(sort(x)), values[order(lengths, decreasing = TRUE)])

##########################################################################

#create table of specialties that each cpt is most commonly associated with
most_common_cpt_specialty <- claim_lines %>% 
  group_by(HICN, specialty, CPT_Code, CPT_Desc) %>% 
  summarize(claim_lines = n()) %>% 
  ungroup() %>% 
  group_by(CPT_Code, CPT_Desc) %>% 
  summarize(num_patients = n(),
            most_common_specialty = modelist(specialty)[1]) %>% 
  select(CPT_Code, most_common_specialty)

#count number of cpt codes most commonly associated with each specialty
most_common_cpt_specialty %>% 
  group_by(most_common_specialty) %>% 
  count() %>% 
  arrange(desc(n))

##########################################################################
### Specialist-driven patient spend accounting for pooled patient risk 

#need to remove hospital bills from this

#mean spend by risk score summarized by specialist category
specialist_total_spend <- claim_lines %>% 
  filter(specialty %in% specialty_list) %>% 
  group_by(HICN, specialty) %>% 
  summarize(claim_lines = n(), 
            sum_paid_amt = sum(Paid_Amt, na.rm = TRUE),
            HCC = mean(HCC, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(specialty) %>% 
  summarize(count = n(), 
            sum_spend = sum(sum_paid_amt, na.rm=TRUE),
            mean_spend = mean(sum_paid_amt, na.rm=TRUE),
            mean_hcc = mean(HCC, na.rm=TRUE))

specialist_total_spend %>% 
  ggplot(aes(x = mean_hcc, y = mean_spend, label = specialty)) + 
    geom_point(color = "#4B9CD3") +
    geom_text_repel() +
    #scale_x_continuous(limits = c(0.75, 2.25)) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
      #title="HCC Risk Score vs Spending per Claim by Specialty",
      x="Average HCC Risk Score",
      y="Average Spend Per Patient") +
    theme_bw(base_size = 20)
#ggsave("Specialist total spend quadrant.pdf", path = "plots", width = 11, height = 8.5)

#mean spend by HCC score at the NPI level (filter on specialty grouping)
for (spec in specialty_list){
  print(spec)
  
  specialist_spend <- claim_lines %>% 
    filter(specialty %in% specialty_list) %>% 
    group_by(HICN, Serv_NPI, specialty) %>% 
    summarize(claim_lines = n(), 
              sum_paid_amt = sum(Paid_Amt, na.rm = TRUE),
              HCC = mean(HCC, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(specialty == spec) %>% 
    group_by(Serv_NPI, specialty) %>% 
    summarize(num_patients = n(), 
              sum_spend = sum(sum_paid_amt, na.rm=TRUE),
              mean_spend = mean(sum_paid_amt, na.rm=TRUE),
              mean_hcc = mean(HCC, na.rm=TRUE)) %>% 
    arrange(desc(num_patients)) %>% 
    filter(num_patients >= 5) %>% 
    ungroup()
  
    quadrant_chart <- specialist_spend %>%  
      filter(mean_spend > 0) %>% 
      ggplot(aes(x = mean_hcc, y = mean_spend, size = num_patients)) + 
      geom_point(color = "#4B9CD3") +
      scale_y_continuous(labels = scales::dollar) +
      geom_hline(aes(yintercept=mean(mean_spend, na.rm = TRUE)), color="#990000", linetype="dashed") +
      geom_vline(aes(xintercept=mean(mean_hcc, na.rm = TRUE)), color="#990000", linetype="dashed") +
      labs(
        title=paste0("Specialty - ", spec),
        x="Average HCC Risk Score",
        y="Average Spend Per Patient") +
      guides(size = FALSE) +
      theme_bw(base_size = 20)
    
    print(quadrant_chart)
    #ggsave(paste0("Specialty - ", spec, ".pdf"), path = "plots", width = 11, height = 8.5)
   
#print top spending NPIs that are in upper left quadrant
   specialist_spend %>%  
     filter(mean_spend > mean(mean_spend, na.rm = TRUE)) %>% 
     filter(mean_hcc < mean(mean_hcc, na.rm = TRUE)) %>% 
     arrange(desc(mean_spend)) %>% 
     head(5) %>% 
     print
}  



##########################################################################
### Clinical variation within specialty

#within each specialty, find most expensive CPT codes, and find top 3 most common associated DX codes  
for (spec in specialty_list){
  #find top most expensive cpts
  expensive_cpts <- claim_lines %>% 
    filter(specialty == spec) %>% 
    group_by(CPT_Code, specialty) %>% 
    summarize(count = n(), 
              mean_spend = mean(Paid_Amt, na.rm=TRUE),
              #median_spend = median(Paid_Amt, na.rm=TRUE),
              most_common_DX = modelist(DX_Code1)[1],
              second_most_common_DX = modelist(DX_Code1)[2],
              third_most_common_DX = modelist(DX_Code1)[3]) %>% 
    arrange(desc(mean_spend)) %>% 
    filter(!is.na(CPT_Code)) %>% 
    filter(count >= 10) %>% 
    head(5) %>% 
    select(specialty, CPT_Code, everything()) %>% 
    print
#perhaps roll up (sum) by HICN first?
  
}

#within each specialty, find most expensive DX codes, and find top 3 most common associated CPT codes  
for (spec in specialty_list){
  #find top most expensive DX
  expensive_dx <- claim_lines %>% 
    filter(specialty == spec) %>% 
    group_by(DX_Code1, specialty) %>% 
    summarize(count = n(), 
              mean_spend = mean(Paid_Amt, na.rm=TRUE),
              most_common_CPT = modelist(CPT_Code)[1],
              second_most_common_CPT = modelist(CPT_Code)[2],
              third_most_common_CPT = modelist(CPT_Code)[3]) %>% 
    arrange(desc(mean_spend)) %>% 
    filter(!is.na(DX_Code1)) %>% 
    filter(count >= 10) %>% 
    head(5) %>% 
    select(specialty, DX_Code1, everything()) %>% 
    print
  
}


#find a list of most expensive dx codes, regardless of specialty
expensive_dx_nospecialty <- claim_lines %>% 
  group_by(HICN, DX_Code1, CPT_Code) %>% 
  summarize(count = n(),
            Paid_Amt = sum(Paid_Amt)) %>% 
  arrange(desc(Paid_Amt)) %>% 
  group_by(DX_Code1) %>% 
  summarize(count = n(), 
            mean_spend = mean(Paid_Amt, na.rm=TRUE),
            most_common_CPT = modelist(CPT_Code)[1],
            second_most_common_CPT = modelist(CPT_Code)[2],
            third_most_common_CPT = modelist(CPT_Code)[3]) %>% 
  arrange(desc(mean_spend)) %>% 
  filter(!is.na(DX_Code1)) %>% 
  filter(count >= 10)

#find all cpt codes that are associated with a given DX code
dx <- "D66"
DX_associated_cpts <- claim_lines %>% 
  filter(DX_Code1 == dx) %>% 
  group_by(CPT_Code) %>% 
  summarize(count = n(), 
            mean_spend = mean(Paid_Amt, na.rm=TRUE)) %>% 
  arrange(desc(mean_spend))

##########################################################################
### Site of service spend differentials (part A vs B) 

# #show number of missing values in Serv Loc Code
# site_of_service <- claim_lines %>% 
#   group_by(Claim_Num) %>% 
#   summarize(claim_lines = n(), 
#             location = modelist(Serv_Loc_Code)[1])
# table(is.na(site_of_service$location)) #70% have no location

#calculate part A vs part B spend within specialty
claim_lines %>% 
  group_by(HICN, Claim_Type, specialty) %>% 
  summarize(claim_lines = n(), 
            Paid_Amt = sum(Paid_Amt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(part = ifelse(Claim_Type %in% c("PHYSICIAN", "DME", "OP"), "B","A")) %>% 
  mutate(part = ifelse(is.na(Claim_Type), NA, part)) %>% 
  group_by(specialty, part) %>% 
  summarize(num_patients = n(), 
            sum_spend = sum(Paid_Amt, na.rm=TRUE),
            mean_spend = mean(Paid_Amt, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(specialty)) %>% 
  filter(specialty %in% specialty_list) %>% 
  group_by(specialty) %>% 
  mutate(sum_patients = sum(num_patients)) %>% 
  ungroup() %>% 
  mutate(pct_patients = num_patients/sum_patients) %>% 
  ggplot(aes(x = (reorder(specialty, mean_spend)), y = mean_spend, 
             color = part, size = pct_patients)) +
    geom_point() +
    scale_y_continuous(labels = scales::dollar) +
    scale_color_manual(name  ="Medicare \nPart",
                         values = c("#4B9CD3", "#707C7C")) +
    scale_size_continuous(name  ="Percent of \nPatient Claims",
                       breaks = c(0.25, 0.5, 0.75, 1),
                       labels = c("25%", "50%", "75%", "100%")) +
    coord_flip() +
    labs(
      title="Average Spend by Specialty and Medicare Part",
      x="Specialty",
      y="Average Spend Per Patient") +
    theme_bw()

#calculate claimtype spend within specialty
claim_lines %>% 
  group_by(HICN, Claim_Type, specialty) %>% 
  summarize(claim_lines = n(), 
            Paid_Amt = sum(Paid_Amt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(specialty, Claim_Type) %>% 
  summarize(num_patients = n(), 
            sum_spend = sum(Paid_Amt, na.rm=TRUE),
            mean_spend = mean(Paid_Amt, na.rm=TRUE)) %>% 
  filter(!is.na(specialty)) %>% 
  filter(specialty %in% specialty_list) %>% 
  group_by(specialty) %>% 
  mutate(sum_patients = sum(num_patients)) %>% 
  ungroup() %>% 
  mutate(pct_patients = num_patients/sum_patients) %>% 
  ggplot(aes(x = (reorder(specialty, mean_spend)), y = mean_spend, 
             color = Claim_Type, size = pct_patients)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(name = "Claim \nType", #https://research.unc.edu/about/branding/colors/
                     values = c("#006DB6", "#CFDE00", "#D87900", "#4B9CD3","#4E9D2D", "#707C7C", "#512C1D")) +
  scale_size_continuous(name  ="Percent of \nPatient Claims",
                        breaks = c(0.25, 0.5, 0.75, 1),
                        labels = c("25%", "50%", "75%", "100%")) +
  
  coord_flip() +
  labs(
    title="Average Spend by Claim Type and Specialty",
    x="Specialty",
    y="Average Spend Per Claim") +
  theme_bw()

#calculate claimtype spend within specialty stacked bar
claim_lines %>% 
  group_by(HICN, Claim_Type, specialty) %>% 
  summarize(claim_lines = n(), 
            Paid_Amt = sum(Paid_Amt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(specialty, Claim_Type) %>% 
  summarize(num_patients = n(), 
            sum_spend = sum(Paid_Amt, na.rm=TRUE),
            mean_spend = mean(Paid_Amt, na.rm=TRUE)) %>% 
  filter(!is.na(specialty)) %>% 
  filter(specialty %in% specialty_list) %>% 
  group_by(specialty) %>% 
  mutate(total_spend = sum(sum_spend)) %>% 
  ungroup() %>% 
  mutate(pct_spend = sum_spend/total_spend) %>% 
  mutate(Claim_Type = factor(Claim_Type, levels = c("IP", "OP", "PHYSICIAN", "SNF", "HHA", "HOSPICE", "DME"))) %>% 
  ggplot(aes(x = (reorder(specialty, total_spend)), fill = Claim_Type)) +
    geom_bar(aes(weight = pct_spend)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(name = "Claim \nType", #https://research.unc.edu/about/branding/colors/
                      values = c("#4B9CD3", "#707C7C", "#D87900", "#006DB6","#CFDE00", "#512C1D", "#4E9D2D")) +
    coord_flip() +
    labs(
      title="Percent of Total Spend by Specialty and Claim Type",
      x="Specialty",
      y="Percent of Total Spend") +
    theme_bw() 

#need to rearrange order of claim type within columns


##########################################################################
### Drug – site/service linkage

# drug_spend <- claim_lines %>% 
#   group_by(Claim_Num, Claim_Type, specialty) %>% 
#   summarize(claim_lines = n(), 
#             Paid_Amt = sum(Paid_Amt, na.rm = TRUE)) %>% 
#   filter(specialty == "Pharma") %>% 
#   arrange(desc(Paid_Amt))
# table(drug_spend$Claim_Type)
# #all drug spending has claim type of PHYSICIAN
# #Can't look at part A vs B, because everything is part B (physician claim type)


#drug spend by person
drug_spend_patient <- claim_lines %>% 
  filter(specialty == "Pharma") %>% 
  group_by(HICN, Serv_NPI) %>% 
  summarize(claim_lines = n(), 
            Paid_Amt = sum(Paid_Amt, na.rm = TRUE)) %>% 
  arrange(desc(Paid_Amt)) %>%
  select(everything(), Serv_NPI) %>% 
  head(10)
drug_spend_patient %>% head(3) %$% sum(Paid_Amt)
#3 people are responsible for over $1 million spending on drugs!!

hic <- "241379298A"
drug_spend_patient_detail <- claim_lines %>% 
  filter(HICN == hic) %>% 
  arrange(Beg_Serv_Dt) %>% 
  filter(specialty == "Pharma")


##########################################################################
### Highest spend in terms of claims and patients in 2017

highest_dollar_claims <- claim_lines %>% 
  arrange(desc(Paid_Amt))

highest_spending_patients <- claim_lines %>% 
  group_by(HICN) %>% 
  summarize(claim_lines = n(), 
            Paid_Amt = sum(Paid_Amt, na.rm = TRUE),
            HCC = mean(HCC, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(Paid_Amt))


##########################################################################
### Claim-level drivers of high spending year over year

#read in 2016 data
claim_lines_2016 <- read_tsv("aco_final_16.txt") %>% 
  left_join(DD_rollup) %>% 
  filter(Paid_Amt != 0) %>% 
  mutate(year = 2016)

#calculate yoy differential in terms of spend by cpts
yoy_cpt_spend_differential <- claim_lines %>%
  mutate(year = 2017) %>% 
  bind_rows(claim_lines_2016) %>% 
  group_by(CPT_Code, year) %>% 
  summarize(sum_spend = sum(Paid_Amt)) %>%
  ungroup() %>% 
  spread(key = year, value = sum_spend) %>% 
  rename(spend_2016 = `2016`,
         spend_2017 = `2017`) %>% 
  mutate(spend_2016 = ifelse(is.na(spend_2016), 0, spend_2016)) %>% 
  mutate(spend_2017 = ifelse(is.na(spend_2017), 0, spend_2017)) %>% 
  mutate(spend_diff = spend_2017 - spend_2016) %>% 
  arrange(desc(spend_diff)) %>% 
  left_join(codes, by = c("CPT_Code" = "Code"))

#calculate yoy differential in terms of spend and num_patients by cpts 
yoy_cpt_spend_patient_differential <- claim_lines %>%
  mutate(year = 2017) %>% 
  bind_rows(claim_lines_2016) %>% 
  group_by(CPT_Code, year, HICN) %>% 
    summarize(claimline_count = n(),
              sum_spend = sum(Paid_Amt)) %>%
  ungroup() %>% 
  group_by(CPT_Code, year) %>% 
  summarize(num_patients = n(),
            sum_spend = sum(sum_spend)) %>%
  ungroup() %>% 
  mutate(year2 = year) %>% 
  spread(key = year, value = sum_spend) %>% 
  rename(spend_2016 = `2016`,
         spend_2017 = `2017`) %>% 
  mutate(spend_2016 = ifelse(is.na(spend_2016), 0, spend_2016)) %>% 
  mutate(spend_2017 = ifelse(is.na(spend_2017), 0, spend_2017)) %>% 
  mutate(spend_diff = spend_2017 - spend_2016) %>% 
  spread(key = year2, value = num_patients) %>% 
  rename(num_patients_2016 = `2016`,
         num_patients_2017 = `2017`) %>% 
  mutate(num_patients_2016 = ifelse(is.na(num_patients_2016), 0, num_patients_2016)) %>% 
  mutate(num_patients_2017 = ifelse(is.na(num_patients_2017), 0, num_patients_2017)) %>% 
  mutate(num_patients_pct_diff = (num_patients_2017 - num_patients_2016)/num_patients_2016) %>% 
  group_by(CPT_Code) %>% 
  summarize(spend_2016 = sum(spend_2016),
            spend_2017 = sum(spend_2017),
            spend_diff = sum(spend_diff),
            num_patients_2016 = sum(num_patients_2016),
            num_patients_2017 = sum(num_patients_2017),
            num_patients_pct_diff = sum(num_patients_pct_diff)) %>% 
  ungroup() %>% 
  mutate(spend_diff_pct = (spend_2017-spend_2016)/spend_2016) %>% 
  arrange(desc(spend_diff)) %>% 
  left_join(codes, by = c("CPT_Code" = "Code")) %>% 
  left_join(most_common_cpt_specialty, by = "CPT_Code")
#there's probably an easier way to do this by spreading by more than one category

#find drgs associated with NA cpt codes;
drg_map <- read_csv("MSDRG Mapping.csv")

top_drgs <- claim_lines %>%
  mutate(year = 2017) %>% 
  bind_rows(claim_lines_2016) %>% 
  filter(is.na(CPT_Code)) %>% 
  group_by(DRG, year, HICN) %>% 
  summarize(claimline_count = n(),
            sum_spend = sum(Paid_Amt)) %>%
  ungroup() %>% 
  group_by(DRG, year) %>% 
  summarize(num_patients = n(),
            sum_spend = sum(sum_spend)) %>%
  ungroup() %>% 
  mutate(year2 = year) %>% 
  spread(key = year, value = sum_spend) %>% 
  rename(spend_2016 = `2016`,
         spend_2017 = `2017`) %>% 
  mutate(spend_2016 = ifelse(is.na(spend_2016), 0, spend_2016)) %>% 
  mutate(spend_2017 = ifelse(is.na(spend_2017), 0, spend_2017)) %>% 
  mutate(spend_diff = spend_2017 - spend_2016) %>% 
  spread(key = year2, value = num_patients) %>% 
  rename(num_patients_2016 = `2016`,
         num_patients_2017 = `2017`) %>% 
  mutate(num_patients_2016 = ifelse(is.na(num_patients_2016), 0, num_patients_2016)) %>% 
  mutate(num_patients_2017 = ifelse(is.na(num_patients_2017), 0, num_patients_2017)) %>% 
  group_by(DRG) %>% 
  summarize(spend_2016 = sum(spend_2016),
            spend_2017 = sum(spend_2017),
            spend_diff = sum(spend_diff),
            num_patients_2016 = sum(num_patients_2016),
            num_patients_2017 = sum(num_patients_2017)) %>% 
  ungroup() %>% 
  mutate(num_patients_diff = (num_patients_2017 - num_patients_2016)/num_patients_2016) %>% 
  mutate(spend_diff_pct = (spend_2017-spend_2016)/spend_2016) %>% 
  arrange(desc(spend_diff)) %>% 
  left_join(drg_map, by = "DRG")

#list top 20 CPTs with greater spend in 2017
yoy_cpt_spend_differential %>% 
  rename(`CPT Code` = CPT_Code,
         `CPT Description` = Description, 
         `2016 Total Spend` = spend_2016,
         `2017 Total Spend` = spend_2017,
         `YOY Differential` = spend_diff) %>% 
  head(15) %>% 
  kable("html") %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)

#calculate yoy differential in terms of cpts, by claim type
yoy_cpt_claimtype_spenddiff <- claim_lines %>%
  mutate(year = 2017) %>% 
  bind_rows(claim_lines_2016) %>% 
  group_by(CPT_Code, CPT_Desc, year, Claim_Type) %>% 
  summarize(sum_spend = sum(Paid_Amt)) %>% 
  spread(key = year, value = sum_spend) %>% 
  rename(spend_2016 = `2016`,
         spend_2017 = `2017`) %>% 
  mutate(spend_2016 = ifelse(is.na(spend_2016), 0, spend_2016)) %>% 
  mutate(spend_2017 = ifelse(is.na(spend_2017), 0, spend_2017)) %>% 
  mutate(spend_diff = spend_2017 - spend_2016) %>% 
  arrange(desc(spend_diff))

yoy_cpt_claimtype_spenddiff %>% 
  filter(Claim_Type == "HHA") %>% 
  head()

##########################################################################
### Injectables

#format injectables dataset
injectables <- claim_lines %>% 
  bind_rows(claim_lines_2016) %>% 
  filter(str_detect(CPT_Code, "^J")) %>% 
  arrange(Beg_Serv_Dt, HICN) %>% 
  group_by(Beg_Serv_Dt, End_Serv_Dt, HICN, CPT_Code) %>% 
  summarize(procedures = n(),
            Paid_Amt = sum(Paid_Amt)) %>% 
  ungroup() %>% 
  group_by(year = year(End_Serv_Dt), quarter = quarter(End_Serv_Dt), 
           month = month(End_Serv_Dt), CPT_Code) %>% 
  summarize(count = n(), 
            procedures = sum(procedures),
            sum_spend = sum(Paid_Amt),
            mean_spend = round(mean(Paid_Amt), 0)) %>% 
  ungroup() %>% 
  left_join(codes, by = c("CPT_Code" = "Code")) %>% 
  rename(CPT_Desc = Description) %>% 
  mutate(year = as.factor(year),
         quarter = as.factor(quarter),
         month = as.factor(month)) %>% 
  arrange(desc(sum_spend))

#plot month/year total spend for injectables
injectables %>% 
  group_by(year, month) %>% 
  summarize(count = n(),
            sum_spend = sum(sum_spend)) %>% 
  ungroup() %>% 
  ggplot(aes(x = month, y = sum_spend, color = year)) +
    geom_line(aes(group = year), size = 2) + 
    scale_y_continuous(name = "Total Spend",
                       labels = scales::dollar) + 
    scale_color_manual(name = "Year",
                       values = c("#707C7C", "#4B9CD3")) + 
    labs(
      title="Total Injectable Spend by Month and Year",
      x="Month",
      y="Total Spend") +
    theme_bw()

#plot specific injectables by quarter
inj_code <- "J9310"

injectables %>% 
  filter(CPT_Code == inj_code) %>% 
  group_by(year, quarter, CPT_Desc) %>% 
  summarize(procedures = sum(procedures),
            sum_spend = sum(sum_spend)) %>% 
  ungroup() %>% 
  ggplot(aes(x = quarter, y = sum_spend, color = year)) +
  geom_line(aes(group = year), size = 2) + 
  scale_x_discrete(name = "Quarter") + 
  scale_y_continuous(name = "Total Spend",
                     labels = scales::dollar) + 
  scale_color_manual(name = "Year",
                     values = c("#707C7C", "#4B9CD3")) +
  labs(
    title=paste0("Total Spend by Year and Quarter, CPT Code ", inj_code),
    x="Quarter",
    y="Total Spend") +
  theme_bw()


