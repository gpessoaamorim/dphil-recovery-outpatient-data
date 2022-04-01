# Baseline drugs in both datasets

# 1. Load libraries------------------

library(readr) # import text files (such as csv)
library(readxl) # import excel files
library(tidyverse) # tidyverse package ecosystem for data handling, namely dplyr package
library(VennDiagram) # generate Venn Diagrams
library(RColorBrewer) # generate color palettes
library(ggplot2) # data visualizations
library(patchwork) # combine plots
library(skimr) # general data inspection
library(stringr) # handling strings
library(ggrepel) # repel labels in ggplot
library(ggridges) # generate ridge plots with ggplot
library(broom) # convert shapefile files for use in ggplot
library(magrittr) # get %<>% operator to facilitate synthax
library(hexbin) # required for hexagonal bin ggplots
library(beepr) # get an alarm when code scripts finish
library(scales) # adjust color scales in graphs
library(dtplyr) # data.table backend for dplyr verbs for faster processing
library(tictoc) # for code benchmarking
library(forcats) # to reorder factors
library(tidytext) # reordering characters according to a grouping variable (for plots)
library(ggthemes) # themes for ggplot 
# library(plotly) # generate interactive plots (for inspection of individual points); commented as only needed ad hoc during development and conflicts with ggplot
library(ggh4x) # additional functions to manipulate facets in ggplot
library(ggalluvial) # river plots in ggplot
library(mmtable2) # to build tables; can be installed as remotes::install_github("ianmoran11/mmtable2")
library(gt) # to build tables
library(ggsankey) # river plots in ggplot
library(ggpubr) # adding stats to plot
library(ggdark) # themes for ggplot 
library(ggvenn) # venn diagrams in ggplot
library(fmsb) # calculations of K
library(tidyfst) # additional functions for tidyverse syntax
library(xfun)
library(formattable) # formatting html tables
library(tableHTML) # exporting html tables
library(htmltools) # exporting html tables
library(webshot)# exporting html tables
webshot::install_phantomjs() # further tools for html tables
library(gtsummary) # handling html tables; can be installed as remotes::install_github("ddsjoberg/gtsummary")
library(ggside) # for plotting parallel graphs
library(flextable) # for exporting tables to word
library(gtsummary) # alternative method to produce tables
library(officer) # for exporting flextable objects to word
library(lubridate) # dealing with dates
library(survival) # survival analyses
library(survminer) # survival analyses
library(ggfortify) # survival plots
library(grattantheme) # wrapping lables
library(irr) # for intraclass coefficient calculations
library(haven) # load stata/sas files
library(maps) # for geospatial visualization
library(viridis)

#  2. Apply system settings ------
# prevent scientific notation in plots
options(scipen=10000) 

# set working directory
setwd("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY")

# hardcode temporary directory within QNAP
tempdir <- function() { "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Temporary directory" }


# 3. Load data ----------------------------

# import list of withdrawn participants ------------------
withdrew<- read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/RECOVERY Consent Withdrawn 2021-10-08.xlsx", 
                      col_types = c("text", 
                                    "skip", 
                                    "skip", 
                                    "skip", 
                                    "skip", 
                                    "skip", 
                                    "skip", 
                                    "skip",
                                    "skip", 
                                    "skip", 
                                    "skip", 
                                    "skip"))%>%.[[1]]


# import randomisation dates ----------------

rand_dates <- read_excel("K:/QNAP/RECOVERY-deidentified/Datasets/Randomisation_dates/RECOVERY Participants 2022-01-12.xlsx")


# wrangle these data
rand_dates%<>%
  mutate(rand_date = str_sub(RandDateTime, 1, 10), # extract randomisation date 
         ParticipantID=as.character(ParticipantID))%>% # format study number
  rename(study_number= ParticipantID)


# Load GP data ----------
## note: extract 59

gp<-read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT08_GDPPR/0059/DP_INT08_GDPPR_Deidentified_2021-08-29_20-25-45.csv", 
             col_types = cols(study_number = col_character(),
                              sex = readr::col_factor(levels = c("0","1")),
                              date_of_death = col_date(format = "%Y-%m-%d"),
                              reporting_period_end_date = col_date(format = "%Y-%m-%d"), 
                              date = col_date(format = "%Y-%m-%d"),
                              record_date = col_date(format = "%Y-%m-%d"),
                              code = col_character(), 
                              value1_condition = col_number(), 
                              value2_condition = col_number(),
                              value1_prescription = col_number(),
                              value2_prescription = col_number()))




# select needed rows

gp%<>%
  select(study_number,
         date,
         record_date,
         lsoa,
         code, 
         value1_prescription,
         value2_prescription,
         gp_system_supplier,
         reporting_period_end_date,
         processed_timestamp)



## convert to lazy datatable for dtplyr operations
gp_dt<-lazy_dt(gp)
rm(gp)

# apply original row number
gp_dt%<>%
  mutate(row_number = row_number())


## check for the presence of excluded participants (duplicates, died before rand) in our data

excluded<-rand_dates%>%
  filter(Excluded=="Y")%>%
  select(study_number)%>%.[[1]]

gp_dt%>%filter(study_number %in% excluded)%>%as_tibble()%>%nrow() # none 




# remove participants who withdrew consent

gp_dt%<>%
  filter(!study_number %in% withdrew) # 3 participants


## calculate total participants  


gp_dt%>%distinct(study_number)%>%as_tibble()%>%nrow() -> participants_total  # 34175
















# Load Dispensing dataset ----------
## note: extract 59
meds <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT07_PCAREMEDS/0059/DP_INT07_PCAREMEDS_DEIDENTIFIE_2021-08-29_23-34-01.csv", 
                 col_types = cols(study_number = col_character(),
                                  bsa_prescription_id = col_character(), 
                                  paiddmd_code = col_character(),
                                  prescribeddmd_code = col_character(),
                                  processing_period_date = col_date(format = "%Y-%m-%d")))


# remove participants who withdrew consent

meds%<>%
  filter(!study_number %in% withdrew)






# apply original row number
meds%<>%
  mutate(row_number = row_number())

## convert to lazy datatable for dtplyr operations
meds_dt<-lazy_dt(meds)

rm(meds)

## trim unnecessary rows

meds_dt%<>%
  select(study_number, bsa_prescription_id, item_id, prescribed_bnf_code, prescribed_bnf_name, prescribeddmd_code, processing_period_date, prescribed_quantity, row_number)


meds_dt%>%distinct(study_number)%>%as_tibble()%>%nrow() -> participants_total_meds  # 34073



## check for the presence of excluded participants (duplicates, died before rand) in our data

excluded<-rand_dates%>%
  filter(Excluded=="Y")%>%
  select(study_number)%>%.[[1]]

meds_dt%>%filter(study_number %in% excluded)%>%as_tibble()%>%nrow() # none



# Load CRF data ---------------

# version: recovery_2203_1b\version_2022_03_08

baseline_crf <- read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2203_1b/version_2022_03_08/adsl.dta")


# select variables of interest
baseline_crf%<>%
  select(study_number=usubjid,
         siteid,
         withdrew=withdrfl,
         crf_rand_ver=crfrver,
         crf_fu_ver = crffver,
         age,
         agegr2,
         agegr2n,
         sex,
         race,
         rand_date = randdt,
         admission_day = hospdy,
         diabetes_crf = diabfl,
         heart_crf = hdfl,
         liver_crf = liverfl,
         renal_crf = kidneyfl,
         lung_crf = lungfl,
         renal_replacement_crf = rrtfl
  )














# Load site geographical data ------

sites <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/RECOVERY sites/RECOVERY Sites 2022-03-29.csv")





# Load RECOVERY drug codelists ------------------
## SNOMED codelists ------------------
file.list <- list.files(path="K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Tools/Codelists/Medications_SNOMED/", full.names=T, pattern = "*.xlsx")

df = lapply(file.list, function(i){
  x=read_excel(i, sheet="Codelist")
  x$codelist=i
  x
})

codelists_snomed<-bind_rows(df)

codelists_snomed$codelist<-str_sub(codelists_snomed$codelist,104, -10)

rm(file.list,df)

### trim out those not relevant

codelists_snomed %<>%
  filter(!codelist %in% c("factorXa_inhibitors",
                          "thrombin_inhibitors",
                          "other_antiplatelets",
                          "amiodarone",
                          "flecainide",
                          "verapamil",
                          "digoxin",
                          "acei_or_arb",
                          "central_antiHT",
                          "vasodilator_antiHT",
                          "nitrates",
                          "atorvastatin",
                          "cerivastatin",
                          "fluvastatin",
                          "lovastatin",
                          "simvastatin",
                          "pitavastatin",
                          "pravastatin",
                          "rosuvastatin",
                          "resins",
                          "oral_antidiabetics",
                          "acarbose_or_similar",
                          "biguanides",
                          "dpp4i",
                          "glinides",
                          "glp1_agonists",
                          "sulphonylureas",
                          "thiazolideniones",
                          "other_antianginals",
                          "any_diuretic_excluding_osmotic",
                          "injectable_glp1_agonists")
  )

### ensure no duplicate codes in each list to avoid overcounting

codelists_snomed%<>%
  group_by(codelist)%>%
  distinct(ConceptId)






## BNF codelists------------------

file.list <- list.files(path="K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Tools/Codelists/Medications_BNF/", full.names=T, pattern = "*.xlsx")

df = lapply(file.list, function(i){
  x=read_excel(i, sheet="Codelist")
  x$codelist=i
  x
})

codelists_bnf<-bind_rows(df)

codelists_bnf$codelist<-str_sub(codelists_bnf$codelist,101, -10)

rm(df, file.list)

# trim to codelists of interest
codelists_bnf %<>%
  filter(!codelist %in% c("factorXa_inhibitors",
                          "thrombin_inhibitors",
                          "other_antiplatelets",
                          "amiodarone",
                          "flecainide",
                          "verapamil",
                          "digoxin",
                          "acei_or_arb",
                          "central_antiHT",
                          "vasodilator_antiHT",
                          "nitrates",
                          "atorvastatin",
                          "fluvastatin",
                          "lovastatin",
                          "cerivastatin",
                          "simvastatin",
                          "pitavastatin",
                          "pravastatin",
                          "rosuvastatin",
                          "resins",
                          "oral_antidiabetics",
                          "acarbose_or_similar",
                          "biguanides",
                          "dpp4i",
                          "glinides",
                          "glp1_agonists",
                          "sulphonylureas",
                          "thiazolideniones",
                          "other_antianginals",
                          "any_diuretic_excluding_osmotic",
                          "injectable_glp1_agonists")
  )

# remove unnecessary fields

codelists_bnf%<>%
  select(-dmd_id, -dmd_name)



### ensure no duplicate codes in each list to avoid overcounting

codelists_bnf%<>%
  group_by(codelist)%>%
  distinct(code)


## BNF chapter codelists ----------------


file.list <- list.files(path="K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Tools/Codelists/BNF chapters/", full.names=T, pattern = "dmd.csv")

df = lapply(file.list, function(i){
  x=read_csv(i, col_types = "cccc")
  x$codelist=i
  x
})

bnf_chapters<-bind_rows(df)

bnf_chapters$codelist<-str_sub(bnf_chapters$codelist,116, -18)

rm(df, file.list)

bnf_chapters%<>%
  mutate(chapter=as.numeric(codelist))

bnf_chapters%<>%
  select(-dmd_type)

## create BNF chapter labels

labels <- c("Gastro-Intestinal System",
            "Cardiovascular System",
            "Respiratory System",
            "Central Nervous System",
            "Infections",
            "Endocrine System",
            "Obstetrics, Gynaecology and Urinary-Tract Disorders",
            "Malignant Disease and Immunosuppression",
            "Nutrition and Blood",
            "Musculoskeletal and Joint Diseases",
            "Eye",
            "Ear, Nose and Oropharynx",
            "Skin",
            "Immunological Products and Vaccines",
            "Anaesthesia",
            "Preparations used in Diagnosis",
            "Other Drugs and Preparations",
            "Dressings",
            "Appliances",
            "Incontinence Appliances",
            "Stoma Appliances")

bnf_chapter_labels<-data.frame(distinct(bnf_chapters,chapter), labels)


rm(labels)




## Codelist labels  -------

codelists_snomed%>%distinct(codelist)->codelist
labels<-c("ACE inhibitors",
          "Alpha adrenergic blockers",
          "Anti-arrhythmics",
          "Anti-IgE antibodies",
          "Antibacterials",
          "Anticoagulants",
          "Antidepressants",
          "Antidyslipidemics",
          "Antifungals",
          "Antihelminthics",
          "Antihypertensives",
          "Antiplatelets",
          "Antipsychotics",
          "Antivirals",
          "Any cardiovascular drug",
          "Diuretics",
          "RAAS inhibitors",
          "Any respiratory drug",
          "Angiotensin-receptor blockers",
          "Angiotensin-receptor/neprylisin inhibitors",
          "Aspirin",
          "Beta-adrenergic blockers",
          "Calcium-channel blockers",
          "Clopidogrel",
          "COVID-19 vaccine",
          "Cromoglycates",
          "Diabetes drugs (non-insulin)",
          "Dihydropyridines",
          "Dipyridamole",
          "Ezetimibe",
          "Fibrates",
          "Histamine antagonists",
          "Inhaled corticosteroids",
          "Immunosuppressive drugs (non-steroidal)",
          "Inhaled bronchodilators",
          "Insulin",
          "Bronchodilators (intramuscular/intravenous)",
          "Leukotriene antagonists",
          "Low-molecular-weight heparins",
          "Loop diuretics",
          "Metformin",
          "Mineralocorticoid receptor antagonists",
          "Nebulised drugs (for asthma/COPD)",
          "Novel oral anticoagulants",
          "Non-dihydropyridines",
          "Adrenoceptor agonists (non-selective)",
          "Beta-2 adrenoceptor oral agonists",
          "Other anti-hypertensives",
          "Other diuretics",
          "P2Y12 inhibitors",
          "PCSK9 inhibitors",
          "Renin antagonists",
          "SGLT2 inhibitors",
          "Statins",
          "Systemic steroids",
          "Thiazide diuretics",
          "Vitamin-K antagonists",
          "Xanthines")

codelist_labels<-tibble(codelist, labels)

rm(codelist, labels)


## Codelist groups --------
cv<-c("acei",
      "alpha_blockers",
      "anti_arrhythmics",
      "anticoagulants",
      "antidyslipidemics",
      "antihypertensives",
      "antiplatelets",
      "any_cv_drug",
      "any_diuretic",
      "any_raasi",
      "arb",
      "arni",
      "aspirin",
      "beta_blockers",
      "ccb",
      "clopidogrel",
      "diabetes_drugs_not_insulin",
      "dihydropyridines",
      "dipyridamole",
      "ezetimibe",
      "fibrates",
      "insulin",
      "LMWH",
      "loop_diuretics",
      "metformin",
      "mra",
      "noacs",
      "non_dihydropyridines",
      "oth_anti_hypertensives",
      "other_diuretics",
      "p2y12",
      "pcsk9",
      "renin_antagonists",
      "sglt2",
      "statins",
      "thiazide_diuretics",
      "vitk_ant")

resp<-c("any_resp_drug",
        "anti_igE",
        "cromoglycates",
        "histamine_antagonists",
        "ics",
        "inhaled_bronchodilators",
        "iv_im_bronchodilators",
        "leukotriene_antagonists",
        "nebulisers_asthma_copd",
        "oral_beta2_agonists",
        "non_selective_adrenoceptor_agonists",
        "xanthines")

oth<-c("antibacterials",
       "antifungals",
       "antihelminthics",
       "antipsychotics",
       "antidepressants",
       "antivirals",
       "covid_vaccine",
       "immunosuppressive_drugs",
       "systemic_steroids")

codelist_labels%<>%
  mutate(group = case_when(codelist %in% cv ~"cv",
                           codelist %in% resp ~"resp",
                           codelist %in% oth ~"oth"))



drugs_paper <- c("any_cv_drug",
                 "any_raasi",
                 "antiplatelets",
                 "anticoagulants",
                 "anti_arrhythmics",
                 "diabetes_drugs_not_insulin",
                 "insulin",
                 "antidyslipidemics",
                 "sglt2",
                 "any_resp_drug",
                 "ics",
                 "inhaled_bronchodilators",
                 "antibacterials",
                 "antifungals",
                 "antipsychotics",
                 "antidepressants",
                 "antivirals",
                 "immunosuppressive_drugs",
                 "systemic_steroids")













rm(cv, resp, oth)

rm(excluded)


# Load SNOMED FSN (fully-specified names) - for code inspection if necessary-------
# FSN <- read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/Lookup_tables/FSN.csv",
#                 col_types = cols(id = col_character(), moduleId = col_character(), conceptId = col_character(), typeId = col_character(), caseSignificanceId = col_character()))


# load GP cluster list to retrieve clusters and code descriptions (for code inspection if necessary) ---------------
# gp_cluster_lookup <-read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/GDPPR_Cluster_refset_1000230_20210914.xlsx")









# 4. General features-----------


## participants --------------

participants_total  # 34175

participants_total_meds  # 34073


## rows -------------

gp_dt%>%as_tibble()%>%nrow()
meds_dt%>%as_tibble()%>%nrow()

## dates ---------------

gp_dt%>%select(date)%>%arrange(desc(date))%>%slice_head(n=1)
gp_dt%>%select(record_date)%>%arrange(desc(record_date))%>%slice_head(n=1)
gp_dt%>%select(reporting_period_end_date)%>%arrange(desc(reporting_period_end_date))%>%slice_head(n=1)
gp_dt%>%select(processed_timestamp)%>%arrange(desc(processed_timestamp))%>%slice_head(n=1)

meds_dt%>%select(processing_period_date)%>%arrange(desc(processing_period_date))%>%slice_head(n=1)


## randomisation dates ----------

# gpes
gp_dt%>%
  left_join(rand_dates, by=c("study_number"))%>%
  select(rand_date)%>%
  arrange(desc(rand_date))%>%
  slice_head(n=1) # 26/07/21

# plot randomisation dates in GPES
gp_dt%>%
  left_join(rand_dates, by=c("study_number"))%>%
  select(rand_date)%>%
  count(rand_date)%>%
  as_tibble()%>%
  ggplot(aes(rand_date, n))+
  geom_point()+
  geom_line(aes(group=1))+
  theme(axis.text.x = element_text(angle=90))

# dispensing
meds_dt%>%
  left_join(rand_dates, by=c("study_number"))%>%
  select(rand_date)%>%
  arrange(desc(rand_date))%>%
  slice_head(n=1) # 26/07/21



# plot randomisation dates in dispensing
meds_dt%>%
  left_join(rand_dates, by=c("study_number"))%>%
  select(rand_date)%>%
  count(rand_date)%>%
  as_tibble()%>%
  ggplot(aes(rand_date, n))+
  geom_point()+
  geom_line(aes(group=1))+
  theme(axis.text.x = element_text(angle=90))





# store last date of randomisation for people recorded in both datasets
# (which is the same: 26/07/21)
last_date<-gp_dt%>%
  left_join(rand_dates, by=c("study_number"))%>%
  select(rand_date)%>%
  arrange(desc(rand_date))%>%
  slice_head(n=1)%>%
  as_tibble()%>%
  .[[1]]


## comparison between participants in each dataset ------------



# total RECOVERY participants randomised until last date
rand_dates%>%
  filter(rand_date<=last_date)%>%
  distinct(study_number)%>%
  nrow() # 41217 participants


RECOVERY<-rand_dates%>%
  filter(rand_date<=last_date,
         NationID=="EN",
         is.na(Excluded))%>%
  select(study_number)%>%
  .[[1]]

RECOVERY%>%length() # 36454 EN RECOVERY participants


#5. Overlap in participants between datasets ------------

# overall numbers
GP<-gp_dt%>%
  distinct(study_number)%>%
  as_tibble()%>%
  .[[1]] 

length(GP)# 34175 participants in GPES

Dispensing<-meds_dt%>%
  distinct(study_number)%>%
  as_tibble()%>%
  .[[1]] 

length(Dispensing) # 34073 participants in Dispensing

# setdiff between GP and RECOVERY
y<-setdiff(GP,RECOVERY)
length(y) # 35


rand_dates%>%
  filter(study_number%in%y)%>%
  mutate(NationID=as.factor(NationID))%>%
  group_by(NationID)%>%
  summarize(n=n_distinct(study_number))%>%
  View() # 14 NI, 1 SC, and 20 WL participants; 35 in total



# setdiff between Dispensing and RECOVERY
z<-setdiff(Dispensing,RECOVERY)
length(z) # 101

rand_dates%>%
  filter(study_number%in%z)%>%
  View()

rand_dates%>%
  filter(study_number%in%z)%>%
  mutate(NationID=as.factor(NationID))%>%
  group_by(NationID)%>%
  summarize(n=n_distinct(study_number))%>%
  View() # 17 NI, 1 SC, 83 WL participants (101 total, all accounted for)


## venn diagram --------------

list_venn <-list("RECOVERY cohort\n(recruited in\nEngland)" = RECOVERY,
                 "RECOVERY cohort\n(included in the\nGP dataset)" = GP,
                 "RECOVERY cohort\n(included in the\nDispensing dataset)"=Dispensing)

ggvenn(list_venn, 
       fill_alpha= 0.2,
       set_name_size = 5)+
  labs(title="Overlap between the overall RECOVERY population recruited in England\nand those represented in the GP and Dispensing datasets")

ggsave("Outputs/Figures/baseline_drugs_recovery/participant_venn_diagram.png",
       dpi="retina",
       width=20,
       height=10)

rm(list_venn)

## explore differences between the sets ---------

# in recovery but not linkage
rand_dates%>%
  filter(study_number %in%
           setdiff(RECOVERY, append(RECOVERY, Dispensing)))%>%
  View()

rand_dates%>%
  filter(study_number %in%
           setdiff(RECOVERY, append(GP, Dispensing)))%>%
  select(study_number)%>%
  .[[1]]->missing_participants

# in linkage but not recovery


rand_dates%>%
  filter(study_number %in%
           setdiff(GP, Dispensing))%>%
  View()

rand_dates%>%
  filter(study_number %in%
           setdiff(Dispensing, GP))%>%
  View()


### different baseline characteristics? -------


# trim to participants of interest

baseline_crf%<>%
  filter(study_number %in% RECOVERY)

# cohort flags
baseline_crf%<>%
  mutate(recovery_flag = as.factor("1"),
         gpes_flag = as.factor(if_else(study_number %in% GP, 1, 0)),
         dispensing_flag = as.factor(if_else(study_number %in% Dispensing, 1,0)),
         missing_flag = as.factor(if_else(study_number %in% missing_participants, 1, 0)))


# summary tables


# all england recovery
baseline_crf%>%
  select(age, 
         agegr2,
         sex,
         race,
         diabetes_crf,
         heart_crf,
         lung_crf,
         renal_crf)%>%
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         sex == "" ~ "Unknown"),
         agegr2=str_sub(agegr2, start=4),
         race = case_when(race == "ASIAN" ~ "Asian",
                          race == "BLACK" ~ "Black",
                          race == "MIXED" ~ "Mixed",
                          race == "OTHER" ~ "Other",
                          race == "UNKNOWN" | race =="" ~ "Unknown",
                          race == "WHITE" ~ "White"),
         diabetes_crf = case_when(diabetes_crf=="Y" ~ "Yes",
                                  diabetes_crf=="N" ~ "No",
                                  diabetes_crf=="U" ~ "Unknown"),
         heart_crf = case_when(heart_crf=="Y" ~ "Yes",
                               heart_crf=="N" ~ "No",
                               heart_crf=="U" ~ "Unknown"),
         renal_crf = case_when(renal_crf=="Y" ~ "Yes",
                               renal_crf=="N" ~ "No",
                               renal_crf=="U" ~ "Unknown"),
         lung_crf = case_when(lung_crf=="Y" ~ "Yes",
                              lung_crf=="N" ~ "No",
                              lung_crf=="U" ~ "Unknown")
         )%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,., length)))%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,desc(.))))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~as.factor(.)))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~fct_relevel(., "Yes", "No", "Unknown")))%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               agegr2 ~ "Age group",
               sex ~ "Sex",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease"))->recovery_table

# GPES cohort
baseline_crf%>%
  filter(gpes_flag=="1")%>%
  select(age, 
         agegr2,
         sex,
         race,
         diabetes_crf,
         heart_crf,
         lung_crf,
         renal_crf)%>%
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         sex == "" ~ "Unknown"),
         agegr2=str_sub(agegr2, start=4),
         race = case_when(race == "ASIAN" ~ "Asian",
                          race == "BLACK" ~ "Black",
                          race == "MIXED" ~ "Mixed",
                          race == "OTHER" ~ "Other",
                          race == "UNKNOWN" | race =="" ~ "Unknown",
                          race == "WHITE" ~ "White"),
         diabetes_crf = case_when(diabetes_crf=="Y" ~ "Yes",
                                  diabetes_crf=="N" ~ "No",
                                  diabetes_crf=="U" ~ "Unknown"),
         heart_crf = case_when(heart_crf=="Y" ~ "Yes",
                               heart_crf=="N" ~ "No",
                               heart_crf=="U" ~ "Unknown"),
         renal_crf = case_when(renal_crf=="Y" ~ "Yes",
                               renal_crf=="N" ~ "No",
                               renal_crf=="U" ~ "Unknown"),
         lung_crf = case_when(lung_crf=="Y" ~ "Yes",
                              lung_crf=="N" ~ "No",
                              lung_crf=="U" ~ "Unknown")
  )%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,., length)))%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,desc(.))))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~as.factor(.)))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~fct_relevel(., "Yes", "No", "Unknown")))%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               agegr2 ~ "Age group",
               sex ~ "Sex",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease"))->gpes_table



# Dispensing cohort
baseline_crf%>%
  filter(dispensing_flag=="1")%>%
  select(age, 
         agegr2,
         sex,
         race,
         diabetes_crf,
         heart_crf,
         lung_crf,
         renal_crf)%>%
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         sex == "" ~ "Unknown"),
         agegr2=str_sub(agegr2, start=4),
         race = case_when(race == "ASIAN" ~ "Asian",
                          race == "BLACK" ~ "Black",
                          race == "MIXED" ~ "Mixed",
                          race == "OTHER" ~ "Other",
                          race == "UNKNOWN" | race =="" ~ "Unknown",
                          race == "WHITE" ~ "White"),
         diabetes_crf = case_when(diabetes_crf=="Y" ~ "Yes",
                                  diabetes_crf=="N" ~ "No",
                                  diabetes_crf=="U" ~ "Unknown"),
         heart_crf = case_when(heart_crf=="Y" ~ "Yes",
                               heart_crf=="N" ~ "No",
                               heart_crf=="U" ~ "Unknown"),
         renal_crf = case_when(renal_crf=="Y" ~ "Yes",
                               renal_crf=="N" ~ "No",
                               renal_crf=="U" ~ "Unknown"),
         lung_crf = case_when(lung_crf=="Y" ~ "Yes",
                              lung_crf=="N" ~ "No",
                              lung_crf=="U" ~ "Unknown")
  )%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,., length)))%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,desc(.))))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~as.factor(.)))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~fct_relevel(., "Yes", "No", "Unknown")))%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               agegr2 ~ "Age group",
               sex ~ "Sex",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease"))->dispensing_table


# missing cohort
baseline_crf%>%
  filter(missing_flag=="1")%>%
  select(age, 
         agegr2,
         sex,
         race,
         diabetes_crf,
         heart_crf,
         lung_crf,
         renal_crf)%>%
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         sex == "" ~ "Unknown"),
         agegr2=str_sub(agegr2, start=4),
         race = case_when(race == "ASIAN" ~ "Asian",
                          race == "BLACK" ~ "Black",
                          race == "MIXED" ~ "Mixed",
                          race == "OTHER" ~ "Other",
                          race == "UNKNOWN" | race =="" ~ "Unknown",
                          race == "WHITE" ~ "White"),
         diabetes_crf = case_when(diabetes_crf=="Y" ~ "Yes",
                                  diabetes_crf=="N" ~ "No",
                                  diabetes_crf=="U" ~ "Unknown"),
         heart_crf = case_when(heart_crf=="Y" ~ "Yes",
                               heart_crf=="N" ~ "No",
                               heart_crf=="U" ~ "Unknown"),
         renal_crf = case_when(renal_crf=="Y" ~ "Yes",
                               renal_crf=="N" ~ "No",
                               renal_crf=="U" ~ "Unknown"),
         lung_crf = case_when(lung_crf=="Y" ~ "Yes",
                              lung_crf=="N" ~ "No",
                              lung_crf=="U" ~ "Unknown")
  )%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,., length)))%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,desc(.))))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~as.factor(.)))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~fct_relevel(., "Yes", "No", "Unknown")))%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               agegr2 ~ "Age group",
               sex ~ "Sex",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease"))->missing_table



#### merged baseline tables ----
baseline_table_cohorts <- tbl_merge(list(recovery_table, gpes_table, dispensing_table, missing_table),
                                    tab_spanner = c("English cohort", "GP cohort", "Dispensing cohort", "Non-linked cohort"))

baseline_table_cohorts%>%
  # gt::tab_source_note(gt::md(""))
  as_flex_table()%>%
  set_caption("Baseline characteristics of the RECOVERY population recruited in England (split by linkage cohort)")->baseline_table_flex

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(baseline_table_flex, path = "Outputs/Tables/baseline_table.docx", pr_section = sect_properties)


#### explore age ----
  
baseline_crf%>%
  select(study_number, 
         Age=age, 
         `English cohort`=recovery_flag, 
         `GP cohort` = gpes_flag, 
         `Dispensing cohort`=dispensing_flag, 
         `Non-linked cohort`=missing_flag)%>%
  pivot_longer(-c(study_number, Age), names_to="Cohort", values_to = "Flag")%>%
  filter(Flag==1)%>%
  mutate(Cohort=fct_relevel(Cohort, "English cohort", "GP cohort", "Dispensing cohort", "Non-linked cohort"))%>%
  
  ggplot(aes(Age, Cohort, fill=Cohort))+
  geom_density_ridges(alpha=0.5, quantile_lines = TRUE, quantiles = 4)+
  # facet_wrap(~Cohort)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        text = element_text(family="Mulish"))+
  labs(title="Age density distributions, per linkage cohort",
       caption="Vertical lines in each density graph show quartiles")
  
ggsave("Outputs/Figures/baseline_drugs_recovery/age_distributions_linkage_cohorts.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")














### different codes? --------------


# gp but not in meds 
gp_not_meds <- setdiff(GP, Dispensing)

gp_dt%>%
  filter(study_number %in% gp_not_meds)%>%
  count(code)%>%
  arrange(desc(n))%>%
  left_join(FSN%>%select(conceptId, term), by=c("code"="conceptId"))%>%
  as_tibble()->gp_not_meds_codes

View(gp_not_meds_codes) # codes for BMI, blood tests, BP measurements, smoking and drinking, covid vaccines.. could be healthier people who don't have regular prescriptions

write_csv(gp_not_meds_codes, "Outputs/Tables/codes_gp_not_dispensing.csv")

# meds but not in gp

meds_not_gp <- setdiff(Dispensing, GP)

meds_dt%>%
  filter(study_number %in% meds_not_gp)%>%
  count(prescribed_bnf_code, prescribed_bnf_name)%>%
  arrange(desc(n))%>%
  as_tibble()->meds_not_gp_codes

View(meds_not_gp_codes) # many different things, but most should be captured by GP extraction (aspirin, statins, amlodipine,clopidogrel, diuretics); also some for PPIs that are not captured

write_csv(meds_not_gp_codes, "Outputs/Tables/codes_dispensing_not_meds.csv")

rm(FSN)

# baseline characteristics

baseline_crf%>%
  filter(study_number %in% gp_not_meds)%>%
  select(age, 
         agegr2,
         sex,
         race,
         diabetes_crf,
         heart_crf,
         lung_crf,
         renal_crf)%>%
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         sex == "" ~ "Unknown"),
         agegr2=str_sub(agegr2, start=4),
         race = case_when(race == "ASIAN" ~ "Asian",
                          race == "BLACK" ~ "Black",
                          race == "MIXED" ~ "Mixed",
                          race == "OTHER" ~ "Other",
                          race == "UNKNOWN" | race =="" ~ "Unknown",
                          race == "WHITE" ~ "White"),
         diabetes_crf = case_when(diabetes_crf=="Y" ~ "Yes",
                                  diabetes_crf=="N" ~ "No",
                                  diabetes_crf=="U" ~ "Unknown"),
         heart_crf = case_when(heart_crf=="Y" ~ "Yes",
                               heart_crf=="N" ~ "No",
                               heart_crf=="U" ~ "Unknown"),
         renal_crf = case_when(renal_crf=="Y" ~ "Yes",
                               renal_crf=="N" ~ "No",
                               renal_crf=="U" ~ "Unknown"),
         lung_crf = case_when(lung_crf=="Y" ~ "Yes",
                              lung_crf=="N" ~ "No",
                              lung_crf=="U" ~ "Unknown")
  )%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,., length)))%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,desc(.))))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~as.factor(.)))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~fct_relevel(., "Yes", "No", "Unknown")))%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               agegr2 ~ "Age group",
               sex ~ "Sex",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease"))->gp_not_meds_table

baseline_crf%>%
  filter(study_number %in% meds_not_gp)%>%
  select(age, 
         agegr2,
         sex,
         race,
         diabetes_crf,
         heart_crf,
         lung_crf,
         renal_crf)%>%
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         sex == "" ~ "Unknown"),
         agegr2=str_sub(agegr2, start=4),
         race = case_when(race == "ASIAN" ~ "Asian",
                          race == "BLACK" ~ "Black",
                          race == "MIXED" ~ "Mixed",
                          race == "OTHER" ~ "Other",
                          race == "UNKNOWN" | race =="" ~ "Unknown",
                          race == "WHITE" ~ "White"),
         diabetes_crf = case_when(diabetes_crf=="Y" ~ "Yes",
                                  diabetes_crf=="N" ~ "No",
                                  diabetes_crf=="U" ~ "Unknown"),
         heart_crf = case_when(heart_crf=="Y" ~ "Yes",
                               heart_crf=="N" ~ "No",
                               heart_crf=="U" ~ "Unknown"),
         renal_crf = case_when(renal_crf=="Y" ~ "Yes",
                               renal_crf=="N" ~ "No",
                               renal_crf=="U" ~ "Unknown"),
         lung_crf = case_when(lung_crf=="Y" ~ "Yes",
                              lung_crf=="N" ~ "No",
                              lung_crf=="U" ~ "Unknown")
  )%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,., length)))%>%
  mutate(across(c(sex,
                  race), 
                ~reorder(.x,desc(.))))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~as.factor(.)))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  renal_crf, 
                  lung_crf),
                ~fct_relevel(., "Yes", "No", "Unknown")))%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               agegr2 ~ "Age group",
               sex ~ "Sex",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease"))->meds_not_gp_table


gp_vs_meds_cohorts <- tbl_merge(list(recovery_table, gp_not_meds_table, meds_not_gp_table),
                                tab_spanner = c("English cohort", "Participants in the GP cohort but not the Dispensing cohort", "Participants in the Dispensing cohort but not the GP cohort"))

gp_vs_meds_cohorts%>%
  # gt::tab_source_note(gt::md(""))
  as_flex_table()%>%
  set_caption("Baseline characteristics of participants captured in the GP or Dispensing cohort but not the otehr")->baseline_table_gp_vs_meds_flex

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(baseline_table_gp_vs_meds_flex, path = "Outputs/Tables/baseline_table_gp_vs_meds.docx", pr_section = sect_properties)

rm(baseline_table_cohorts, baseline_table_flex, baseline_table_gp_vs_meds_flex, gpes_table, missing_table, recovery_table, dispensing_table, gp_vs_meds_cohorts, gp_not_meds_table, meds_not_gp_table, baseline_table_cohorts, baseline_table_flex, y, z)



### different locations?-----------


#### import maps of all UK countries 
### from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-bfc/explore?showTable=true

postcodes<-read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/UK maps/Postcode longitude and latitute/National_Statistics_Postcode_Lookup_UK_Coordinates.csv")

postcodes%<>%
  select(`Postcode 3`,
         Easting,
         Northing)


sites%<>%
  left_join(postcodes, by=c("Postcode"="Postcode 3"))

baseline_crf%>%
  filter(study_number %in% RECOVERY)%>%
  left_join(sites%>%mutate(SiteID=as.character(SiteID)), by=c("siteid"="SiteID"))%>%
  mutate(gp_not_dispensing = as.factor(if_else(study_number  %in% gp_not_meds, 1, 0)),
         dispensing_not_gp = as.factor(if_else(study_number %in% meds_not_gp, 1, 0)))%>%
  select(study_number,siteid, SiteName, recovery_flag, missing_flag, gpes_flag, dispensing_flag, gp_not_dispensing, dispensing_not_gp, Easting, Northing)%>%
  mutate(SiteID=as.integer(siteid))->participants_sites

site_counts<-participants_sites%>%
  rename(`English cohort`=recovery_flag,
         `GP cohort`=gpes_flag,
         `Dispensing cohort`=dispensing_flag,
         `Non-linked cohort`=missing_flag,
         `GP cohort\n(not in Dispensing)`=gp_not_dispensing,
         `Dispensing cohort\n(not in GP)`=dispensing_not_gp)%>%
  pivot_longer(-c(SiteID, SiteName,study_number, Easting, Northing), names_to = "Cohort", values_to="Flag")%>%
  filter(Flag=="1")%>%
  group_by(Cohort)%>%
  mutate(n=n_distinct(study_number))%>%
  mutate(Cohort = paste0(Cohort, "\nn=", n)) %>%
  group_by(SiteID, SiteName, Cohort, Northing, Easting)%>%
  summarise(Participants=n_distinct(study_number))%>%
  ungroup()%>%
  mutate(Cohort=factor(Cohort, levels=c("English cohort\nn=36454", 
                                        "GP cohort\nn=34140", 
                                        "Dispensing cohort\nn=33972", 
                                        "Non-linked cohort\nn=1110", 
                                        "GP cohort\n(not in Dispensing)\nn=1372", 
                                        "Dispensing cohort\n(not in GP)\nn=1204")))%>%
  arrange(Cohort, Participants)

filename<-"K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Tools/UK countries/Countries_(December_2021)_UK_BGC/CTRY_DEC_2021_UK_BGC.shp"

# load map to shape file
# trim<-glue::trim
countries_map<-raster::shapefile(filename)

countries_map_df<-broom::tidy(countries_map)

# plot the map (using eastings/northings instead of longitude and latitude as usual)
map<-ggplot()+
  geom_polygon(data=countries_map_df,
               aes(x=long, y=lat, group=group), 
               colour="black", 
               fill="grey",
               size=0.1,
               alpha=0.2,
               )+
  geom_point(data=site_counts,
             mapping=aes(x=Easting,
                         y=Northing,
                         size=Participants,
                         color=Participants),
             alpha=1/2)+
  theme_void(base_size=25) + 
  theme(text = element_text(family="Mulish"),
        legend.position="right",
        legend.key.width = unit(2, 'inch'),
        panel.spacing.x = unit(2, "cm"),
        plot.caption = element_text(size=20,hjust = 0), 
        plot.title.position = "plot", 
        # plot.subtitle.position="plot",
        plot.caption.position =  "plot")+
  scale_color_viridis(breaks=seq(0,2000,200))+
  guides(size="none")+
  labs(title="Geographical distribution of RECOVERY participants recruited in England (split by linkage cohort)",
       # caption = "English cohort: all participants recruited in England\nGP cohort: subset with linked GP data\nDispensing cohort: subset with linked Dispensing data\nNon-linked cohort: subset with no linkage to either dataset\nGP cohort (not in Dispensing): subset with linked GP data but no Dispensing data\nDispensing cohort (not in GP): subset with linked Dispensing data but no GP data"
       )+
  facet_wrap(~as.factor(Cohort))
  

ggsave("Outputs/Figures/baseline_drugs_recovery/UK_maps1.png",
       last_plot(),
       width=6.79*2.5, 
       height=6.18*2.5,
       dpi = "retina")

# side by side 
map+facet_wrap(~as.factor(Cohort), ncol=6)+
  theme(legend.position="bottom")

ggsave("Outputs/Figures/baseline_drugs_recovery/UK_maps1_side.png",
       last_plot(),
       width=6.79*3, 
       height=5.84*1.5,
       units="in",
       dpi = "retina")

# ALTERNATIVE PLOT
# load UK area file with longitude/latitute
# from https://opendata.camden.gov.uk/Maps/National-Statistics-Postcode-Lookup-UK-Coordinates/77ra-mbbn

# 
# postcodes<-read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/UK maps/Postcode longitude and latitute/National_Statistics_Postcode_Lookup_UK_Coordinates.csv")
# 
# postcodes%<>%
#   select(`Postcode 3`,
#          Longitude,
#          Latitude)
# 
# UK<-map_data("world")%>%filter(region=="UK")%>%ungroup()
# 
# 
# 
# sites%<>%
#   left_join(postcodes, by=c("Postcode"="Postcode 3"))
# 
# baseline_crf%>%
#   filter(study_number %in% RECOVERY)%>%
#   left_join(sites%>%mutate(SiteID=as.character(SiteID)), by=c("siteid"="SiteID"))%>%
#   mutate(gp_not_dispensing = as.factor(if_else(study_number  %in% gp_not_meds, 1, 0)),
#          dispensing_not_gp = as.factor(if_else(study_number %in% meds_not_gp, 1, 0)))%>%
#   select(study_number,siteid, SiteName, recovery_flag, missing_flag, gpes_flag, dispensing_flag, gp_not_dispensing, dispensing_not_gp, Longitude, Latitude)%>%
#   mutate(SiteID=as.integer(siteid))->participants_sites
# 
# 
# site_counts<-participants_sites%>%
#   rename(`English cohort`=recovery_flag,
#          `GP cohort`=gpes_flag,
#          `Dispensing cohort`=dispensing_flag,
#          `Non-linked cohort`=missing_flag,
#          `GP cohort \n(but not in Dispensing)`=gp_not_dispensing,
#          `Dispensing cohort \n(but not in GP)`=dispensing_not_gp)%>%
#   pivot_longer(-c(SiteID, SiteName,study_number, Longitude, Latitude), names_to = "Cohort", values_to="Flag")%>%
#   filter(Flag=="1")%>%
#   group_by(SiteID, SiteName, Cohort, Latitude, Longitude)%>%
#   summarise(Participants=n_distinct(study_number))%>%
#   mutate(Cohort=factor(Cohort, levels=c("English cohort", "GP cohort", "Dispensing cohort", "Non-linked cohort", "GP cohort \n(but not in Dispensing)", "Dispensing cohort \n(but not in GP)")))%>%
#   arrange(Cohort, Participants)
# 
# map<-ggplot() +
#   geom_polygon(data = UK,
#                #data=poly_counties_df,
#                aes(x=long, 
#                    y = lat, 
#                    group = group
#                ), 
#                fill="grey", 
#                alpha=0.3,
#                color="black") +
#   geom_point(data=site_counts,
#              mapping=aes(x=Longitude,
#                          y=Latitude,
#                          size=Participants,
#                          color=Participants),
#              alpha=1/2)+
#   facet_wrap(~as.factor(Cohort))+
#   theme_void(base_size=25) + 
#   theme(text = element_text(family="Mulish"),
#         legend.position="right",
#         # legend.key.width = unit(2, 'cm'),
#         legend.key.height = unit(2, 'cm'),
#         # panel.background = element_rect(fill = 'white'),
#         # plot.background = element_rect(fill = 'white')
#         panel.spacing.x = unit(2, "cm"))+
#   ylim(50,59) + 
#   coord_map()+
#   scale_color_viridis(breaks=seq(0,2000,200))+
#   guides(size="none")
# 
# map





### different randomisation dates? --------
# timeseries of randomisation dates for people in different sets 

recovery_not_linked <- setdiff(recovery_rands, append(recovery_gp, recovery_meds))

rand_dates%>%
  select(study_number, rand_date)%>%
  filter(study_number %in% recovery_rands)%>%
  mutate(recovery_rands = if_else(study_number %in% recovery_rands, 1,0),
         recovery_gp = if_else(study_number %in% recovery_gp, 1,0),
         recovery_meds = if_else(study_number %in% recovery_meds, 1,0),
         recovery_not_linked = if_else(study_number %in% recovery_not_linked, 1,0))%>%
  pivot_longer(-c(study_number, rand_date), names_to = "group", values_to="flag")%>%
  filter(flag!="0")%>%
  mutate(month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
  group_by(group, month)%>%
  summarise(Participants=n_distinct(study_number))->x

x%>%
  ggplot(aes(month, Participants, color=group, group=group))+
  geom_line()+
  geom_point()+
  scale_color_discrete(labels=c("GP dataset cohort", "Dispensing dataset cohort", "Non-linked participants", "RECOVERY English cohort"))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m %Y")+
  theme_gray(base_size=20)+
  labs(x="Month of randomisation",
       title=str_wrap("Timeseries of RECOVERY participants who were randomised in England, included in either the GP or Dispensing datasets, or not linked",200),
       caption="Each point represents aggregated counts of participants randomised per month and belonging to each of the colour-coded groups",
       color=NULL)+
  theme(legend.position="bottom")




ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_participants_linked_not_lonked.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")








rm(list_venn, recovery_rands, recovery_gp, recovery_meds, missing_participants, GP, Dispensing, last_date, x)



# 6. BNF chapters ------


## timeseries (anytime) -------

### entry counts -------
gp_dt%>%
  select(study_number, date, code)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  group_by(month)%>%
  summarise(Entries=n())%>%
  mutate(Dataset="GP")%>%
  as_tibble() ->x

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  rename(month=processing_period_date)%>%
  group_by(month)%>%
  summarise(Entries=n())%>%
  mutate(Dataset="Dispensing")%>%
  mutate(month=as.character(month))%>%
  as_tibble->x1

t<-rbind(x, x1, stringsAsFactors = F)

p1<-t%>%
  mutate(month=as.Date(month))%>%
  filter(month>="2000-01-01")%>%
  ggplot(aes(month, Entries, color=Dataset, group=Dataset))+
  geom_line()+
  geom_point()+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        strip.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+
  labs(title="Timeseries depicting representation of medications of any type in the GP and Dispensing datasets",
       subtitle="Entry counts")+
  ylim(0, NA)+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")), na.rm=T, linetype="dashed")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  theme(legend.position="none")







### participant counts -------

gp_dt%>%
  select(study_number, date, code)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  group_by(month)%>%
  summarise(Participants=n_distinct(study_number))%>%
  mutate(Dataset="GP")%>%
  as_tibble() ->x

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  rename(month=processing_period_date)%>%
  group_by(month)%>%
  summarise(Participants=n_distinct(study_number))%>%
  mutate(Dataset="Dispensing")%>%
  mutate(month=as.character(month))%>%
  as_tibble->x1

t<-rbind(x, x1, stringsAsFactors = F)

p2<-t%>%
  mutate(month=as.Date(month))%>%
  filter(month>="2000-01-01")%>%
  ggplot(aes(month, Participants, color=Dataset, group=Dataset))+
  geom_line()+
  geom_point()+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        strip.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+
  labs(subtitle="Participant counts",
       caption=str_wrap("Vertical lines placed at 01/06/18, and 29/08/21 (date extract received). Plot capped before 2000. Each point represents aggregated entry or participant counts per month.", 200))+
  ylim(0, NA)+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")), na.rm=T, linetype="dashed")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

p<-p1/p2


ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_bnf_total_counts.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")


## alternative plot (dual axes)


# gp_dt%>%
#   select(study_number, date, code)%>%
#   mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
#   inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
#   group_by(month)%>%
#   summarise(Entries=n(),
#             Participants = n_distinct(study_number))%>%
#   mutate(Dataset="GP")%>%
#   as_tibble() ->x
# 
# meds_dt%>%
#   select(study_number, processing_period_date, prescribed_bnf_code)%>%
#   rename(month=processing_period_date)%>%
#   group_by(month)%>%
#   summarise(Entries=n(),
#             Participants = n_distinct(study_number))%>%  mutate(Dataset="Dispensing")%>%
#   mutate(month=as.character(month))%>%
#   as_tibble->x1
# 
# t<-rbind(x,x1)
# 
# t%<>%
#   mutate(month=as.Date(month))%>%
#   filter(month>="2000-01-01")
# 
# 
# coef <-(t%>%arrange(desc(Entries))%>%select(Entries)%>%slice_head()%>%.[[1]])/
#   (t%>%arrange(desc(Participants))%>%select(Participants)%>%slice_head()%>%.[[1]])
# 
# 
# 
# t%>%  
#   ggplot(aes(month, value, color=Dataset))+
#   geom_line(aes(y=Entries/coef))+
#   geom_line(aes(y=Participants))+
#   geom_point(aes(y=Entries/coef), shape=1)+
#   geom_point(aes(y=Participants), shape=2)+
#   theme_gray(base_size=20)+
#   theme(axis.title.x=element_blank(),
#         strip.text = element_text(size=20),
#         legend.position = "bottom",
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
#   )+
#   labs(title="Timeseries depicting representation of medications of any type in the GP and Dispensing datasets",
#        subtitle="Entry counts")+
#   ylim(0, NA)+
#   geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")+
#   geom_vline(xintercept = as.numeric(as.Date("2018-06-01")), na.rm=T, linetype="dashed")+
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
#   theme(legend.position="bottom")+
#   scale_y_continuous(name="Participants",
#                      sec.axis = sec_axis(~.*coef, name="Entries"))



























## entry and participant counts per chapter (any time) ------


gp_dt%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  select(study_number, code, chapter, date, record_date, row_number)%>%
  as_tibble()%>%
  group_by(chapter)%>%
  summarise(Entries=n(),
            Participants = n_distinct(study_number),
            Participants_proportion = round(n_distinct(study_number)/participants_total*100, 1))%>%
  left_join(bnf_chapter_labels, by=c("chapter"))%>%
  mutate(dataset="GP")->x

meds_dt%>%
  select(study_number, prescribed_bnf_code, prescribed_bnf_name, processing_period_date)%>%
  mutate(chapter=as.numeric(str_sub(prescribed_bnf_code, 1,2)))%>%
  as_tibble()%>%
  group_by(chapter)%>%
  summarise(Entries=n(),
            Participants = n_distinct(study_number),
            Participants_proportion = round(n_distinct(study_number)/participants_total_meds*100, 1))%>%
  left_join(bnf_chapter_labels, by=c("chapter"))%>%
  mutate(dataset="Dispensing")->x1


t<-rbind(x,x1)




## plot (vertical bars)


t%<>%
  rename(Dataset=dataset)

p1<-t%>%
  mutate(labels=reorder(labels, chapter))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(data=t%>%filter(labels=="Cardiovascular System"), aes(label=Entries), size=5, position = position_dodge(width = 1.3), vjust=-.2)+
  geom_text(data=t%>%filter(labels!="Cardiovascular System"), aes(label=Entries), size=5, position = position_dodge(width = 1), vjust=-.2)+
  theme_gray(base_size=20)+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        legend.position="none")+
  labs(subtitle="Entries")+
  scale_x_discrete(expand=expansion(c(0.05,0.05)))+
  scale_fill_discrete(limits=as.character(c("Dispensing", "GP")))+
  scale_y_continuous(expand=expansion(c(0,0.1)))


p2<-t%>%
  mutate(labels=reorder(labels, chapter))%>%
  ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Participants), size=5, position = position_dodge(width = 1), vjust=-.2)+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust=1)
  )+
  labs(subtitle="Participants",
       x="Chapter",
       caption="Counts computed based on all available data (no time restrictions)")+
  scale_x_discrete(expand=expansion(c(0.05,0.05)))
  scale_fill_discrete(limits=as.character(c("Dispensing", "GP")))+
  scale_y_continuous(expand=expansion(c(0,0.1)))

p<-p1/p2+
  plot_annotation(title="Counts of entries and participants per BNF chapter and per dataset")&
  theme(plot.title = element_text(size=20))


p

ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_counts_vertical.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")

# alternative plot (horizontal bars)

# p1<-t%>%
#   mutate(labels=reorder(labels, desc(chapter)))%>%
#   rename(Dataset=dataset)%>%
#   ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
#   geom_bar(stat='identity', position='dodge')+
#   geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.2)+
#   theme_gray(base_size=20)+
#   theme(axis.text.x = element_blank(),
#         axis.title.y= element_blank(),
#         axis.title.x=element_blank(),
#         legend.position="bottom")+
#   labs(subtitle="Entries")+
#   scale_x_discrete(expand=expansion(c(0.05,0.05)))+
#   scale_y_continuous(expand=expansion(c(0,0.1)))+
#   coord_flip()
# 
# 
# 
# p2<-t%>%
#   mutate(labels=reorder(labels, desc(chapter)))%>%
#   rename(Dataset=dataset)%>%
#   ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
#   geom_bar(stat='identity', position='dodge')+
#   geom_text(aes(label=Participants), size=5, position = position_dodge(width = .9), hjust=-.2)+
#   theme_gray(base_size=20)+
#   theme(axis.text.y = element_blank(),
#         axis.title.y= element_blank(),
#         axis.title.x=element_blank(),
#         legend.position = "none")+
#   labs(subtitle="Participants")+
#   scale_x_discrete(expand=expansion(c(0.05,0.05)))+
#   scale_y_continuous(expand=expansion(c(0,0.1)))+
#   coord_flip()
# 
# 
# p<-p1+p2+
#   plot_annotation(title="BNF chapter counts per dataset")&
#   theme(plot.title = element_text(size=20))
# 
# 
# 
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_counts.png", 
#        last_plot(),
#        width=25, 
#        height=10,
#        dpi = "retina")




# table
 
# t%>%
#   filter(dataset=="GP")%>%
#   select(Chapter=labels,
#          Entries_GP = Entries,
#          Participants_GP = Participants,
#          Participants_proportion_GP = Participants_proportion)%>%
#   right_join( 
#     t%>%
#       filter(dataset=="Dispensing")%>%
#       select(Chapter=labels,
#              Entries_Dispensing = Entries,
#              Participants_Dispensing = Participants,
#              Participants_proportion_Dispensing = Participants_proportion))->t1
# 
# 
# write_csv(t1, "Outputs/Tables/counts_per_bnf_chapter_both_datasets.csv")
# 
# 

rm(x,x1, x2, p, p1, p2, t, t1)

























## timeseries (entries) -----


### anytime after 2000------
gp_dt%>%
  select(study_number, date, code)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  group_by(month, chapter)%>%
  summarise(Entries=n())%>%
  mutate(Dataset="GP")%>%
  left_join(bnf_chapter_labels, by=c("chapter"))%>%
  as_tibble() ->x

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  mutate(chapter=as.numeric(str_sub(prescribed_bnf_code, 1,2)))%>%
  rename(month=processing_period_date)%>%
  group_by(month, chapter)%>%
  summarise(Entries=n())%>%
  mutate(Dataset="Dispensing")%>%
  left_join(bnf_chapter_labels, by=c("chapter"))%>%
  mutate(month=as.character(month))%>%
  as_tibble->x1

t<-rbind(x, x1, stringsAsFactors = F)

t%>%
  mutate(labels=reorder(labels, chapter))%>%
  filter(month>="2000-01-01")%>%
  mutate(month=as.Date(month))%>%
  ggplot(aes(month, Entries, color=Dataset, group=Dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels, scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        strip.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+
  labs(title="Timeseries of entries per BNF chapter and per dataset",
       caption=str_wrap("Vertical dashed line placed at 29/08/21 (date extract received). Plot capped before 2000. Each point represents aggregated entries per month. Note that y-axes differ in each facet", 200))+
  ylim(0, NA)+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")


ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_timeseries.png", 
       last_plot(),
       width=25, 
       height=12,
       dpi = "retina")

### after 2021------

t%>%
  mutate(labels=reorder(labels, chapter))%>%
  filter(month>= "2021-01-01")%>%
  mutate(month=as.Date(month))%>%
  ggplot(aes(month, Entries, color=Dataset, group=Dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels, 
             labeller = label_wrap_gen(width = 30),
             scales = "free_y")+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        strip.text = element_text(size=20),
        legend.position = "bottom")+
  labs(title="Timeseries of entries per BNF chapter and per dataset (2021 onwards)",
       caption=str_wrap("Vertical dashed line placed at 29/08/21 (date extract received). Plot capped before 2021. Each point represents aggregated entries per month. Note that y-axes differ in each facet", 200))+
  ylim(0, NA)+
  scale_x_date()+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")



ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_timeseries_after_2021.png", 
       last_plot(),
       width=25, 
       height=12,
       dpi = "retina")

### June 2018 to May 2021------

t%>%
  mutate(labels=reorder(labels, chapter))%>%
  filter(month>= "2018-06-01",
         month<"2021-05-01")%>%  
  mutate(month=as.Date(month))%>%
  ggplot(aes(month, Entries, color=Dataset, group=Dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels, 
             labeller = label_wrap_gen(width = 30),
             scales = "free_y")+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        strip.text = element_text(size=20),
        legend.position = "bottom")+
  labs(title="Timeseries of entries per BNF chapter and per dataset (June 2018 to May 2021)",
       caption=str_wrap("Plot capped shows only entries with a date between June 2018 and May 2021 (first and last month adequately represented in both datasets). Each point represents aggregated counts per month. Note that y-axes differ in each facet", 200))+
  ylim(0, NA)+
  scale_x_date()



ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_timeseries_2018-2021.png", 
       last_plot(),
       width=25, 
       height=12,
       dpi = "retina")

rm(x,x1,t)


























## timeseries (participants) ------



### anytime after 2000------
gp_dt%>%
  select(study_number, date, code)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  group_by(month, chapter)%>%
  summarise(Participants=n_distinct(study_number))%>%
  mutate(Dataset="GP")%>%
  left_join(bnf_chapter_labels, by=c("chapter"))%>%
  as_tibble() ->x

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  mutate(chapter=as.numeric(str_sub(prescribed_bnf_code, 1,2)))%>%
  rename(month=processing_period_date)%>%
  group_by(month, chapter)%>%
  summarise(Participants=n_distinct(study_number))%>%
  mutate(Dataset="Dispensing")%>%
  left_join(bnf_chapter_labels, by=c("chapter"))%>%
  mutate(month=as.character(month))%>%
  as_tibble->x1

t<-rbind(x, x1, stringsAsFactors = F)

t%>%
  mutate(labels=reorder(labels, chapter))%>%
  filter(month>="2000-01-01")%>%
  mutate(month=as.Date(month))%>%
  ggplot(aes(month, Participants, color=Dataset, group=Dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels, scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        strip.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+
  labs(title="Timeseries of individual participant counts per BNF chapter and per dataset",
       caption=str_wrap("Vertical dashed line placed at 29/08/21 (date extract received). Plot capped before 2000. Each point represents aggregated counts per month. Note that y-axes differ in each facet", 200))+
  ylim(0, NA)+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")


ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_timeseries_participants.png", 
       last_plot(),
       width=25, 
       height=12,
       dpi = "retina")

### after 2021------

t%>%
  mutate(labels=reorder(labels, chapter))%>%
  filter(month>= "2021-01-01")%>%
  mutate(month=as.Date(month))%>%
  ggplot(aes(month, Participants, color=Dataset, group=Dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels, 
             labeller = label_wrap_gen(width = 30),
             scales = "free_y")+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        strip.text = element_text(size=20),
        legend.position = "bottom")+
  labs(title="Timeseries of individual participant counts per BNF chapter and per dataset (2021 onwards)",
       caption=str_wrap("Vertical dashed line placed at 29/08/21 (date extract received). Plot capped before 2021. Each point represents aggregated counts per month. Note that y-axes differ in each facet", 200))+
  ylim(0, NA)+
  scale_x_date()+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")



ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_timeseries_participants_after_2021.png", 
       last_plot(),
       width=25, 
       height=12,
       dpi = "retina")


### June 2018 till May 2021------

t%>%
  mutate(labels=reorder(labels, chapter))%>%
  filter(month>= "2018-06-01",
         month<"2021-05-01")%>%
  mutate(month=as.Date(month))%>%
  ggplot(aes(month, Participants, color=Dataset, group=Dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels, 
             labeller = label_wrap_gen(width = 30),
             scales = "free_y")+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        strip.text = element_text(size=20),
        legend.position = "bottom")+
  labs(title="Timeseries of individual participant counts per BNF chapter and per dataset (June 2018 until May 2021)",
       caption=str_wrap("Plot capped shows only entries with a date between June 2018 and May 2021 (first and last month adequately represented in both datasets). Each point represents aggregated counts per month. Note that y-axes differ in each facet", 200))+
  ylim(0, NA)+
  scale_x_date()



ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_timeseries_participants_2018-2021.png", 
       last_plot(),
       width=25, 
       height=12,
       dpi = "retina")


rm(x,x1, t)

## unique codes --------


gp_dt%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  group_by(study_number, chapter)%>%
  summarise(distinct_snomed_codes = n_distinct(code))%>%
  left_join(bnf_chapter_labels)%>%
  mutate(Dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  mutate(chapter = as.integer(str_sub(prescribed_bnf_code,0,2)))%>%
  group_by(study_number, chapter)%>%
  summarise(distinct_bnf_codes = n_distinct(prescribed_bnf_code),
            distinct_snomed_codes = n_distinct(prescribeddmd_code))%>%
  left_join(bnf_chapter_labels, by=c("chapter"))%>%
  mutate(Dataset="Dispensing")%>%
  as_tibble()->x2

x1%<>%
  rename(snomed_gp = distinct_snomed_codes)%>%
  select(-Dataset, -labels)

x2%<>%
  rename(snomed_dispensing = distinct_snomed_codes,
         bnf_dispensing = distinct_bnf_codes)%>%
  select(-Dataset, -labels)

t<-x1%>%
  right_join(x2, by=c("study_number", "chapter"))

t%<>%
  left_join(bnf_chapter_labels, by=c("chapter"))


# plot
t%>%
  pivot_longer(-c(study_number, chapter, labels), names_to="key", values_to = "value")%>%
  ggplot(aes(value, fill=key))+
  geom_boxplot(aes(y=key), alpha=0.5)+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  labs(title="Boxplots depicting number of distinct codes per participant, for each BNF chapter",
       subtitle="SNOMED codes in the GP dataset, and SNOMED and BNF codes in the Dispensing dataset",
       y="",
       x="Number of distinct codes",
       fill="")+
  scale_fill_discrete(limits= c("bnf_dispensing", "snomed_dispensing", "snomed_gp"),
                      labels = c("BNF codes (Dispensing dataset)", "SNOMED codes (Dispensing dataset)", "SNOMED codes (GP dataset)"))

ggsave("Outputs/Figures/baseline_drugs_recovery/distinct_codes_bnf_chapters.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")



### summary stats in table (TEMPLATE)------
t%<>%
  pivot_longer(-c(study_number, chapter, labels), names_to="key", values_to = "value")

t%>%  
  group_by(chapter, labels, key)%>%
  summarise(median=median(value, na.rm = T),
            Q1 = quantile(value, probs=0.25, na.rm=T),
            Q3 = quantile(value, probs=0.75, na.rm=T))%>%
  pivot_wider(id_cols=c(chapter, labels), names_from=key, values_from = c(median, Q1, Q3))%>%
  transmute(BNF_dispensing = paste0(median_bnf_dispensing, " (", Q1_bnf_dispensing, "-", Q3_bnf_dispensing, ")"),
            SNOMED_dispensing = paste0(median_snomed_dispensing, " (", Q1_snomed_dispensing, "-", Q3_snomed_dispensing, ")"),
            SNOMED_GP = paste0(median_snomed_gp, " (", Q1_snomed_gp, "-", Q3_snomed_gp, ")"))%>%
  mutate(SNOMED_GP =  if_else(str_detect(SNOMED_GP, pattern="NA"), "N/A", SNOMED_GP))%>%
  ungroup()%>%
  rename('GP dataset\n(SNOMED codes)\n \nMedian (IQR)' = SNOMED_GP,
         'Dispensing dataset\n(SNOMED codes)\n \nMedian (IQR)' = SNOMED_dispensing,
         'Dispensing dataset\n(BNF codes)\n \nMedian (IQR)' = BNF_dispensing,
         'BNF chapter' = labels)%>%
  select(-chapter)->table


set_flextable_defaults(
  font.size = 10, 
  theme_fun = theme_vanilla,
  padding = 6,
  text.align = "left",
  font.family = "Mulish")


table%<>%
  flextable()%>%
  set_caption("Average number of distinct SNOMED or BNF codes per participant for each BNF chapter")%>%
  width(j=1, width=4)%>%
  width(j=2:4, width=2)%>%
  add_footer_lines("The GP dataset contains only SNOMED codes, whereas the Dispensing dataset contains both SNOMED and BNF codes.\nIQR - Interquartile range")%>%
  align(i=NULL, j=2:4, align="center", part="all")%>%
  flextable::font(fontname="Mulish")

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(table, path = "Outputs/Tables/unique_codes_per_bnf_chapter_both_datasets.docx", pr_section = sect_properties)


rm(x1, x2, t, table)




## day intervals (based on individual codes)--------

meds_dt%>%
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  distinct(study_number, prescribed_bnf_code, processing_period_date, .keep_all=T)%>%
  group_by(study_number, prescribed_bnf_code)%>%
  filter(n()>1)%>%
  arrange(processing_period_date)%>%
  group_by(prescribed_bnf_code, study_number)%>%
  summarise(average=round(as.numeric(median(diff(processing_period_date))),0))%>%
  mutate(chapter = as.integer(str_sub(prescribed_bnf_code,0,2)))%>%
  left_join(bnf_chapter_labels)%>%
  mutate(dataset="Dispensing")%>%
  rename(code=prescribed_bnf_code)%>%
  as_tibble()->p1


gp_dt%>%
  select(study_number, date, code)%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  distinct(study_number, code, date, .keep_all=T)%>%
  group_by(study_number, code)%>%
  filter(n()>1)%>%
  arrange(date)%>%
  group_by(code, study_number)%>%
  summarise(average=round(as.numeric(median(diff(date))),0))%>%
  left_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  left_join(bnf_chapter_labels)%>%
  mutate(dataset="GP")%>%
  select(code, study_number, average, chapter, labels, dataset)%>%
  as_tibble()->p



t<-rbind(p,p1)

rm(p,p1)



### plot (ridges) -----

t%>%
  group_by(labels, dataset)%>%
  summarise(median=round(median(average, na.rm = T),0),
            Q1 = round(quantile(average, probs=0.25, na.rm=T),0),
            Q3 = round(quantile(average, probs=0.75, na.rm=T),0))%>%
  right_join(bnf_chapter_labels, by=c("labels"))%>%
  mutate(labels=as.factor(reorder(labels, chapter)))->medians

t%>%
  mutate(labels=fct_reorder(labels, desc(chapter)))%>%
  ggplot(aes(x=average, fill=dataset))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(limits=c(NA,1000), breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360, 500,750,1000))+
  theme_gray(base_size=20)+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom"
        
  )+
  labs(x = "Average day difference",
       title="Time interval between consecutive entries",
       subtitle="(per BNF chapter and per dataset)",
       caption=str_wrap("Day differences calculated at individual code and participant level as the median day different between consecutive entries. Vertical lines depict medians per BNF chapter and dataset. Entries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date", 200),
       fill="Dataset",
       y="Chapter")


ggsave("Outputs/Figures/baseline_drugs_recovery/day_differences_per_chapter_both_datasets.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")




# alternative plot (faceted densities)


# 
# t%>%
#   ggplot(aes(x=average, fill=dataset))+
#   geom_density(alpha=0.5, outline.type = "both")+
#   facet_wrap(~as.factor(reorder(labels, chapter)),
#              labeller = label_wrap_gen(width = 30),
#              scales="free")+
#   scale_x_continuous(limits=c(NA,1000), breaks=c(0,30,90,180, 240, 500,750,1000))+
#   geom_vline(data=medians%>%group_by(labels), aes(xintercept=median, color=dataset, group=labels), linetype="dashed", size=1.2)+
#   theme_gray(base_size=20)+
#   theme(axis.title.y = element_blank(),
#         # axis.text.y = element_blank()        
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position = "bottom"
#         
#   )+
#   labs(x = "Median day difference",
#        title="Average day difference between consecutive entries for the same participant",
#        subtitle="(per BNF chapter and per dataset)",
#        caption=str_wrap("Vertical lines depict medians per BNF chapter and dataset. Average day difference calculated as the median day difference between consecutive entries per BNF chapter and per participant. Since multiple drugs may be recorded with the same date due to polypharmacy, entries with the same date were excluded so that a unique prescribing/dispensing event per distinct date was retained", 200))



### table -----


medians<-medians[order(medians$chapter),]


set_flextable_defaults(
  font.size = 10, 
  theme_fun = theme_vanilla,
  padding = 6,
  text.align = "left",
  font.family = "Mulish")


medians%>%
  rename('BNF chapter' = labels,
         Median=median)%>%
  mutate(IQR = paste0(Q1, "-", Q3))%>%
  select(-Q1,-Q3)%>%
  pivot_wider(c('BNF chapter'), names_from = "dataset", values_from = c("Median", "IQR"))%>%
  # mutate(Median_Dispensing = paste0(Median_Dispensing, " (",IQR_Dispensing,")"),
  #        Median_GP = paste0(Median_GP, " (",IQR_GP,")"))%>%
  select(Median_GP, IQR_GP, Median_Dispensing, IQR_Dispensing)%>%
  flextable()%>%
  set_header_labels(values=list(Median_GP="Median", 
                                IQR_GP = "IQR",
                                Median_Dispensing="Median",
                                IQR_Dispensing="IQR"))%>%
  set_caption("Time intervals (in days) between consecutive entries (per BNF chapter and per dataset)")%>%
  add_footer_lines("Day differences calculated at individual code and participant level as the median day different between consecutive entries.\nEntries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date")%>%
  width(j=1, width=4)%>%
  width(j=2:5, width=1)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=c(2,4), align="right", part="body")%>%
  align(i=NULL, j=c(3,5), align="left", part="body")%>%
  align(i=NULL, j=c(2,4), align="right", part="header")%>%
  align(i=NULL, j=c(3,5), align="left", part="header")%>%
  add_header_row(values = c("","GP dataset", "Dispensing dataset"), colwidths = c(1,2,2))->table

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())


table

save_as_docx(table, path = "Outputs/Tables/day_difference_bnf_chapters.docx", pr_section = sect_properties)

rm(p,p1, table, medians)

### proportions in each time interval -----

t%>%
  mutate(labels=fct_reorder(labels, chapter))%>%
  group_by(labels, dataset)%>%
  summarise(total = n_distinct(study_number),
            interval_30 = round(n_distinct(study_number[average<=30])/total*100,0),
            interval_60 = round(n_distinct(study_number[average<=60])/total*100,0),
            interval_90 = round(n_distinct(study_number[average<=90])/total*100,0),
            interval_180 = round(n_distinct(study_number[average<=180])/total*100,0),
            interval_360 = round(n_distinct(study_number[average<=360])/total*100,0))%>%
  rename("30" = interval_30,
         "60" = interval_60,
         "90" = interval_90,
         "180" = interval_180,
         "360" = interval_360)%>%
  select(-total)%>%
  pivot_longer(-c(labels, dataset), names_to="key", values_to="value")%>%
  mutate(key=as.numeric(key))->t1

t1%>%  
  ggplot(aes(x=as.numeric(key), y=value, fill=dataset, color=dataset))+
  geom_point(aes(color=dataset))+
  geom_line(aes(color=dataset))+
  geom_text_repel(data=t1%>%filter(dataset=="GP"), 
                  aes(label=paste0(value, "%")),
                  direction="y",
                  nudge_y = 5,
                  size=8)+
  geom_text_repel(data=t1%>%filter(dataset=="Dispensing"), 
                  aes(label=paste0(value, "%")),
                  direction="y",
                  nudge_y = -5,
                  size=8)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position="right")+  
  labs(y="Participants (%)",
       title="Proportions of participants with distinct average time intervals between consecutive entries",
       subtitle="(per BNF chapter and dataset)",
       x="Timeframe (in days)",
       caption="Time intervals calculated as the median day difference between consecutive entries with the same BNF code for the same participant\nDuplicate entries (same BNF code, same participant, same date) were excluded to avoid counting of zero-day intervals between those\nProportions refer to total number of participants with an entry for each BNF chapter and dataset at any point in time",
       fill="Dataset",
       color="Dataset")+
  scale_x_continuous(breaks=c(30,60,90, 180, 360))+
  scale_y_continuous(breaks=seq(0,100, by=20), expand=expansion(c(0,0.2)))


ggsave("Outputs/Figures/baseline_drugs_recovery/time_intervals_proportions_bnf_chapters.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")




rm(t, p, t, p1, plot1, plot2, medians)


## distinct entries per month ------------


gp_dt%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  count(study_number, month, chapter)%>%
  left_join(bnf_chapter_labels)%>%
  mutate(Dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  inner_join(bnf_chapters, by=c("prescribed_bnf_code" = "bnf_code"))%>%
  mutate(month=as.Date(processing_period_date))%>%
  count(study_number, month, chapter)%>%
  left_join(bnf_chapter_labels)%>%
  mutate(Dataset="Dispensing")%>%
  as_tibble()->x2

p<-rbind(x1, x2)

rm(x1,x2)

p%>%
  filter(!is.na(labels))%>%
  mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"))%>%
  mutate(labels=reorder(labels, desc(labels)))%>%
  select(labels, Dataset, n)%>%
  ggplot(aes(n, labels, fill=labels))+
  stat_density_ridges(scale=0.8, quantile_lines = T, quantiles=2, alpha=0.5)+
  facet_wrap(~Dataset, ncol=2)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y=element_blank())+
  scale_x_continuous(limits = c(NA,10), breaks=seq(1,10,by=1))+
  labs(title="Distributions of entry counts per month and per participant",
       subtitle="Aggregated per per BNF chapter and dataset",
       caption="Plots capped at 10. Vertical lines in each density plot represent each drug chapter's median")

ggsave("Outputs/Figures/baseline_drugs_recovery/entries_per_month_bnf_chapters.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


## cumulative counts ---------

gp_dt%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  filter(month>= "2018-06-01",
         month<= "2021-05-01")%>%  
  distinct(study_number, month, chapter)%>%
  group_by(study_number, chapter)%>%
  filter(month==min(month))%>%
  group_by(month, chapter)%>%
  summarise(participants=n_distinct(study_number))%>%
  group_by(chapter)%>%
  mutate(cumulative_participants=cumsum(participants))%>%
  select(-participants)%>%
  left_join(bnf_chapter_labels)%>%
  mutate(Dataset="GP")%>%
  as_tibble()->x1

x1%<>%
  mutate(month=as.Date(month))

meds_dt%>%
  mutate(month=as.Date(processing_period_date))%>%
  filter(month>= "2018-06-01",
         month<= "2021-05-01")%>%  
  inner_join(bnf_chapters, by=c("prescribed_bnf_code" = "bnf_code"))%>%
  distinct(study_number, month, chapter)%>%
  group_by(study_number, chapter)%>%
  filter(month==min(month))%>%
  group_by(month, chapter)%>%
  summarise(participants=n_distinct(study_number))%>%
  group_by(chapter)%>%
  mutate(cumulative_participants=cumsum(participants))%>%
  select(-participants)%>%
  left_join(bnf_chapter_labels)%>%
  mutate(Dataset="Dispensing")%>%
  as_tibble()->x2


t<-rbind(x1, x2)

t%>%
  ggplot(aes(month, cumulative_participants, color=Dataset, group=Dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels, scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  labs(title="Cumulative participant counts per BNF chapter and dataset (from June 2018 until May 2021)",
       x="Month",
       y="Participants",
       caption="Each point represents cumulative monthly counts of participants first represented in each dataset (between June 2018 and May 2021)")

ggsave("Outputs/Figures/baseline_drugs_recovery/cumulative_counts_bnf_chapters.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

rm(x1, x2, t)






## agreement ------

### participant level -------

#### wrangling ---------

gp_dt%>%
  select(study_number, code, date, row_number)%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  select(study_number, code, chapter, date, row_number)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  filter(month>= "2018-06-01",
         month<= "2021-05-01")%>%  
  distinct(study_number, chapter)%>%
  mutate(dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  mutate(chapter=as.numeric(str_sub(prescribed_bnf_code, 1,2)))%>%
  rename(month=processing_period_date)%>%  
  filter(month>= "2018-06-01",
         month<= "2021-05-01")%>%  
  distinct(study_number,chapter)%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()->x2

t<-rbind(x1, x2)

rm(x1,x2)

t%>%distinct(study_number)%>%nrow()->participants

t%>%distinct(study_number)->participants_list


t%<>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, chapter), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, chapter)%>%
  mutate(study_number=as.numeric(study_number))


#### calculate agreement ----



kappa_bnf_participants<-list()

for (i in 1:23) {
  
  t%>%
    filter(chapter==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, "0")))%>%  
    select(-chapter, -study_number)%>%
    mutate(across(everything(), as.integer))->table
  
  try(k<-Kappa.test(table$GP, table$Dispensing))
  
  if(exists("k"))
  {
    
    kappa_bnf_participants[[i]]<-k
    
  }
  else{
    kappa_bnf_participants[[i]]<-list()
  }
  
  rm(i, table, k)
  
}

kappa_bnf_participants_results<-data.frame()

for (i in 1:23) {
  
  name<-i
  
  try(
    table<-(data.frame(
      chapter = name,
      kappa = unlist(kappa_bnf_participants[[i]]$Result$estimate),
      CI_lower = unlist(kappa_bnf_participants[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_bnf_participants[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_bnf_participants[[i]]$Result$p.value),
      judgement = unlist(kappa_bnf_participants[[i]]$Judgement)
    ))
  )
  
  try(kappa_bnf_participants_results%<>%bind_rows(table))
  
  
  rm(table, name, i)
  
}




kappa_bnf_participants_results%>%
  right_join(bnf_chapter_labels)%>%
  mutate_when(!is.na(CI_lower), 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate_when(p_value==0.00,
              p_value="<0.001")%>%
  
  left_join (
    (t%>%
       group_by(chapter)%>%
       summarise(GP_Dispensing = n_unique(study_number[GP=="1" & Dispensing=="1"]),
                 GP_only = n_unique(study_number[GP=="1" & Dispensing=="0"]),
                 Dispensing_only = n_unique(study_number[GP=="0" & Dispensing=="1"]),
                 nil = participants - GP_Dispensing - GP_only - Dispensing_only)),
    by=c("chapter"))%>%
  
  select(Chapter=chapter,
         Label=labels,
         GP_Dispensing,
         GP_only,
         Dispensing_only,
         nil,
         kappa,
         CI,
         p_value,
         judgement)%>%
  arrange(Chapter)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))-> bnf_chapter_agreement_participants











bnf_chapter_agreement_participants%<>%
  mutate(across(3:6, ~replace_na(.x, 0)))%>%
  rowwise() %>%
  mutate(prop = round(sum(across(3:5), na.rm = T)/sum(across(3:6), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either dataset (%)' = paste0(sum(across(3:5), na.rm = T),
                                       " (",
                                       prop,
                                       "%)"))%>%
  mutate(prop1 = round(sum(across(3:3))/sum(across(3:6), na.rm = T)*100,1),
         prop2 = round(sum(across(4:4))/sum(across(3:6), na.rm = T)*100,2),
         prop3 = round(sum(across(5:5))/sum(across(3:6), na.rm = T)*100,1),
         prop4 = round(sum(across(6:6))/sum(across(3:6), na.rm = T)*100,1))%>%
  mutate(prop1=if_else(is.na(prop1), 0, prop1))%>%
  mutate(prop2=if_else(is.na(prop2), 0, prop2))%>%
  mutate(prop3=if_else(is.na(prop3), 0, prop3))%>%
  mutate(prop4=if_else(is.na(prop4), 0, prop4))%>%
  mutate(GP_Dispensing=paste0(GP_Dispensing, " (", prop1, "%)"),
         GP_only=paste0(GP_only, " (", prop2, "%)"),
         Dispensing_only=paste0(Dispensing_only, " (", prop3, "%)"),
         nil=paste0(nil, " (", prop4, "%)"))%>%
  select(-c(prop, prop1, prop2, prop3, prop4))%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%
  mutate(Chapter = paste0(Chapter, ": ", Label))%>%
  select(-Label)%>%
  rename(K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement)%>%
  relocate('Either dataset (%)', .after = 'Chapter')


#### table (formattable) (NOT IN GREAT SHAPE) -----

bnf_chapter_agreement_participants%>%
  rename("Participants in either dataset (%)" = "Either dataset (%)",
         'Participants in both datasets (%)' = GP_Dispensing,
         'Participants in GP dataset only (%)' = GP_only,
         'Participants in Dispensing dataset only (%)'=Dispensing_only,
         'Participants in neither dataset (%)'=nil)%>%
  formattable(
    align=c("l"),
    list(area(col=2) ~color_tile("transparent", "pink"),
      area(col=3:6) ~color_tile("transparent", "green"),
      area(row=c(bnf_chapter_agreement_participants%>%
                   ungroup()%>%
                  mutate(row=row.names(.))%>%
                   filter(!is.na(K))%>%
                   select(row)%>%
                   .[[1]]), 
           col=K) ~color_bar("lightblue")))->table

table # need to printscreen this from the browser to save table


#### table (flextable) -----

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())


bnf_chapter_agreement_participants%>%
  rename("Participants in either dataset (%)" = "Either dataset (%)",
         'Participants in both datasets (%)' = GP_Dispensing,
         'Participants in GP dataset only (%)' = GP_only,
         'Participants in Dispensing dataset only (%)'=Dispensing_only,
         'Participants in neither dataset (%)'=nil)%>%
  flextable()%>%
  set_caption("Agreement between the GP and Dispensing dataset for each BNF chapter\n(for records between June 2018 and May 2021)")%>%
  width(j=1, width=4)%>%
  width(j=2:10, width=1)%>%
  flextable::font(fontname="Mulish")%>%
  set_header_labels(values=list(CI="95% CI", 
                                p_value = "p-value"))%>%
  add_footer_lines("CI: confidence interval")%>%
  align(i=NULL, j=2:10, align="center", part="all")->table_flex

save_as_docx(table_flex, path = "Outputs/Tables/agreement_bnf_chapters.docx", pr_section = sect_properties)


rm(bnf_agreement_participants, kappa_bnf_participants, kappa_bnf_participants_results, participants, participants_list, table_flex)




















### monthly prescription level (ABANDONED)---------

# gp_dt%>%
#   select(study_number, code, date, row_number)%>%
#   inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
#   select(study_number, code, chapter, date, row_number)%>%
#   mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
#   filter(month>= "2018-01-01",
#          month<= "2021-05-01")%>%  
#   distinct(study_number, chapter, month)%>%
#   mutate(dataset="GP")%>%
#   as_tibble()->x1
# 
# meds_dt%>%
#   select(study_number, processing_period_date, prescribed_bnf_code)%>%
#   mutate(chapter=as.numeric(str_sub(prescribed_bnf_code, 1,2)))%>%
#   rename(month=processing_period_date)%>%  
#   mutate(month=as.character(month))%>%
#   filter(month>= "2018-01-01",
#          month<= "2021-05-01")%>%  
#   distinct(study_number, chapter, month)%>%
#   mutate(dataset="Dispensing")%>%
#   as_tibble()->x2
# 
# t<-rbind(x1, x2)
# 
# rm(x1,x2)
# 
# t%>%distinct(study_number)%>%nrow()->participants
# 
# t%>%distinct(study_number)->participants_list
# 
# t%<>%
#   mutate(flag="1")%>%
#   pivot_wider(c(study_number, chapter, month), names_from=dataset, values_from = flag)%>%
#   mutate(GP=replace_na(GP,"0"))%>%
#   mutate(Dispensing=replace_na(Dispensing,"0"))%>%
#   arrange(study_number, chapter)%>%
#   mutate(study_number=as.numeric(study_number))
# 
# 
# d1<-as.Date("2018-06-01", "%Y-%m-%d")
# d2<-as.Date("2021-05-01", "%Y-%m-%d")
# 
# month<-data.frame(month=format(seq(d1, d2, by="month"), "%Y-%m-%d"))
# 
# bnf_chapters%>%
#   distinct(chapter)%>%
#   .[[1]]->chapters
# 
# t%>%
#   distinct(study_number)%>%
#   .[[1]]->participants
# 
# d<-expand.grid(month, chapters, participants)
# 
# 
# t%>%
#   right_join(d)%>%
#   mutate(GP=replace_na(GP,"0"))%>%
#   mutate(Dispensing=replace_na(Dispensing,"0"))%>%
#   filter(GP=="0"&Dispensing=="0")
# 
# 
# 
# kappa_bnf_entries<-list()
# 
# for (i in 1:23) {
#   
#   t%>%
#     filter(chapter==i)%>%
#     mutate(study_number=as.character(study_number))%>%
#     right_join(participants_list)%>%
#     mutate(across(c(GP, Dispensing), ~replace_na(.x, "0")))%>%  
#     select(-chapter, -study_number, -month)%>%
#     mutate(across(everything(), as.integer))->table
#   
#   try(k<-Kappa.test(table$GP, table$Dispensing))
#   
#   if(exists("k"))
#   {
#     
#     kappa_bnf_entries[[i]]<-k
#     
#   }
#   else{
#     kappa_bnf_entries[[i]]<-list()
#   }
#   
#   rm(i, table, k)
#   
# }
# 
# 
# t%>%
#   filter(chapter==2)%>%
#   mutate(study_number=as.character(study_number))%>%
#   right_join(participants_list)%>%
#   mutate(across(c(GP, Dispensing), ~replace_na(.x, "0")))%>%
#   filter(GP=="0")%>% View()
# # select(-chapter, -study_number, -month)%>%
# # mutate(across(everything(), as.integer))->table
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# kappa_bnf_entries_results<-data.frame()
# 
# for (i in 1:23) {
#   
#   name<-i
#   
#   try(
#     table<-(data.frame(
#       chapter = name,
#       kappa = unlist(kappa_bnf_participants[[i]]$Result$estimate),
#       CI_lower = unlist(kappa_bnf_participants[[i]]$Result$conf.int[1]),
#       CI_upper = unlist(kappa_bnf_participants[[i]]$Result$conf.int[2]),
#       p_value = unlist(kappa_bnf_participants[[i]]$Result$p.value),
#       judgement = unlist(kappa_bnf_participants[[i]]$Judgement)
#     ))
#   )
#   
#   try(kappa_bnf_entries_results%<>%bind_rows(table))
#   
#   
#   rm(table, name, i)
#   
# }
# 
# 
# kappa_bnf_entries_results%>%
#   right_join(bnf_chapter_labels)%>%
#   mutate_when(!is.na(CI_lower), 
#               CI = paste0(
#                 round(CI_lower,2),
#                 " : ",
#                 round(CI_upper, 2)))%>%
#   
#   mutate(kappa=round(kappa,2),
#          p_value=as.character(round(p_value, 2)))%>%
#   
#   mutate_when(p_value==0.00,
#               p_value="<0.001")%>%
#   
#   left_join (
#     (t2%>%
#        group_by(chapter)%>%
#        summarise(GP_Dispensing = n_unique(study_number[GP=="1" & Dispensing=="1"]),
#                  GP_only = n_unique(study_number[GP=="1" & Dispensing=="0"]),
#                  Dispensing_only = n_unique(study_number[GP=="0" & Dispensing=="1"]),
#                  nil = participants - GP_Dispensing - GP_only - Dispensing_only)),
#     by=c("chapter"))%>%
#   
#   select(Chapter=chapter,
#          Label=labels,
#          "GP+/Dispensing+"=GP_Dispensing,
#          "GP+/Dispensing-"=GP_only,
#          "GP-/Dispensing+"=Dispensing_only,
#          "GP-/Dispensing-"=nil,
#          K=kappa,
#          '95% CI'=CI,
#          'p-value' = p_value,
#          Judgement=judgement)%>%
#   arrange(Chapter)%>%
#   as_tibble()%>%
#   mutate(K=as.numeric(K))-> bnf_chapter_agreement_participants
# 
# bnf_chapter_agreement_participants%>%
#   formattable(
#     align=c("l"),
#     list(
#       area(col=3:6) ~color_tile("transparent", "pink"),
#       area(row=c(bnf_chapter_agreement_participants%>%
#                    mutate(row=row.names(.))%>%
#                    filter(!is.na(K))%>%
#                    select(row)%>%
#                    .[[1]]), 
#            col=K) ~color_bar("lightblue")))->table
# 
# 
# rm(bnf_agreement_participants, kappa_bnf_participants, kappa_bnf_participants_results, participants, participants_list)
# 
# 
# 
# 
# 
# rm(d1,d2, dat, month, chapters, participants)








## discordant participant pairs per month --------------



gp_dt%>%
  select(study_number, code, date, row_number)%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  select(study_number, code, chapter, date, row_number)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  filter(month>= "2018-01-01",
         month<= "2021-05-01")%>%
  distinct(study_number, chapter, month)%>%
  mutate(dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  mutate(chapter=as.numeric(str_sub(prescribed_bnf_code, 1,2)))%>%
  rename(month=processing_period_date)%>%
  mutate(month=as.character(month))%>%
  filter(month>= "2018-01-01",
         month<= "2021-05-01")%>%
  distinct(study_number, chapter, month)%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()->x2

t<-rbind(x1, x2)

rm(x1,x2)

t%>%distinct(study_number)%>%nrow()->participants

t%>%distinct(study_number)->participants_list

t%<>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, chapter, month), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, chapter)%>%
  mutate(study_number=as.numeric(study_number))

t%<>%
  mutate(group = as.factor(case_when(GP=="1" & Dispensing=="1" ~ "Both",
                           GP=="1" & Dispensing=="0" ~ "GP",
                           GP=="0" & Dispensing=="1" ~ "Dispensing")))%>%
  mutate(month=as.Date(month))%>%
  group_by(month, chapter, group)%>%
  summarise(participants = n())%>%
  pivot_wider(c(month, chapter), names_from = "group", values_from = "participants")%>%
  mutate(Either=sum(GP, Dispensing, Both, na.rm = T))%>%
  pivot_longer(3:6, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(bnf_chapter_labels)%>%
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))

t%>%
  ungroup()%>%
  mutate(labels=as.factor(reorder(labels, chapter)))%>%
  ggplot(aes(month, Participants, color=Dataset, group=Dataset, shape=Dataset))+
  geom_point()+
  geom_line()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle=90))+
  scale_y_continuous(limits = c(0, NA))+
  # scale_x_date(date_breaks = "1 month", date_labels = "%m %Y")+
  labs(x="Month",
       y="Participants",
       color="Dataset",
       title="Number of participants captured in each dataset along time, per BNF chapter",
       caption="Points depict monthly counts of participants recorded in either dataset, both datasets, or in the Dispensing or GP datasets only. Timeseries restricted to June 2018 to May 2021")+
  scale_color_discrete(limits=c( "Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_shape_discrete(limits=c( "Either dataset","Both datasets", "Dispensing only", "GP only"))
  


ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_timeseries_bnf_chapters.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")



#### barplot ----------

t%>%
  mutate(group = as.factor(case_when(GP=="1" & Dispensing=="1" ~ "Both",
                                     GP=="1" & Dispensing=="0" ~ "GP",
                                     GP=="0" & Dispensing=="1" ~ "Dispensing")))%>%
  group_by(chapter, group)%>%
  summarise(participants = n())%>%
  pivot_wider(c(chapter), names_from = "group", values_from = "participants")%>%
  mutate(Either=sum(GP, Dispensing, Both, na.rm = T))%>%
  pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(bnf_chapter_labels)%>%
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
  ungroup()%>%
  mutate(labels=reorder(labels, chapter))%>%
  
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.3)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(color="Dataset",
       title="Number of participants captured in each dataset, per BNF chapter",
       caption="Proportions presented are in reference to the number of participants identified using either dataset. Calculations restricted to the time period between June 2018 and May 2021"
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_bnf_chapters.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")
















### investigate presence of participants in GP but not dispensing along time-------

meds_dt%>%
  distinct(study_number)%>%
  as_tibble()%>%
  .[[1]]->participants_meds

gp_dt%>%
  distinct(study_number)%>%
  as_tibble()%>%
  .[[1]]->participants_gp

participants_gp_only <- setdiff(participants_gp, participants_meds)


gp_dt%>%
  select(study_number, code, date, row_number)%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  select(study_number, code, chapter, date)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  filter(month>= "2018-01-01",
         month<= "2021-05-01")%>%
  distinct(study_number, chapter, month)%>%
  mutate(dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  mutate(chapter=as.numeric(str_sub(prescribed_bnf_code, 1,2)))%>%
  rename(month=processing_period_date)%>%
  mutate(month=as.character(month))%>%
  filter(month>= "2018-01-01",
         month<= "2021-05-01")%>%
  distinct(study_number, chapter, month)%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()->x2


t<-rbind(x1, x2)

rm(x1,x2)

t%>%distinct(study_number)%>%nrow()->participants

t%>%distinct(study_number)->participants_list

t%<>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, chapter, month), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, chapter)%>%
  mutate(study_number=as.numeric(study_number))

t%<>%
  filter(GP=="1" & Dispensing=="0")%>%
  mutate(group = if_else(study_number %in% participants_gp_only, "Participants not captured in Dispensing dataset (at any time point)", "Participants captured in Dispensing dataset (at any time point)"))%>%
  group_by(month, chapter, group)%>%
  summarise(participants = n())
  
  
t%<>%
  ungroup()%>%
  left_join(bnf_chapter_labels)%>%
  mutate(month=as.Date(month))%>%
  mutate(labels=as.factor(reorder(labels, chapter)))

t%>%
  ggplot(aes(month, participants, color=group, group=group))+
  geom_point()+
  geom_line()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        # axis.text.x = element_text(angle=90)
        )+
  scale_y_continuous(limits = c(0, NA))+
  # scale_x_date(date_breaks = "1 month", date_labels = "%m %Y")+
  labs(x="Month",
       y="Participants",
       color="",
       title="Distribution of participants captured in the GP dataset but not the Dispensing dataset in each month",
       caption=str_wrap("Points depict monthly participant counts. Participants with no records in the Dispensing dataset at any time point (red) are expected to represent lack of coverage, whereas those with a record at any time point in the Dispensing dataset (blue) represent non-adherence (i.e. having a record of prescribing but not of dispensing).Timeseries restricted to June 2018 to May 2021", 200))


ggsave("Outputs/Figures/baseline_drugs_recovery/participants_gp_not_dispensing_timeseries_bnf_chapters.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")










rm(bnf_chapters, bnf_chapter_labels)




# 7. Drug groups of interest ---------

## Generate summary table --------





### GP dataset ------

#### counts and quantity prescribed ----

gp_dt%>%
  inner_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  mutate(value1_prescription=if_else(gp_system_supplier=="TPP", value2_prescription, value1_prescription))%>%
  group_by(codelist)%>%
  summarise(Entries=n(),
            Participants = n_distinct(study_number),
            Participants_proportion = round(n_distinct(study_number)/participants_total*100, 1),
            Average_entries_per_participant = round(n()/n_distinct(study_number),1),
            Value1_prescription_median = median(value1_prescription, na.rm = T))%>%
  as_tibble()->x

x%<>%
  left_join(
gp_dt%>%
  inner_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  distinct(study_number, date, code, codelist)%>%
  group_by(codelist)%>%
  summarise(entries_no_duplicates = n())%>%
  as_tibble()
  )
  


#### interval between entries ------
gp_dt%>%  
  inner_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  as_tibble()%>%
  group_by(codelist, study_number)%>%
  filter(n()>1)%>%
  arrange(date)%>%
  summarise(average=as.numeric(median(diff(date)), na.rm = T))%>%
  group_by(codelist)%>%
  summarise(avg_day_interval=round(median(average,  na.rm = T), digits=0))->x1

#### entries per month -------
gp_dt%>%  
  inner_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  group_by(codelist, study_number, month)%>%
  summarise(entries_per_month = n())%>%
  group_by(codelist)%>%
  summarise(avg_entries_per_month=round(median(entries_per_month,  na.rm = T), digits=0))%>%
  as_tibble()->x2

#### aggregation -----

t1<-x%>%
  left_join(x1, by=c("codelist"))%>%
  left_join(x2, by=c("codelist"))

t1%<>%
  right_join(codelist_labels, by=c("codelist"))

t1%<>%mutate(Dataset="GP")

t1%<>%rename(Average_quantity_prescribed = Value1_prescription_median)

rm(x, x1, x2)







### Meds dataset ------

##### general counts and quantity ----

meds_dt%>%
  inner_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  rename(month=processing_period_date)%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  group_by(codelist)%>%
  summarise(Entries=n(),
            Participants = n_distinct(study_number),
            Participants_proportion = round(n_distinct(study_number)/participants_total_meds*100, 1),
            Average_entries_per_participant = round(n()/n_distinct(study_number),1),
            Average_quantity_prescribed = median(prescribed_quantity, na.rm = T))%>%
  as_tibble()->x

#### day difference between consecutive entries -----
meds_dt%>%  
  inner_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  rename(month=processing_period_date)%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  group_by(codelist, study_number)%>%
  filter(n()>1)%>%
  arrange(month)%>%
  summarise(average=as.numeric(median(diff(month)), na.rm = T))%>%
  group_by(codelist)%>%
  summarise(avg_day_interval=round(median(average,  na.rm = T), digits=0))%>%
  as_tibble()->x1

#### entries per event -----
meds_dt%>%  
  inner_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  rename(month=processing_period_date)%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  group_by(codelist, study_number, month)%>%
  summarise(entries_per_prescription = n())%>%
  group_by(codelist)%>%
  summarise(avg_entries_per_month=round(median(entries_per_prescription,  na.rm = T), digits=0))%>%
  as_tibble()->x2


#### aggregation =====

t2<-x%>%
  left_join(x1, by=c("codelist"))%>%
  left_join(x2, by=c("codelist"))

t2%<>%
  right_join(codelist_labels, by=c("codelist"))

t2%<>%mutate(Dataset="Dispensing")

# rm(x, x1, x2)



### aggregate summaries from both datasets ------
joint_table<-rbind(t1, t2%>%mutate(entries_no_duplicates = NA))

# rm(t1, t2)

joint_table%<>%
  select(Dataset, labels, codelist, group, Entries, Participants, entries_no_duplicates, Participants_proportion, avg_entries_per_month, Average_entries_per_participant, Average_quantity_prescribed, avg_day_interval)

joint_table%<>%
  mutate(across(c(Entries, Participants, Participants_proportion), ~replace_na(.,0)))%>%
  mutate(across(c(avg_entries_per_month, Average_entries_per_participant, Average_quantity_prescribed, avg_day_interval), ~replace_na(.,"N/A")))


write_csv(joint_table, "Outputs/Tables/summary_table_drug_groups.csv")

# joint_table%>%
#   mutate(Participants = paste0(Participants, " (", Participants_proportion, "%)"))%>%
#   select(-codelist, - Participants_proportion)%>%
#   rename(
#     'Entries per month' = avg_entries_per_month,
#     'Entries per participant'= Average_entries_per_participant,
#     'Quantity prescribed/dispensed' = Average_quantity_prescribed,
#     'Interval between entries (in days)' = avg_day_interval)%>%
#   mutate(across(everything(), as.character))%>%
#   pivot_longer(-c(labels, Dataset, group), names_to="key", values_to="value")->joint_table_pivotted
# 
style_list = list(cell_text(align = "center"))
# 
# joint_table_pivotted%>%
#   mutate(key= fct_relevel(key,"Participants", "Entries", "Entries per participant", "Entries per month", 'Interval between entries (in days)', "Quantity prescribed/dispensed"))%>%
#   mutate(Dataset= fct_relevel(Dataset,"GP", "Dispensing"))%>%
#   mmtable(cells=value, table_name="General summary features (cardiovascular drugs")+
#   header_top_left(Dataset)+
#   header_top(key)+
#   header_left(labels)+
#   header_format(Dataset, style_list)+
#   header_format(key, style_list)+
#   cells_format(cell_predicate = T, style_list)+
#   header_merged_cols()



# some investigation of nebulisers (in dispensing)

# codelists_snomed%>%
#   filter(codelist=="nebulisers_asthma_copd")%>%
#   select(ConceptId)%>%
#   .[[1]]->nebulisers_snomed
# 
# codelists_bnf%>%
#   filter(codelist=="nebulisers_asthma_copd")%>%
#   select(code)%>%
#   .[[1]]->nebulisers_bnf
# 
# meds_dt%>%
#   filter(prescribed_bnf_code %in% nebulisers_bnf)%>%
#   arrange(study_number, processing_period_date, bsa_prescription_id, item_id)%>%
#   group_by(bsa_prescription_id)%>%
#   filter(n()>2)%>%
#   as_tibble()%>%
#   View()
# 
# 
# meds_dt%>%  
#   filter(codelist=="nebulisers_asthma_copd")%>%
#   group_by(codelist, study_number, processing_period_date)%>%
#   summarise(entries_per_prescription = n())%>%
#   as_tibble()%>%
#   View()
# 
# meds_dt%>%
#   filter(codelist=="nebulisers_asthma_copd",
#     study_number=="1044975")%>%
#   as_tibble()%>%
#   view()



## visualization ----------
### counts --------

#### main drugs ----------------
p1<-
  joint_table%>%
  #mutate(Dataset=reorder(Dataset, desc(Dataset)))%>%
  mutate(labels=reorder(labels, labels))%>%
  filter(codelist %in% drugs_paper)%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=4.5, position = position_dodge(width = 1.1), vjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  scale_y_continuous(expand=expansion(c(0,0.3)))

p2<-
  joint_table%>%
  #mutate(Dataset=reorder(Dataset, desc(Dataset)))%>%
  mutate(labels=reorder(labels, labels))%>%
  filter(codelist %in% drugs_paper)%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=paste0(Participants, "\n(", Participants_proportion, "%)")), size=4.5, position = position_dodge(width = .9), vjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=30, hjust=1),
        axis.title.x = element_blank())+
  scale_y_continuous(expand=expansion(c(0,0.4)))

p<-p1/p2+plot_annotation(title="Participant and entry counts per drug group",
                         subtitle="For the period between June 2018 and May 2021")&
  theme(plot.title = element_text(size=30),
        plot.subtitle = element_text(size=20))



ggsave("Outputs/Figures/baseline_drugs_recovery/counts_vertical.png",
       last_plot(),
       width=25,
       height=10,
       dpi="retina")

rm(p1,p2,p)



##### focus on no duplicate entries ------


p1<-joint_table%>%
  filter(codelist %in% drugs_paper)%>%
  select(labels, Entries, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none")+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(title="Entry counts in both datasets",
       subtitle = "Counting all entries in the GP dataset",
       x="Drug group")


p2<-joint_table%>%
  filter(codelist %in% drugs_paper)%>%
  select(labels, Entries, entries_no_duplicates, Dataset)%>%
  mutate(Entries=ifelse(Dataset=="GP", entries_no_duplicates, Entries))%>%
  mutate(labels = reorder(labels, Entries))%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "right",
        axis.title.y=element_blank(),
        axis.text.y=element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(       subtitle = "Excluding duplicate entries in the GP dataset",
              caption="Duplicate defined as an entry with the same date and same code for a given participant. All calculations performed for the period between June 2018 and May 2021")


p<-p1+p2

p

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_no_duplicates.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")



#### additional drugs ----------------
p1<-
  joint_table%>%
  #mutate(Dataset=reorder(Dataset, desc(Dataset)))%>%
  mutate(labels=reorder(labels, labels))%>%
  filter(!codelist %in% drugs_paper)%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=3, position = position_dodge(width = 1), vjust=-.1)+
  theme_gray(base_size=15)+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  scale_y_continuous(expand=expansion(c(0,0.3)))

p2<-
  joint_table%>%
  #mutate(Dataset=reorder(Dataset, desc(Dataset)))%>%
  mutate(labels=reorder(labels, labels))%>%
  filter(!codelist %in% drugs_paper)%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=paste0(Participants, "\n(", Participants_proportion, "%)")), size=3, position = position_dodge(width = 1), vjust=-.1)+
  theme_gray(base_size=15)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=30, hjust=1, size=10),
        axis.title.x = element_blank())+
  scale_y_continuous(expand=expansion(c(0,0.4)))

p<-p1/p2+plot_annotation(title="Participant and entry counts per drug group (for additional drugs)",
                         subtitle="For the period between June 2018 and May 2021")&
  theme(plot.title = element_text(size=30),
        plot.subtitle = element_text(size=20))



ggsave("Outputs/Figures/baseline_drugs_recovery/counts_vertical_additional_drugs.png",
       last_plot(),
       width=25,
       height=10,
       dpi="retina")




##### focus on no duplicate entries ------


p1<-joint_table%>%
  filter(!codelist %in% drugs_paper)%>%
  select(labels, Entries, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none")+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(title="Entry counts in both datasets  (additional drugs)",
       subtitle = "Counting all entries in the GP dataset",
       x="Drug group")


p2<-joint_table%>%
  filter(!codelist %in% drugs_paper)%>%
  select(labels, Entries, entries_no_duplicates, Dataset)%>%
  mutate(Entries=ifelse(Dataset=="GP", entries_no_duplicates, Entries))%>%
  mutate(labels = reorder(labels, Entries))%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "right",
        axis.title.y=element_blank(),
        axis.text.y=element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(       subtitle = "Excluding duplicate entries in the GP dataset",
              caption="Duplicate defined as an entry with the same date and same code for a given participant")


p<-p1+p2

p

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_no_duplicates_additional_drugs.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")



### visualization: counts  ----

#### CV drugs -----

##### plot ------
p1<-
  joint_table%>%
  filter(group=="cv")%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.y=element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))

p2<-
  joint_table%>%
  filter(group=="cv")%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=paste0(Participants, " (", Participants_proportion, "%)")), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "right",
        axis.title.y=element_blank(),
        axis.text.y = element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.4)))

p<-p1+p2+plot_annotation(title="Recording frequency for cardiovascular drugs")&
  theme(plot.title = element_text(size=30))

p


ggsave("Outputs/Figures/baseline_drugs_recovery/counts_cv_drugs.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")

rm(p1,p2,p)


##### focus on no duplicate entries ------


p1<-joint_table%>%
  filter(group=="cv")%>%
  select(labels, Entries, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none")+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(title="Entry counts for cardiovascular drugs in both datasets",
    subtitle = "Counting all entries in the GP dataset",
    x="Drug group")


p2<-joint_table%>%
  filter(group=="cv")%>%
  select(labels, Entries, entries_no_duplicates, Dataset)%>%
  mutate(Entries=ifelse(Dataset=="GP", entries_no_duplicates, Entries))%>%
  mutate(labels = reorder(labels, Entries))%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "right",
        axis.title.y=element_blank(),
        axis.text.y=element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(       subtitle = "Excluding duplicate entries in the GP dataset",
              caption="Duplicate defined as an entry with the same date and same code for a given participant")


p<-p1+p2

p

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_cv_drugs_no_duplicates.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")







# vertical bars (alternative plot)


# p1<-
#   joint_table%>%
#   filter(group=="cv")%>%
#   select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
#   mutate(labels = reorder(labels, Entries))%>%
#   ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
#   geom_bar(stat='identity', position='dodge')+
#   geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
#   theme_gray(base_size=20)+
#   theme(legend.position = "none",
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank())+
#   scale_y_continuous(expand=expansion(c(0,0.3)))
# 
# p2<-
#   joint_table%>%
#   filter(group=="cv")%>%
#   select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
#   mutate(labels = reorder(labels, Entries))%>%
#   ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
#   geom_bar(stat='identity', position='dodge')+
#   geom_text(aes(label=paste0(Participants, " (", Participants_proportion, "%)")), size=5, position = position_dodge(width = .9), hjust=-.1)+
#   theme_gray(base_size=20)+
#   theme(legend.position = "bottom")+
#   scale_y_continuous(expand=expansion(c(0,0.4)))
# 
# p<-p1/p2+plot_annotation(title="Frequency of drug groups of interest (cardiovascular)")&
#   theme(plot.title = element_text(size=30))
# 
# 
# 
# ggsave("Outputs/Figures/counts_cv_drugs_vertical.png",
#        last_plot(),
#        width=25,
#        height=15,
#        dpi="retina")


## table 
joint_table%>%
  filter(group=="cv", Dataset=="GP")%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  mutate(Participants = paste0(Participants, " (", Participants_proportion, "%)"),
         Entries=as.character(Entries))%>%
  select(-Participants_proportion, -Dataset)%>%
  
left_join(
  joint_table%>%
    filter(group=="cv", Dataset=="Dispensing")%>%
    select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
    mutate(Participants = paste0(Participants, " (", Participants_proportion, "%)"),
           Entries=as.character(Entries))%>%
    select(-Participants_proportion, -Dataset),
  by=c("labels"))%>%
rename(Entries_GP=Entries.x,
       Entries_Dispensing=Entries.y,
       Participants_GP = Participants.x,
       Participants_Dispensing = Participants.y)%>%
  flextable()%>%
  add_header_row(values = c("","GP dataset", "Dispensing dataset"), colwidths = c(1,2, 2))%>%
  set_header_labels(values=list(labels="Drug group", 
                                Entries_GP="Entries", 
                                Participants_GP = "Participants (%)",
                                Entries_Dispensing = "Entries", 
                                Participants_Dispensing = "Participants (%)"))%>%
  set_caption("Entry and participant counts for cardiovascular drugs, per dataset")%>%
  width(j=1, width=4)%>%
  width(j=2:5, width=1.5)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=2:5, align="center", part="all")->table
  
table

save_as_docx(table, path = "Outputs/Tables/counts_cv_table.docx", pr_section = sect_properties)



rm(table)








#### Respiratory drugs -----

##### plot -----
p1<-
  joint_table%>%
  filter(group=="resp")%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.y=element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))

p2<-
  joint_table%>%
  filter(group=="resp")%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=paste0(Participants, " (", Participants_proportion, "%)")), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "right",
        axis.title.y=element_blank(),
        axis.text.y = element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.4)))

p<-p1+p2+plot_annotation(title="Frequency of drug groups of interest (respiratory)")&
  theme(plot.title = element_text(size=30))


ggsave("Outputs/Figures/baseline_drugs_recovery/counts_resp_drugs.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")



##### focus on no duplicate entries-----


p1<-joint_table%>%
  filter(group=="resp")%>%
  select(labels, Entries, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none")+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(title="Entry counts for respiratory drugs in both datasets",
       subtitle = "Counting all entries in the GP dataset",
       x="Drug group")


p2<-joint_table%>%
  filter(group=="resp")%>%
  select(labels, Entries, entries_no_duplicates, Dataset)%>%
  mutate(Entries=ifelse(Dataset=="GP", entries_no_duplicates, Entries))%>%
  mutate(labels = reorder(labels, Entries))%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "right",
        axis.title.y=element_blank(),
        axis.text.y=element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(       subtitle = "Excluding duplicate entries in the GP dataset",
              caption="Duplicate defined as an entry with the same date and same code for a given participant")


p<-p1+p2

p

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_resp_drugs_no_duplicates.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")














#### Other drugs -----

##### plot -----
p1<-
  joint_table%>%
  filter(group=="oth")%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.y=element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))

p2<-
  joint_table%>%
  filter(group=="oth")%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  mutate(labels = reorder(labels, Entries))%>%
  ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=paste0(Participants, " (", Participants_proportion, "%)")), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "right",
        axis.title.y=element_blank(),
        axis.text.y = element_blank())+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.4)))

p<-p1+p2+plot_annotation(title="Frequency of drug groups of interest (others)")&
  theme(plot.title = element_text(size=30))


ggsave("Outputs/Figures/baseline_drugs_recovery/counts_other_drugs.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")


rm(p,p1,p2)


##### focus on no duplicate entries ------


p1<-joint_table%>%
  ungroup()%>%
  filter(group=="oth")%>%
  select(labels, Entries, Dataset)%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none")+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(title="Entry counts for other drugs in both datasets",
       subtitle = "Counting all entries in the GP dataset",
       x="Drug group")


p2<-joint_table%>%
  ungroup()%>%
  filter(group=="oth")%>%
  select(labels, Entries, entries_no_duplicates, Dataset)%>%
  mutate(Entries=as.numeric(ifelse(Dataset=="GP", entries_no_duplicates, Entries)))%>%
  mutate(Entries=replace_na(Entries,0))%>%
  mutate(labels=fct_reorder(labels, desc(Entries)))%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=5, position = position_dodge(width = .9), hjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "right",
        axis.title.y=element_blank(),
        axis.text.y=element_blank()
        )+
  coord_flip()+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  scale_x_discrete(limits=rev)+
  labs(       subtitle = "Excluding duplicate entries in the GP dataset",
              caption="Duplicate defined as an entry with the same date and same code for a given participant")


p<-p1+p2

p

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_oth_drugs_no_duplicates.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")
























### timeseries (entries) -----------

#### main drugs ----------------------

gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(codelist %in% drugs_paper)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(codelist %in% drugs_paper)%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  ggplot(aes(month, Entries, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly entry counts per drug group",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated entry counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_entries_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

#### additional drugs -------


gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(!codelist %in% drugs_paper)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(!codelist %in% drugs_paper)%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  ggplot(aes(month, Entries, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly entry counts per drug group (additional drugs)",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated entry counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_entries_additional_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")























#### CV drugs --------------
gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="cv"])%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="cv"])%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  ggplot(aes(month, Entries, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly entry counts per drug group (cardiovascular drugs)",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated entry counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_cv.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")



#### Resp drugs --------------
gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="resp"])%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="resp"])%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  ggplot(aes(month, Entries, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly entry counts per drug group (respiratory drugs)",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated entry counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_resp.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")






#### Other drugs --------------


gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="oth"])%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="oth"])%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  ggplot(aes(month, Entries, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly entry counts per drug group (other drugs)",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated entry counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_other.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


### timeseries (participants) -----------


#### main drugs --------------------

gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(codelist %in% drugs_paper)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(codelist %in% drugs_paper)%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  ggplot(aes(month, Participants, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly participant counts per drug group",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated participant counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_participants_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

















#### additional drugs ---------------------




gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(!codelist %in% drugs_paper)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(!codelist %in% drugs_paper)%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  ggplot(aes(month, Participants, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly participant counts per drug group (additional drugs)",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated participant counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_participants_additional_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")






























#### CV drugs --------------
gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="cv"])%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="cv"])%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  ggplot(aes(month, Participants, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly participant counts per drug group (cardiovascular drugs)",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated participant counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_cv_participants.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")



#### Resp drugs --------------
gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="resp"])%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="resp"])%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  ggplot(aes(month, Participants, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly participant counts per drug group (respiratory drugs)",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated participant counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_resp_participants.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")






#### Other drugs --------------


gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="oth"])%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  filter(codelist %in% codelist_labels$codelist[codelist_labels$group=="oth"])%>%
  rename(month=processing_period_date)%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x2

t<-rbind(x1, x2)


t%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
  ggplot(aes(month, Participants, color=dataset, group = dataset))+
  geom_line()+
  geom_point()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  labs(title="Timeseries of monthly participant counts per drug group (other drugs)",
       subtitle="Per dataset",
       x="Month",
       color="Dataset",
       caption=str_wrap("Each point represents aggregated participant counts per month. Note that y-axes differ in each facet. Timeseries restricted to the period between June 2018 and May 2021", 200))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        strip.text = element_text(size=20))+
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), na.rm=T, linetype="dashed")+
  ylim(0,NA)



ggsave("Outputs/Figures/baseline_drugs_recovery/timeseries_other_participant.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")





## agreement ----


gp_dt%>%
  select(study_number, code, date, row_number)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  filter(month>= "2018-06-01",
         month<= "2021-05-01")%>%
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>%
  distinct(study_number, codelist)%>%
  mutate(dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  rename(month=processing_period_date)%>%  
  filter(month>= "2018-06-01",
         month<= "2021-05-01")%>%  
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  distinct(study_number,codelist)%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()->x2

t<-rbind(x1, x2)

rm(x1,x2)

t%>%distinct(study_number)%>%nrow()->participants

t%>%distinct(study_number)->participants_list


t%<>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, codelist), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, codelist)%>%
  mutate(study_number=as.numeric(study_number))


#### calculate agreement ----

drug_groups <- t%>%distinct(codelist)%>%.[[1]]

kappa_codelists_participants<-list()

for (i in drug_groups) {
  
  t%>%
    filter(codelist==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, "0")))%>%  
    select(-codelist, -study_number)%>%
    mutate(across(everything(), as.integer))->table
  
  try(k<-Kappa.test(table$GP, table$Dispensing))
  
  if(exists("k"))
  {
    
    kappa_codelists_participants[[i]]<-k
    
  }
  else{
    kappa_codelists_participants[[i]]<-list()
  }
  
  rm(i, table, k)
  
}

kappa_codelists_participants_results<-data.frame()

for (i in drug_groups) {
  
  name<-i
  
  try(
    table<-(data.frame(
      codelist = name,
      kappa = unlist(kappa_codelists_participants[[i]]$Result$estimate),
      CI_lower = unlist(kappa_codelists_participants[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_codelists_participants[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_codelists_participants[[i]]$Result$p.value),
      judgement = unlist(kappa_codelists_participants[[i]]$Judgement)
    ))
  )
  
  try(kappa_codelists_participants_results%<>%bind_rows(table))
  
  
  rm(table, name, i)
  
}


kappa_codelists_participants_results%>%
  right_join(codelist_labels)%>%
  mutate_when(!is.na(CI_lower), 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate_when(p_value==0.00,
              p_value="<0.001")%>%
  
  left_join (
    (t%>%
       group_by(codelist)%>%
       summarise(GP_Dispensing = n_unique(study_number[GP=="1" & Dispensing=="1"]),
                 GP_only = n_unique(study_number[GP=="1" & Dispensing=="0"]),
                 Dispensing_only = n_unique(study_number[GP=="0" & Dispensing=="1"]),
                 nil = participants - GP_Dispensing - GP_only - Dispensing_only)),
    by=c("codelist"))%>%
  arrange(labels)%>%
  select(Label=labels,
         GP_Dispensing,
         GP_only,
         Dispensing_only,
         nil,
         kappa,
         CI,
         p_value,
         judgement,
         codelist)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))-> codelists_agreement_participants



codelists_agreement_participants%<>%
  mutate(across(3:6, ~replace_na(.x, 0)))%>%
  rowwise() %>%
  mutate(prop = round(sum(across(2:4), na.rm = T)/sum(across(2:5), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either dataset (%)' = paste0(sum(across(2:4), na.rm = T),
                                       " (",
                                       prop,
                                       "%)"))%>%
  mutate(prop1 = round(sum(across(2:2))/sum(across(2:5), na.rm = T)*100,1),
         prop2 = round(sum(across(3:3))/sum(across(2:5), na.rm = T)*100,2),
         prop3 = round(sum(across(4:4))/sum(across(2:5), na.rm = T)*100,1),
         prop4 = round(sum(across(5:5))/sum(across(2:5), na.rm = T)*100,1))%>%
  mutate(prop1=if_else(is.na(prop1), 0, prop1))%>%
  mutate(prop2=if_else(is.na(prop2), 0, prop2))%>%
  mutate(prop3=if_else(is.na(prop3), 0, prop3))%>%
  mutate(prop4=if_else(is.na(prop4), 0, prop4))%>%
  mutate(GP_Dispensing=paste0(GP_Dispensing, " (", prop1, "%)"),
         GP_only=paste0(GP_only, " (", prop2, "%)"),
         Dispensing_only=paste0(Dispensing_only, " (", prop3, "%)"),
         nil=paste0(nil, " (", prop4, "%)"))%>%
  select(-c(prop, prop1, prop2, prop3, prop4))%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%
  rename('Drug group' = Label,
         K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement)%>%
  relocate('Either dataset (%)', .after = 'Drug group')




#### table (flextable) -----


##### main drugs ------------------------



codelists_agreement_participants%>%
  filter(codelist %in% drugs_paper)%>%
  select(-codelist)%>%
  rename("Participants in either dataset (%)" = "Either dataset (%)",
         'Participants in both datasets (%)' = GP_Dispensing,
         'Participants in GP dataset only (%)' = GP_only,
         'Participants in Dispensing dataset only (%)'=Dispensing_only,
         'Participants in neither dataset (%)'=nil)%>%
  flextable()%>%
  set_caption("Agreement between the GP and Dispensing dataset for presence of a record at any time point")%>%
  width(j=1, width=4)%>%
  width(j=2:10, width=1)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=2:10, align="center", part="all")->table_flex

save_as_docx(table_flex, path = "Outputs/Tables/agreement_main_drugs.docx", pr_section = sect_properties)




##### additional drugs -------------------------

codelists_agreement_participants%>%
  filter(!codelist %in% drugs_paper)%>%
  select(-codelist)%>%
  rename("Participants in either dataset (%)" = "Either dataset (%)",
         'Participants in both datasets (%)' = GP_Dispensing,
         'Participants in GP dataset only (%)' = GP_only,
         'Participants in Dispensing dataset only (%)'=Dispensing_only,
         'Participants in neither dataset (%)'=nil)%>%
  flextable()%>%
  set_caption("Agreement between the GP and Dispensing dataset for presence of a record at any time point (additional drugs)")%>%
  width(j=1, width=4)%>%
  width(j=2:10, width=1)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=2:10, align="center", part="all")->table_flex

save_as_docx(table_flex, path = "Outputs/Tables/agreement_additional_drugs.docx", pr_section = sect_properties)
































##### cv ------

codelists_agreement_participants%>%
  filter(group=="cv")%>%
  select(-group)%>%
  flextable()%>%
  set_caption("Agreement between the GP and Dispensing dataset for cardiovascular drugs")%>%
  width(j=1, width=4)%>%
  width(j=2:9, width=1)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=2:9, align="center", part="all")->table_flex

save_as_docx(table_flex, path = "Outputs/Tables/agreement_cv_drugs.docx", pr_section = sect_properties)


##### resp ------

codelists_agreement_participants%>%
  filter(group=="resp")%>%
  select(-group)%>%
  flextable()%>%
  set_caption("Agreement between the GP and Dispensing dataset for respiratory drugs")%>%
  width(j=1, width=4)%>%
  width(j=2:9, width=1)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=2:9, align="center", part="all")->table_flex

save_as_docx(table_flex, path = "Outputs/Tables/agreement_resp_drugs.docx", pr_section = sect_properties)


##### other ------

codelists_agreement_participants%>%
  filter(group=="oth")%>%
  select(-group)%>%
  flextable()%>%
  set_caption("Agreement between the GP and Dispensing dataset for other drugs")%>%
  width(j=1, width=4)%>%
  width(j=2:9, width=1)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=2:9, align="center", part="all")->table_flex

save_as_docx(table_flex, path = "Outputs/Tables/agreement_oth_drugs.docx", pr_section = sect_properties)













#### table (formattable) -----

##### cv -------

codelists_agreement_participants%>%
  filter(group=="cv")%>%
  select(-group)%>% 
  formattable(
    align=c("l"),
    list(
      area(col=2:5) ~color_tile("transparent", "pink"),
      area(row=c(codelists_agreement_participants%>%
                   filter(group=="cv")%>%
                   mutate(row=row.names(.))%>%
                   filter(!is.na(K))%>%
                   select(row)%>%
                   .[[1]]),
           col=K) ~color_bar("lightblue")))->table

table # need to printscreen this from the browser to save table


##### resp -------

codelists_agreement_participants%>%
  filter(group=="resp")%>%
  select(-group)%>% 
  formattable(
    align=c("l"),
    list(
      area(col=2:5) ~color_tile("transparent", "pink"),
      area(row=c(codelists_agreement_participants%>%
                   filter(group=="resp")%>%
                   mutate(row=row.names(.))%>%
                   filter(!is.na(K))%>%
                   select(row)%>%
                   .[[1]]),
           col=K) ~color_bar("lightblue")))->table

table # need to printscreen this from the browser to save table



##### other ------

codelists_agreement_participants%>%
  filter(group=="oth")%>%
  select(-group)%>% 
  formattable(
    align=c("l"),
    list(
      area(col=2:5) ~color_tile("transparent", "pink"),
      area(row=c(codelists_agreement_participants%>%
                   filter(group=="oth")%>%
                   mutate(row=row.names(.))%>%
                   filter(!is.na(K))%>%
                   select(row)%>%
                   .[[1]]),
           col=K) ~color_bar("lightblue")))->table

table # need to printscreen this from the browser to save table


























rm(bnf_agreement_participants, kappa_bnf_participants, kappa_bnf_participants_results, participants, participants_list, table_flex)




#### plots ----------

gp_dt%>%
  select(study_number, code, date, row_number)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  filter(month>= "2018-06-01",
         month<= "2021-05-01")%>%
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>%
  distinct(study_number, codelist)%>%
  mutate(dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  rename(month=processing_period_date)%>%  
  filter(month>= "2018-06-01",
         month<= "2021-05-01")%>%  
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  distinct(study_number,codelist)%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()->x2

t<-rbind(x1, x2)

rm(x1,x2)

t%>%distinct(study_number)%>%nrow()->participants

t%>%distinct(study_number)->participants_list


t%<>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, codelist), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, codelist)%>%
  mutate(study_number=as.numeric(study_number))


t%<>%
  mutate(group = as.factor(case_when(GP=="1" & Dispensing=="1" ~ "Both",
                                     GP=="1" & Dispensing=="0" ~ "GP",
                                     GP=="0" & Dispensing=="1" ~ "Dispensing")))%>%
  group_by(codelist, group)%>%
  summarise(participants = n())%>%
  pivot_wider(c(codelist), names_from = "group", values_from = "participants")%>%
  mutate(Either=sum(GP, Dispensing, Both, na.rm = T))%>%
  pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(codelist_labels)%>%
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
  ungroup()




##### cv ------

t%>%
  filter(group=="cv")%>%
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.9)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(color="Dataset",
       title="Number of participants captured in each dataset (cardiovascular drugs)",
       caption="Proportions presented are in reference to the number of participants identified using either dataset for each group. Calculations restricted to the time period between June 2018 and May 2021"
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_cv.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


##### resp ------

t%>%
  filter(group=="resp")%>%
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.3)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(color="Dataset",
       title="Number of participants captured in each dataset (respiratory drugs)",
       caption="Proportions presented are in reference to the number of participants identified using either dataset for each group. Calculations restricted to the time period between June 2018 and May 2021"
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_resp.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

##### other ------

t%>%
  filter(group=="oth")%>%
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.3)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(color="Dataset",
       title="Number of participants captured in each dataset (other drugs)",
       caption="Proportions presented are in reference to the number of participants identified using either dataset for each group. Calculations restricted to the time period between June 2018 and May 2021"
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_other.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")







### unique codes --------

#### general counting -------

gp_dt%>%
  inner_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  group_by(study_number, codelist)%>%
  summarise(distinct_snomed_codes = n_distinct(code))%>%
  left_join(codelist_labels)%>%
  mutate(Dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  inner_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  group_by(study_number, codelist)%>%
  summarise(distinct_bnf_codes = n_distinct(prescribed_bnf_code),
            distinct_snomed_codes = n_distinct(prescribeddmd_code))%>%
  left_join(codelist_labels)%>%
  mutate(Dataset="Dispensing")%>%
  as_tibble()->x2

x1%<>%
  rename(snomed_gp = distinct_snomed_codes)%>%
  select(-Dataset)

x1%<>%
  pivot_longer(snomed_gp, names_to = "key", values_to="value")

x2%<>%
  rename(snomed_dispensing = distinct_snomed_codes,
         bnf_dispensing = distinct_bnf_codes)%>%
  select(-Dataset)

x2%<>%
  pivot_longer(c(snomed_dispensing,bnf_dispensing), names_to = "key", values_to="value")

t<-rbind(x1, x2)

t%<>%pivot_wider(names_from=key, values_from = value)



##### plots-----

##### main drugs -----------

t%>%
  filter(codelist %in% drugs_paper)%>%
  pivot_longer(-c(study_number, codelist, labels, group), names_to="key", values_to = "value")%>%
  ggplot(aes(value, fill=key))+
  geom_boxplot(aes(y=key), alpha=0.5)+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  labs(title="Boxplots depicting number of distinct codes per participant",
       subtitle="SNOMED codes in the GP dataset, and SNOMED and BNF codes in the Dispensing dataset",
       y="",
       x="Number of distinct codes",
       fill="",
       caption = "Plots capped at 15. 
       Boxplots depict quartile 1, quartile 2(median), and quartile 3, with whiskers spanning to 1.5 times the interquartile range either below quartile 1 or above quartile 3. 
       Points depict outliers beyond these margins
       ")+
  scale_fill_discrete(limits= c("bnf_dispensing", "snomed_dispensing", "snomed_gp"),
                      labels = c("BNF codes (Dispensing dataset)", "SNOMED codes (Dispensing dataset)", "SNOMED codes (GP dataset)"))+
  scale_x_continuous(breaks=seq(1,15, by=2), 
                     limits=c(1,15)
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/distinct_codes_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")



##### additional drugs --------

t%>%
  filter(!codelist %in% drugs_paper)%>%
  pivot_longer(-c(study_number, codelist, labels, group), names_to="key", values_to = "value")%>%
  ggplot(aes(value, fill=key))+
  geom_boxplot(aes(y=key), alpha=0.5)+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  labs(title="Boxplots depicting number of distinct codes per participant",
       subtitle="SNOMED codes in the GP dataset, and SNOMED and BNF codes in the Dispensing dataset",
       y="",
       x="Number of distinct codes",
       fill="",
       caption = "Plots capped at 15. 
       Boxplots depict quartile 1, quartile 2(median), and quartile 3, with whiskers spanning to 1.5 times the interquartile range either below quartile 1 or above quartile 3. 
       Points depict outliers beyond these margins
       ")+
  scale_fill_discrete(limits= c("bnf_dispensing", "snomed_dispensing", "snomed_gp"),
                      labels = c("BNF codes (Dispensing dataset)", "SNOMED codes (Dispensing dataset)", "SNOMED codes (GP dataset)"))+
  scale_x_continuous(breaks=seq(1,15, by=2), 
                     limits=c(1,15)
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/distinct_codes_additional_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")




































##### CV -----
t%>%
  filter(group=="cv")%>%
  pivot_longer(-c(study_number, codelist, labels, group), names_to="key", values_to = "value")%>%
  ggplot(aes(value, fill=key))+
  geom_boxplot(aes(y=key), alpha=0.5)+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  labs(title="Boxplots depicting number of distinct codes per participant, for cardiovascular drugs",
       subtitle="SNOMED codes in the GP dataset, and SNOMED and BNF codes in the Dispensing dataset",
       y="",
       x="Number of distinct codes",
       fill="",
       caption = "Plots capped at 15. 
       Boxplots depict quartile 1, quartile 2(median), and quartile 3, with whiskers spanning to 1.5 times the interquartile range either below quartile 1 or above quartile 3. 
       Points depict outliers beyond these margins
       ")+
  scale_fill_discrete(limits= c("bnf_dispensing", "snomed_dispensing", "snomed_gp"),
                      labels = c("BNF codes (Dispensing dataset)", "SNOMED codes (Dispensing dataset)", "SNOMED codes (GP dataset)"))+
  scale_x_continuous(breaks=seq(1,15, by=2), 
                     limits=c(1,15)
                     )

ggsave("Outputs/Figures/baseline_drugs_recovery/distinct_codes_cv_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

##### resp ------

t%>%
  filter(group=="resp")%>%
  pivot_longer(-c(study_number, codelist, labels, group), names_to="key", values_to = "value")%>%
  ggplot(aes(value, fill=key))+
  geom_boxplot(aes(y=key), alpha=0.5)+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  labs(title="Boxplots depicting number of distinct codes per participant, for respiratory drugs",
       subtitle="SNOMED codes in the GP dataset, and SNOMED and BNF codes in the Dispensing dataset",
       y="",
       x="Number of distinct codes",
       fill="",
       caption = "Plots capped at 15. 
       Boxplots depict quartile 1, quartile 2(median), and quartile 3, with whiskers spanning to 1.5 times the interquartile range either below quartile 1 or above quartile 3. 
       Points depict outliers beyond these margins
       ")+
  scale_fill_discrete(limits= c("bnf_dispensing", "snomed_dispensing", "snomed_gp"),
                      labels = c("BNF codes (Dispensing dataset)", "SNOMED codes (Dispensing dataset)", "SNOMED codes (GP dataset)"))+
  scale_x_continuous(breaks=seq(1,15, by=2), 
                     limits=c(1,15)
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/distinct_codes_resp_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")




##### other ------

t%>%
  filter(group=="oth")%>%
  pivot_longer(-c(study_number, codelist, labels, group), names_to="key", values_to = "value")%>%
  ggplot(aes(value, fill=key))+
  geom_boxplot(aes(y=key), alpha=0.5)+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  labs(title="Boxplots depicting number of distinct codes per participant, for other drugs",
       subtitle="SNOMED codes in the GP dataset, and SNOMED and BNF codes in the Dispensing dataset",
       y="",
       x="Number of distinct codes",
       fill="",
       caption = "Plots capped at 15. 
       Boxplots depict quartile 1, quartile 2(median), and quartile 3, with whiskers spanning to 1.5 times the interquartile range either below quartile 1 or above quartile 3. 
       Points depict outliers beyond these margins
       ")+
  scale_fill_discrete(limits= c("bnf_dispensing", "snomed_dispensing", "snomed_gp"),
                      labels = c("BNF codes (Dispensing dataset)", "SNOMED codes (Dispensing dataset)", "SNOMED codes (GP dataset)"))+
  scale_x_continuous(breaks=seq(1,15, by=2), 
                     limits=c(1,15)
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/distinct_codes_oth_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")






















####  tables------

##### cv ------

t%>%  
  pivot_longer(-c(study_number, codelist, labels, group), names_to="key", values_to = "value")%>%

  filter(group=="cv")%>%
  
  group_by(codelist, labels, key)%>%
  summarise(median=median(value, na.rm = T),
            Q1 = quantile(value, probs=0.25, na.rm=T),
            Q3 = quantile(value, probs=0.75, na.rm=T))%>%
  pivot_wider(id_cols=c(codelist, labels), names_from=key, values_from = c(median, Q1, Q3))%>%
  transmute(BNF_dispensing = paste0(median_bnf_dispensing, " (", Q1_bnf_dispensing, "-", Q3_bnf_dispensing, ")"),
            SNOMED_dispensing = paste0(median_snomed_dispensing, " (", Q1_snomed_dispensing, "-", Q3_snomed_dispensing, ")"),
            SNOMED_GP = paste0(median_snomed_gp, " (", Q1_snomed_gp, "-", Q3_snomed_gp, ")"))%>%
  mutate(SNOMED_GP =  if_else(str_detect(SNOMED_GP, pattern="NA"), "N/A", SNOMED_GP))%>%
  ungroup()%>%
  rename('GP dataset\n(SNOMED codes)\n \nMedian (IQR)' = SNOMED_GP,
         'Dispensing dataset\n(SNOMED codes)\n \nMedian (IQR)' = SNOMED_dispensing,
         'Dispensing dataset\n(BNF codes)\n \nMedian (IQR)' = BNF_dispensing,
         'Codelist' = labels)%>%
  select(-codelist)->table


set_flextable_defaults(
  font.size = 10, 
  theme_fun = theme_vanilla,
  padding = 6,
  text.align = "left",
  font.family = "Mulish")


table%<>%
  flextable()%>%
  set_caption("Average number of distinct SNOMED or BNF codes per participant (cardiovascular drugs)")%>%
  width(j=1, width=4)%>%
  width(j=2:4, width=2)%>%
  add_footer_lines("The GP dataset contains only SNOMED codes, whereas the Dispensing dataset contains both SNOMED and BNF codes.\nIQR - Interquartile range")%>%
  align(i=NULL, j=2:4, align="center", part="all")%>%
  flextable::font(fontname="Mulish")

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(table, path = "Outputs/Tables/unique_codes_cv_drugs_both_datasets.docx", pr_section = sect_properties)


rm(x1, x2, table)


##### resp -----

t%>%  
  pivot_longer(-c(study_number, codelist, labels, group), names_to="key", values_to = "value")%>%
  
  filter(group=="resp")%>%
  
  group_by(codelist, labels, key)%>%
  summarise(median=median(value, na.rm = T),
            Q1 = quantile(value, probs=0.25, na.rm=T),
            Q3 = quantile(value, probs=0.75, na.rm=T))%>%
  pivot_wider(id_cols=c(codelist, labels), names_from=key, values_from = c(median, Q1, Q3))%>%
  transmute(BNF_dispensing = paste0(median_bnf_dispensing, " (", Q1_bnf_dispensing, "-", Q3_bnf_dispensing, ")"),
            SNOMED_dispensing = paste0(median_snomed_dispensing, " (", Q1_snomed_dispensing, "-", Q3_snomed_dispensing, ")"),
            SNOMED_GP = paste0(median_snomed_gp, " (", Q1_snomed_gp, "-", Q3_snomed_gp, ")"))%>%
  mutate(SNOMED_GP =  if_else(str_detect(SNOMED_GP, pattern="NA"), "N/A", SNOMED_GP))%>%
  ungroup()%>%
  rename('GP dataset\n(SNOMED codes)\n \nMedian (IQR)' = SNOMED_GP,
         'Dispensing dataset\n(SNOMED codes)\n \nMedian (IQR)' = SNOMED_dispensing,
         'Dispensing dataset\n(BNF codes)\n \nMedian (IQR)' = BNF_dispensing,
         'Codelist' = labels)%>%
  select(-codelist)->table


set_flextable_defaults(
  font.size = 10, 
  theme_fun = theme_vanilla,
  padding = 6,
  text.align = "left",
  font.family = "Mulish")


table%<>%
  flextable()%>%
  set_caption("Average number of distinct SNOMED or BNF codes per participant (respiratory drugs)")%>%
  width(j=1, width=4)%>%
  width(j=2:4, width=2)%>%
  add_footer_lines("The GP dataset contains only SNOMED codes, whereas the Dispensing dataset contains both SNOMED and BNF codes.\nIQR - Interquartile range")%>%
  align(i=NULL, j=2:4, align="center", part="all")%>%
  flextable::font(fontname="Mulish")

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(table, path = "Outputs/Tables/unique_codes_resp_drugs_both_datasets.docx", pr_section = sect_properties)


rm(x1, x2, table)

##### other -----

t%>%  
  pivot_longer(-c(study_number, codelist, labels, group), names_to="key", values_to = "value")%>%
  
  filter(group=="oth")%>%
  
  group_by(codelist, labels, key)%>%
  summarise(median=median(value, na.rm = T),
            Q1 = quantile(value, probs=0.25, na.rm=T),
            Q3 = quantile(value, probs=0.75, na.rm=T))%>%
  pivot_wider(id_cols=c(codelist, labels), names_from=key, values_from = c(median, Q1, Q3))%>%
  transmute(BNF_dispensing = paste0(median_bnf_dispensing, " (", Q1_bnf_dispensing, "-", Q3_bnf_dispensing, ")"),
            SNOMED_dispensing = paste0(median_snomed_dispensing, " (", Q1_snomed_dispensing, "-", Q3_snomed_dispensing, ")"),
            SNOMED_GP = paste0(median_snomed_gp, " (", Q1_snomed_gp, "-", Q3_snomed_gp, ")"))%>%
  mutate(SNOMED_GP =  if_else(str_detect(SNOMED_GP, pattern="NA"), "N/A", SNOMED_GP))%>%
  ungroup()%>%
  rename('GP dataset\n(SNOMED codes)\n \nMedian (IQR)' = SNOMED_GP,
         'Dispensing dataset\n(SNOMED codes)\n \nMedian (IQR)' = SNOMED_dispensing,
         'Dispensing dataset\n(BNF codes)\n \nMedian (IQR)' = BNF_dispensing,
         'Codelist' = labels)%>%
  select(-codelist)->table


set_flextable_defaults(
  font.size = 10, 
  theme_fun = theme_vanilla,
  padding = 6,
  text.align = "left",
  font.family = "Mulish")


table%<>%
  flextable()%>%
  set_caption("Average number of distinct SNOMED or BNF codes per participant (other drugs)")%>%
  width(j=1, width=4)%>%
  width(j=2:4, width=2)%>%
  add_footer_lines("The GP dataset contains only SNOMED codes, whereas the Dispensing dataset contains both SNOMED and BNF codes.\nIQR - Interquartile range")%>%
  align(i=NULL, j=2:4, align="center", part="all")%>%
  flextable::font(fontname="Mulish")

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(table, path = "Outputs/Tables/unique_codes_oth_drugs_both_datasets.docx", pr_section = sect_properties)


rm(x1, x2, table)




### time intervals between entries ------------------

#### wrangling and calculations -----

gp_dt%>%
  select(study_number, date, code)%>%
  distinct(study_number, code, date, .keep_all=T)%>%
  group_by(study_number, code)%>%
  filter(n()>1)%>%
  arrange(date)%>%
  group_by(code, study_number)%>%
  summarise(average=round(as.numeric(median(diff(date))),0))%>%
  inner_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  mutate(dataset="GP")%>%
  select(code, study_number, average, codelist, dataset)%>%
  as_tibble()->p



meds_dt%>%
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  distinct(study_number, prescribed_bnf_code, processing_period_date, .keep_all=T)%>%
  group_by(study_number, prescribed_bnf_code)%>%
  filter(n()>1)%>%
  arrange(processing_period_date)%>%
  group_by(prescribed_bnf_code, study_number)%>%
  summarise(average=round(as.numeric(median(diff(processing_period_date))),0))%>%
  inner_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  mutate(dataset="Dispensing")%>%
  rename(code=prescribed_bnf_code)%>%
  as_tibble()->p1


t<-rbind(p,p1)


t%<>%
  left_join(codelist_labels)

rm(p,p1)


#### plots (ridges) -----

##### main drugs -------------------------

t%>%
  filter(codelist %in% drugs_paper)%>%
  mutate(labels=fct_reorder(labels, desc(labels)))%>%
  ggplot(aes(x=average, fill=dataset))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360), limits=c(0, 360))+
  theme_gray(base_size=20)+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom"
        
  )+
  labs(x = "Average day difference",
       title="Time interval between consecutive entries",
       caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual code and participant level as the median day different between consecutive entries. Vertical lines depict medians per drug group and dataset. Entries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date", 200),
       fill="Dataset",
       y="Drug group")


ggsave("Outputs/Figures/baseline_drugs_recovery/day_differences_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")



##### additional drugs -------------------------

t%>%
  filter(!codelist %in% drugs_paper)%>%
  mutate(labels=fct_reorder(labels, desc(labels)))%>%
  ggplot(aes(x=average, fill=dataset))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360), limits=c(0, 360))+
  theme_gray(base_size=20)+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom"
        
  )+
  labs(x = "Average day difference",
       title="Time interval between consecutive entries (additional drugs)",
       caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual code and participant level as the median day different between consecutive entries. Vertical lines depict medians per drug group and dataset. Entries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date", 200),
       fill="Dataset",
       y="Drug group")


ggsave("Outputs/Figures/baseline_drugs_recovery/day_differences_additional_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

































##### cv ------

t%>%
  filter(group=="cv")%>%
  mutate(labels=fct_reorder(labels, desc(labels)))%>%
  ggplot(aes(x=average, fill=dataset))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360), limits=c(0, 360))+
  theme_gray(base_size=20)+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom"
        
  )+
  labs(x = "Average day difference",
       title="Time interval between consecutive entries (cardiovascular drugs)",
       caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual code and participant level as the median day different between consecutive entries. Vertical lines depict medians per drug group and dataset. Entries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date", 200),
       fill="Dataset",
       y="Drug group")


ggsave("Outputs/Figures/baseline_drugs_recovery/day_differences_cv.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


##### resp ------

t%>%
  filter(group=="resp")%>%
  mutate(labels=fct_reorder(labels, desc(labels)))%>%
  ggplot(aes(x=average, fill=dataset))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360), limits=c(0, 360))+
  theme_gray(base_size=20)+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom"
        
  )+
  labs(x = "Average day difference",
       title="Time interval between consecutive entries (respiratory drugs)",
       caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual code and participant level as the median day different between consecutive entries. Vertical lines depict medians per drug group and dataset. Entries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date", 200),
       fill="Dataset",
       y="Drug group")


ggsave("Outputs/Figures/baseline_drugs_recovery/day_differences_resp.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


##### other ------

t%>%
  filter(group=="oth")%>%
  mutate(labels=fct_reorder(labels, desc(labels)))%>%
  ggplot(aes(x=average, fill=dataset))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360), limits=c(0, 360))+
  theme_gray(base_size=20)+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom"
        
  )+
  labs(x = "Average day difference",
       title="Time interval between consecutive entries (other drugs)",
       caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual code and participant level as the median day different between consecutive entries. Vertical lines depict medians per drug group and dataset. Entries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date", 200),
       fill="Dataset",
       y="Drug group")


ggsave("Outputs/Figures/baseline_drugs_recovery/day_differences_oth.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

















#### tables -----

##### cv ------


t%>%
  filter(group=="cv")%>%
  group_by(labels, dataset)%>%
  summarise(median=round(median(average, na.rm = T),0),
            Q1 = round(quantile(average, probs=0.25, na.rm=T),0),
            Q3 = round(quantile(average, probs=0.75, na.rm=T),0))->medians



set_flextable_defaults(
  font.size = 10, 
  theme_fun = theme_vanilla,
  padding = 6,
  text.align = "left",
  font.family = "Mulish")


medians%>%
  rename('Drug group' = labels,
         Median=median)%>%
  mutate(IQR = paste0(Q1, "-", Q3))%>%
  select(-Q1,-Q3)%>%
  pivot_wider(c('Drug group'), names_from = "dataset", values_from = c("Median", "IQR"))%>%
  select(Median_GP, IQR_GP, Median_Dispensing, IQR_Dispensing)%>%
  flextable()%>%
  set_header_labels(values=list(Median_GP="Median", 
                                IQR_GP = "IQR",
                                Median_Dispensing="Median",
                                IQR_Dispensing="IQR"))%>%
  set_caption("Time intervals (in days) between consecutive entries (for cardiovascular drugs)")%>%
  add_footer_lines("Day differences calculated at individual code and participant level as the median day different between consecutive entries.\nEntries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date")%>%
  width(j=1, width=4)%>%
  width(j=2:5, width=1)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=c(2,4), align="right", part="body")%>%
  align(i=NULL, j=c(3,5), align="left", part="body")%>%
  align(i=NULL, j=c(2,4), align="right", part="header")%>%
  align(i=NULL, j=c(3,5), align="left", part="header")%>%
  add_header_row(values = c("","GP dataset", "Dispensing dataset"), colwidths = c(1,2,2))->table

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())


table

save_as_docx(table, path = "Outputs/Tables/day_difference_cv.docx", pr_section = sect_properties)

rm(p,p1, table, medians)



##### resp ------


t%>%
  filter(group=="resp")%>%
  group_by(labels, dataset)%>%
  summarise(median=round(median(average, na.rm = T),0),
            Q1 = round(quantile(average, probs=0.25, na.rm=T),0),
            Q3 = round(quantile(average, probs=0.75, na.rm=T),0))->medians



set_flextable_defaults(
  font.size = 10, 
  theme_fun = theme_vanilla,
  padding = 6,
  text.align = "left",
  font.family = "Mulish")


medians%>%
  rename('Drug group' = labels,
         Median=median)%>%
  mutate(IQR = paste0(Q1, "-", Q3))%>%
  select(-Q1,-Q3)%>%
  pivot_wider(c('Drug group'), names_from = "dataset", values_from = c("Median", "IQR"))%>%
  select(Median_GP, IQR_GP, Median_Dispensing, IQR_Dispensing)%>%
  flextable()%>%
  set_header_labels(values=list(Median_GP="Median", 
                                IQR_GP = "IQR",
                                Median_Dispensing="Median",
                                IQR_Dispensing="IQR"))%>%
  set_caption("Time intervals (in days) between consecutive entries (for respiratory drugs)")%>%
  add_footer_lines("Day differences calculated at individual code and participant level as the median day different between consecutive entries.\nEntries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date")%>%
  width(j=1, width=4)%>%
  width(j=2:5, width=1)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=c(2,4), align="right", part="body")%>%
  align(i=NULL, j=c(3,5), align="left", part="body")%>%
  align(i=NULL, j=c(2,4), align="right", part="header")%>%
  align(i=NULL, j=c(3,5), align="left", part="header")%>%
  add_header_row(values = c("","GP dataset", "Dispensing dataset"), colwidths = c(1,2,2))->table

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())


table

save_as_docx(table, path = "Outputs/Tables/day_difference_resp.docx", pr_section = sect_properties)

rm(p,p1, table, medians)



##### other ------


t%>%
  filter(group=="oth")%>%
  group_by(labels, dataset)%>%
  summarise(median=round(median(average, na.rm = T),0),
            Q1 = round(quantile(average, probs=0.25, na.rm=T),0),
            Q3 = round(quantile(average, probs=0.75, na.rm=T),0))->medians



set_flextable_defaults(
  font.size = 10, 
  theme_fun = theme_vanilla,
  padding = 6,
  text.align = "left",
  font.family = "Mulish")


medians%>%
  rename('Drug group' = labels,
         Median=median)%>%
  mutate(IQR = paste0(Q1, "-", Q3))%>%
  select(-Q1,-Q3)%>%
  pivot_wider(c('Drug group'), names_from = "dataset", values_from = c("Median", "IQR"))%>%
  select(Median_GP, IQR_GP, Median_Dispensing, IQR_Dispensing)%>%
  flextable()%>%
  set_header_labels(values=list(Median_GP="Median", 
                                IQR_GP = "IQR",
                                Median_Dispensing="Median",
                                IQR_Dispensing="IQR"))%>%
  set_caption("Time intervals (in days) between consecutive entries (for other drugs)")%>%
  add_footer_lines("Day differences calculated at individual code and participant level as the median day different between consecutive entries.\nEntries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date")%>%
  width(j=1, width=4)%>%
  width(j=2:5, width=1)%>%
  # add_footer_lines("")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=c(2,4), align="right", part="body")%>%
  align(i=NULL, j=c(3,5), align="left", part="body")%>%
  align(i=NULL, j=c(2,4), align="right", part="header")%>%
  align(i=NULL, j=c(3,5), align="left", part="header")%>%
  add_header_row(values = c("","GP dataset", "Dispensing dataset"), colwidths = c(1,2,2))->table

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())


table

save_as_docx(table, path = "Outputs/Tables/day_difference_oth.docx", pr_section = sect_properties)

rm(p,p1, table, medians)


















#### proportions in each time interval -----

##### main drugs -----------------

t%>%
  filter(codelist %in% drugs_paper)%>%
  group_by(labels, dataset)%>%
  summarise(total = n_distinct(study_number),
            interval_30 = round(n_distinct(study_number[average<=30])/total*100,0),
            interval_60 = round(n_distinct(study_number[average<=60])/total*100,0),
            interval_90 = round(n_distinct(study_number[average<=90])/total*100,0),
            interval_180 = round(n_distinct(study_number[average<=180])/total*100,0),
            interval_360 = round(n_distinct(study_number[average<=360])/total*100,0))%>%
  rename("30" = interval_30,
         "60" = interval_60,
         "90" = interval_90,
         "180" = interval_180,
         "360" = interval_360)%>%
  select(-total)%>%
  pivot_longer(-c(labels, dataset), names_to="key", values_to="value")%>%
  # mutate(key=fct_relevel(key, levels=c("30", "60", "90", "180", "360")))%>%
  mutate(key=as.numeric(key))->t1

t1%>%
  ggplot(aes(x=as.numeric(key), y=value, fill=dataset, color=dataset))+
  geom_point(aes(color=dataset))+
  geom_line(aes(color=dataset))+
  geom_text_repel(data=t1%>%filter(dataset=="GP"), 
                  aes(label=paste0(value, "%")),
                  direction="y",
                  nudge_y = 5,
                  size=8)+
  geom_text_repel(data=t1%>%filter(dataset=="Dispensing"), 
                  aes(label=paste0(value, "%")),
                  direction="y",
                  nudge_y = -5,
                  size=8)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position="right")+
  labs(y="Participants (%)",
       title="Proportions of participants with distinct average time intervals between consecutive entries",
       x="Time between consecutive records (in days)",
       caption="Time intervals calculated as the median day difference between consecutive entries with the same drug code for the same participant\nDuplicate entries (same drug code, same participant, same date) were excluded to avoid counting duplicate entries as distinct events\nProportions refer to total number of participants with an entry for each drug group and dataset",
       fill="Dataset",
       color="Dataset")+
  scale_x_continuous(breaks=c(30,60,90, 180, 360))+
  scale_y_continuous(breaks=seq(0,100, by=20), expand=expansion(c(0,0.2)))


ggsave("Outputs/Figures/baseline_drugs_recovery/time_intervals_proportions_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

##### additional drugs ----------------

t%>%
  filter(!codelist %in% drugs_paper)%>%
  group_by(labels, dataset)%>%
  summarise(total = n_distinct(study_number),
            interval_30 = round(n_distinct(study_number[average<=30])/total*100,0),
            interval_60 = round(n_distinct(study_number[average<=60])/total*100,0),
            interval_90 = round(n_distinct(study_number[average<=90])/total*100,0),
            interval_180 = round(n_distinct(study_number[average<=180])/total*100,0),
            interval_360 = round(n_distinct(study_number[average<=360])/total*100,0))%>%
  rename("30" = interval_30,
         "60" = interval_60,
         "90" = interval_90,
         "180" = interval_180,
         "360" = interval_360)%>%
  select(-total)%>%
  pivot_longer(-c(labels, dataset), names_to="key", values_to="value")%>%
  # mutate(key=fct_relevel(key, levels=c("30", "60", "90", "180", "360")))%>%
  mutate(key=as.numeric(key))->t1

t1%>%
  ggplot(aes(x=as.numeric(key), y=value, fill=dataset, color=dataset))+
  geom_point(aes(color=dataset))+
  geom_line(aes(color=dataset))+
  geom_text_repel(data=t1%>%filter(dataset=="GP"), 
                  aes(label=paste0(value, "%")),
                  direction="y",
                  nudge_y = 5,
                  size=8)+
  geom_text_repel(data=t1%>%filter(dataset=="Dispensing"), 
                  aes(label=paste0(value, "%")),
                  direction="y",
                  nudge_y = -5,
                  size=8)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position="right")+
  labs(y="Participants (%)",
       title="Proportions of participants with distinct average time intervals between consecutive entries (additional drugs)",
       x="Time between consecutive records (in days)",
       caption="Time intervals calculated as the median day difference between consecutive entries with the same drug code for the same participant\nDuplicate entries (same drug code, same participant, same date) were excluded to avoid counting duplicate entries as distinct events\nProportions refer to total number of participants with an entry for each drug group and dataset",
       fill="Dataset",
       color="Dataset")+
  scale_x_continuous(breaks=c(30,60,90, 180, 360))+
  scale_y_continuous(breaks=seq(0,100, by=20), expand=expansion(c(0,0.2)))


ggsave("Outputs/Figures/baseline_drugs_recovery/time_intervals_proportions_additional_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


















##### cv ------


t%>%
  filter(group=="cv")%>%
  group_by(labels, dataset)%>%
  summarise(total = n_distinct(study_number),
            interval_30 = round(n_distinct(study_number[average<=30])/total*100,0),
            interval_60 = round(n_distinct(study_number[average<=60])/total*100,0),
            interval_90 = round(n_distinct(study_number[average<=90])/total*100,0),
            interval_180 = round(n_distinct(study_number[average<=180])/total*100,0),
            interval_360 = round(n_distinct(study_number[average<=360])/total*100,0))%>%
  rename("30" = interval_30,
         "60" = interval_60,
         "90" = interval_90,
         "180" = interval_180,
         "360" = interval_360)%>%
  select(-total)%>%
  pivot_longer(-c(labels, dataset), names_to="key", values_to="value")%>%
  mutate(key=fct_relevel(key, levels=c("30", "60", "90", "180", "360")))%>%
  
  ggplot(aes(x=key, y=value, fill=dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(value, "%")), size=5, position = position_dodge(width = .9), vjust=-0.1)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom")+
  labs(y="Participants (%)",
       title="Proportions of participants with distinct average time intervals between consecutive entries (cardiovascular drugs)",
       x="Timeframe (in days)",
       caption="Time intervals calculated as the median day difference between consecutive entries with the same drug code for the same participant\nDuplicate entries (same drug code, same participant, same date) were excluded to avoid counting duplicate entries as distinct events\nProportions refer to total number of participants with an entry for each drug group and dataset",
       fill="Dataset")+
  scale_x_discrete(breaks=c(30,60,90, 180, 360))+
  scale_y_continuous(expand=expansion(c(0,0.2)))


ggsave("Outputs/Figures/baseline_drugs_recovery/time_intervals_proportions_cv.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


##### resp ------


t%>%
  filter(group=="resp")%>%
  group_by(labels, dataset)%>%
  summarise(total = n_distinct(study_number),
            interval_30 = round(n_distinct(study_number[average<=30])/total*100,0),
            interval_60 = round(n_distinct(study_number[average<=60])/total*100,0),
            interval_90 = round(n_distinct(study_number[average<=90])/total*100,0),
            interval_180 = round(n_distinct(study_number[average<=180])/total*100,0),
            interval_360 = round(n_distinct(study_number[average<=360])/total*100,0))%>%
  rename("30" = interval_30,
         "60" = interval_60,
         "90" = interval_90,
         "180" = interval_180,
         "360" = interval_360)%>%
  select(-total)%>%
  pivot_longer(-c(labels, dataset), names_to="key", values_to="value")%>%
  mutate(key=fct_relevel(key, levels=c("30", "60", "90", "180", "360")))%>%
  
  ggplot(aes(x=key, y=value, fill=dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(value, "%")), size=5, position = position_dodge(width = .9), vjust=-0.1)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom")+
  labs(y="Participants (%)",
       title="Proportions of participants with distinct average time intervals between consecutive entries (respiratory drugs)",
       x="Timeframe (in days)",
       caption="Time intervals calculated as the median day difference between consecutive entries with the same drug code for the same participant\nDuplicate entries (same drug code, same participant, same date) were excluded to avoid counting duplicate entries as distinct events\nProportions refer to total number of participants with an entry for each drug group and dataset",
       fill="Dataset")+
  scale_x_discrete(breaks=c(30,60,90, 180, 360))+
  scale_y_continuous(expand=expansion(c(0,0.2)))


ggsave("Outputs/Figures/baseline_drugs_recovery/time_intervals_proportions_resp.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


##### other ------


t%>%
  filter(group=="oth")%>%
  group_by(labels, dataset)%>%
  summarise(total = n_distinct(study_number),
            interval_30 = round(n_distinct(study_number[average<=30])/total*100,0),
            interval_60 = round(n_distinct(study_number[average<=60])/total*100,0),
            interval_90 = round(n_distinct(study_number[average<=90])/total*100,0),
            interval_180 = round(n_distinct(study_number[average<=180])/total*100,0),
            interval_360 = round(n_distinct(study_number[average<=360])/total*100,0))%>%
  rename("30" = interval_30,
         "60" = interval_60,
         "90" = interval_90,
         "180" = interval_180,
         "360" = interval_360)%>%
  select(-total)%>%
  pivot_longer(-c(labels, dataset), names_to="key", values_to="value")%>%
  mutate(key=fct_relevel(key, levels=c("30", "60", "90", "180", "360")))%>%
  
  ggplot(aes(x=key, y=value, fill=dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(value, "%")), size=5, position = position_dodge(width = .9), vjust=-0.1)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position="bottom")+
  labs(y="Participants (%)",
       title="Proportions of participants with distinct average time intervals between consecutive entries (other drugs)",
       x="Timeframe (in days)",
       caption="Time intervals calculated as the median day difference between consecutive entries with the same drug code for the same participant\nDuplicate entries (same drug code, same participant, same date) were excluded to avoid counting duplicate entries as distinct events\nProportions refer to total number of participants with an entry for each drug group and dataset",
       fill="Dataset")+
  scale_x_discrete(breaks=c(30,60,90, 180, 360))+
  scale_y_continuous(expand=expansion(c(0,0.2)))


ggsave("Outputs/Figures/baseline_drugs_recovery/time_intervals_proportions_other.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

































### visualization: entries per month ---------------


gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  count(study_number, month, code)%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  group_by(codelist, month, study_number)%>%
  summarise(average=median(n, na.rm=T))%>%
  mutate(Dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
  mutate(month=as.Date(processing_period_date))%>%
  count(study_number, month, prescribed_bnf_code)%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  group_by(codelist, month, study_number)%>%
  summarise(average=median(n, na.rm=T))%>%
  mutate(Dataset="Dispensing")%>%
  as_tibble()->x2

t<-rbind(x1, x2)

rm(x1,x2)


##### main drugs-----



t%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  filter(!is.na(labels))%>%
  filter(codelist %in% drugs_paper)%>%
  mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"))%>%
  mutate(labels=reorder(labels, desc(labels)))%>%
  select(labels, Dataset, average)%>%
  ggplot(aes(average, labels, fill=labels))+
  stat_density_ridges(scale=0.8, quantile_lines = T, quantiles=2, alpha=0.5)+
  facet_wrap(~Dataset, ncol=2)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.y=element_blank())+
  scale_x_continuous(limits = c(NA,10), breaks=seq(1,10,by=1))+
  labs(title="Distributions of entry counts per month and per participant",
       subtitle="Aggregated per drug group and dataset",
       caption="Plots capped at 10. Vertical lines in each density plot represent each drug group's median. Counts calculated for each individual code and averaged per participant and per month",
       x="Entries per participant per month (median)")


ggsave("Outputs/Figures/baseline_drugs_recovery/entries_per_month.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


##### additional drugs ----------

t%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  filter(!is.na(labels))%>%
  filter(!codelist %in% drugs_paper)%>%
  mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"))%>%
  mutate(labels=reorder(labels, desc(labels)))%>%
  select(labels, Dataset, average)%>%
  ggplot(aes(average, labels, fill=labels))+
  stat_density_ridges(scale=0.8, quantile_lines = T, quantiles=2, alpha=0.5)+
  facet_wrap(~Dataset, ncol=2)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.y=element_blank())+
  scale_x_continuous(limits = c(NA,10), breaks=seq(1,10,by=1))+
  labs(title="Distributions of entry counts per month and per participant (additional drugs)",
       subtitle="Aggregated per drug group and dataset",
       caption="Plots capped at 10. Vertical lines in each density plot represent each drug group's median. Counts calculated for each individual code and averaged per participant and per month",
       x="Entries per participant per month (median)")


ggsave("Outputs/Figures/baseline_drugs_recovery/entries_per_month_additional_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")
















##### cv drugs ------

t%>%
  filter(!is.na(labels))%>%
  filter(group=="cv")%>%
  mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"))%>%
  mutate(labels=reorder(labels, desc(labels)))%>%
  select(labels, Dataset, n)%>%
  ggplot(aes(n, labels, fill=labels))+
  stat_density_ridges(scale=0.8, quantile_lines = T, quantiles=2, alpha=0.5)+
  facet_wrap(~Dataset, ncol=2)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y=element_blank())+
  scale_x_continuous(limits = c(NA,10), breaks=seq(1,10,by=1))+
  labs(title="Distributions of entry counts per month and per participant (cardiovascular drugs)",
       subtitle="Aggregated per drug group and dataset",
       caption="Plots capped at 10. Vertical lines in each density plot represent each drug group's median")


ggsave("Outputs/Figures/baseline_drugs_recovery/entries_per_month_cv_density.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

##### resp drugs ------

t%>%
  filter(!is.na(labels))%>%
  filter(group=="resp")%>%
  mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"))%>%
  mutate(labels=reorder(labels, desc(labels)))%>%
  select(labels, Dataset, n)%>%
  ggplot(aes(n, labels, fill=labels))+
  stat_density_ridges(scale=0.8, quantile_lines = T, quantiles=2, alpha=0.5)+
  facet_wrap(~Dataset, ncol=2)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y=element_blank())+
  scale_x_continuous(limits = c(NA,10), breaks=seq(1,10,by=1))+
  labs(title="Distributions of entry counts per month and per participant (respiratory drugs)",
       subtitle="Aggregated per drug group and dataset",
       caption="Plots capped at 10. Vertical lines in each density plot represent each drug group's median")



ggsave("Outputs/Figures/baseline_drugs_recovery/entries_per_month_resp_density.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")

##### other drugs ------


t%>%
  filter(!is.na(labels))%>%
  filter(group=="oth")%>%
  mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"))%>%
  mutate(labels=reorder(labels, desc(labels)))%>%
  select(labels, Dataset, n)%>%
  ggplot(aes(n, labels, fill=labels))+
  stat_density_ridges(scale=0.8, quantile_lines = T, quantiles=2, alpha=0.5)+
  facet_wrap(~Dataset, ncol=2)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y=element_blank())+
  scale_x_continuous(limits = c(NA,10), breaks=seq(1,10,by=1))+
  labs(title="Distributions of entry counts per month and per participant (other drugs)",
       subtitle="Aggregated per drug group and dataset",
       caption="Plots capped at 10. Vertical lines in each density plot represent each drug group's median")



ggsave("Outputs/Figures/baseline_drugs_recovery/entries_per_month_oth_density.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")


#### summary medians (all groups)----------

joint_table%>%
  filter(!is.na(labels))%>%
  filter(avg_entries_per_month!="N/A")%>%
  mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"),
         labels=reorder(labels, desc(labels)))%>%
  ggplot(aes(labels, avg_entries_per_month, group=Dataset, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  coord_flip()+
  theme_gray(base_size=20)+
  theme(legend.position="bottom")+
  labs(title="Average number of entries per participant per month",
       subtitle="by drug group and dataset",
       caption="Averages calculated as median",
       x="Drug group",
       y="Entries")+
  facet_wrap(~Dataset, ncol=2)


ggsave("Outputs/Figures/baseline_drugs_recovery/entries_per_month_bars.png",
       last_plot(),
       width=25,
       height=15,
       dpi="retina")









## barcharts with proportions captured in each dataset --------

### timeseries -----

#### calculations ------
gp_dt%>%
  select(study_number, code, date, row_number)%>%
  inner_join(codelists_snomed, by=c("code" = "ConceptId"))%>%
  select(study_number, code, codelist, date, row_number)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  filter(month>= "2018-01-01",
         month<= "2021-05-01")%>%
  distinct(study_number, codelist, month)%>%
  mutate(dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code" = "code"))%>%
  rename(month=processing_period_date)%>%
  mutate(month=as.character(month))%>%
  filter(month>= "2018-01-01",
         month<= "2021-05-01")%>%
  distinct(study_number, codelist, month)%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()->x2

t<-rbind(x1, x2)

rm(x1,x2)

t%>%distinct(study_number)%>%nrow()->participants

t%>%distinct(study_number)->participants_list

t%<>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, codelist, month), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, codelist)%>%
  mutate(study_number=as.numeric(study_number))

t%<>%
  mutate(group = as.factor(case_when(GP=="1" & Dispensing=="1" ~ "Both",
                                     GP=="1" & Dispensing=="0" ~ "GP",
                                     GP=="0" & Dispensing=="1" ~ "Dispensing")))%>%
  mutate(month=as.Date(month))%>%
  group_by(month, codelist, group)%>%
  summarise(participants = n())%>%
  pivot_wider(c(month, codelist), names_from = "group", values_from = "participants")%>%
  mutate(Either=sum(GP, Dispensing, Both, na.rm = T))%>%
  pivot_longer(3:6, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(codelist_labels)%>%
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))


#### main drugs -------
t%>%
  ungroup()%>%
  filter(codelist %in% drugs_paper)%>%
  mutate(labels=as.factor(reorder(labels, labels)))%>%
  ggplot(aes(month, Participants, color=Dataset, group=Dataset, shape=Dataset))+
  geom_point()+
  geom_line()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle=90))+
  scale_y_continuous(limits = c(0, NA))+
  # scale_x_date(date_breaks = "1 month", date_labels = "%m %Y")+
  labs(x="Month",
       y="Participants",
       color="Dataset",
       title="Number of participants captured in each dataset along time",
       caption="Points depict monthly counts of participants recorded in either dataset, both datasets, or in the Dispensing or GP datasets only. Timeseries restricted to June 2018 to May 2021")+
  scale_color_discrete(limits=c( "Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_shape_discrete(limits=c( "Either dataset","Both datasets", "Dispensing only", "GP only"))



ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_timeseries_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

#### additional drugs -------
t%>%
  ungroup()%>%
  filter(!codelist %in% drugs_paper)%>%
  mutate(labels=as.factor(reorder(labels, labels)))%>%
  ggplot(aes(month, Participants, color=Dataset, group=Dataset, shape=Dataset))+
  geom_point()+
  geom_line()+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle=90))+
  scale_y_continuous(limits = c(0, NA))+
  # scale_x_date(date_breaks = "1 month", date_labels = "%m %Y")+
  labs(x="Month",
       y="Participants",
       color="Dataset",
       title="Number of participants captured in each dataset along time (additional drugs)",
       caption="Points depict monthly counts of participants recorded in either dataset, both datasets, or in the Dispensing or GP datasets only. Timeseries restricted to June 2018 to May 2021")+
  scale_color_discrete(limits=c( "Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_shape_discrete(limits=c( "Either dataset","Both datasets", "Dispensing only", "GP only"))



ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_timeseries_additional_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

### barplot ----------


#### calculations ------
gp_dt%>%
  select(study_number, code, date, row_number)%>%
  inner_join(codelists_snomed, by=c("code" = "ConceptId"))%>%
  select(study_number, code, codelist, date, row_number)%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  filter(month>= "2018-01-01",
         month<= "2021-05-01")%>%
  distinct(study_number, codelist)%>%
  mutate(dataset="GP")%>%
  as_tibble()->x1

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code" = "code"))%>%
  rename(month=processing_period_date)%>%
  mutate(month=as.character(month))%>%
  filter(month>= "2018-01-01",
         month<= "2021-05-01")%>%
  distinct(study_number, codelist)%>%
  mutate(dataset="Dispensing")%>%
  as_tibble()->x2

t<-rbind(x1, x2)

rm(x1,x2)

t%>%distinct(study_number)%>%nrow()->participants

t%>%distinct(study_number)->participants_list

t%<>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, codelist), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, codelist)%>%
  mutate(study_number=as.numeric(study_number))

t%<>%
  mutate(group = as.factor(case_when(GP=="1" & Dispensing=="1" ~ "Both",
                                     GP=="1" & Dispensing=="0" ~ "GP",
                                     GP=="0" & Dispensing=="1" ~ "Dispensing")))%>%
  group_by(codelist, group)%>%
  summarise(participants = n())%>%
  pivot_wider(codelist, names_from = "group", values_from = "participants")%>%
  mutate(Either=sum(GP, Dispensing, Both, na.rm = T))%>%
  pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(codelist_labels)%>%
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))


#### main drugs ------------

t%>%  
  ungroup()%>%
  filter(codelist %in% drugs_paper)%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
  mutate(labels=reorder(labels, labels))%>%
  
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.5)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(color="Dataset",
       title="Number of participants captured in each dataset",
       caption="Proportions presented are in reference to the number of participants identified using either dataset. Calculations restricted to the time period between June 2018 and May 2021"
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


#### additional drugs ------------

t%>%  
  ungroup()%>%
  filter(!codelist %in% drugs_paper)%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
  mutate(labels=reorder(labels, labels))%>%

ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=4)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=20)+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.7)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(color="Dataset",
       title="Number of participants captured in each dataset (additional drugs)",
       caption="Proportions presented are in reference to the number of participants identified using either dataset. Calculations restricted to the time period between June 2018 and May 2021"
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_additional_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")


















## visualization: quantity prescribed vs day difference (EXPLORATORY, NOT NEEDED) ------

#### cv drugs --------------

##### summary counts ------

joint_table%>%
  mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"))%>%
  filter(group=="cv")%>%
  filter(across(everything(), ~.!="N/A"))%>%
  mutate(avg_day_interval=as.numeric(avg_day_interval),
         Average_quantity_prescribed=as.numeric(Average_quantity_prescribed))%>%
  ggplot(aes(avg_day_interval, Average_quantity_prescribed))+
  geom_point(aes(color=Dataset), alpha=0.5, size=10)+
  labs(x="Interval between entries",
       y="Quantity prescribed/dispensed",
       title="Association between day interval between consecutive entries and quantity prescribed/dispensed (cardiovascular drugs)",
       subtitle="Per drug group and dataset",
       caption="Drug group level aggregations performed using medians\nDrug groups containing drugs that may be prescribed at the same time were removed")+
  theme_gray(base_size=20)+
  facet_wrap(~labels)+
  scale_x_continuous(limits=c(0,65))+
  scale_y_continuous(limits=c(0,65))+
  theme(legend.position = "bottom")+
  geom_abline(slope=1, intercept=0, linetype="dashed", size=0.5)


# alternative plot
# joint_table%>%
#   filter(group=="cv")%>%
#   filter(across(everything(), ~.!="N/A"))%>%
#   mutate(avg_day_interval=as.numeric(avg_day_interval),
#          Average_quantity_prescribed=as.numeric(Average_quantity_prescribed))%>%
#   ggplot(aes(avg_day_interval, Average_quantity_prescribed))+
#   geom_point(aes(color=labels, 
#                  #size=Entries
#                  ), alpha=0.5)+
#   geom_text_repel(data=joint_table%>%
#                     filter(Average_quantity_prescribed!="28" | avg_day_interval!="28",
#                            group=="cv",
#                            filter(across(everything(), ~.!="N/A")))%>%
#                     mutate(Average_quantity_prescribed=as.numeric(Average_quantity_prescribed),
#                            avg_day_interval=as.numeric(avg_day_interval))%>%
#                     select(labels, Dataset, Average_quantity_prescribed, avg_day_interval)%>%
#                     group_by(Dataset),
#                   aes(label=labels, color=labels),  max.overlaps = 20, force=2, force_pull = -.1, show.legend = F, direction="both", size=5, verbose=T)+
#   labs(x="Interval between entries",
#        y="Quantity prescribed",
#        title="Association between day interval between consecutive entries and quantity prescribed",
#        subtitle="Per drug group and dataset",
#        caption="Each point represents a drug group\n38 data points not labelled (those in the centre)\nDrug group level aggregations performed using medians\nDrug groups containing drugs that may be prescribed at the same time were removed")+
#   theme_gray(base_size=20)+
#   facet_wrap(~Dataset)+
#   scale_x_continuous(limits=c(0,65))+
#   scale_y_continuous(limits=c(0,65))+
#   theme(legend.position="none")



ggsave("Outputs/Figures/baseline_drugs_recovery/quantity_vs_day_difference_cv.png",
       last_plot(),
       width=25,
       height=10,
       dpi="retina")

##### with individual day counts and quantities ------
# 
# gp_dt%>%
#   inner_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
#   as_tibble()%>%
#   group_by(codelist, study_number)%>%
#   filter(n()>1)%>%
#   distinct(study_number, date, codelist)%>%
#   arrange(date)%>%
#   group_by(codelist, study_number)%>%
#   summarise(day_difference=as.numeric(median(diff(date)), na.rm = T))%>%
#   mutate(dataset="GP")->x1
# 
# meds_dt%>%  
#   inner_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
#   as_tibble()%>%
#   group_by(codelist, study_number)%>%
#   filter(n()>1)%>%
#   distinct(study_number, processing_period_date, codelist)%>%
#   arrange(processing_period_date)%>%
#   summarise(day_difference=as.numeric(median(diff(processing_period_date)), na.rm = T))%>%
#   mutate(dataset="Dispensing")->x2
# 
# gp_dt%>%
#   inner_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
#   mutate(value1_prescription=if_else(gp_system_supplier=="TPP", value2_prescription, value1_prescription))%>%
#   group_by(study_number, codelist)%>%
#   summarise(quantity = median(value1_prescription, na.rm = T))%>%
#   as_tibble()%>%
#   select(codelist, study_number, quantity)%>%
#   mutate(dataset="GP")->x3
# 
# meds_dt%>%
#   inner_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
#   group_by(codelist,study_number)%>%
#   summarise(quantity = median(prescribed_quantity, na.rm = T))%>%
#   as_tibble()%>%
#   mutate(dataset="Dispensing")->x4
# 
# t<-rbind(x1 %>%left_join(x3),
#          x2%>%left_join(x4))
# 
# t%<>%
#   left_join(codelist_labels)
# 
# 
# p<-t%>%
#   filter(group=="cv")%>%
#   group_by(dataset)%>%
#   # slice_sample(prop=0.1)%>%
#   ggplot(aes(day_difference, quantity, color=dataset))+
#   geom_point(alpha=0.1)+
#   facet_wrap(~labels, scales="free")+
#   geom_xsidedensity(aes(y=stat(density))) +
#   geom_ysidedensity(aes(x=stat(density)))  +
#   geom_smooth()+
#   # geom_abline(slope=1, intercept=0, linetype="dashed", size=0.5, na.rm=T)
#   ggside(x.pos = "bottom", y.pos = "left") +
#   theme(ggside.panel.scale.x = .4,
#         ggside.panel.scale.y = .25)+
#   scale_yfill_manual(c("#00BFC4", "#F8766D"))+
#   scale_xfill_manual(c("#00BFC4", "#F8766D"))
# 
# 
# 
# 
# 
# 
# 
# 
# #### resp drugs --------------
# 
# joint_table%>%
#   mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"))%>%
#   filter(group=="resp")%>%
#   filter(across(everything(), ~.!="N/A"))%>%
#   mutate(avg_day_interval=as.numeric(avg_day_interval),
#          Average_quantity_prescribed=as.numeric(Average_quantity_prescribed))%>%
#   ggplot(aes(avg_day_interval, Average_quantity_prescribed))+
#   geom_point(aes(color=Dataset), alpha=0.5, size=10)+
#   labs(x="Interval between entries",
#        y="Quantity prescribed/dispensed",
#        title="Association between day interval between consecutive entries and quantity prescribed/dispensed (respiratory drugs)",
#        subtitle="Per drug group and dataset",
#        caption="Drug group level aggregations performed using medians\nDrug groups containing drugs that may be prescribed at the same time were removed")+
#   theme_gray(base_size=20)+
#   facet_wrap(~labels)+
#   scale_x_continuous(limits=c(0,65))+
#   scale_y_continuous(limits=c(0,65))+
#   theme(legend.position = "bottom")+
#   geom_abline(slope=1, intercept=0, linetype="dashed", size=0.5)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/quantity_vs_day_difference_resp.png",
#        last_plot(),
#        width=25,
#        height=10,
#        dpi="retina")
# 
# 
# #### oth drugs --------------
# 
# joint_table%>%
#   mutate(Dataset=fct_relevel(Dataset, "GP", "Dispensing"))%>%
#   filter(group=="oth")%>%
#   filter(across(everything(), ~.!="N/A"))%>%
#   mutate(avg_day_interval=as.numeric(avg_day_interval),
#          Average_quantity_prescribed=as.numeric(Average_quantity_prescribed))%>%
#   ggplot(aes(avg_day_interval, Average_quantity_prescribed))+
#   geom_point(aes(color=Dataset), alpha=0.5, size=10)+
#   labs(x="Interval between entries",
#        y="Quantity prescribed/dispensed",
#        title="Association between day interval between consecutive entries and quantity prescribed/dispensed (other drugs)",
#        subtitle="Per drug group and dataset",
#        caption="Drug group level aggregations performed using medians\nDrug groups containing drugs that may be prescribed at the same time were removed")+
#   theme_gray(base_size=20)+
#   facet_wrap(~labels)+
#   scale_x_continuous(limits=c(0,65))+
#   scale_y_continuous(limits=c(0,65))+
#   theme(legend.position = "bottom")+
#   geom_abline(slope=1, intercept=0, linetype="dashed", size=0.5)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/quantity_vs_day_difference_oth.png",
#        last_plot(),
#        width=25,
#        height=10,
#        dpi="retina")
# 


rm(x, t, t1, t2, p2, table, drug_groups, joint_table)







# 8. Baseline drug exposure ---------------------

# merge randomisation dates and calculate time since randomisation

## gp

gp_dt%>%
  select(study_number, date, code)%>%
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<="2021-05-31")%>%
  filter(date<rand_date)%>%
  mutate(time_before_rand = as.integer(difftime(rand_date, date, units = "days")))%>%
  mutate(dataset="GP")%>%
  select(study_number, code, codelist, date, rand_date, time_before_rand, dataset)%>%
  as_tibble()->x1
  

## meds

meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<="2021-05-31")%>%
  filter(processing_period_date<rand_date)%>%
  mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
  mutate(time_before_rand = abs(interval(rand_month, processing_period_date)%/%months(1)))%>%
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  select(-rand_month)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_before_rand, dataset)%>%
  as_tibble()->x2


x<-rbind(x1, x2)

rm(x1, x2)

x%>%distinct(study_number)%>%nrow()->total_participants


x%>%
  filter(dataset=="GP")%>%
  filter(time_before_rand<=90 & time_before_rand>=0)%>%
  mutate('1 month' = as.factor(if_else(time_before_rand<=30, 1, 0)),
         '2 months' = as.factor(if_else(time_before_rand<=60, 1, 0)),
         '3 months' = as.factor(if_else(time_before_rand<=90, 1, 0)))%>%

  rbind(
    x%>%
      filter(dataset=="Dispensing")%>%
      filter(time_before_rand<=3 & time_before_rand>=1)%>%
      mutate('1 month' = as.factor(if_else(time_before_rand<=1, 1, 0)),
             '2 months' = as.factor(if_else(time_before_rand<=2, 1, 0)),
             '3 months' = as.factor(if_else(time_before_rand<=3, 1, 0)))
    
  )->t


t%>%
  ungroup()%>%
  pivot_longer(c('1 month', '2 months', '3 months'), names_to = "Timeframe", values_to = "Flag")%>%
  group_by(dataset, codelist, Timeframe)%>%
  summarise(Participants = n_distinct(study_number[Flag==1]))%>%
  rbind(
  
  t%>%
    ungroup()%>%
    pivot_longer(c('1 month', '2 months', '3 months'), names_to = "Timeframe", values_to = "Flag")%>%
    group_by(codelist, Timeframe)%>%
    summarise(Participants = n_distinct(study_number[Flag==1]))%>%
    mutate(dataset="Either"),
  
  t%>%
    ungroup()%>%
    pivot_longer(c('1 month', '2 months', '3 months'), names_to = "Timeframe", values_to = "Flag")%>%
    filter(Flag==1)%>%
    distinct(study_number, codelist, dataset, Timeframe, Flag)%>%
    pivot_wider(names_from = dataset, values_from=Flag)%>%
    group_by(codelist, Timeframe)%>%
    summarise(Participants = n_distinct(study_number[!is.na(GP) & !is.na(Dispensing)]))%>%
    mutate(dataset="Both")
    )%>%
  ungroup()-> t1
  

gc()

## visualize -----


### main drugs -----------------


t1%>%
  left_join(codelist_labels)%>%
  filter(codelist %in% drugs_paper)%>%
  mutate(dataset=factor(dataset, levels=c("Either", "Dispensing", "GP")),
         Timeframe=str_sub(Timeframe, 0, 1))%>%
  filter(dataset!="Both")%>%
  group_by(labels)%>%
  mutate(Prop=round(Participants/total_participants*100,0))%>%
  
  ggplot(aes(Timeframe, Participants, fill=dataset))+
  geom_bar(stat='identity', position="dodge")+
  theme_gray(base_size=15)+
  theme(legend.position = "bottom")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=3, position = position_dodge(width=1))+
  facet_wrap(~labels, scales="free_y", labeller = label_wrap_gen(width = 30))+
  scale_fill_discrete(limits= c("Either","Dispensing", "GP"))+
  labs(title="Participants identified using different lookback periods before randomisation",
       x="Lookback period before randomisation (in months)",
       fill="Dataset",
       caption = "For each record, the interval before randomisation was calculated in exact days before the randomisation date (for records in the GP dataset) or the number of calendar months predecing the month of randomisation (for records in the Dispensing dataset).
       For the Dispensing dataset, any records occuring in the month of randomisation were excluded as it is not possible to distinguish if these occured before or after randomisation. ")+
  scale_y_continuous(expand=expansion(c(0,0.6)))


ggsave("Outputs/Figures/baseline_drugs_recovery/baseline_timeframes_main_drugs.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")


### additional drugs -------
t1%>%
  left_join(codelist_labels)%>%
  filter(!codelist %in% drugs_paper)%>%
  mutate(dataset=factor(dataset, levels=c("Either", "Dispensing", "GP")),
         Timeframe=str_sub(Timeframe, 0, 1))%>%
  filter(dataset!="Both")%>%
  group_by(labels)%>%
  mutate(Prop=round(Participants/total_participants*100,0))%>%
  
  ggplot(aes(Timeframe, Participants, fill=dataset))+
  geom_bar(stat='identity', position="dodge")+
  theme_gray(base_size=15)+
  theme(legend.position = "bottom")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=3, position = position_dodge(width=1))+
  facet_wrap(~labels, scales="free_y", labeller = label_wrap_gen(width = 30))+
  scale_fill_discrete(limits= c("Either", "Dispensing", "GP"))+
  labs(title="Participants identified using different lookback periods before randomisation (additional drugs)",
       x="Lookback period before randomisation (in months)",
       fill="Dataset",
       caption = "For each record, the interval before randomisation was calculated in exact days before the randomisation date (for records in the GP dataset) or the number of calendar months predecing the month of randomisation (for records in the Dispensing dataset).
       For the Dispensing dataset, any records occuring in the month of randomisation were excluded as it is not possible to distinguish if these occured before or after randomisation. ")+
  scale_y_continuous(expand=expansion(c(0,0.6)))


ggsave("Outputs/Figures/baseline_drugs_recovery/baseline_timeframes_additional_drugs.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")












### cv ------

t1%>%
  left_join(codelist_labels)%>%
  filter(group=="cv")%>%
  mutate(dataset=factor(dataset, levels=c("GP", "Dispensing", "Either")),
         Timeframe=str_sub(Timeframe, 0, 1))%>%
  group_by(labels)%>%
  mutate(Prop=round(Participants/total_participants*100,0))%>%
  
  ggplot(aes(Timeframe, Participants, fill=dataset))+
  geom_bar(stat='identity', position="dodge")+
  theme_gray(base_size=15)+
  theme(legend.position = "bottom")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=3, position = position_dodge(width=1))+
  facet_wrap(~labels, scales="free_y", labeller = label_wrap_gen(width = 30))+
  scale_fill_discrete(limits= c("GP", "Dispensing", "Either"))+
  labs(title="Participants identified using different lookback periods before randomisation (cardiovascular drugs)",
       x="Lookback period before randomisation (in months)",
       fill="Dataset",
       caption = "For each record, the interval before randomisation was calculated in exact days before the randomisation date (for records in the GP dataset) or the number of calendar months predecing the month of randomisation (for records in the Dispensing dataset).
       For the Dispensing dataset, any records occuring in the month of randomisation were excluded as it is not possible to distinguish if these occured before or after randomisation. ")+
  scale_y_continuous(expand=expansion(c(0,0.6)))

  
ggsave("Outputs/Figures/baseline_drugs_recovery/baseline_cv_timeframes.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")

### resp ------

t1%>%
  left_join(codelist_labels)%>%
  filter(group=="resp")%>%
  mutate(dataset=factor(dataset, levels=c("GP", "Dispensing", "Either")),
         Timeframe=str_sub(Timeframe, 0, 1))%>%
  group_by(labels)%>%
  mutate(Prop=round(Participants/total_participants*100,0))%>%
  
  ggplot(aes(Timeframe, Participants, fill=dataset))+
  geom_bar(stat='identity', position="dodge")+
  theme_gray(base_size=15)+
  theme(legend.position = "bottom")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=3, position = position_dodge(width=1))+
  facet_wrap(~labels, scales="free_y", labeller = label_wrap_gen(width = 30))+
  scale_fill_discrete(limits= c("GP", "Dispensing", "Either"))+
  labs(title="Participants identified using different lookback periods before randomisation (respiratory drugs)",
       x="Lookback period before randomisation (in months)",
       fill="Dataset",
       caption = "For each record, the interval before randomisation was calculated in exact days before the randomisation date (for records in the GP dataset) or the number of calendar months predecing the month of randomisation (for records in the Dispensing dataset).
       For the Dispensing dataset, any records occuring in the month of randomisation were excluded as it is not possible to distinguish if these occured before or after randomisation. ")+
  scale_y_continuous(expand=expansion(c(0,0.6)))


ggsave("Outputs/Figures/baseline_drugs_recovery/baseline_resp_timeframes.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")


### other ------

t1%>%
  left_join(codelist_labels)%>%
  filter(group=="oth")%>%
  mutate(dataset=factor(dataset, levels=c("GP", "Dispensing", "Either")),
         Timeframe=str_sub(Timeframe, 0, 1))%>%
  group_by(labels)%>%
  mutate(Prop=round(Participants/total_participants*100,0))%>%
  
  ggplot(aes(Timeframe, Participants, fill=dataset))+
  geom_bar(stat='identity', position="dodge")+
  theme_gray(base_size=15)+
  theme(legend.position = "bottom")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=3, position = position_dodge(width=1))+
  facet_wrap(~labels, scales="free_y", labeller = label_wrap_gen(width = 30))+
  scale_fill_discrete(limits= c("GP", "Dispensing", "Either"))+
  labs(title="Participants identified using different lookback periods before randomisation (other drugs)",
       x="Lookback period before randomisation (in months)",
       fill="Dataset",
       caption = "For each record, the interval before randomisation was calculated in exact days before the randomisation date (for records in the GP dataset) or the number of calendar months predecing the month of randomisation (for records in the Dispensing dataset).
       For the Dispensing dataset, any records occuring in the month of randomisation were excluded as it is not possible to distinguish if these occured before or after randomisation. ")+
  scale_y_continuous(expand=expansion(c(0,0.6)))


ggsave("Outputs/Figures/baseline_drugs_recovery/baseline_oth_timeframes.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")


## agreement for baseline exposure based on 3m lookback --------------------

t%>%
  filter('3 months'>0)%>%
  distinct(study_number, codelist, dataset)%>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, codelist), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, codelist)%>%
  mutate(study_number=as.numeric(study_number))->t2


t%>%distinct(study_number)->participants_list

t%>%distinct(study_number)%>%nrow()->participants

#### calculate agreement ----

drug_groups <- t2%>%distinct(codelist)%>%.[[1]]

kappa_codelists_participants<-list()

for (i in drug_groups) {
  
  t2%>%
    filter(codelist==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, "0")))%>%  
    select(-codelist, -study_number)%>%
    mutate(across(everything(), as.integer))->table
  
  try(k<-Kappa.test(table$GP, table$Dispensing))
  
  if(exists("k"))
  {
    
    kappa_codelists_participants[[i]]<-k
    
  }
  else{
    kappa_codelists_participants[[i]]<-list()
  }
  
  rm(i, table, k)
  
}

kappa_codelists_participants_results<-data.frame()

for (i in drug_groups) {
  
  name<-i
  
  try(
    table<-(data.frame(
      codelist = name,
      kappa = unlist(kappa_codelists_participants[[i]]$Result$estimate),
      CI_lower = unlist(kappa_codelists_participants[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_codelists_participants[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_codelists_participants[[i]]$Result$p.value),
      judgement = unlist(kappa_codelists_participants[[i]]$Judgement)
    ))
  )
  
  try(kappa_codelists_participants_results%<>%bind_rows(table))
  
  
  rm(table, name, i)
  
}


kappa_codelists_participants_results%>%
  right_join(codelist_labels)%>%
  mutate_when(!is.na(CI_lower), 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate_when(p_value==0.00,
              p_value="<0.001")%>%
  
  left_join (
    (t2%>%
       group_by(codelist)%>%
       summarise(GP_Dispensing = n_unique(study_number[GP=="1" & Dispensing=="1"]),
                 GP_only = n_unique(study_number[GP=="1" & Dispensing=="0"]),
                 Dispensing_only = n_unique(study_number[GP=="0" & Dispensing=="1"]),
                 nil = participants - GP_Dispensing - GP_only - Dispensing_only)),
    by=c("codelist"))%>%
  arrange(labels)%>%
  select(labels,
         group,
         GP_Dispensing,
         GP_only,
         Dispensing_only,
         nil,
         kappa,
         CI,
         p_value,
         judgement)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))-> codelists_agreement_participants


### produce tables -----
# add total counts and proportions (either dataset)

codelists_agreement_participants%<>%
  mutate(across(3:6, ~replace_na(.x, 0)))%>%
  rowwise() %>%
  mutate(prop = round(sum(across(3:5), na.rm = T)/sum(across(3:6), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either dataset (%)' = paste0(sum(across(3:5), na.rm = T),
                                       " (",
                                       prop,
                                       "%)"))%>%
  mutate(prop1 = round(sum(across(3:3))/sum(across(3:6), na.rm = T)*100,1),
         prop2 = round(sum(across(4:4))/sum(across(3:6), na.rm = T)*100,2),
         prop3 = round(sum(across(5:5))/sum(across(3:6), na.rm = T)*100,1),
         prop4 = round(sum(across(6:6))/sum(across(3:6), na.rm = T)*100,1))%>%
  mutate(prop1=if_else(is.na(prop1), 0, prop1))%>%
  mutate(prop2=if_else(is.na(prop2), 0, prop2))%>%
  mutate(prop3=if_else(is.na(prop3), 0, prop3))%>%
  mutate(prop4=if_else(is.na(prop4), 0, prop4))%>%
  mutate(GP_Dispensing=paste0(GP_Dispensing, " (", prop1, "%)"),
         GP_only=paste0(GP_only, " (", prop2, "%)"),
         Dispensing_only=paste0(Dispensing_only, " (", prop3, "%)"),
         nil=paste0(nil, " (", prop4, "%)"))%>%
  select(-c(prop, prop1, prop2, prop3, prop4))%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%
  rename("Drug group"=labels,
         K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement)%>%
  relocate('Either dataset (%)', .after = 'Drug group')


## bring back codelist groups

codelists_agreement_participants%<>%
  left_join(codelist_labels, by=c("Drug group" = "labels"))%>%
  select(-group.x, -group.y)


# flextable


### main drugs 
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

codelists_agreement_participants%>%
  filter(codelist %in% drugs_paper)%>%
  select(-codelist)%>%
  rename("Participants in either dataset (%)" = "Either dataset (%)",
         'Participants in both datasets (%)' = GP_Dispensing,
         'Participants in GP dataset only (%)' = GP_only,
         'Participants in Dispensing dataset only (%)'=Dispensing_only,
         'Participants in neither dataset (%)'=nil)%>%
  flextable()%>%
  set_caption("Agreement for medication exposure at randomisation between the GP and Dispensing dataset (based on a 3 month lookback period)")%>%
  width(j=1, width=4)%>%
  width(j=2:10, width=1)%>%
  flextable::font(fontname="Mulish")%>%
  set_header_labels(values=list(CI="95% CI", 
                                p_value = "p-value"))%>%
  add_footer_lines("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset; CI: confidence interval")%>%
  align(i=NULL, j=2:10, align="center", part="all")->table_flex



save_as_docx(table_flex, path = "Outputs/Tables/agreement_baseline_main_drugs.docx", pr_section = sect_properties)


### additional drugs 
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

codelists_agreement_participants%>%
  filter(!codelist %in% drugs_paper)%>%
  select(-codelist)%>%
  rename("Participants in either dataset (%)" = "Either dataset (%)",
         'Participants in both datasets (%)' = GP_Dispensing,
         'Participants in GP dataset only (%)' = GP_only,
         'Participants in Dispensing dataset only (%)'=Dispensing_only,
         'Participants in neither dataset (%)'=nil)%>%
  flextable()%>%
  set_caption("Agreement for medication exposure at randomisation between the GP and Dispensing dataset (based on a 3 month lookback period)")%>%
  width(j=1, width=4)%>%
  width(j=2:10, width=1)%>%
  flextable::font(fontname="Mulish")%>%
  set_header_labels(values=list(CI="95% CI", 
                                p_value = "p-value"))%>%
  add_footer_lines("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset; CI: confidence interval")%>%
  align(i=NULL, j=2:10, align="center", part="all")->table_flex



save_as_docx(table_flex, path = "Outputs/Tables/agreement_baseline_additional_drugs.docx", pr_section = sect_properties)





## participants captured in each dataset --------------

t2%>%
  mutate(group = as.factor(case_when(GP=="1" & Dispensing=="1" ~ "Both",
                                     GP=="1" & Dispensing=="0" ~ "GP",
                                     GP=="0" & Dispensing=="1" ~ "Dispensing")))%>%
  group_by(codelist, group)%>%
  summarise(participants = n())%>%
  pivot_wider(codelist, names_from = "group", values_from = "participants")%>%
  mutate(Either=sum(GP, Dispensing, Both, na.rm = T))%>%
  pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(codelist_labels)%>%
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))->t3


#### main drugs ------------

t3%>%  
  ungroup()%>%
  filter(codelist %in% drugs_paper)%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
  mutate(labels=reorder(labels, labels))%>%
  
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.5)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(color="Dataset",
       title="Number of participants captured in each dataset at randomisation",
       subtitle="Based on a 3 month lookback period",
       caption=str_wrap("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset. 'Dispensing only' is the number of participants captured in the Dispensing dataset but not the GP dataset, and vice-versa. Proportions presented are in reference to the number of participants identified using either dataset. Calculations restricted to the time period between June 2018 and May 2021",200
  ))

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

#### main drugs ------------

t3%>%  
  ungroup()%>%
  filter(!codelist %in% drugs_paper)%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
  mutate(labels=reorder(labels, labels))%>%
  
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=4)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.8)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(color="Dataset",
       title="Number of participants captured in each dataset at randomisation (additional drugs)",
       subtitle="Based on a 3 month lookback period",
       caption=str_wrap("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset. 'Dispensing only' is the number of participants captured in the Dispensing dataset but not the GP dataset, and vice-versa. Proportions presented are in reference to the number of participants identified using either dataset. Calculations restricted to the time period between June 2018 and May 2021",200
       ))

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_additional_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")








































## final tables of participant counts at baseline -----------

### cv ------
# 
# t1%>%
#   ungroup()%>%
#   left_join(codelist_labels)%>%
#   filter(Timeframe == '3 months')%>%
#   filter(dataset=="Either")%>%
#   filter(group=="cv")%>%
#   mutate(Prop=round(Participants/total_participants*100,0))%>%
#   mutate(group=factor(group, levels=c("cv", "resp", "oth")))%>%
#   arrange(group, labels)%>%
#   select(labels, Participants, Prop)%>%
#   mutate(Participants = paste0(Participants, " (", Prop, "%)"))%>%
#   select(-Prop)%>%
#   rename('Drug group' = labels)%>%
#   flextable()%>%
#   set_caption("Baseline concomitant medications at randomisation (cardiovascular drugs)")%>%
#   width(j=1, width=4)%>%
#   width(j=2, width=2)%>%
#   add_footer_lines("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset")%>%
#   align(i=NULL, j=1, align="left", part="all")%>%
#   align(i=NULL, j=2, align="center", part="all")%>%
#   flextable::font(fontname="Mulish")->table
# 
# sect_properties <- prop_section(
#   page_size = page_size(orient = "landscape",
#                         width = 15, 
#                         #height = 11.7
#   ),
#   type = "continuous",
#   page_margins = page_mar())
# 
# save_as_docx(table, path = "Outputs/Tables/baseline_cv_counts.docx", pr_section = sect_properties)
# 
# 
# ### respiratory --------
# 
# t1%>%
#   ungroup()%>%
#   left_join(codelist_labels)%>%
#   filter(Timeframe == '3 months')%>%
#   filter(dataset=="Either")%>%
#   filter(group=="resp")%>%
#   mutate(Prop=round(Participants/total_participants*100,0))%>%
#   mutate(group=factor(group, levels=c("cv", "resp", "oth")))%>%
#   arrange(group, labels)%>%
#   select(labels, Participants, Prop)%>%
#   mutate(Participants = paste0(Participants, " (", Prop, "%)"))%>%
#   select(-Prop)%>%
#   rename('Drug group' = labels)%>%
#   flextable()%>%
#   set_caption("Baseline concomitant medications at randomisation (respiratory drugs)")%>%
#   width(j=1, width=4)%>%
#   width(j=2, width=2)%>%
#   add_footer_lines("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset")%>%
#   align(i=NULL, j=1, align="left", part="all")%>%
#   align(i=NULL, j=2, align="center", part="all")%>%
#   flextable::font(fontname="Mulish")->table
# 
# sect_properties <- prop_section(
#   page_size = page_size(orient = "landscape",
#                         width = 15, 
#                         #height = 11.7
#   ),
#   type = "continuous",
#   page_margins = page_mar())
# 
# save_as_docx(table, path = "Outputs/Tables/baseline_resp_counts.docx", pr_section = sect_properties)
# 
# ### other -----
# 
# 
# t1%>%
#   ungroup()%>%
#   left_join(codelist_labels)%>%
#   filter(Timeframe == '3 months')%>%
#   filter(dataset=="Either")%>%
#   filter(group=="oth")%>%
#   mutate(Prop=round(Participants/total_participants*100,0))%>%
#   mutate(group=factor(group, levels=c("cv", "resp", "oth")))%>%
#   arrange(group, labels)%>%
#   select(labels, Participants, Prop)%>%
#   mutate(Participants = paste0(Participants, " (", Prop, "%)"))%>%
#   select(-Prop)%>%
#   rename('Drug group' = labels)%>%
#   flextable()%>%
#   set_caption("Baseline concomitant medications at randomisation (other drugs)")%>%
#   width(j=1, width=4)%>%
#   width(j=2, width=2)%>%
#   add_footer_lines("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset")%>%
#   align(i=NULL, j=1, align="left", part="all")%>%
#   align(i=NULL, j=2, align="center", part="all")%>%
#   flextable::font(fontname="Mulish")->table
# 
# sect_properties <- prop_section(
#   page_size = page_size(orient = "landscape",
#                         width = 15, 
#                         #height = 11.7
#   ),
#   type = "continuous",
#   page_margins = page_mar())
# 
# save_as_docx(table, path = "Outputs/Tables/baseline_oth_counts.docx", pr_section = sect_properties)




























# 9. Drug initiation after randomisation ---------------


## calculate pts not at risk (all groups) -------
# (defined as taking those drugs at baseline, i.e. within 3 months prior)

# timeframe for this is 6 months after randomisation, so we also need to exclude people randomised before December 2020 (so 6 months extra is end of May 2021 - last month with complete data)

as.Date("2021-05-31")-180

### gp ------

gp_dt%>%
  select(study_number, date, code)%>%
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  filter(date<rand_date)%>% # events happening before randomisation date
  mutate(time_before_rand = as.integer(difftime(rand_date, date, units = "days")))%>%
  filter(time_before_rand<=90)%>% # events happening 3 months prior
  mutate(dataset="GP")%>%
  select(study_number, code, codelist, date, rand_date, time_before_rand, dataset)%>%
  as_tibble()->x1


### meds ------

meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
  filter(processing_period_date<rand_month)%>% # selecting only events happening before randomisation with some degree of certainty (previous calendar month)
  mutate(time_before_rand = abs(interval(rand_month, processing_period_date)%/%months(1)))%>%
  filter(time_before_rand<=3)%>% # events happening in the 3 calendar months prior
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  select(-rand_month)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_before_rand, dataset)%>%
  as_tibble()->x2


x<-rbind(x1, x2)

rm (x1, x2)


x%>%distinct(codelist, study_number) -> participants_not_at_risk # this contains a dataframe with participants not at risk for each drug group
# note this is based on entries on either dataset (to reduce impact of different handling of temporal data)
  
rm(x)














## calculate overall drug initiation (days after rand; all groups) -----------------

### gp ------

gp_dt%>%
  select(study_number, date, code)%>%
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  filter(date>rand_date)%>% # censor to events happening after randomisation date
  mutate(time_after_rand = as.integer(difftime(date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  mutate(dataset="GP")%>%
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>%
  as_tibble()->x1


### meds ------

meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
  filter(processing_period_date>rand_month)%>% # selecting only events happening after randomisation with some degree of certainty (following calendar month)
  mutate(time_after_rand = as.integer(difftime(processing_period_date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>%
  as_tibble()->x2

x<-rbind(x1, x2)

rm (x1, x2)


## restrict drug initiation to pts at risk for each category and produce survival dataset


drug_groups <- distinct(participants_not_at_risk, codelist)%>%.[[1]]

survival_data<-data.frame()

for (i in drug_groups) {
  

  participants_not_at_risk%>% # list of participants not at risk since already on drug at baseline (for each drug group)
    filter(codelist==i)%>% # trim to group of interest
    select(study_number)%>%
    .[[1]]->participant_not_at_risk_list # extract list of participants already on drug before rand
  
  rand_dates%>%
    filter(NationID=="EN")%>% # people randomised in England
    filter(is.na(Excluded))%>% # not excluded (duplicates, withdrawn consent)
    filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
    filter(!study_number %in% participant_not_at_risk_list)%>% # exclude people on drug at randomisation 
    select(study_number)->participants_at_risk # list of patients at risk
  
  
  
  x%>%
    select(study_number, codelist, time_after_rand, dataset)%>%
    filter(codelist==i)%>% # drug group
    filter(!study_number %in% participant_not_at_risk_list)%>% # of all those with records after randomisation, we're selecting only those who are at risk of starting drug
    filter(time_after_rand<=180)%>% # right censoring at 180 days
    # select(study_number, time_after_rand, dataset)%>%
    right_join(expand.grid(participants_at_risk%>%.[[1]], # this ensures there is a censored entry for each dataset for people with no records (rather than just events) 
                           c("Dispensing", "GP"))%>%
                 rename(study_number=Var1,
                        dataset=Var2)
    )%>%
    arrange(study_number, dataset, time_after_rand)%>%
    group_by(study_number, dataset)%>%
    slice_head(n=1)%>% # select first observation for each dataset
    mutate(event=if_else(!is.na(time_after_rand), 2, 1), # apply event flags (2 if time after rand is not null, 1 if otherwise)
           time_after_rand=ifelse(event==1, 180, time_after_rand), # apply time after rand of 180 if censored
           codelist = i # apply codelist name to censored participants
    )%>%
    select(study_number, time_after_rand, dataset, event, codelist)  -> table

survival_data%<>%bind_rows(table)

rm(table, i)

}

# rm(x)







### add counts in both datasets  ------
  # here we need to choose the first event for people who had events in both datasets, and make sure all other participants are censored

survival_data%>%
  group_by(codelist, study_number)%>%
  filter(event=="2")%>%
  filter(n()>1)%>%
  arrange(time_after_rand)%>% # sort by time to event
  slice_head(n=1)%>% # select first record for KM plot (will be the event or censoring at 180 days if no event)
  ungroup()%>%
  right_join(survival_data%>%distinct(study_number, codelist), by=c("study_number", "codelist"))%>% # bring back all combinations of pts at risk for each codelist
  mutate(event=as.numeric(if_else(is.na(time_after_rand), "1", "2")),
         time_after_rand=as.numeric(replace_na(time_after_rand,"180")))%>% # apply censoring flag and date for those not captured in both datasets 
  mutate(dataset="Both") ->a # apply dataset flag and export

survival_data%<>%bind_rows(a)

rm(a)




### add counts in either dataset ------
# for each participant and codelist we just need to select the first event (which will be a drug record, or the censoring event if there is no record)
survival_data%>%
  group_by(codelist, study_number)%>%
  arrange(time_after_rand)%>%
  slice_head(n=1)%>% 
  mutate(dataset="Either")->a

survival_data%<>%bind_rows(a)

rm(a)










## produce survival model --------------

fit<-survfit(
    Surv(time = time_after_rand, 
         event) ~ dataset, 
    data=survival_data)


survival_data%<>%left_join(codelist_labels)

survival_data%<>%
  mutate(dataset=fct_relevel(dataset, "Either", "GP", "Dispensing", "Both"))









## survival plots ------------------




### cv ------
# 
# ggsurvplot_facet(fit, 
#              fun="event", 
#              data=survival_data%>%filter(group=="cv"),
#              # conf.int = T, 
#              # risk.table = T,
#              # cumevents=T,
#              legend.title="Dataset",
#              # legend.labs=c("Dispensing", "GP"),
#              title="Survival curve for drug initiation after randomisation according to each dataset (cardiovascular drugs)",
#              facet.by = "labels",
#              short.panel.labs = T,
#              # pval = T, 
#              # pval.method = T,
#              scales="free_y"
#              )+
#   theme_bw(base_size=15)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/drug_initiation_cv.png",
#        last_plot(),
#        width=25,
#        height=10,
#        dpi="retina")

### resp ------
# 
# ggsurvplot_facet(fit, 
#                  fun="event", 
#                  data=survival_data%>%filter(group=="resp"),
#                  # conf.int = T, 
#                  # risk.table = T,
#                  # cumevents=T,
#                  legend.title="Dataset",
#                  # legend.labs=c("Dispensing", "GP"),
#                  title="Survival curve for drug initiation after randomisation according to each dataset (respiratory drugs)",
#                  facet.by = "labels",
#                  short.panel.labs = T,
#                  # pval = T, 
#                  # pval.method = T,
#                  scales="free_y"
# )+
#   theme_bw(base_size=15)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/drug_initiation_resp.png",
#        last_plot(),
#        width=25,
#        height=10,
#        dpi="retina")
# 
# 
# ### oth ------
# 
# ggsurvplot_facet(fit, 
#                  fun="event", 
#                  data=survival_data%>%filter(group=="oth"),
#                  # conf.int = T, 
#                  # risk.table = T,
#                  # cumevents=T,
#                  legend.title="Dataset",
#                  # legend.labs=c("Dispensing", "GP"),
#                  title="Survival curve for drug initiation after randomisation according to each dataset (other drugs)",
#                  facet.by = "labels",
#                  short.panel.labs = T,
#                  # pval = T, 
#                  # pval.method = T,
#                  scales="free_y"
# )+
#   theme_bw(base_size=15)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/drug_initiation_oth.png",
#        last_plot(),
#        width=25,
#        height=10,
#        dpi="retina")

### main groups --------------


ggsurvplot_facet(fit, 
                 fun="event", 
                 data=survival_data%>%filter(codelist %in% drugs_paper),
                 # conf.int = T, 
                 # risk.table = T,
                 # cumevents=T,
                 legend.title="Dataset",
                 # legend.labs=c("Dispensing", "GP"),
                 title="Survival curve for drug initiation after randomisation according to each dataset",
                 facet.by = "labels",
                 short.panel.labs = T,
                 # pval = T, 
                 # pval.method = T,
                 # scales="free_y",
                 # nrow = 9,
)+
  theme_bw(base_size=15)+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30)
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/drug_initiation_main.png",
       last_plot(),
       width=25,
       height=10,
       dpi="retina")


### all others -------------

ggsurvplot_facet(fit, 
                 fun="event", 
                 data=survival_data%>%filter(!codelist %in% drugs_paper),
                 # conf.int = T, 
                 # risk.table = T,
                 # cumevents=T,
                 legend.title="Dataset",
                 # legend.labs=c("Dispensing", "GP"),
                 title="Survival curve for drug initiation after randomisation according to each dataset (additional drug groups)",
                 facet.by = "labels",
                 short.panel.labs = T,
                 # pval = T, 
                 # pval.method = T,
                 # scales="free_y",
                 # nrow = 9,
                 )+
  theme_bw(base_size=15)+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30)
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/drug_initiation_supp.png",
       last_plot(),
       width=25,
       height=10,
       dpi="retina")






## investigate differences in immunosuppressive drugs -------

# (requires loading of SNOMED FSN from the initial section of the script)


x%>%
  filter(codelist=="immunosuppressive_drugs")%>%
  distinct(study_number, dataset)%>%
  arrange(study_number)%>%
  group_by(study_number)%>%
  filter(n()==1)->immunossuppresion_divergent_participants

x%>%
  filter(codelist=="immunosuppressive_drugs",
         dataset=="GP",
         study_number %in% immunossuppresion_divergent_participants$study_number)%>%
  select(study_number, code, dataset)%>%
  bind_rows(
    x%>%
      filter(codelist=="immunosuppressive_drugs",
             dataset=="Dispensing",
             study_number %in% immunossuppresion_divergent_participants$study_number)%>%
      select(study_number, code, dataset)
  )%>%
  group_by(code, dataset)%>%
  summarise(participants=n_distinct(study_number))->immunosuppression_codes

immunosuppression_codes%<>%
  left_join(FSN%>%select(conceptId, term), by = c("code" = "conceptId"))


meds_dt%>%
  distinct(prescribed_bnf_name, prescribed_bnf_code)%>%
  rename(term=prescribed_bnf_name)%>%
  as_tibble()->bnf_codes

immunosuppression_codes%<>%
  left_join(bnf_codes, by=c("code"="prescribed_bnf_code"))


immunosuppression_codes%<>%
  filter(dataset=="GP")%>%
  select(code, participants, dataset, term.x)%>%
  rename(term=term.x)%>%
  bind_rows(
    immunosuppression_codes%>%
      filter(dataset=="Dispensing")%>%
      select(code, participants,  dataset, term.y)%>%
      rename(term=term.y)
  )%>%arrange(desc(participants))


view(immunosuppression_codes)

write_csv(immunosuppression_codes, "Outputs/Tables/immunosuppression_codes.csv")


x%>%
  filter(codelist=="immunosuppressive_drugs")%>%
  distinct(study_number, code, dataset)%>%
  group_by(code, dataset)%>%
  summarise(participants=n_distinct(study_number))->immunosuppression_codes_all


immunosuppression_codes_all%<>%
  left_join(FSN%>%select(conceptId, term), by = c("code" = "conceptId"))


meds_dt%>%
  distinct(prescribed_bnf_name, prescribed_bnf_code)%>%
  rename(term=prescribed_bnf_name)%>%
  as_tibble()->bnf_codes

immunosuppression_codes_all%<>%
  left_join(bnf_codes, by=c("code"="prescribed_bnf_code"))


immunosuppression_codes_all%<>%
  filter(dataset=="GP")%>%
  select(code, participants, dataset, term.x)%>%
  rename(term=term.x)%>%
  bind_rows(
    immunosuppression_codes_all%>%
      filter(dataset=="Dispensing")%>%
      select(code, participants,  dataset, term.y)%>%
      rename(term=term.y)
  )%>%arrange(desc(participants))


view(immunosuppression_codes_all)





# differences in codes:
## dispensing: inclusion of sulfasalazine, mesalazine, balsalize (not in GP extraction)
## meds: inclusion of cosentyx/secukinumab (not in BNF), some drugs that are in codelist not picked up by dispensing (but accounting for small numbers)



# so most differences are due to people not being captured rather than differences in codes; assume this is due to different patterns regarding who prescribes these drugs (hospital vs GP) and where they are collected (hospital pharmacy and not community)


rm(immunossuppresion_divergent_participants, immunosuppression_codes, immunosuppression_codes_all)


rm(fit, table, drug_groups, participant_not_at_risk_list, participants_at_risk, participants_not_at_risk)




## overall day differences (for BA plots)------

survival_data%>%
  filter(event=="2")%>%
  group_by(study_number, codelist)%>%
  filter(!dataset=="Either")%>%
  filter(n_distinct(dataset)>1)->a

a%<>%
  select(study_number, codelist, time_after_rand, dataset)%>%
  pivot_wider(names_from=dataset, values_from=time_after_rand)%>%
  mutate(avg=(GP+Dispensing)/2,
         diff=Dispensing-GP)

a%<>%
  group_by(codelist)%>%
  mutate(mean_diff=mean(diff),
         lower = mean_diff - 1.96*sd(diff),
         upper= mean_diff + 1.96*sd(diff))

a%<>%
  left_join(codelist_labels)



### intraclass correlation coefficients  -------------------------
# using library irr

survival_data%>%
  filter(dataset%in%c("GP", "Dispensing"))%>%
  filter(event=="2")%>%
  select(study_number, dataset, time_after_rand, codelist)%>%
  pivot_wider(names_from = dataset, values_from = time_after_rand)%>%
  filter(!is.na(Dispensing) &!is.na(GP))->icc_data

icc(ratings = icc_data%>%select(-study_number, -codelist),
    model="twoway",
    type="agreement",
    unit="average")->icc_result

icc_results<-list(icc_result)


drug_groups <- distinct(survival_data, codelist)%>%.[[1]]

for (i in drug_groups) {
  
  
  icc(ratings = icc_data%>%
        filter(codelist==i)%>%
        select(-study_number, -codelist),
      model="twoway",
      type="agreement",
      unit="average")->icc_result
  
  icc_results[[i]]<-icc_result
  
}

# build table with results

icc_results_table<-data_frame()

for (i in drug_groups) {
  
  name<-i
  
  try(
    table<-(data.frame(
      codelist = name,
      pairs = unlist(icc_results[[i]]$subjects),
      icc = unlist(icc_results[[i]]$value),
      CI_lower = unlist(icc_results[[i]]$lbound),
      CI_upper = unlist(icc_results[[i]]$ubound),
      p_value = unlist(icc_results[[i]]$p.value)
    ))
  )
  
  try(icc_results_table%<>%bind_rows(table))
  
  
  rm(table, name, i)
  
}


data.frame(labels="Aggregated",
           pairs = unlist(icc_results[[1]]$subjects),
           icc = unlist(icc_results[[1]]$value),
           CI_lower = unlist(icc_results[[1]]$lbound),
           CI_upper = unlist(icc_results[[1]]$ubound),
           p_value = unlist(icc_results[[1]]$p.value))%>%
bind_rows(icc_results_table%>%
            left_join(codelist_labels%>%select(labels, codelist))%>%
            arrange(labels))%>%
  mutate(across(3:6, ~round(., 2)))%>%
  mutate(p_value=as.character(p_value))%>%
  mutate_when(p_value<0.001,
              p_value=as.character("<0.001"))%>%
  select(labels, pairs, icc, CI_lower, CI_upper, p_value, codelist)%>%
  mutate(CI=paste0(CI_lower, " : ", CI_upper))%>%
  select(-CI_lower, -CI_upper)%>%
  as.data.frame()->icc_results_table
  
icc_results_table%>%
  select('Drug group' = labels,
         Pairs=pairs,
         ICC=icc,
         CI,
         'p-value' = p_value)%>%
  flextable()%>%
  set_caption("Intraclass correlation coefficients for medication initiation after randomisation between the GP and Dispensing datasets")%>%
  width(j=1, width=4)%>%
  width(j=2:5, width=2)%>%
  flextable::font(fontname="Mulish")%>%
  add_footer_lines("Intraclass correlation coefficient calculated based on absolute agreements, and using two-way mixed effects models with averaged measures as reference. Initiation calculated only for patients not at risk, defined as those not identified as taking a drug at randomisation based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset.")%>%
  align(i=NULL, j=2:5, align="center", part="all")->table_flex



sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(table_flex, path = "Outputs/Tables/icc_initiation_all_drugs.docx", pr_section = sect_properties)











































### main drugs only --------------


a%>%
  filter(codelist %in% drugs_paper)%>%
  distinct(codelist, .keep_all = T)->BA_labels

a%>%
  filter(codelist %in% drugs_paper)%>%
  ggplot(aes(avg, diff))+
  geom_point(alpha=0.2)+
  geom_hline(aes(yintercept = mean_diff)) +
  geom_hline(aes(yintercept = lower), color = "red", linetype="dashed") +
  geom_hline(aes(yintercept = upper), color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot for drug initation after randomisation in the GP and Dispensing dataset") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement")+
  facet_wrap(~labels,
             #labeller = label_wrap_gen(width = 30),
  )+
  geom_text(data=BA_labels, aes(x= 150, y=mean_diff, label=paste0("Mean difference:\n", round(mean_diff, 2))), color="black")+
  geom_text(data=BA_labels, aes(x= 150, y=lower*2, label=paste0("Mean difference - 1.96 SD:\n", round(lower, 2))), color="red")+
  geom_text(data=BA_labels, aes(x= 150, y=upper*2, label=paste0("Mean difference + 1.96 SD:\n", round(upper, 2))), color="red")+
  geom_text(data=as.data.frame(icc_results_table%>%filter(codelist %in% drugs_paper, icc>0)), aes(x=50, y=150, label=paste0("ICC: ", icc, " ", CI)))+
  # geom_text(aes(x= 20, y=150, label="GP date before Dispensing date"), color="black")+
  # geom_text(aes(x= 20, y=-150, label="Dispensing date before GP date"), color="black")+
  theme_gray(base_size=10)+
  labs(caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, or no record in the 3 calendar months prior to the month of randomisation for the Dispensing dataset). Differences (vertical axis) calculated  as days after randomisation in GP dataset minus Dispensing dataset. The black horizontal line depicts the mean difference acrosss all observations in each drug group, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 300))+
  theme(legend.position = "bottom")

ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_main_drugs.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")

### additional drugs only --------------


a%>%
  filter(!codelist %in% drugs_paper)%>%
  filter(labels!="Aggregated" & codelist!="Aggregated")%>%
  distinct(codelist, .keep_all = T)->BA_labels

a%>%
  ungroup()%>%
  filter(!codelist %in% drugs_paper)%>%
  filter(labels!="Aggregated" & codelist!="Aggregated")%>%
  ggplot(aes(avg, diff))+
  geom_point(alpha=0.2)+
  geom_hline(aes(yintercept = mean_diff)) +
  geom_hline(aes(yintercept = lower), color = "red", linetype="dashed") +
  geom_hline(aes(yintercept = upper), color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot for drug initation after randomisation in the GP and Dispensing dataset (additional drugs)") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement")+
  facet_wrap(~labels,
             #labeller = label_wrap_gen(width = 30),
  )+
  geom_text(data=BA_labels, aes(x= 150, y=mean_diff, label=paste0("Mean difference:\n", round(mean_diff, 2))), color="black")+
  geom_text(data=BA_labels, aes(x= 150, y=lower*2, label=paste0("Mean difference - 1.96 SD:\n", round(lower, 2))), color="red")+
  geom_text(data=BA_labels, aes(x= 150, y=upper*2, label=paste0("Mean difference + 1.96 SD:\n", round(upper, 2))), color="red")+
  geom_text(data=as.data.frame(icc_results_table%>%filter(!codelist %in% drugs_paper & !labels=="Aggregated", icc>0)), aes(x=50, y=150, label=paste0("ICC: ", icc, " ", CI)))+
  # geom_text(aes(x= 20, y=150, label="GP date before Dispensing date"), color="black")+
  # geom_text(aes(x= 20, y=-150, label="Dispensing date before GP date"), color="black")+
  theme_gray(base_size=10)+
  labs(caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, or no record in the 3 calendar months prior to the month of randomisation for the Dispensing dataset). Differences (vertical axis) calculated  as days after randomisation in GP dataset minus Dispensing dataset. The black horizontal line depicts the mean difference acrosss all observations in each drug group, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 300))+
  theme(legend.position = "bottom")

ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_additional_drugs.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")



rm(BA_labels)



### aggregated plot  -----

mean_diff=mean(a$diff)
lower = mean_diff - 1.96*sd(a$diff)
upper= mean_diff + 1.96*sd(a$diff)


a%>%
  mutate(group = case_when(group=="cv" ~ "Cardiovascular",
                           group=="oth" ~ "Other",
                           group=="resp" ~ "Respiratory"))%>%
  mutate(group = fct_relevel(group, "Cardiovascular", "Respiratory", "Other"))%>%
  ggplot(aes(avg, diff))+
  geom_point(alpha=0.2)+
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  annotate("text", x= 180, y=mean_diff, label=paste0("Mean difference:\n", round(mean_diff, 2)), color="black")+
  annotate("text", x= 180, y=lower, label=paste0("Mean difference - 1.96 SD:\n", round(lower, 2)), color="red")+
  annotate("text", x= 180, y=upper, label=paste0("Mean difference + 1.96 SD:\n", round(upper, 2)), color="red")+
  annotate("text", x= 20, y=150, label="GP date before Dispensing date", color="black")+
  annotate("text", x= 20, y=-150, label="Dispensing date before GP date", color="black")+
  annotate("text", x= 100, y=150, label=paste0("ICC: ", icc_results_table[icc_results_table$labels=="Aggregated", ]$icc, " ", icc_results_table[icc_results_table$labels=="Aggregated", ]$CI), color="black")+
  ggtitle("Bland-Altman Plot for drug initation after randomisation in the GP and Dispensing dataset") +
  ylab("Difference Between Measurements\n(GP - Dispensing)") +
  xlab("Average Measurement")+
  theme_gray(base_size=20)+
  labs(caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, or no record in the 3 calendar months prior to the month of randomisation for the Dispensing dataset). Differences (vertical axis) calculated as days after randomisation in GP dataset minus Dispensing dataset. The black horizontal line depicts the mean difference across all observations, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 180))+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot")

ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_all_aggregated.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")







### plot per groups ------
# 
# a%>%
#   mutate(group = case_when(group=="cv" ~ "Cardiovascular",
#                          group=="oth" ~ "Other",
#                          group=="resp" ~ "Respiratory"))%>%
#   mutate(group = fct_relevel(group, "Cardiovascular", "Respiratory", "Other"))%>%
#   ggplot(aes(avg, diff, color=group))+
#   geom_point(alpha=0.2)+
#   geom_hline(aes(yintercept = mean_diff)) +
#   geom_hline(aes(yintercept = lower), color = "red", linetype="dashed") +
#   geom_hline(aes(yintercept = upper), color = "red", linetype="dashed") +
#   ggtitle("Bland-Altman Plot for drug initation after randomisation in the GP and Dispensing dataset") +
#   ylab("Difference Between Measurements") +
#   xlab("Average Measurement")+
#   facet_wrap(~labels,
#              #labeller = label_wrap_gen(width = 30)
#              )+
#   theme_gray(base_size=10)+
#   labs(color="Drug group",
#        caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, or no record in the 3 calendar months prior to the month of randomisation for the Dispensing dataset). Differences (vertical axis) calculated  as days after randomisation in GP dataset minus Dispensing dataset. The black horizontal line depicts the mean difference across all observations in each drug group, while the dashed lines depict 95% confidence intervals", 300))+
#   theme(legend.position = "bottom")
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_all.png",
#        last_plot(),
#        width=20,
#        height=10,
#        dpi="retina")














## test with different dispensing dates ------



### gp ------

gp_dt%>%
  select(study_number, date, code)%>%
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  filter(date>rand_date)%>% # censor to events happening after randomisation date
  mutate(time_after_rand = as.integer(difftime(date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  mutate(dataset="GP")%>%
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>%
  as_tibble()->x1


### meds ------

meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
  filter(processing_period_date>rand_month)%>% # selecting only events happening after randomisation with some degree of certainty (following calendar month)
  mutate(processing_period_date= ceiling_date(processing_period_date, "month") - 1)%>%
  mutate(time_after_rand = as.integer(difftime(processing_period_date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>%
  as_tibble()->x2

x<-rbind(x1, x2)

rm (x1, x2)


## restrict drug initiation to pts at risk for each category and produce survival dataset


drug_groups <- distinct(participants_not_at_risk, codelist)%>%.[[1]]

survival_data<-data.frame()

for (i in drug_groups) {
  
  
  participants_not_at_risk%>% # list of participants not at risk since already on drug at baseline (for each drug group)
    filter(codelist==i)%>% # trim to group of interest
    select(study_number)%>%
    .[[1]]->participant_not_at_risk_list # extract list of participants already on drug before rand
  
  rand_dates%>%
    filter(NationID=="EN")%>% # people randomised in England
    filter(is.na(Excluded))%>% # not excluded (duplicates, withdrawn consent)
    filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
    filter(!study_number %in% participant_not_at_risk_list)%>% # exclude people on drug at randomisation 
    select(study_number)->participants_at_risk # list of patients at risk
  
  
  
  x%>%
    select(study_number, codelist, time_after_rand, dataset)%>%
    filter(codelist==i)%>% # drug group
    filter(!study_number %in% participant_not_at_risk_list)%>% # of all those with records after randomisation, we're selecting only those who are at risk of starting drug
    filter(time_after_rand<=180)%>% # right censoring at 180 days
    # select(study_number, time_after_rand, dataset)%>%
    right_join(expand.grid(participants_at_risk%>%.[[1]], # this ensures there is a censored entry for each dataset for people with no records (rather than just events) 
                           c("Dispensing", "GP"))%>%
                 rename(study_number=Var1,
                        dataset=Var2)
    )%>%
    arrange(study_number, dataset, time_after_rand)%>%
    group_by(study_number, dataset)%>%
    slice_head(n=1)%>% # select first observation for each dataset
    mutate(event=if_else(!is.na(time_after_rand), 2, 1), # apply event flags (2 if time after rand is not null, 1 if otherwise)
           time_after_rand=ifelse(event==1, 180, time_after_rand), # apply time after rand of 180 if censored
           codelist = i # apply codelist name to censored participants
    )%>%
    select(study_number, time_after_rand, dataset, event, codelist)  -> table
  
  survival_data%<>%bind_rows(table)
  
  rm(table, i)
  
}

# rm(x)

  
  
survival_data%>%  
  filter(event=="2")%>%
  group_by(study_number, codelist)%>%
  filter(!dataset=="Either")%>%
  filter(n_distinct(dataset)>1)->a

a%<>%
  select(study_number, codelist, time_after_rand, dataset)%>%
  pivot_wider(names_from=dataset, values_from=time_after_rand)%>%
  mutate(avg=(GP+Dispensing)/2,
         diff=Dispensing-GP)

a%<>%
  group_by(codelist)%>%
  mutate(mean_diff=mean(diff),
         lower = mean_diff - 1.96*sd(diff),
         upper= mean_diff + 1.96*sd(diff))

a%<>%
  left_join(codelist_labels)



survival_data%>%
  filter(dataset%in%c("GP", "Dispensing"))%>%
  filter(event=="2")%>%
  select(study_number, dataset, time_after_rand, codelist)%>%
  pivot_wider(names_from = dataset, values_from = time_after_rand)%>%
  filter(!is.na(Dispensing) &!is.na(GP))->icc_data

icc(ratings = icc_data%>%select(-study_number, -codelist),
    model="twoway",
    type="agreement",
    unit="average")->icc_result


data.frame(labels="Aggregated",
           pairs = unlist(icc_result$subjects),
           icc = unlist(icc_result$value),
           CI_lower = unlist(icc_result$lbound),
           CI_upper = unlist(icc_result$ubound),
           p_value = unlist(icc_result$p.value))%>%
  mutate(across(3:6, ~round(., 2)))%>%
  mutate(p_value=as.character(p_value))%>%
  mutate_when(p_value<0.001,
              p_value=as.character("<0.001"))%>%
  select(labels, pairs, icc, CI_lower, CI_upper, p_value)%>%
  mutate(CI=paste0("(", CI_lower, " : ", CI_upper, ")"))%>%
  select(-CI_lower, -CI_upper)%>%
  as.data.frame()->icc_results_table


mean_diff=mean(a$diff)
lower = mean_diff - 1.96*sd(a$diff)
upper= mean_diff + 1.96*sd(a$diff)


a%>%
  mutate(group = case_when(group=="cv" ~ "Cardiovascular",
                           group=="oth" ~ "Other",
                           group=="resp" ~ "Respiratory"))%>%
  mutate(group = fct_relevel(group, "Cardiovascular", "Respiratory", "Other"))%>%
  ggplot(aes(avg, diff))+
  geom_point(alpha=0.2)+
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  annotate("text", x= 180, y=mean_diff, label=paste0("Mean difference:\n", round(mean_diff, 2)), color="black")+
  annotate("text", x= 180, y=lower, label=paste0("Mean difference - 1.96 SD:\n", round(lower, 2)), color="red")+
  annotate("text", x= 180, y=upper, label=paste0("Mean difference + 1.96 SD:\n", round(upper, 2)), color="red")+
  annotate("text", x= 20, y=150, label="GP date before Dispensing date", color="black")+
  annotate("text", x= 20, y=-150, label="Dispensing date before GP date", color="black")+
  annotate("text", x= 100, y=150, label=paste0("ICC: ", icc_results_table[icc_results_table$labels=="Aggregated", ]$icc, " ", icc_results_table[icc_results_table$labels=="Aggregated", ]$CI), color="black")+
  ggtitle("Bland-Altman Plot for drug initation after randomisation in the GP and Dispensing dataset") +
  ylab("Difference Between Measurements\n(GP - Dispensing)") +
  xlab("Average Measurement")+
  theme_gray(base_size=20)+
  labs(caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, or no record in the 3 calendar months prior to the month of randomisation for the Dispensing dataset). Differences (vertical axis) calculated as days after randomisation in GP dataset minus Dispensing dataset. The black horizontal line depicts the mean difference across all observations, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 180),
       subtitle="Converting Dispensing date to last date of the processing month (instead of first)")+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot")



ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_all_aggregated_inverted_dispensing_date.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")







## agreement for initiation -----------------

survival_data%>%
  mutate(flag=if_else(event=="2", 1,0))%>%
  select(-event, -time_after_rand)%>%
  pivot_wider(names_from = "dataset", values_from="flag")%>%
  select(-Either)->t



t%>%distinct(study_number)->participants_list

t%>%distinct(study_number)%>%nrow()->participants

#### calculate agreement ----

drug_groups <- t%>%distinct(codelist)%>%.[[1]]

kappa_codelists_participants<-list()

for (i in drug_groups) {
  
  t%>%
    filter(codelist==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, "0")))%>%  
    select(-codelist, -study_number)%>%
    mutate(across(everything(), as.integer))->table
  
  try(k<-Kappa.test(table$GP, table$Dispensing))
  
  if(exists("k"))
  {
    
    kappa_codelists_participants[[i]]<-k
    
  }
  else{
    kappa_codelists_participants[[i]]<-list()
  }
  
  rm(i, table, k)
  
}

kappa_codelists_participants_results<-data.frame()

for (i in drug_groups) {
  
  name<-i
  
  try(
    table<-(data.frame(
      codelist = name,
      kappa = unlist(kappa_codelists_participants[[i]]$Result$estimate),
      CI_lower = unlist(kappa_codelists_participants[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_codelists_participants[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_codelists_participants[[i]]$Result$p.value),
      judgement = unlist(kappa_codelists_participants[[i]]$Judgement)
    ))
  )
  
  try(kappa_codelists_participants_results%<>%bind_rows(table))
  
  
  rm(table, name, i)
  
}


kappa_codelists_participants_results%>%
  right_join(codelist_labels)%>%
  mutate_when(!is.na(CI_lower), 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate_when(p_value==0.00,
              p_value="<0.001")%>%
  
  left_join (
    (t%>%
       group_by(codelist)%>%
       summarise(GP_Dispensing = n_unique(study_number[GP=="1" & Dispensing=="1"]),
                 GP_only = n_unique(study_number[GP=="1" & Dispensing=="0"]),
                 Dispensing_only = n_unique(study_number[GP=="0" & Dispensing=="1"]),
                 nil = participants - GP_Dispensing - GP_only - Dispensing_only)),
    by=c("codelist"))%>%
  arrange(labels)%>%
  select(labels,
         group,
         GP_Dispensing,
         GP_only,
         Dispensing_only,
         nil,
         kappa,
         CI,
         p_value,
         judgement)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))-> codelists_agreement_participants

### produce tables -----
# add total counts and proportions (either dataset)

codelists_agreement_participants%<>%
  mutate(across(3:6, ~replace_na(.x, 0)))%>%
  rowwise() %>%
  mutate(prop = round(sum(across(3:5), na.rm = T)/sum(across(3:6), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either dataset (%)' = paste0(sum(across(3:5), na.rm = T),
                                                       " (",
                                                       prop,
                                                       "%)"))%>%
  mutate(prop1 = round(sum(across(3:3))/sum(across(3:6), na.rm = T)*100,1),
         prop2 = round(sum(across(4:4))/sum(across(3:6), na.rm = T)*100,2),
         prop3 = round(sum(across(5:5))/sum(across(3:6), na.rm = T)*100,1),
         prop4 = round(sum(across(6:6))/sum(across(3:6), na.rm = T)*100,1))%>%
  mutate(prop1=if_else(is.na(prop1), 0, prop1))%>%
  mutate(prop2=if_else(is.na(prop2), 0, prop2))%>%
  mutate(prop3=if_else(is.na(prop3), 0, prop3))%>%
  mutate(prop4=if_else(is.na(prop4), 0, prop4))%>%
  mutate(GP_Dispensing=paste0(GP_Dispensing, " (", prop1, "%)"),
         GP_only=paste0(GP_only, " (", prop2, "%)"),
         Dispensing_only=paste0(Dispensing_only, " (", prop3, "%)"),
         nil=paste0(nil, " (", prop4, "%)"))%>%
  select(-c(prop, prop1, prop2, prop3, prop4))%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%
  rename("Drug group"=labels,
         K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement)%>%
  relocate('Either dataset (%)', .after = 'Drug group')




## add icc

codelists_agreement_participants%<>%
  left_join(icc_results_table, by=c('Drug group'="labels"))%>%
  select(-group, -codelist, -Judgement)%>%
  rename(Pairs=pairs, ICC=icc)%>%
  relocate(CI, .after=ICC)%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))

## bring back codelist groups

codelists_agreement_participants%<>%
  left_join(codelist_labels, by=c("Drug group" = "labels"))%>%
  select(-group)


# flextable

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

codelists_agreement_participants%>%
  select(-codelist, -Pairs)%>%
  rename('Both datasets (%)' = GP_Dispensing,
         'GP dataset  only (%)' = GP_only,
         'Dispensing dataset only (%)'=Dispensing_only,
         'Neither dataset (%)'=nil)%>%
  flextable()%>%
  set_caption("Agreement for medication initiation after randomisation between the GP and Dispensing dataset")%>%
  width(j=1, width=4)%>%
  width(j=2:12, width=1)%>%
  flextable::font(fontname="Mulish")%>%
  set_header_labels(values=list(CI="95% CI", 
                                p_value = "p-value"))%>%
  add_header_row(values = c("","Number of participants with a record", "Time since randomisation"), colwidths = c(1,8,3)) %>%
  add_footer_lines("Initiation calculated only for patients not at risk, defined as those not identified as taking a drug at randomisation based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset. ICC: intraclass correlation coefficient; CI: confidence interval")%>%
  align(i=NULL, j=2:11, align="center", part="all")%>%
  border(i=1, j=2, border.right = fp_border(color="dark grey"), part="header")%>%
  border(i=2, j=9, border.right = fp_border(color="dark grey"), part="header")%>%
  border(i=NULL, j=1, border.right = fp_border(color="dark grey"), part="body")%>%
  border(i=NULL, j=9, border.right = fp_border(color="dark grey"), part="body")->table_flex


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_all_drugs.docx", pr_section = sect_properties)

# main drugs
codelists_agreement_participants%>%
  filter(codelist %in% drugs_paper)%>%
  select(-codelist,-Pairs)%>%
  rename('Both datasets (%)' = GP_Dispensing,
         'GP dataset  only (%)' = GP_only,
         'Dispensing dataset only (%)'=Dispensing_only,
         'Neither dataset (%)'=nil)%>%
  flextable()%>%
  set_caption("Agreement for medication initiation after randomisation between the GP and Dispensing dataset")%>%
  width(j=1, width=4)%>%
  width(j=2:12, width=1)%>%
  flextable::font(fontname="Mulish")%>%
  set_header_labels(values=list(CI="95% CI", 
                                p_value = "p-value"))%>%
  add_header_row(values = c("","Presence or absence of record", "Time since randomisation"), colwidths = c(1,8,3)) %>%
  add_footer_lines("Initiation calculated only for patients not at risk, defined as those not identified as taking a drug at randomisation based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset. ICC: intraclass correlation coefficient; CI: confidence interval")%>%
  align(i=NULL, j=2:12, align="center", part="all")%>%
  border(i=1, j=2, border.right = fp_border(color="dark grey"), part="header")%>%
  border(i=2, j=9, border.right = fp_border(color="dark grey"), part="header")%>%
  border(i=NULL, j=1, border.right = fp_border(color="dark grey"), part="body")%>%
  border(i=NULL, j=9, border.right = fp_border(color="dark grey"), part="body")->table_flex


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_main_drugs.docx", pr_section = sect_properties)







## table with proportions initiating drug within 180 days-------

# survival_data%>%
#   group_by(codelist, dataset)%>%
#   summarise(Participants=n_distinct(study_number[event=="2"]),
#             Prop=round(Participants/n_distinct(study_number)*100,1))%>%
#   ungroup()%>%
#   mutate(count = paste0(Participants, " (", Prop, "%)"))%>%
#   select(-Participants, -Prop)%>%
#   pivot_wider(names_from="dataset", values_from="count")%>%
#   left_join(codelist_labels%>%select(codelist, labels))%>%
#   ungroup()%>%
#   mutate(labels=reorder(labels, labels))%>%
#   arrange(labels)%>%
#   select('Drug group' = labels, Either, GP, Dispensing)->table
# 
# table%<>%
#   flextable()%>%
#   set_caption("Drug initiation after randomisation (as captured in distinct data sources")%>%
#   width(j=1, width=4)%>%
#   width(j=2:4, width=2)%>%
#   add_footer_lines("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, or no record in the 3 calendar months prior to the month of randomisation for the Dispensing dataset.")%>%
#   align(i=NULL, j=1, align="left", part="all")%>%
#   align(i=NULL, j=2:4, align="center", part="all")%>%
#   flextable::font(fontname="Mulish")%>%
#   add_header_row(values=c("", "Dataset"), colwidths = c(1, 3))
# 
# sect_properties <- prop_section(
#   page_size = page_size(orient = "landscape",
#                         width = 15, 
#                         #height = 11.7
#   ),
#   type = "continuous",
#   page_margins = page_mar())
# 
# save_as_docx(table, path = "Outputs/Tables/drug_initiation.docx", pr_section = sect_properties)
# 
# rm(p, a, lower, mean_diff, upper)


## plots of counts lost by not having GP data -----------


t%>%
  mutate(group = as.factor(case_when(GP=="1" & Dispensing=="1" ~ "Both",
                                     GP=="1" & Dispensing=="0" ~ "GP",
                                     GP=="0" & Dispensing=="1" ~ "Dispensing")))%>%
  group_by(codelist, group)%>%
  summarise(participants = n())%>%
  pivot_wider(c(codelist), names_from = "group", values_from = "participants")%>%
  mutate(Either=sum(GP, Dispensing, Both, na.rm = T))%>%
  select(-5)%>%
  pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(codelist_labels)%>%
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
  filter(Participants>0)%>%
  ungroup()->t1
  
t1%>%
  filter(codelist %in% drugs_paper)%>%
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~labels, scales="free",
             labeller = label_wrap_gen(width = 20))+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        strip.text.x = element_text(size = 10),
        axis.text.y=element_text(size=10))+
  labs(title="Number of participants identified as initiating a drug within 6 months after randomisation from each dataset",
       y="Participants",
       fill="",
       caption = str_wrap("Counts were calculated as the number of participants with a record in either dataset, in both datasets, in the Dispensing but not the GP dataset, and in the GP but not the Dispensing dataset. Proportions were calculated using the number of people identified in either dataset as reference.", 200))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=3)+
  scale_y_continuous(expand=expansion(c(0,0.5)))
  


ggsave("Outputs/Figures/baseline_drugs_recovery/drug_initiation_proportions_datasets.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")

t1%>%
  filter(!codelist %in% drugs_paper)%>%
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~labels, scales="free",
             labeller = label_wrap_gen(width = 20))+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        strip.text.x = element_text(size = 10),
        axis.text.y=element_text(size=10))+
  labs(title="Number of participants identified as initiating a drug within 6 months after randomisation from each dataset (additional drugs)",
       y="Participants",
       fill="",
       caption = str_wrap("Counts were calculated as the number of participants with a record in either dataset, in both datasets, in the Dispensing but not the GP dataset, and in the GP but not the Dispensing dataset. Proportions were calculated using the number of people identified in either dataset as reference.", 200))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  geom_text(aes(label=paste0(Participants, " (", Prop, "%)")), vjust=-0.1, size=3)+
  scale_y_continuous(expand=expansion(c(0,0.5)))



ggsave("Outputs/Figures/baseline_drugs_recovery/drug_initiation_proportions_datasets_other_drugs.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")





























# 10. temporal stability -------------------------

## gp data ------

rm(meds_dt)

## extract 59 --------

gp_dt_59 <- gp_dt

rm(gp_dt)

extract_59_date <- "2021-07-27"

## extract 57 ----------

# extract list of excluded participants (duplicates, errors, etc)
excluded<-rand_dates%>%
  filter(Excluded=="Y")%>%
  select(study_number)%>%.[[1]]

# load data and apply initial transformations
gp_dt_57<-read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT08_GDPPR/0057/DP_INT08_GDPPR_Deidentified_2021-08-29_20-35-11.csv", 
             col_types = cols(study_number = col_character(),
                              sex = readr::col_factor(levels = c("0","1")),
                              date_of_death = col_date(format = "%Y-%m-%d"),
                              reporting_period_end_date = col_date(format = "%Y-%m-%d"), 
                              date = col_date(format = "%Y-%m-%d"),
                              record_date = col_date(format = "%Y-%m-%d"),
                              code = col_character(), 
                              value1_condition = col_number(), 
                              value2_condition = col_number(),
                              value1_prescription = col_number(),
                              value2_prescription = col_number()))%>%
  # transform to lazy data.table
  lazy_dt%>%
  
  # trim columns
  select(study_number,
         date,
         record_date,
         lsoa,
         code, 
         value1_prescription,
         value2_prescription,
         gp_system_supplier,
         reporting_period_end_date,
         processed_timestamp)%>%
  
  # apply original row number
  mutate(row_number = row_number()) %>%
  
  # remove excluded participants 
  
  filter(!study_number %in% excluded) %>%

  # remove participants who withdrew consent
  
  filter(!study_number %in% withdrew)

extract_57_date <- "2021-06-29"



## extract 53 --------------

# load data and apply initial transformations
gp_dt_53<-read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT08_GDPPR/0053/DP_INT08_GDPPR_Deidentified_2021-08-29_20-42-48.csv", 
                   col_types = cols(study_number = col_character(),
                                    sex = readr::col_factor(levels = c("0","1")),
                                    date_of_death = col_date(format = "%Y-%m-%d"),
                                    reporting_period_end_date = col_date(format = "%Y-%m-%d"), 
                                    date = col_date(format = "%Y-%m-%d"),
                                    record_date = col_date(format = "%Y-%m-%d"),
                                    code = col_character(), 
                                    value1_condition = col_number(), 
                                    value2_condition = col_number(),
                                    value1_prescription = col_number(),
                                    value2_prescription = col_number()))%>%
  # transform to lazy data.table
  lazy_dt%>%
  
  # trim columns
  select(study_number,
         date,
         record_date,
         lsoa,
         code, 
         value1_prescription,
         value2_prescription,
         gp_system_supplier,
         reporting_period_end_date,
         processed_timestamp)%>%
  
  # apply original row number
  mutate(row_number = row_number()) %>%
  
  # remove excluded participants 
  
  filter(!study_number %in% excluded) %>%
  
  # remove participants who withdrew consent
  
  filter(!study_number %in% withdrew)


extract_53_date <- "2021-05-25"



## monthly counts of entries, participants, and distinct codes in each extract ------


# this needs to be restricted to participants randomised before the most recent randomisation date for participants in extract 53 (as changes to earlier records in later extracts might be simply due to more people being randomised)

participants<- gp_dt_53%>%distinct(study_number)%>%as_tibble()%>%.[[1]]

rand_dates%>%
  filter(study_number %in% participants)%>%
  arrange(desc(rand_date))%>%
  select(rand_date)%>%
  slice_head(n=1)%>%
  .[[1]] -> last_rand_date

# calculations

gp_dt_59%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  group_by(month)%>%
  summarise(Entries=n(),
            Participants=n_distinct(study_number),
            Codes = n_distinct(code))%>%
  mutate(extract="59")%>%
  as_tibble()->counts_59
  
gp_dt_57%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  group_by(month)%>%
  summarise(Entries=n(),
            Participants=n_distinct(study_number),
            Codes = n_distinct(code))%>%
  mutate(extract="57")%>%
  as_tibble()->counts_57

gp_dt_53%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  group_by(month)%>%
  summarise(Entries=n(),
            Participants=n_distinct(study_number),
            Codes = n_distinct(code))%>%
  mutate(extract="53")%>%
  as_tibble()->counts_53

counts<-bind_rows(counts_53, counts_57, counts_59)

rm(counts_53, counts_57, counts_59)



### plots ------

# 2010 onwards

counts%>%
  filter(month>="2010-01-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Codes")))%>%
  ggplot(aes(month, value, color=extract, group=extract))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  facet_wrap(~key, nrow=3, scales="free_y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="red")+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="dark green")+
  labs(x="Month",
       y="Counts",
       title="GP dataset stability across different extracts",
       color="Extract",
       caption="Plot capped before 2010 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in extract 53).
       Vertical dashed lines placed at the date of receipt of each extract:
       Extract 53: 25/05/2021
       Extract 57: 29/06/2021
       Extract 59: 27/07/2021")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"), # template
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1))+
  scale_x_date(date_breaks = "6 months", date_labels = "%B %Y")

# 2018 onwards

counts%>%
  filter(month>="2018-01-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")))%>%
  ggplot(aes(month, value, color=extract, group=extract))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  facet_wrap(~key, ncol=3, scales="free_y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="red")+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="dark green")+
  labs(x="Month",
       y="Counts",
       title="GP dataset stability across different extracts (records with date within since 2018)",
       color="Extract",
       caption="Plot capped before 2018 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in extract 53).
       Vertical dashed lines placed at the date of receipt of each extract:
        Extract 53 (red line): 25/05/2021
        Extract 57 (green line): 29/06/2021
        Extract 59 (blue line): 27/07/2021")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1))+
  scale_x_date(date_breaks = "6 months", date_labels = "%B %Y")

ggsave("Outputs/Figures/baseline_drugs_recovery/temporal_stability_gp.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")

# 2021 onwards

counts%>%
  filter(month>="2021-01-01",
         month<="2022-01-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")))%>%
  ggplot(aes(month, value, color=extract, group=extract))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  facet_wrap(~key, ncol=3, scales="free_y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="red")+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="dark green")+
  labs(x="Month",
       y="Counts",
       title="GP dataset stability across different extracts (records with date within 2021)",
       color="Extract",
       caption="
       Plot capped to entries with date within 2021 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in extract 53).
       Vertical dashed lines placed at the date of receipt of each extract:
        Extract 53 (red line): 25/05/2021
        Extract 57 (green line): 29/06/2021
        Extract 59 (blue line): 27/07/2021")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1),
        panel.grid.minor = element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")

ggsave("Outputs/Figures/baseline_drugs_recovery/temporal_stability_gp_2021.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")






## investigate codes changing in different GP data extracts (IN PROGRESS) ------


gp_dt_59%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  distinct(code, month)%>%
  mutate(extract="59")%>%
  as_tibble()->codes_59

gp_dt_53%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  distinct(code, month)%>%
  mutate(extract="53")%>%
  as_tibble()->codes_53

codes<-rbind(codes_59, codes_53)

rm(codes_53, codes_59)

codes%>%
  filter(month>="2019-01-01" & month <="2021-03-01")%>%
  mutate(flag="1")%>%
  group_by(code, month)%>%
  pivot_wider(names_from="extract", values_from="flag")%>%
  filter(is.na(`53`))%>%
  ungroup()->missing_codes

missing_codes%>%
  left_join(FSN%>%select(conceptId, term), by = c("code"="conceptId"))%>%
  distinct(code, .keep_all=T)%>%
  View()


gp_dt_59%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month>="2019-01-01" & month <="2021-03-01")%>%
  distinct(study_number,code, month)%>%
  mutate(extract="59")%>%
  as_tibble()->codes_59

gp_dt_53%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month>="2019-01-01" & month <="2021-03-01")%>%
  distinct(study_number,code, month)%>%
  mutate(extract="53")%>%
  as_tibble()->codes_53

codes<-rbind(codes_59, codes_53)

















rm(gp_dt_53, gp_dt_57, gp_dt_59, counts)



## meds data -----------------



# this needs to be restricted to participants randomised before the most recent randomisation date for participants in extract 53 (as changes to earlier records in later extracts might be simply due to more people being randomised)



## extract 53 --------------

# load data and apply initial transformations
meds_dt_53<-read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT07_PCAREMEDS/0053/DP_INT07_PCAREMEDS_DEIDENTIFIE_2021-08-29_21-51-01.csv", 
                     col_types = cols(study_number = col_character(),
                                      bsa_prescription_id = col_character(), 
                                      paiddmd_code = col_character(),
                                      prescribeddmd_code = col_character(),
                                      processing_period_date = col_date(format = "%Y-%m-%d")))%>%
  # transform to lazy data.table
  lazy_dt%>%
  
  # apply original row number
  mutate(row_number = row_number()) %>%
  
  # trim columns
  select(study_number, 
         bsa_prescription_id, 
         item_id, 
         prescribed_bnf_code, 
         prescribed_bnf_name, 
         prescribeddmd_code, 
         processing_period_date, 
         prescribed_quantity, 
         row_number)%>%
  
  # remove excluded participants 
  
  filter(!study_number %in% excluded) %>%
  
  # remove participants who withdrew consent
  
  filter(!study_number %in% withdrew)



meds_dt_53%>%
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  rename(month=processing_period_date)%>%
  group_by(month)%>%
  summarise(Entries=n(),
            Participants=n_distinct(study_number),
            Codes = n_distinct(prescribed_bnf_code))%>%
  mutate(extract="53")%>%
  as_tibble()->counts_53


participants<- meds_dt_53%>%distinct(study_number)%>%as_tibble()%>%.[[1]]

rand_dates%>%
  filter(study_number %in% participants)%>%
  arrange(desc(rand_date))%>%
  select(rand_date)%>%
  slice_head(n=1)%>%
  .[[1]] -> last_rand_date


rm(meds_dt_53)

## extract 59 --------

meds_dt_59<-read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT07_PCAREMEDS/0059/DP_INT07_PCAREMEDS_DEIDENTIFIE_2021-08-29_23-34-01.csv", 
                   col_types = cols(study_number = col_character(),
                                    bsa_prescription_id = col_character(), 
                                    paiddmd_code = col_character(),
                                    prescribeddmd_code = col_character(),
                                    processing_period_date = col_date(format = "%Y-%m-%d")))%>%
  # transform to lazy data.table
  lazy_dt%>%
  
  # apply original row number
  mutate(row_number = row_number()) %>%
  
  # trim columns
  select(study_number, 
         bsa_prescription_id, 
         item_id, 
         prescribed_bnf_code, 
         prescribed_bnf_name, 
         prescribeddmd_code, 
         processing_period_date, 
         prescribed_quantity, 
         row_number)%>%
  
  # remove excluded participants 
  
  filter(!study_number %in% excluded) %>%
  
  # remove participants who withdrew consent
  
  filter(!study_number %in% withdrew)


# calculations

meds_dt_59%>%
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  rename(month=processing_period_date)%>%
  group_by(month)%>%
  summarise(Entries=n(),
            Participants=n_distinct(study_number),
            Codes = n_distinct(prescribed_bnf_code))%>%
  mutate(extract="59")%>%
  as_tibble()->counts_59

rm(meds_dt_59)





## extract 57 ----------

# load data and apply initial transformations
meds_dt_57<-read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT07_PCAREMEDS/0057/DP_INT07_PCAREMEDS_DEIDENTIFIE_2021-08-29_23-22-50.csv", 
                     col_types = cols(study_number = col_character(),
                                      bsa_prescription_id = col_character(), 
                                      paiddmd_code = col_character(),
                                      prescribeddmd_code = col_character(),
                                      processing_period_date = col_date(format = "%Y-%m-%d")))%>%
  # transform to lazy data.table
  lazy_dt%>%
  
  # apply original row number
  mutate(row_number = row_number()) %>%
  
  # trim columns
  select(study_number, 
         bsa_prescription_id, 
         item_id, 
         prescribed_bnf_code, 
         prescribed_bnf_name, 
         prescribeddmd_code, 
         processing_period_date, 
         prescribed_quantity, 
         row_number)%>%
  
  # remove excluded participants 
  
  filter(!study_number %in% excluded) %>%
  
  # remove participants who withdrew consent
  
  filter(!study_number %in% withdrew)






meds_dt_57%>%
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date)%>%
  rename(month=processing_period_date)%>%
  group_by(month)%>%
  summarise(Entries=n(),
            Participants=n_distinct(study_number),
            Codes = n_distinct(prescribed_bnf_code))%>%
  mutate(extract="57")%>%
  as_tibble()->counts_57


rm(meds_dt_57)





### aggregate counts across datasets

counts<-bind_rows(counts_53, counts_57, counts_59)

rm(counts_53, counts_57, counts_59)



### plots ------

# 2010 onwards

counts%>%
  filter(month>="2010-01-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Codes")))%>%
  ggplot(aes(month, value, color=extract, group=extract, shape=extract))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  facet_wrap(~key, nrow=3, scales="free_y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="red")+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="dark green")+
  labs(x="Month",
       y="Counts",
       title="Dispensing dataset stability across different extracts",
       color="Extract",
       caption="Plot capped before 2010 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in extract 53).
       Vertical dashed lines placed at the date of receipt of each extract:
       Extract 53: 25/05/2021
       Extract 57: 29/06/2021
       Extract 59: 27/07/2021")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1))+
  scale_x_date(date_breaks = "6 months", date_labels = "%B %Y")

# 2018 onwards

counts%>%
  filter(month>="2018-01-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")))%>%
  ggplot(aes(month, value, group=extract, shape = extract))+
  # geom_point(alpha=0.5)+
  # geom_line(alpha=0.5)+
  geom_bar(aes(fill=extract), stat='identity', position='dodge', 
           # color="black"
           )+
  facet_wrap(~key, ncol=3, scales="free_y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="red")+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="dark green")+
  labs(x="Month",
       y="Counts",
       title="Dispensing dataset stability across different extracts (records with date within since 2018)",
       fill="Extract",
       # shape="Extract",
       caption="Plot capped before 2018 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in extract 53).
       Vertical dashed lines placed at the date of receipt of each extract:
        Extract 53 (red line): 25/05/2021
        Extract 57 (green line): 29/06/2021
        Extract 59 (blue line): 27/07/2021")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1))+
  scale_x_date(date_breaks = "6 months", date_labels = "%B %Y",
               limits=c(as.Date(NA), "2022-01-01"))+
  scale_y_continuous(limits=c(0, NA))

ggsave("Outputs/Figures/baseline_drugs_recovery/temporal_stability_dispensing.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")

# 2021 onwards

counts%>%
  filter(month>="2021-01-01",
         month<="2022-01-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")))%>%
  ggplot(aes(month, value, color=extract, group=extract, shape=extract))+
  # geom_point(alpha=0.5)+
  # geom_line(alpha=0.5)+
  geom_bar(aes(fill=extract), stat='identity', position='dodge', 
           # color="black"
           )+
  facet_wrap(~key, ncol=3, scales="free_y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="red")+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="dark green")+
  labs(x="Month",
       y="Counts",
       title="Dispensing dataset stability across different extracts (records with date within 2021)",
       color="Extract",
       shape="Extract",
       fill="Extract",
       caption="
       Plot capped to entries recorded during 2021 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in extract 53).
       Vertical dashed lines placed at the date of receipt of each extract:
        Extract 53 (red line): 25/05/2021
        Extract 57 (green line): 29/06/2021
        Extract 59 (blue line): 27/07/2021")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1))+
  scale_x_date(limits=c(as.Date("2021-01-01"), "2022-01-01"), date_breaks = "1 month", date_labels = "%B %Y")

ggsave("Outputs/Figures/baseline_drugs_recovery/temporal_stability_dispensing_2021.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")


# plots above with line charts instead of bars



# 2018 onwards

counts%>%
  filter(month>="2018-01-01")%>%
  rename("Unique BNF codes"=Codes)%>%
  pivot_longer(-c(month, extract), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Unique SNOMED codes")))%>%
  ggplot(aes(month, 
             value, 
             color=extract, 
             fill=extract, 
             group=extract, 
             shape=extract,
             size=extract))+
  geom_point(alpha=0.4, stroke=1.5)+
  geom_line(alpha=0.4, size=1)+
  facet_wrap(~key, ncol=3, scales="free_y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="red")+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="dark green")+
  labs(x="Month",
       y="Counts",
       title="Dispensing dataset stability across different extracts (since 2018)",
       fill="Extract",
       shape="Extract",
       color="Extract",
       size="Extract",
       caption="Plot capped before 2018 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in extract 53).
       Vertical dashed lines placed at the date of receipt of each extract:
        Extract 53 (red line): 25/05/2021
        Extract 57 (green line): 29/06/2021
        Extract 59 (blue line): 27/07/2021")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1))+
  scale_x_date(date_breaks = "6 months", date_labels = "%B %Y",
               limits=c(as.Date(NA), "2022-01-01"))+
  scale_y_continuous(limits=c(0, NA))+
  scale_shape_manual(values= c(23, 24, 25))+
  scale_size_manual(values=c(4,3,2))



ggsave("Outputs/Figures/baseline_drugs_recovery/temporal_stability_dispensing_lines.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")

# 2021 onwards

counts%>%
  filter(month>="2021-01-01",
         month<="2022-01-01")%>%
  rename("Unique SNOMED codes"=Codes)%>%
  pivot_longer(-c(month, extract), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Unique SNOMED codes")))%>%
  ggplot(aes(month, 
             value, 
             color=extract, 
             fill=extract, 
             size=extract, 
             group=extract, 
             shape=extract))+
  geom_point(alpha=0.4, stroke=1.5)+
  geom_line(alpha=0.4, size=1)+
  # geom_bar(aes(fill=extract), stat='identity', position='dodge', 
  #          # color="black"
  # )+
  facet_wrap(~key, ncol=3, scales="free_y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="red")+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="dark green")+
  labs(x="Month",
       y="Counts",
       title="Dispensing dataset stability across different extracts (records with date within 2021)",
       color="Extract",
       shape="Extract",
       fill="Extract",
       size="Extract",
       caption="
       Plot capped to entries recorded during 2021 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in extract 53).
       Vertical dashed lines placed at the date of receipt of each extract:
        Extract 53 (red line): 25/05/2021
        Extract 57 (green line): 29/06/2021
        Extract 59 (blue line): 27/07/2021")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1))+
  scale_x_date(limits=c(as.Date("2021-01-01"), "2022-01-01"), date_breaks = "1 month", date_labels = "%B %Y")+
  scale_y_continuous(limits=c(0,NA))+
  scale_shape_manual(values= c(23, 24, 25))+
  scale_size_manual(values=c(4,3,2))

ggsave("Outputs/Figures/baseline_drugs_recovery/temporal_stability_dispensing_2021_lines.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")


















# 11. CDISC alignment ---------------------------

















# thank you come again :)
