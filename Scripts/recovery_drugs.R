# Baseline drugs in both datasets

# 0. code to save workspace image 

# save.image("Workspace/recovery_workspace_image.RData")

# 1. Load libraries -----

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
# library(mmtable2) # to build tables; can be installed as remotes::install_github("ianmoran11/mmtable2")
library(gt) # to build tables
# library(ggsankey) # river plots in ggplot
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
library(gtsummary) # handling html tables; can be installed as remotes::install_github("ddsjoberg/gtsummary")
library(ggside) # for plotting parallel graphs
library(flextable) # for exporting tables to word
library(gtsummary) # alternative method to produce tables
library(officer) # for exporting flextable objects to word
library(lubridate) # dealing with dates
library(survival) # survival analyses
library(survminer) # survival analyses
library(ggfortify) # survival plots
# library(grattantheme) # wrapping lables
library(irr) # for intraclass coefficient calculations
library(haven) # load stata/sas files
library(maps) # for geospatial visualization
library(viridis) # color gradients for plots
source("Tools/Table templates/customtab.R") # some tools to customize flextables
customtab_defaults() 
library(Cairo) # export vector images
library(rvg) # exporting vector images
library(svglite)  # exporting vector images 
library(ggsci) # color palletes for dark backgrounds
library(grDevices) # color handling
library(venneuler)
library(BioVenn)

#  2. Apply system settings ------
# prevent scientific notation in plots
options(scipen=100000) 

# set working directory
setwd("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY")

# hardcode temporary directory within QNAP
tempdir <- function() { "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Temporary directory" }

# save.image("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Workspace/recovery_workspace_image.Rdata")


# default plotting themes

theme_update(text=element_text(family="Mulish"))
update_geom_defaults("text", list(family="Mulish",
                                  size=3))

# Oxpop theme for powerpoint
oxpop_blue_panel<- (
  # dark_mode(
  #theme_fivethirtyeight(base_size = 20))+
  theme(plot.background = element_rect(fill = "transparent", color=NA), # the color argument removes white margin
        panel.background = element_rect(fill = "transparent"), # for ppt BG color use #0d1d41
        
        panel.grid.major = element_line(color = "grey90", size=0.1, linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey90", size=0.1, linetype = "dashed"),
        
        strip.background = element_blank(),
        legend.background = element_blank(),
        
        axis.title.y = element_text(family="Mulish",
                                    color = "white"),
        axis.title.x = element_text(family="Mulish",
                                    color = "white"),
        axis.text.y = element_text(family="Mulish",
                                   color = "white"),
        axis.text.x = element_text(family="Mulish",
                                   color = "white"),
        plot.subtitle = element_text(hjust=0),
        plot.caption = element_text(hjust=0),
        
        strip.text = element_text(size=20, color="white"),
        
        axis.ticks = element_line(color="white"),
        
        text = element_text(family="Mulish",color = "White", size=25),
        panel.border = element_blank()
  )
)

## force dplyr select function as standard -----

select<-dplyr::select

# 3. Load data ----------------------------

## import list of withdrawn participants ------------------
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


## import randomisation dates ----------------

rand_dates <- read_excel("K:/QNAP/RECOVERY-deidentified/Datasets/Randomisation_dates/RECOVERY Participants 2022-01-12.xlsx")


# wrangle these data
rand_dates%<>%
  mutate(rand_date = str_sub(RandDateTime, 1, 10), # extract randomisation date 
         ParticipantID=as.character(ParticipantID))%>% # format study number
  rename(study_number= ParticipantID)

## import list of missing nhs numbers-------

missing_nhs_numbers_feb22 <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/NHS number status/DQ_PID_Issues_Reasons_By_Rever_2022-02-23_23-42-26.csv")%>%
  filter(field_name=="nhs_number",
         reason %in% c("incorrect syntax",
                       "not scottish, but no nhs_number",
                       "not in national Personal Demographics Service"))
missing_nhs_numbers_aug22 <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/NHS number status/DQ_PID_Issues_Reasons_By_Rever_2022-08-05_09-22-25.csv")%>%
  filter(field_name=="nhs_number",
         reason %in% c("incorrect syntax",
                       "not scottish, but no nhs_number",
                       "not in national Personal Demographics Service"))



## Load GP data ----------
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


### remove excluded participants (duplicates, died before rand) --------

excluded<-rand_dates%>%
  filter(Excluded=="Y")%>%
  select(study_number)%>%.[[1]]

gp_dt%>%filter(study_number %in% excluded)%>%as_tibble()%>%nrow() # none 




### remove participants who withdrew consent ----------

gp_dt%<>%
  filter(!study_number %in% withdrew) # 3 participants


## calculate total participants  


gp_dt%>%distinct(study_number)%>%as_tibble()%>%nrow() -> participants_total  # 34175
















## Load Dispensing dataset ----------
## note: extract 59
meds <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT07_PCAREMEDS/0059/DP_INT07_PCAREMEDS_DEIDENTIFIE_2021-08-29_23-34-01.csv", 
                 col_types = cols(study_number = col_character(),
                                  bsa_prescription_id = col_character(), 
                                  paiddmd_code = col_character(),
                                  prescribeddmd_code = col_character(),
                                  processing_period_date = col_date(format = "%Y-%m-%d")))


### remove participants who withdrew consent ---------

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



## Load CRF data ---------------

# version: recovery_2203_1b\version_2022_03_08

baseline_crf <- read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2206_1/version_2022_07_01/adsl.dta")

drugs_crf<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2203_1b/version_2022_03_08/addose.dta")

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
         renal_replacement_crf = rrtfl,
         vte_prophylaxis_group = vteprgr,
         # macrolides = macrofl,
         antiplatelets = antiplfl,
         oral_ac = oralacfl
  )














## Load site geographical data ------

sites <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/RECOVERY sites/RECOVERY Sites 2022-03-29.csv")





## Load RECOVERY drug codelists ------------------
### SNOMED codelists ------------------
file.list <- list.files(path="K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Tools/Codelists/Medications_SNOMED/", full.names=T, pattern = ".xlsx")

df = lapply(file.list, function(i){
  x=read_excel(i, sheet="Codelist")
  x$codelist=i
  x
})

codelists_snomed<-bind_rows(df)

codelists_snomed$codelist<-str_sub(codelists_snomed$codelist,104, -10)

rm(file.list,df)

### ensure no duplicate codes in each list to avoid overcounting

codelists_snomed%<>%
  group_by(codelist)%>%
  distinct(ConceptId)






### BNF codelists------------------

file.list <- list.files(path="K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Tools/Codelists/Medications_BNF/", full.names=T, pattern = ".xlsx")

df = lapply(file.list, function(i){
  x=read_excel(i, sheet="Codelist")
  x$codelist=i
  x
})

codelists_bnf<-bind_rows(df)

codelists_bnf$codelist<-str_sub(codelists_bnf$codelist,101, -10)

rm(df, file.list)


# remove unnecessary fields

codelists_bnf%<>%
  select(-dmd_id, -dmd_name)



### ensure no duplicate codes in each list to avoid overcounting

codelists_bnf%<>%
  group_by(codelist)%>%
  distinct(code)


### BNF chapter codelists ----------------


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




# create a list with drugs of interest for this analysis
drugs_paper <- c("antiplatelets",
                 "oral_anticoagulants",
                 "anti_arrhythmics",
                 "antidyslipidemics",
                 "any_raasi",
                 "sglt2",
                 "diabetes_drugs_not_insulin",
                 "insulin",
                 "ics",
                 "inhaled_bronchodilators",
                 "antibacterials",
                 # "macrolides",
                 "antifungals",
                 "antivirals",
                 "immunosuppressive_drugs",
                 "systemic_steroids",
                 "antidepressants",
                 "antipsychotics")



codelists_bnf%<>%
  filter(codelist %in% drugs_paper)

codelists_snomed%<>%
  filter(codelist %in% drugs_paper)


### Codelist labels  -------


labels<-c("Antiplatelets",
          "Oral anticoagulants",
          "Anti-arrhythmics",
          "Antidyslipidemics",
          "RAAS inhibitors",
          "SGLT2 inhibitors",
          "Insulin",
          "Diabetes drugs (non-insulin)",
          
          "Inhaled corticosteroids",
          "Inhaled bronchodilators",
          
          "Antibacterials",
          # "Macrolides",
          "Antifungals",
          "Antivirals",
          
          "Immunosuppressive drugs (non-steroidal)",
          "Systemic steroids",
          
          "Antidepressants",
          "Antipsychotics")

codelist_labels<-tibble(drugs_paper, labels)%>%
  mutate(labels=factor(labels, levels=labels))%>%
  rename(codelist=drugs_paper)%>%
  arrange(labels)



rm(excluded)


### load GP cluster list to retrieve clusters and code descriptions---------------
gp_cluster_lookup <-read_excel("Tools/GDPPR_Cluster_refset_1000230_20210914.xlsx", 
                                                      col_types = c("skip", "skip", "skip", 
                                                                    "skip", "skip", "skip", "skip", "text", 
                                                                    "text", "skip", "skip", "skip", "skip", 
                                                                    "skip"))











## load SNOMED fully-specified names ------

FSN <- read_csv("Tools/SNOMED terminology/FSN.csv", 
                col_types = cols(id = col_character(), 
                                 effectiveTime = col_skip(), active = col_skip(), 
                                 moduleId = col_skip(), conceptId = col_character(), 
                                 languageCode = col_skip(), typeId = col_skip(), 
                                 caseSignificanceId = col_skip()))


# 4. General features-----------


## participants

participants_total  # 34175

participants_total_meds  # 34073


## rows

gp_dt%>%as_tibble()%>%nrow()
meds_dt%>%as_tibble()%>%nrow()

## dates

gp_dt%>%select(date)%>%arrange(desc(date))%>%slice_head(n=1)
gp_dt%>%select(record_date)%>%arrange(desc(record_date))%>%slice_head(n=1)
gp_dt%>%select(reporting_period_end_date)%>%arrange(desc(reporting_period_end_date))%>%slice_head(n=1)
gp_dt%>%select(processed_timestamp)%>%arrange(desc(processed_timestamp))%>%slice_head(n=1)

meds_dt%>%select(processing_period_date)%>%arrange(desc(processing_period_date))%>%slice_head(n=1)


## randomisation dates

# gpes
gp_dt%>%
  distinct(study_number)%>%
  left_join(rand_dates%>%select(study_number, rand_date), by=c("study_number"))%>%
  select(rand_date)%>%
  arrange(desc(rand_date))%>%
  slice_head(n=1) # 26/07/21

# plot randomisation dates in GPES
gp_dt%>%
  distinct(study_number)%>%
  left_join(rand_dates%>%select(study_number, rand_date), by=c("study_number"))%>%
  select(rand_date)%>%
  count(rand_date)%>%
  as_tibble()%>%
  ggplot(aes(rand_date, n))+
  geom_point()+
  geom_line(aes(group=1))+
  theme(axis.text.x = element_text(angle=90))

# dispensing
meds_dt%>%
  distinct(study_number)%>%
  left_join(rand_dates%>%select(study_number, rand_date), by=c("study_number"))%>%
  select(rand_date)%>%
  arrange(desc(rand_date))%>%
  slice_head(n=1) # 26/07/21



# plot randomisation dates in dispensing
meds_dt%>%
  distinct(study_number)%>%
  left_join(rand_dates%>%select(study_number, rand_date), by=c("study_number"))%>%  select(rand_date)%>%
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


# RESULTS: 1.1 Linkage completeness ------------

## generate flags for each dataset--------


# total RECOVERY participants randomised until last date
rand_dates%>%
  filter(rand_date<=last_date)%>%
  distinct(study_number)%>%
  nrow() # 41217 participants


RECOVERY<-rand_dates%>%
  filter(rand_date<=last_date,
         NationID=="EN",
         is.na(Excluded))%>%
  left_join(baseline_crf%>%select(study_number, age)%>%mutate(study_number=as.character(study_number)))%>%
  filter(age>15)%>%
  select(study_number)%>%
  .[[1]]

RECOVERY%>%length() # 36202 EN RECOVERY participants recruited in the period of interest (i.e. could have had linkage data received), aged 16 or over


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


### restrict CRF to population of interest ------

baseline_crf%<>%
  filter(study_number %in% RECOVERY)


## venn diagram (Figure 1) --------------

list_venn <-list("RECOVERY\nEngland cohort" = RECOVERY,
                 "GP dataset cohort" = GP,
                 "Dispensing dataset cohort"=Dispensing)

venn_plot<-ggvenn(list_venn, 
       # fill_alpha= 0.2,
       set_name_size = 5)+
  theme_void(base_size=25)+
  labs(
    # title="Overlap between the overall RECOVERY population recruited in England\nand those represented in the GP and Dispensing datasets"
    )

venn_plot

### THESIS - FIGURE 1------

ggsave("Outputs/Figures/Thesis/participant_venn_diagram.png",
       device="png",
       dpi="retina",
       width=20,
       height=40,
       units="cm",
       limitsize=F)

ggsave("Outputs/Figures/Thesis/HR/participant_venn_diagram.tiff",
       dpi="retina",
       width=20,
       height=40,
       units="cm",
       limitsize=F)



rm(list_venn)


### proportional venn diagram --------


windowsFonts("Mulish" = windowsFont("Mulish"))

draw.venn(RECOVERY, GP, Dispensing,
          xtitle=NULL,
          ytitle=NULL,
          ztitle=NULL,
          nr_f="Mulish",
          nr_s=2,
          title=NULL,
          subtitle=NULL,
          output="jpg",
          filename="Outputs/Figures/baseline_drugs_recovery/overall_venn.jpg")




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


## summary tables ------


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
  
  mutate(race= na_if(race,"Unknown"))%>%
  mutate(race=fct_relevel(race, "White", "Black", "Asian", "Other", "Mixed"),
         race=factor(race, levels = c("White", "Black", "Asian", "Other", "Mixed")))%>%
  mutate(across(c(race), is.na, .names = "{.col}_missing")) %>%
  relocate(race_missing, .after = "race")%>%
  select(-agegr2)%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     race ~ c("{n} ({p}%)",
                              "{N_miss} ({p_miss})"),
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               sex ~ "Female",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease",
               race_missing ~ "Unknown"),
    value=list(diabetes_crf ~"Yes",
               heart_crf ~"Yes",
               renal_crf ~"Yes",
               lung_crf ~"Yes",
               sex ~ "Female"
               ),
    missing="no")%>%
  modify_column_indent(columns=label, rows=endsWith(variable, "_missing"))->recovery_table

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
  
  mutate(race= na_if(race,"Unknown"))%>%
  mutate(race=fct_relevel(race, "White", "Black", "Asian", "Other", "Mixed"),
         race=factor(race, levels = c("White", "Black", "Asian", "Other", "Mixed")))%>%
  mutate(across(c(race), is.na, .names = "{.col}_missing")) %>%
  relocate(race_missing, .after = "race")%>%
  select(-agegr2)%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     race ~ c("{n} ({p}%)",
                              "{N_miss} ({p_miss})"),
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               sex ~ "Female",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease",
               race_missing ~ "Unknown"),
    value=list(diabetes_crf ~"Yes",
               heart_crf ~"Yes",
               renal_crf ~"Yes",
               lung_crf ~"Yes",
               sex ~ "Female"
    ),
    missing="no")%>%
  modify_column_indent(columns=label, rows=endsWith(variable, "_missing"))->gpes_table



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
  
  mutate(race= na_if(race,"Unknown"))%>%
  mutate(race=fct_relevel(race, "White", "Black", "Asian", "Other", "Mixed"),
         race=factor(race, levels = c("White", "Black", "Asian", "Other", "Mixed")))%>%
  mutate(across(c(race), is.na, .names = "{.col}_missing")) %>%
  relocate(race_missing, .after = "race")%>%
  select(-agegr2)%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     race ~ c("{n} ({p}%)",
                              "{N_miss} ({p_miss})"),
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               sex ~ "Female",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease",
               race_missing ~ "Unknown"),
    value=list(diabetes_crf ~"Yes",
               heart_crf ~"Yes",
               renal_crf ~"Yes",
               lung_crf ~"Yes",
               sex ~ "Female"
    ),
    missing="no")%>%
  modify_column_indent(columns=label, rows=endsWith(variable, "_missing"))->dispensing_table


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
  
  mutate(race= na_if(race,"Unknown"))%>%
  mutate(race=fct_relevel(race, "White", "Black", "Asian", "Other", "Mixed"),
         race=factor(race, levels = c("White", "Black", "Asian", "Other", "Mixed")))%>%
  mutate(across(c(race), is.na, .names = "{.col}_missing")) %>%
  relocate(race_missing, .after = "race")%>%
  select(-agegr2)%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     race ~ c("{n} ({p}%)",
                              "{N_miss} ({p_miss})"),
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               sex ~ "Female",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease",
               race_missing ~ "Unknown"),
    value=list(diabetes_crf ~"Yes",
               heart_crf ~"Yes",
               renal_crf ~"Yes",
               lung_crf ~"Yes",
               sex ~ "Female"
    ),
    missing="no")%>%
  modify_column_indent(columns=label, rows=endsWith(variable, "_missing"))->missing_table


# in GPES but not dispensing

gp_not_meds <- setdiff(GP, Dispensing)

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
  
  mutate(race= na_if(race,"Unknown"))%>%
  mutate(race=fct_relevel(race, "White", "Black", "Asian", "Other", "Mixed"),
         race=factor(race, levels = c("White", "Black", "Asian", "Other", "Mixed")))%>%
  mutate(across(c(race), is.na, .names = "{.col}_missing")) %>%
  relocate(race_missing, .after = "race")%>%
  select(-agegr2)%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     race ~ c("{n} ({p}%)",
                              "{N_miss} ({p_miss})"),
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               sex ~ "Female",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease",
               race_missing ~ "Unknown"),
    value=list(diabetes_crf ~"Yes",
               heart_crf ~"Yes",
               renal_crf ~"Yes",
               lung_crf ~"Yes",
               sex ~ "Female"
    ),
    missing="no")%>%
  modify_column_indent(columns=label, rows=endsWith(variable, "_missing"))->gp_not_meds_table


# in Dispensing but not GPES

meds_not_gp <- setdiff(Dispensing, GP)

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
  
  mutate(race= na_if(race,"Unknown"))%>%
  mutate(race=fct_relevel(race, "White", "Black", "Asian", "Other", "Mixed"),
         race=factor(race, levels = c("White", "Black", "Asian", "Other", "Mixed")))%>%
  mutate(across(c(race), is.na, .names = "{.col}_missing")) %>%
  relocate(race_missing, .after = "race")%>%
  select(-agegr2)%>%
  
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     race ~ c("{n} ({p}%)",
                              "{N_miss} ({p_miss})"),
                     all_categorical() ~ "{n} ({p}%)"),
    label=list(age ~ "Age",
               sex ~ "Female",
               race ~ "Ethnicity",
               diabetes_crf ~ "Diabetes",
               heart_crf ~ "Chronic heart disease",
               renal_crf ~ "Severe renal impairment",
               lung_crf ~ "Chronic lung disease",
               race_missing ~ "Unknown"),
    value=list(diabetes_crf ~"Yes",
               heart_crf ~"Yes",
               renal_crf ~"Yes",
               lung_crf ~"Yes",
               sex ~ "Female"
    ),
    missing="no")%>%
  modify_column_indent(columns=label, rows=endsWith(variable, "_missing"))->meds_not_gp_table



#### merged baseline tables
baseline_table_cohorts <- tbl_merge(list(recovery_table, gpes_table, dispensing_table, gp_not_meds_table, meds_not_gp_table, missing_table),
                                    tab_spanner = c("English cohort", "GP dataset", "Dispensing dataset", "GP dataset but not Dispensing", "Dispensing dataset but not GP", "Non-linked cohort"))

baseline_table_cohorts%>%
  custom_tab_gtsummary(header="Baseline characteristics of the RECOVERY population recruited in England (split by linkage cohort)", footer=NULL)->baseline_table_flex

baseline_table_flex

baseline_table_cohorts

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

### THESIS - TABLE 1 -----



save_as_docx(baseline_table_flex, path = "Outputs/Tables/baseline_table.docx", pr_section = sect_properties)


save_as_docx(baseline_table_cohorts, path = "Outputs/Tables/Thesis/Table 1.docx", 
             pr_section = sect_properties
             )

#### explore age ----
  
baseline_crf%<>%
  mutate(gp_not_dispensing_flag = as.factor(if_else(study_number%in%gp_not_meds, 1, 0)),
         dispensing_not_gp_flag = as.factor(if_else(study_number%in%meds_not_gp, 1, 0)))

baseline_crf%>%
  select(study_number, 
         Age=age, 
         `English cohort`=recovery_flag, 
         `GP dataset` = gpes_flag, 
         `Dispensing dataset`=dispensing_flag,
         `GP dataset but not Dispensing` = gp_not_dispensing_flag,
         `Dispensing dataset but not GP` = dispensing_not_gp_flag,
         `Non-linked cohort`=missing_flag)%>%
  pivot_longer(-c(study_number, Age), names_to="Cohort", values_to = "Flag")%>%
  filter(Flag==1)%>%
  mutate(Cohort=fct_relevel(Cohort, "English cohort", "GP dataset", "Dispensing dataset", "GP dataset but not Dispensing", "Dispensing dataset but not GP", "Non-linked cohort"),
         Cohort=fct_relevel(Cohort, rev))%>%
  
  ggplot(aes(Age, Cohort, fill=Cohort))+
  geom_density_ridges(alpha=0.5, quantile_lines = TRUE, quantiles = 4)+
  # facet_wrap(~Cohort)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        text = element_text(family="Mulish"))+
  labs(title="Age density distributions, per linkage cohort",
       caption="Vertical lines in each density graph show quartiles")
  
ggsave("Outputs/Figures/Thesis/age_distributions_linkage_cohorts.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")


ggsave("Outputs/Figures/Thesis/HR/age_distributions_linkage_cohorts.tiff", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")













### different codes? --------------


# gp but not in meds 
# gp_dt%>%
#   filter(study_number %in% gp_not_meds)%>%
#   count(code)%>%
#   arrange(desc(n))%>%
#   left_join(FSN%>%select(conceptId, term), by=c("code"="conceptId"))%>%
#   as_tibble()->gp_not_meds_codes
# 
# View(gp_not_meds_codes) # codes for BMI, blood tests, BP measurements, smoking and drinking, covid vaccines.. could be healthier people who don't have regular prescriptions
# 
# write_csv(gp_not_meds_codes, "Outputs/Tables/codes_gp_not_dispensing.csv")

# meds but not in gp


# meds_dt%>%
#   filter(study_number %in% meds_not_gp)%>%
#   count(prescribed_bnf_code, prescribed_bnf_name)%>%
#   arrange(desc(n))%>%
#   as_tibble()->meds_not_gp_codes
# 
# View(meds_not_gp_codes) # many different things, but most should be captured by GP extraction (aspirin, statins, amlodipine,clopidogrel, diuretics); also some for PPIs that are not captured
# 
# write_csv(meds_not_gp_codes, "Outputs/Tables/codes_dispensing_not_meds.csv")
# 
# rm(FSN)


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
  rename(`England cohort`=recovery_flag,
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
  mutate(Cohort=factor(Cohort, levels=c("England cohort\nn=36202", 
                                        "GP cohort\nn=34140", 
                                        "Dispensing cohort\nn=33972", 
                                        "Non-linked cohort\nn=858", 
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
        legend.position="bottom",
        legend.key.width = unit(2, 'inch'),
        panel.spacing.x = unit(2, "cm"),
        plot.caption = element_text(size=20,hjust = 0), 
        plot.title.position = "plot", 
        legend.direction = "horizontal",
        # plot.subtitle.position="plot",
        plot.caption.position =  "plot")+
  scale_color_viridis(breaks=seq(0,2000,200),
                      limits=c(0,1600))+
  guides(size="none")+
  labs(
    # title="Geographical distribution of RECOVERY participants recruited in England (split by linkage cohort)",
       # caption = "English cohort: all participants recruited in England\nGP cohort: subset with linked GP data\nDispensing cohort: subset with linked Dispensing data\nNon-linked cohort: subset with no linkage to either dataset\nGP cohort (not in Dispensing): subset with linked GP data but no Dispensing data\nDispensing cohort (not in GP): subset with linked Dispensing data but no GP data"
       )+
  facet_wrap(~as.factor(Cohort),
             ncol=3)
  
map

# ggsave("Outputs/Figures/baseline_drugs_recovery/UK_maps1.png",
#        last_plot(),
#        width=6.79*2.5, 
#        height=6.18*2.5,
#        dpi = "retina")
# 

# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/UK_maps1_side.png",
#        last_plot(),
#        width=6.79*3, 
#        height=5.84*1.5,
#        units="in",
#        dpi = "retina")

ggsave("Outputs/Figures/Thesis/recruitment_maps.png",
       width=6.79*3, 
       height=5.84*1.5,
       units="in",
       dpi = "retina")


ggsave("Outputs/Figures/Thesis/recruitment_maps_vertical_layout.png",
       width=5*3, 
       height=15*1.5,
       units="in",
       scale=1,
       dpi = "retina")



# ggsave("Outputs/Figures/Thesis/HR/recruitment_maps.tiff",
#        last_plot(),
#        width=6.79*3, 
#        height=5.84*1.5,
#        units="in",
#        dpi = "retina")


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

recovery_not_linked <- setdiff(RECOVERY, append(GP, Dispensing))

rand_dates%>%
  select(study_number, rand_date)%>%
  filter(study_number %in% RECOVERY)%>%
  mutate(`English cohort` = if_else(study_number %in% RECOVERY, 1,0),
         `GP dataset` = if_else(study_number %in% GP, 1,0),
         `Dispensing dataset` = if_else(study_number %in% Dispensing, 1,0),
         `Non-linked cohort` = if_else(study_number %in% recovery_not_linked, 1,0),
         `GP dataset but not Dispensing`= if_else(study_number %in% gp_not_meds, 1,0),
         `Dispensing dataset but not GP`= if_else(study_number %in% meds_not_gp, 1,0))%>%
  pivot_longer(-c(study_number, rand_date), names_to = "group", values_to="flag")%>%
  filter(flag!="0")%>%
  mutate(month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")),
         group=as.factor(group))%>%
  group_by(group, month)%>%
  summarise(Participants=n_distinct(study_number))->x

x%>%
  mutate(group=fct_relevel(group, "English cohort", "GP dataset", "Dispensing dataset", "GP dataset but not Dispensing", "Dispensing dataset but not GP", "Non-linked cohort"))%>%
  ggplot(aes(month, Participants, color=group, group=group, shape=group))+
  geom_line()+
  geom_point()+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  theme_gray(base_size=20)+
  theme(panel.grid.minor = element_blank())+
  labs(x="Month of randomisation",
       title=str_wrap("Month of randomisation for RECOVERY participants recruited in England, according to linkage status",200),
       caption="Each point represents aggregated counts of participants randomised per month and belonging to each of the colour-coded groups",
       color="Linkage status",
       shape="Linkage status")+
  theme(legend.position="bottom")




ggsave("Outputs/Figures/Thesis/timeseries_randomisations_per_linkage_status.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")



ggsave("Outputs/Figures/Thesis/HR/timeseries_randomisations_per_linkage_status.tiff", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")







rm(list_venn, recovery_rands, recovery_gp, recovery_meds, GP, Dispensing, x)



## investigate CRF records for non-linked cohort ------

baseline_crf%>%
  select(study_number, antiplatelets, oral_ac)%>%
  filter(study_number %in% missing_participants)%>%
  pivot_longer(-study_number, values_to = "flag", names_to = "drug")%>%
  count(drug, flag)

## check missing nhs_numbers------

baseline_crf%>%
  filter(study_number %in% missing_participants)%>%
  filter(study_number %in% missing_nhs_numbers_feb22$study_number)
  # 241/858 had missing/incorrect numbers

baseline_crf%>%
  filter(study_number %in% missing_participants)%>%
  filter(study_number %in% missing_nhs_numbers_aug22$study_number)
  # later only 38/858



# RESULTS: 1.2 Capture of different drugs (BNF chapters) ------


## timeseries -------

### entry counts -------
# anytime
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



### anytime after 2000
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

### after 2021

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

### June 2018 to May 2021

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






















### participant counts -------
# anytime

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


### anytime after 2000
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

### after 2021

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


### June 2018 till May 2021

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

























## barcharts of entry and participant counts per chapter  ------

### any time ------


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

t%>%
  mutate(labels=reorder(labels, chapter))%>%
  right_join(expand.grid(labels=bnf_chapter_labels%>%select(labels)%>%.[[1]],
                         dataset=c("Dispensing", "GP")))%>%
  mutate(across(c(Entries, Participants, Participants_proportion), ~replace_na(.,0)))%>%
  rename(Dataset=dataset)->transformed_data_bnf_chapter_counts


##### plot -----

bnf_chapters_entries_barchart<-transformed_data_bnf_chapter_counts%>%
  ggplot(aes(labels, Entries, color=Dataset, fill=Dataset))+
  geom_bar(stat='identity', position=position_dodge2(preserve="single", width=0.9))+
  geom_text(data=transformed_data_bnf_chapter_counts%>%filter(labels=="Cardiovascular System"), aes(label=Entries), size=5, position = position_dodge(width = 1.5), vjust=-.2, color="black")+
  geom_text(data=transformed_data_bnf_chapter_counts%>%filter(labels!="Cardiovascular System"), aes(label=Entries), size=5, position = position_dodge(width = 1), vjust=-.2, color="black")+
  oxpop_blue_panel+
  theme_gray(base_size=20)+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        legend.position="none",
        axis.title.y=element_blank())+
  labs(subtitle="Entries")+
  scale_x_discrete(expand=expansion(c(0.05,0.05)))+
  scale_fill_discrete(breaks=as.character(c("Dispensing", "GP")))+
  scale_y_continuous(expand=expansion(c(0,0.1)))


bnf_chapters_participants_barchart<-transformed_data_bnf_chapter_counts%>%
  ggplot(aes(labels, Participants, color=Dataset, fill=Dataset))+
  geom_bar(stat='identity', position=position_dodge2(preserve="single", width=0.9))+
  geom_text(aes(label=paste0(Participants, "\n(", Participants_proportion, "%)")), size=4, position = position_dodge(width = 1), vjust=-.2, color="black")+
  oxpop_blue_panel+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y=element_blank())+
  labs(# subtitle="Participants",
       # x="",
       # caption="Counts computed based on all available data (no time restrictions)"
    )+
  scale_x_discrete(expand=expansion(c(0.05,0.05)))+
  scale_fill_discrete(breaks=as.character(c("Dispensing", "GP")))+
  scale_y_continuous(expand=expansion(c(0,0.5)))

p<-bnf_chapters_entries_barchart/bnf_chapters_participants_barchart+
  plot_annotation(title="Counts of entries and participants per BNF chapter and per dataset")&
  theme(plot.title = element_text(size=20),
        plot.background = element_blank())


p

ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_counts_vertical.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")

#### poster -------


transformed_data_bnf_chapter_counts%>%
  ggplot(aes(labels, Participants, color=Dataset, fill=Dataset))+
  #geom_col(position=position_dodge(preserve="single", width=0.1), width=0.5)+
  geom_col(position=position_dodge(preserve="single", width=0.5), width=0.5)+
  
  geom_text(aes(label=paste0(Participants, "\n(", round(Participants_proportion, 0), "%)")), size=12*0.36, position = position_dodge(width = 1.3), vjust=-.2, color="white")+
  oxpop_blue_panel+
  # theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust=1, 
                                   # vjust=-0.01
                                   ),
        axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        text=element_text(size=20)
        )+
  labs(# subtitle="Participants",
    # x="",
    # caption="Counts computed based on all available data (no time restrictions)"
  )+
  scale_x_discrete(labels=function(x) str_wrap(x, width = 30), 
                   #expand=expansion(c(0.05,0.05))
                   )+
  scale_fill_manual(breaks=as.character(c("GP", "Dispensing")), values=c("#A05935", "#0D6379"))+
  scale_color_manual(breaks=as.character(c("GP", "Dispensing")), values=c("#A05935", "#0D6379"))+
  # scale_color_tron()+
  # scale_fill_tron()+
  scale_y_continuous(expand=expansion(c(0,0.5)))

ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_counts_vertical_participants_poster.png",
       width = 42,
       height = 15,
       units="cm",
       dpi="retina",
       # type="cairo-png"
       )

#### slides -----

transformed_data_bnf_chapter_counts%>%
  ggplot(aes(labels, Participants, color=Dataset, fill=Dataset))+
  geom_bar(stat='identity', position=position_dodge2(preserve="single", width=0.9))+
  geom_text(aes(label=paste0(Participants, "\n(", round(Participants_proportion,0), "%)")), size=5, position = position_dodge(width = 1), vjust=-.2, color="white")+
  oxpop_blue_panel+
  # theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust=1),
        axis.title.x = element_blank(),
        # axis.title.y=element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(
          # t = 20,  # Top margin
          # r = 10,  # Right margin
          # b = 40,  # Bottom margin
          l = 50)  # Left margin
        )+
  labs(
       # title="Number of participants identified for each BNF chapter, per dataset",
    y="Participants (%)",
    # caption="Counts computed based on all available data (no time restrictions)"
  )+
  scale_x_discrete(expand=expansion(c(0.05,0.05)))+
  scale_fill_discrete(breaks=as.character(c("Dispensing", "GP")))+
  scale_y_continuous(expand=expansion(c(0,0.5)))

ggsave("Outputs/Figures/ictmc_slides/bnf_chapter_counts_vertical_slides.png", 
       last_plot(),
       width=65, 
       height=30,
       units="cm",
       dpi = "retina")
# 
# # HORIZONTAL LINES
# transformed_data_bnf_chapter_counts%>%
#   mutate(labes=fct_rev(labels))%>%
#   ggplot(aes(labels, Participants, color=Dataset, fill=Dataset))+
#   geom_bar(stat='identity', position=position_dodge2(preserve="single", width=0.9))+
#   geom_text(aes(label=paste0(Participants, " (", Participants_proportion, "%)")), size=5, position = position_dodge(width = 1), hjust=-.2, color="white")+
#   oxpop_blue_panel+
#   # theme_gray(base_size=20)+
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 30, hjust=1),
#         axis.title.x = element_blank(),
#         # axis.title.y=element_blank(),
#         plot.background = element_blank(),
#         plot.margin = margin(
#           # t = 20,  # Top margin
#           # r = 10,  # Right margin
#           # b = 40,  # Bottom margin
#           l = 50)  # Left margin
#   )+
#   labs(
#     # title="Number of participants identified for each BNF chapter, per dataset",
#     y="Participants (%)",
#     x="Chapter"
#     # caption="Counts computed based on all available data (no time restrictions)"
#   )+
#   scale_x_discrete(expand=expansion(c(0.05,0.05)),
#                    limits=rev)+
#   scale_fill_discrete(breaks=as.character(c("Dispensing", "GP")))+
#   scale_y_continuous(expand=expansion(c(0,0.5)))+
#   coord_flip()
# 
# 
# ggsave("Outputs/Figures/ictmc_slides/bnf_chapter_counts_slides.png", 
#        last_plot(),
#        width=45, 
#        height=27,
#        units="cm",
#        dpi = "retina")





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

rm(x,x1, x2, p, p1, p2, t, t1, t2)




###  2018 to 2021 -----


gp_dt%>%
  inner_join(bnf_chapters, by=c("code" = "dmd_id"))%>%
  select(study_number, code, chapter, date, record_date, row_number)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month>="2018-06-01",
         month<="2021-05-01")%>%
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
  rename(month=processing_period_date)%>%  
  filter(month>= "2018-06-01",
         month<= "2021-05-01")%>%  
  as_tibble()%>%
  group_by(chapter)%>%
  summarise(Entries=n(),
            Participants = n_distinct(study_number),
            Participants_proportion = round(n_distinct(study_number)/participants_total_meds*100, 1))%>%
  left_join(bnf_chapter_labels, by=c("chapter"))%>%
  mutate(dataset="Dispensing")->x1


t<-rbind(x,x1)

t%>%
  mutate(labels=reorder(labels, chapter))%>%
  right_join(expand.grid(labels=bnf_chapter_labels%>%select(labels)%>%.[[1]],
                         dataset=c("Dispensing", "GP")))%>%
  mutate(across(c(Entries, Participants, Participants_proportion), ~replace_na(.,0)))%>%
  rename(Dataset=dataset)->transformed_data_bnf_chapter_counts


###### plot ----

bnf_chapters_entries_barchart<-transformed_data_bnf_chapter_counts%>%
  ggplot(aes(labels, Entries, color=Dataset, fill=Dataset))+
  geom_bar(stat='identity', position=position_dodge2(preserve="single", width=0.9))+
  geom_text(data=transformed_data_bnf_chapter_counts%>%filter(labels=="Cardiovascular System"), aes(label=Entries), size=5, position = position_dodge(width = 1.5), vjust=-.2, color="black")+
  geom_text(data=transformed_data_bnf_chapter_counts%>%filter(labels!="Cardiovascular System"), aes(label=Entries), size=5, position = position_dodge(width = 1), vjust=-.2, color="black")+
  oxpop_blue_panel+
  # theme_gray(base_size=20)+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        legend.position="none",
        axis.title.y=element_blank())+
  labs(subtitle="Entries")+
  scale_x_discrete(expand=expansion(c(0.05,0.05)))+
  scale_fill_discrete(breaks=as.character(c("Dispensing", "GP")))+
  scale_y_continuous(expand=expansion(c(0,0.1)))


bnf_chapters_participants_barchart<-transformed_data_bnf_chapter_counts%>%
  ggplot(aes(labels, Participants, color=Dataset, fill=Dataset))+
  geom_bar(stat='identity', position=position_dodge2(preserve="single", width=0.9))+
  geom_text(aes(label=paste0(Participants, "\n(", Participants_proportion, "%)")), size=5, position = position_dodge(width = 1), vjust=-.2, color="black")+
  oxpop_blue_panel+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y=element_blank())+
  labs(# subtitle="Participants",
    # x="",
    # caption="Counts computed based on all available data (no time restrictions)"
  )+
  scale_x_discrete(expand=expansion(c(0.05,0.05)))+
  scale_fill_discrete(breaks=as.character(c("Dispensing", "GP")))+
  scale_y_continuous(expand=expansion(c(0,0.5)))

p<-bnf_chapters_entries_barchart/bnf_chapters_participants_barchart+
  plot_annotation(title="Counts of entries and participants per BNF chapter and per dataset")&
  theme(plot.title = element_text(size=20),
        plot.background = element_blank())


p

ggsave("Outputs/Figures/baseline_drugs_recovery/bnf_chapter_counts_vertical.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")

###### slides -----

transformed_data_bnf_chapter_counts%>%
  ggplot(aes(labels, Participants, color=Dataset, fill=Dataset))+
  geom_bar(stat='identity', position=position_dodge2(preserve="single", width=0.9))+
  # geom_text(aes(label=paste0(Participants, "\n(", round(Participants_proportion,0), "%)")), size=5, position = position_dodge(width = 1), vjust=-.2, color="white")+
  oxpop_blue_panel+
  # theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust=1),
        axis.title.x = element_blank(),
        # axis.title.y=element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(
          # t = 20,  # Top margin
          # r = 10,  # Right margin
          # b = 40,  # Bottom margin
          l = 50)  # Left margin
  )+
  labs(
    # title="Number of participants idenfitied for each BNF chapter, per dataset",
    y="Participants (%)",
    # caption="Counts computed based on all available data (no time restrictions)"
  )+
  scale_x_discrete(expand=expansion(c(0.05,0.05)))+
  scale_fill_discrete(breaks=as.character(c("Dispensing", "GP")))+
  scale_y_continuous(expand=expansion(c(0,0.5)))

ggsave("Outputs/Figures/ictmc_slides/bnf_chapter_counts_vertical_slides.png", 
       last_plot(),
       width=65, 
       height=30,
       units="cm",
       dpi = "retina")


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

ggsave("Outputs/Figures/Thesis/distinct_codes_bnf_chapters.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")



### summary stats in table
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



### plot (ridges)

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



### table


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
  theme(legend.position="right",
        plot.caption = element_text(size=15))+  
  labs(y="Participants (%)",
       title="Proportions of participants with distinct average time intervals between consecutive entries",
       subtitle="(per BNF chapter and dataset)",
       x="Timeframe (in days)",
       fill="Dataset",
       color="Dataset",
       caption="Time intervals calculated as the median day difference between consecutive entries with the same BNF code for the same participant\nDuplicate entries (same BNF code, same participant, same date) were excluded to avoid counting of zero-day intervals between those\nProportions refer to total number of participants with an entry for each BNF chapter and dataset at any point in time",
)+
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





## agreement ------

#### wrangling

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


#### calculate agreement



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


#### table (flextable

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
  custom_tab(header="Agreement between the GP and Dispensing dataset for each BNF chapter (for records between June 2018 and May 2021)",
             footer="CI: confidence interval")%>%
  width(j=1, width=4)%>%
  width(j=2:10, width=1)%>%
  flextable::font(fontname="Mulish")%>%
  set_header_labels(values=list(CI="95% CI", 
                                p_value = "p-value"))%>%
  align(i=NULL, j=2:10, align="center", part="all")->table_flex

save_as_docx(table_flex, path = "Outputs/Tables/agreement_bnf_chapters_2018_2021.docx", pr_section = sect_properties)


rm(bnf_agreement_participants, kappa_bnf_participants, kappa_bnf_participants_results, participants, participants_list, table_flex)




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



t%>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, chapter, month), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, chapter)%>%
  mutate(study_number=as.numeric(study_number))%>%
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
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))%>%
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


### participants in each dataset (barplot) ------

t%>%
  select(-month)%>%
  distinct(study_number, chapter, dataset)%>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, chapter), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"),
         Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, chapter)%>%
  mutate(study_number=as.numeric(study_number))%>%
  mutate(group = as.factor(case_when(GP=="1" & Dispensing=="1" ~ "Both",
                                     GP=="1" & Dispensing=="0" ~ "GP",
                                     GP=="0" & Dispensing=="1" ~ "Dispensing")))%>%
  group_by(chapter, group)%>%
  summarise(participants = n())%>%
  pivot_wider(chapter, names_from = "group", values_from = "participants")%>%
  mutate(Either=sum(GP, Dispensing, Both, na.rm = T))%>%
  pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(bnf_chapter_labels)%>%
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))%>%
  ungroup()%>%
  mutate(labels=as.factor(reorder(labels, chapter)))%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
  ungroup()%>%
  mutate(labels=reorder(labels, chapter))%>%
  
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5,
            color="black")+
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
       title="Number of participants captured in each dataset, per BNF chapter",
       caption="Proportions presented are in reference to the number of participants identified using either dataset. Calculations restricted to the time period between June 2018 and May 2021"
  )

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_bnf_chapters.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")
















### investigate presence of participants in GP but not dispensing along time

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


  

# RESULTS: 1.2 Capture of drugs of interest ------

## Generate summary table





### GP dataset

#### counts and quantity prescribed

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
  


#### interval between entries
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

#### entries per month
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

#### aggregation

t1<-x%>%
  left_join(x1, by=c("codelist"))%>%
  left_join(x2, by=c("codelist"))

t1%<>%
  right_join(codelist_labels, by=c("codelist"))

t1%<>%mutate(Dataset="GP")

t1%<>%rename(Average_quantity_prescribed = Value1_prescription_median)

rm(x, x1, x2)







### Meds dataset

##### general counts and quantity

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

#### day difference between consecutive entries
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

#### entries per event
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


#### aggregation

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
  select(Dataset, labels, codelist, Entries, Participants, entries_no_duplicates, Participants_proportion, avg_entries_per_month, Average_entries_per_participant, Average_quantity_prescribed, avg_day_interval)

joint_table%<>%
  mutate(across(c(Entries, Participants, Participants_proportion), ~replace_na(.,0)))%>%
  mutate(across(c(avg_entries_per_month, Average_entries_per_participant, Average_quantity_prescribed, avg_day_interval), ~replace_na(as.character(.),"N/A")))


write_csv(joint_table, "Outputs/Tables/summary_table_drug_groups.csv")





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



### plots (counts) --------

specific_drugs_entries_barchart<-
  joint_table%>%
  mutate(labels=reorder(labels, labels))%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  ggplot(aes(labels, Entries, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Entries), size=4.5, position = position_dodge(width = 1.1), vjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  scale_y_continuous(expand=expansion(c(0,0.3)))

specific_drugs_participants_barchart<-
  joint_table%>%
  mutate(labels=reorder(labels, labels))%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=paste0(Participants, "\n(", Participants_proportion, "%)")), size=4.5, position = position_dodge(width = .9), vjust=-.1)+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=30, hjust=1),
        axis.title.x = element_blank())+
  scale_y_continuous(expand=expansion(c(0,0.4)))

p<-specific_drugs_entries_barchart/specific_drugs_participants_barchart+plot_annotation(title="Participant and entry counts per drug group",
                         subtitle="For the period between June 2018 and May 2021")&
  theme(plot.title = element_text(size=30),
        plot.subtitle = element_text(size=20))

p

ggsave("Outputs/Figures/baseline_drugs_recovery/counts_vertical.png",
       last_plot(),
       width=25,
       height=10,
       dpi="retina")

rm(p1,p2,p)



##### focus on no duplicate entries ----


p1<-joint_table%>%
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


#### slides ------

p<-
  joint_table%>%
  filter(codelist!="macrolides")%>%
  ungroup()%>%
  mutate(group=case_when(codelist%in% c("anti_arrhythmics",
                                        "antidyslipidemics",
                                        "antiplatelets",
                                        "any_raasi",
                                        "diabetes_drugs_not_insulin",
                                        "ics",
                                        "insulin",
                                        "oral_anticoagulants",
                                        "sglt2") ~ "Cardiovascular",
                         codelist %in% c("inhaled_bronchodilators",
                                         "ics") ~ "Respiratory",
                         codelist %in% c("antibacterials",
                                         "antifungals",
                                         "antivirals"
                                         #,  "macrolides"
                                         ) ~ "Antimicrobials",
                         codelist %in% c("immunosuppressive_drugs",
                                         "systemic_steroids") ~ "Immunosuppression",
                         codelist %in% c("antipsychotics",
                                         "antidepressants") ~ "Mental health"
                         ))%>%
  select(labels, group,Entries, Participants, Participants_proportion, Dataset)%>%
  # mutate(labels=reorder_within(labels, labels, group))%>%
  ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  # geom_text(aes(label=paste0(Participants, "\n(", Participants_proportion, "%)")), size=4.5, position = position_dodge(width = .9), vjust=-.1, color="white")+
  theme_gray(base_size=20)+
  # facet_wrap(~as.factor(group), scales="free_x", ncol=5)+
  oxpop_blue_panel+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=30, hjust=1),
        axis.title.x = element_blank())+
  scale_y_continuous(expand=expansion(c(0,0.4)))+
  # scale_x_reordered()+
  scale_x_discrete(limits=labels)


p

ggsave("Outputs/Figures/ictmc_slides/drug_groups_counts_slides.png",
       last_plot(),
       width=65, 
       height=30,
       units="cm",
       dpi = "retina")







### timeseries (entries) -----------


gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Entries = n())%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
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





### timeseries (participants) -----------



gp_dt%>%
  left_join(codelists_snomed, by =c("code" = "ConceptId"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  group_by(codelist, month)%>%
  summarise(Participants = n_distinct(study_number))%>%
  mutate(dataset="GP")%>%
  as_tibble()%>%
  mutate(month=as.Date(month))->x1

meds_dt%>%
  left_join(codelists_bnf, by =c("prescribed_bnf_code" = "code"))%>%
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


codelists_agreement_participants%>%
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





## unique codes --------

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


t%>%
  pivot_longer(-c(study_number, codelist, labels), names_to="key", values_to = "value")%>%
  ggplot(aes(value, fill=key))+
  geom_boxplot(aes(y=key), alpha=0.5)+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  labs(
    # title="Boxplots depicting number of distinct codes per participant",
      #  subtitle="SNOMED codes in the GP dataset, and SNOMED and BNF codes in the Dispensing dataset",
       y="",
       x="Number of distinct codes",
       fill="",
       #caption = "Plots capped at 15. 
       # Boxplots depict quartile 1, quartile 2(median), and quartile 3, with whiskers spanning to 1.5 times the interquartile range either below quartile 1 or above quartile 3. 
       # Points depict outliers beyond these margins
       # "
       )+
  scale_fill_discrete(limits= c("bnf_dispensing", "snomed_dispensing", "snomed_gp"),
                      labels = c("BNF codes (Dispensing dataset)", "SNOMED codes (Dispensing dataset)", "SNOMED codes (GP dataset)"))+
  scale_x_continuous(breaks=seq(1,15, by=2), 
                     limits=c(1,15)
  )

ggsave("Outputs/Figures/Thesis/distinct_codes_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")







## combined participant counts plot (BNF chapters and specific drugs) -----


transformed_data_bnf_chapter_counts%>%
  ggplot(aes(labels, Participants, color=Dataset, fill=Dataset))+
  geom_bar(stat='identity', position=position_dodge2(preserve="single", width=0.9))+
  geom_text(aes(label=paste0(Participants, "\n(", Participants_proportion, "%)")), size=4, position = position_dodge(width = 1), vjust=-.2, color="black")+
  oxpop_blue_panel+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y=element_blank())+
  labs(# subtitle="Participants",
    # x="",
    # caption="Counts computed based on all available data (no time restrictions)"
  )+
  scale_x_discrete(expand=expansion(c(0.05,0.05)))+
  scale_fill_discrete(breaks=as.character(c("Dispensing", "GP")))+
  scale_y_continuous(expand=expansion(c(0,0.5)))+
  theme(legend.position = "none", 
        text=element_text(color="black", family="Mulish"),
        plot.margin = unit(c(0,0,0,1), "cm"))+
  labs(y=NULL,
       subtitle="BNF chapters")+
  scale_x_discrete(expand=expansion(c(0.05,0)))


ggsave("Outputs/Figures/Thesis/bnf_participants_barchart.png",
       width=60,
       height=30,
       units = "cm",
       dpi="retina")




joint_table%>%
  mutate(labels=reorder(labels, labels))%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  ggplot(aes(labels, Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
    geom_text(
    data=.%>%group_by(labels, Dataset),
            aes(label=paste0(Participants, " (", Participants_proportion, "%)"),
                y = 25000,
                ), 
            size=4, 
            position = position_dodge(width = 0.9), 
            #vjust=-.2, 
            hjust=1,
            color="black")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=30, hjust=1),
        axis.title.x = element_blank())+
  scale_y_continuous(expand=expansion(c(0,0.4)))+
  theme(text=element_text(family="Mulish", color="black"))+
  labs(y=NULL,
       subtitle="Specific drug groups")

ggsave("Outputs/Figures/Thesis/specific_drugs_participants_barchart.png",
       last_plot(),
       width=60,
       height=30,
       units = "cm",
       dpi="retina")



##### alternative plots (horizontal) --------


p1<-transformed_data_bnf_chapter_counts%>%
  ggplot(aes(fct_rev(labels), Participants, color=Dataset, fill=Dataset))+
  geom_bar(stat='identity', position=position_dodge2(preserve="single", width=0.9))+
  geom_text(
    data=.%>%group_by(labels, Dataset),
            aes(label=paste0(Participants, " (", Participants_proportion, "%)"),
                y = 25000,
                ), 
            size=4, 
            position = position_dodge(width = 0.9), 
            #vjust=-.2, 
            hjust=1,
            color="black")+
  oxpop_blue_panel+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        # axis.title.x = element_blank(),
        axis.title.y=element_blank()
        )+
  labs(# subtitle="Participants",
    # x="",
    # caption="Counts computed based on all available data (no time restrictions)"
  )+
  scale_fill_discrete(breaks=as.character(c("Dispensing", "GP")))+
  # scale_y_continuous(expand=expansion(c(0,0.5)))+
  theme(legend.position = "none", 
        text=element_text(color="black", family="Mulish"),
        plot.margin = unit(c(0,0,0,1), "cm"))+
  labs(y=NULL,
       subtitle="BNF chapters")+
  # scale_x_discrete(expand=expansion(c(0.05,0)))+
  coord_flip()


ggsave("Outputs/Figures/Thesis/bnf_participants_barchart_vertical.png",
       width=60,
       height=30,
       units = "cm",
       dpi="retina")




p2<-joint_table%>%
  mutate(labels=reorder(labels, labels))%>%
  select(labels, Entries, Participants, Participants_proportion, Dataset)%>%
  
  ggplot(aes(fct_rev(labels), 
             Participants, fill=Dataset, group=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(
    data=.%>%group_by(labels, Dataset),
    aes(label=paste0(Participants, " (", Participants_proportion, "%)"),
        y = 25000,
    ), 
    size=4, 
    position = position_dodge(width = 0.9), 
    #vjust=-.2, 
    hjust=1,
    color="black")+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        # axis.title.x = element_blank(),
        axis.title.y=element_blank()
        )+
  # scale_y_continuous(expand=expansion(c(0,0.4)))+
  theme(text=element_text(family="Mulish", color="black"))+
  labs(y=NULL,
       subtitle="Specific drug groups")+
  coord_flip()

ggsave("Outputs/Figures/Thesis/specific_drugs_participants_barchart_vertical.png",
       last_plot(),
       width=60,
       height=30,
       units = "cm",
       dpi="retina")

p1/p2

ggsave("Outputs/Figures/Thesis/drugs_participants_barchart_vertical.png",
       last_plot(),
       width=60,
       height=60,
       units = "cm",
       dpi="retina")



# RESULTS: 1.3 temporal stability -------------------------

## gp data ------

### extract 59

extract_59_date <- "2021-07-27"

### extract 57

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



### extract 53

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



### monthly counts of entries, participants, and distinct codes in each extract

# this needs to be restricted to participants randomised before the most recent randomisation date for participants in extract 53 (as changes to earlier records in later extracts might be simply due to more people being randomised)

participants<- gp_dt_53%>%distinct(study_number)%>%as_tibble()%>%.[[1]]

rand_dates%>%
  filter(study_number %in% participants)%>%
  arrange(desc(rand_date))%>%
  select(rand_date)%>%
  slice_head(n=1)%>%
  .[[1]] -> last_rand_date_53_53

# calculations

gp_dt%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
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
  filter(rand_date<=last_rand_date_53_53)%>%
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
  filter(rand_date<=last_rand_date_53_53)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  group_by(month)%>%
  summarise(Entries=n(),
            Participants=n_distinct(study_number),
            Codes = n_distinct(code))%>%
  mutate(extract="53")%>%
  as_tibble()->counts_53

counts_gp<-bind_rows(counts_53, counts_57, counts_59)%>%
  mutate(Dataset = "GP")

rm(counts_53, counts_57, counts_59)




### investigate codes changing in different GP data extracts  ------

# any time 
gp_dt%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  distinct(code, month)%>%
  mutate(extract="59")%>%
  as_tibble()->codes_59

gp_dt_53%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  distinct(code, month)%>%
  mutate(extract="53")%>%
  as_tibble()->codes_53

codes<-rbind(codes_59, codes_53)

rm(codes_53, codes_59)

# restricting to 2019-2021
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

gp_dt%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month>="2019-01-01" & month <="2021-03-01")%>%
  distinct(study_number,code, month)%>%
  mutate(extract="59")%>%
  as_tibble()->codes_59

gp_dt_53%>%
  select(study_number, code, date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month>="2019-01-01" & month <="2021-03-01")%>%
  distinct(study_number,code, month)%>%
  mutate(extract="53")%>%
  as_tibble()->codes_53

codes<-rbind(codes_59, codes_53)

rm(codes_53, codes_59)




### quantify stability in one time point (April 2021)-------


gp_dt%>%
  select(study_number, code, date, record_date, gp_system_supplier, lsoa, reporting_period_end_date, processed_timestamp, row_number)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  filter(date<=extract_53_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month=="2021-04-01")%>%
  mutate(extract="59")%>%
  as_tibble()->codes_59


gp_dt_57%>%
  select(study_number, code, date, record_date, gp_system_supplier, lsoa, reporting_period_end_date, processed_timestamp, row_number)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  filter(date<=extract_53_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month=="2021-04-01")%>%
  mutate(extract="57")%>%
  as_tibble()->codes_57


gp_dt_53%>%
  select(study_number, code, date, record_date, gp_system_supplier, lsoa, reporting_period_end_date, processed_timestamp, row_number)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  filter(date<=extract_53_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month=="2021-04-01")%>%
  mutate(extract="53")%>%
  as_tibble()->codes_53

# simplify to study number, code, and date only for merging (along with row_number as unique identifier within extract)
codes_simplified_59<-codes_59%>%select(study_number_59 = study_number, code_59 = code, date_59 = date, row_number_59 = row_number)
codes_simplified_57<-codes_57%>%select(study_number_57 = study_number, code_57 = code, date_57 = date, row_number_57 = row_number)  
codes_simplified_53<-codes_53%>%select(study_number_53 = study_number, code_53 = code, date_53 = date, row_number_53 = row_number)


# merge codes from the different extracts
codes_merged<-codes_simplified_53%>%
  full_join(codes_simplified_57, by=c("study_number_53"="study_number_57",
                                      "code_53"="code_57",
                                      "date_53"="date_57"))%>%
  full_join(codes_simplified_59, by=c("study_number_53"="study_number_59",
                                      "code_53"="code_59",
                                      "date_53"="date_59"))%>%
  rename(study_number = study_number_53,
         code = code_53,
         date = date_53)


# create flags for which category each record falls into in each extract
progression_flagged<-codes_merged%>%
  group_by(study_number, code, date)%>%
  mutate(extract_53 = case_when(is.na(row_number_53)~ "Absent",
                                !is.na(row_number_53) & n_distinct(row_number_53)==1 ~"New records",
                                !is.na(row_number_53) & n_distinct(row_number_53)>1 ~"New duplicates"))%>%
  mutate(extract_57 = case_when(n_distinct(row_number_57)==n_distinct(row_number_53) & !is.na(row_number_53)~ "Overlap",
                                is.na(row_number_53) & !is.na(row_number_57) ~ "New records",
                                n_distinct(row_number_57)>n_distinct(row_number_53) & !is.na(row_number_53) ~ "New duplicates",
                                !is.na(row_number_53) & is.na(row_number_57) ~ "Deletion",
                                is.na(row_number_57) & is.na(row_number_53) ~"Absent"))%>%
  mutate(extract_59 = case_when(n_distinct(row_number_59)==n_distinct(row_number_57) & !is.na(row_number_57)~ "Overlap",
                                is.na(row_number_57) & !is.na(row_number_59) ~ "New records",
                                n_distinct(row_number_59)>n_distinct(row_number_57) & !is.na(row_number_57) ~ "New duplicates",
                                !is.na(row_number_57) & is.na(row_number_59) ~ "Deletion",
                                is.na(row_number_59) & is.na(row_number_57) ~"Absent"))


# transform to format suitable for alluvial plot
unique_records_progression<-progression_flagged%>%
  group_by(study_number, code, date)%>%
  mutate(record_identifier = cur_group_id())%>%
  group_by(record_identifier)%>%
  distinct(extract_53, extract_57, extract_59)%>%
  pivot_longer(-record_identifier, names_to = "Extract", values_to="Category")%>%
  mutate(Extract = str_sub(Extract, 9, 11))


# compute counts
counts_categories_extracts<-unique_records_progression%>%
  # filter(code%in%bnf_chapters$dmd_id) # restricting to drug records
  group_by(Extract, Category)%>%
  summarise(Counts = n_distinct(record_identifier))%>%
  group_by(Extract)%>%
  mutate(Proportion = round(Counts/sum(Counts)*100, 1))%>%
  ungroup()%>%
  mutate(Category=fct_relevel(Category, "Overlap", "New records", "New duplicates", "Deletion", "Absent"),
         Category=fct_relevel(Category, rev))

# produce plot
counts_categories_extracts%>%
  mutate(Extract = case_when(Extract=="53" ~ "May 2021",
                             Extract=="57" ~ "June 2021",
                             Extract=="59" ~ "July 2021"),
         Extract=fct_relevel(Extract, "May 2021", "June 2021", "July 2021"))%>%
  ggplot(aes(Extract, Counts, fill=Category))+
  geom_col(color="black", width=0.2)+
  geom_label_repel(aes(label=paste0(Category, ": ", Counts," (", Proportion, "%)"), group=Category), 
                   position=position_stack(vjust=0.5), 
                   color="black",
                   direction="y",
                   hjust = 0,
                   max.overlaps=20,
                   size=20*0.36,
                   force_pull = 0.5)+
  theme(text=element_text(size=20, family="Mulish"),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0),
        plot.caption.position =  "plot")+
  labs(title="Discrepancies for GP data in April 2021 between consecutive data extracts",
       caption="
       Unique records defined as distinct study number/date/code combinations. 
       Records were assigned a category in each extract based on the following definitions: 
       - Overlap: no difference from the previous extract;
       - New record:  appears for the first time in that extract and has no duplicates;
       - New duplicate:  duplicates added in that extract;
       - Deletion: present in a previous extract but not the current one; 
       - Absent: not present in the current extract but present in a later one",
       y="Unique record counts")+
  scale_x_discrete(expand=expansion(c(0.1,0.5)))

ggsave("Outputs/Figures/baseline_drugs_recovery/gp_data_extracts_alluvial_april_2021.png",
       dpi="retina",
       width=65,
       height=30,
       units="cm",
       limitsize = F)




# check what are these new records

progression_flagged%>%
  ungroup()%>%
  filter(extract_59=="New records")%>%
  select(code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  mutate(Cluster_Desc=fct_explicit_na(Cluster_Desc, "NA"))%>%
  count(Cluster_Desc, ConceptId_Description)%>%
  arrange(desc(n))%>%
  View()




# alluvial plot (abandoned as too large)
# unique_records_progression%>%
#   ggplot(aes(y=Category,
#              x=Extract,
#              stratum=Category,
#              alluvium=record_identifier,
#              fill=Category))+
#   geom_flow()+
#   geom_stratum(na.rm=T)
# 
# unique_records_progression%>%
#   ggplot(aes(x=Extract,
#              stratum=Category,
#              alluvium=record_identifier,
#              fill=Category,
#              label=Category))+
#   geom_flow(stat="alluvium", color="darkgray")+
#   geom_stratum(na.rm=T)+
#   theme(legend.position="bottom")->alluvial_plot

### earlier time point (April 2020) -------


gp_dt%>%
  select(study_number, code, date, record_date, gp_system_supplier, lsoa, reporting_period_end_date, processed_timestamp, row_number)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  filter(date<=extract_53_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month=="2020-04-01")%>%
  mutate(extract="59")%>%
  as_tibble()->codes_59


gp_dt_57%>%
  select(study_number, code, date, record_date, gp_system_supplier, lsoa, reporting_period_end_date, processed_timestamp, row_number)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  filter(date<=extract_53_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month=="2020-04-01")%>%
  mutate(extract="57")%>%
  as_tibble()->codes_57


gp_dt_53%>%
  select(study_number, code, date, record_date, gp_system_supplier, lsoa, reporting_period_end_date, processed_timestamp, row_number)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
  filter(date<=extract_53_date)%>%
  mutate(month=as.Date(paste0(str_sub(date, 1, 8), "01")))%>%
  filter(month=="2020-04-01")%>%
  mutate(extract="53")%>%
  as_tibble()->codes_53

# simplify to study number, code, and date only for merging (along with row_number as unique identifier within extract)
codes_simplified_59<-codes_59%>%select(study_number_59 = study_number, code_59 = code, date_59 = date, row_number_59 = row_number)
codes_simplified_57<-codes_57%>%select(study_number_57 = study_number, code_57 = code, date_57 = date, row_number_57 = row_number)  
codes_simplified_53<-codes_53%>%select(study_number_53 = study_number, code_53 = code, date_53 = date, row_number_53 = row_number)


# merge codes from the different extracts
codes_merged<-codes_simplified_53%>%
  full_join(codes_simplified_57, by=c("study_number_53"="study_number_57",
                                      "code_53"="code_57",
                                      "date_53"="date_57"))%>%
  full_join(codes_simplified_59, by=c("study_number_53"="study_number_59",
                                      "code_53"="code_59",
                                      "date_53"="date_59"))%>%
  rename(study_number = study_number_53,
         code = code_53,
         date = date_53)


# create flags for which category each record falls into in each extract
progression_flagged<-codes_merged%>%
  group_by(study_number, code, date)%>%
  mutate(extract_53 = case_when(is.na(row_number_53)~ "Absent",
                                !is.na(row_number_53) & n_distinct(row_number_53)==1 ~"New records",
                                !is.na(row_number_53) & n_distinct(row_number_53)>1 ~"New duplicates"))%>%
  mutate(extract_57 = case_when(n_distinct(row_number_57)==n_distinct(row_number_53) & !is.na(row_number_53)~ "Overlap",
                                is.na(row_number_53) & !is.na(row_number_57) ~ "New records",
                                n_distinct(row_number_57)>n_distinct(row_number_53) & !is.na(row_number_53) ~ "New duplicates",
                                !is.na(row_number_53) & is.na(row_number_57) ~ "Deletion",
                                is.na(row_number_57) & is.na(row_number_53) ~"Absent",
                                n_distinct(row_number_57) < n_distinct(row_number_53) & !is.na(row_number_53) ~ "Less duplicates"))%>%
  mutate(extract_59 = case_when(n_distinct(row_number_59)==n_distinct(row_number_57) & !is.na(row_number_57)~ "Overlap",
                                is.na(row_number_57) & !is.na(row_number_59) ~ "New records",
                                n_distinct(row_number_59)>n_distinct(row_number_57) & !is.na(row_number_57) ~ "New duplicates",
                                !is.na(row_number_57) & is.na(row_number_59) ~ "Deletion",
                                is.na(row_number_59) & is.na(row_number_57) ~"Absent",
                                n_distinct(row_number_59) < n_distinct(row_number_57) & !is.na(row_number_57) ~ "Less duplicates"))



# transform to format suitable for alluvial plot
unique_records_progression<-progression_flagged%>%
  group_by(study_number, code, date)%>%
  mutate(record_identifier = cur_group_id())%>%
  group_by(record_identifier)%>%
  distinct(extract_53, extract_57, extract_59)%>%
  pivot_longer(-record_identifier, names_to = "Extract", values_to="Category")%>%
  mutate(Extract = str_sub(Extract, 9, 11))


# compute counts
counts_categories_extracts<-unique_records_progression%>%
  # filter(code%in%bnf_chapters$dmd_id) # restricting to drug records
  group_by(Extract, Category)%>%
  summarise(Counts = n_distinct(record_identifier))%>%
  group_by(Extract)%>%
  mutate(Proportion = round(Counts/sum(Counts)*100, 1))%>%
  ungroup()%>%
  mutate(Category=fct_relevel(Category, "Less duplicates", "Overlap", "New records", "New duplicates", "Deletion", "Absent"),
         Category=fct_relevel(Category, rev))

# produce plot
counts_categories_extracts%>%
  mutate(Extract = case_when(Extract=="53" ~ "May 2021",
                             Extract=="57" ~ "June 2021",
                             Extract=="59" ~ "July 2021"),
         Extract=fct_relevel(Extract, "May 2021", "June 2021", "July 2021"),
         Category=fct_relevel(Category, "Absent", "Less duplicates", "Deletion","New duplicates","New records","Overlap"))%>%
  ggplot(aes(Extract, Counts, fill=Category))+
  geom_col(color="black", width=0.2)+
  geom_label_repel(aes(label=paste0(Category, ": ", Counts," (", Proportion, "%)"), group=Category), 
                   position=position_stack(vjust=0.5), 
                   color="black",
                   direction="y",
                   hjust = 0,
                   max.overlaps=20,
                   size=20*0.36,
                   force_pull = 0.5)+
  theme(text=element_text(size=20, family="Mulish"),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0),
        plot.caption.position =  "plot")+
  labs(title="Discrepancies for GP data in April 2020 between consecutive data extracts",
       caption="
       Unique records defined as distinct study number/date/code combinations. 
       Records were assigned a category in each extract based on the following definitions: 
       - Overlap: no difference from the previous extract (including same number of duplicate records);
       - New record:  appears for the first time in that extract and has no duplicates;
       - New duplicate:  duplicates added in that extract;
       - Deletion: present in a previous extract but not the current one; 
       - Absent: not present in the current extract but present in a later one
       - Less duplicates: number of duplicates reduced compared to previous extract",
       y="Unique record counts")+
  scale_x_discrete(expand=expansion(c(0.1,0.5)))+
  scale_fill_manual(breaks=c("Absent", "Less duplicates", "New duplicates", "New records", "Overlap"), values=c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))

ggsave("Outputs/Figures/baseline_drugs_recovery/gp_data_extracts_alluvial_april_2020.png",
       dpi="retina",
       width=65,
       height=30,
       units="cm",
       limitsize = F)



# check what are these new records

progression_flagged%>%
  ungroup()%>%
  filter(extract_59=="New records")%>%
  select(code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  mutate(Cluster_Desc=fct_explicit_na(Cluster_Desc, "NA"))%>%
  count(Cluster_Desc, ConceptId_Description)%>%
  arrange(desc(n))%>%
  View()







rm(gp_dt_53, gp_dt_57)

## meds data -----------------



# this needs to be restricted to participants randomised before the most recent randomisation date for participants in extract 53 (as changes to earlier records in later extracts might be simply due to more people being randomised)



### extract 53

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


### calculations
meds_dt_53%>%
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53_53)%>%
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
  .[[1]] -> last_rand_date_53


rm(meds_dt_53)

### extract 59


# calculations

meds_dt%>%
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53)%>%
  rename(month=processing_period_date)%>%
  group_by(month)%>%
  summarise(Entries=n(),
            Participants=n_distinct(study_number),
            Codes = n_distinct(prescribed_bnf_code))%>%
  mutate(extract="59")%>%
  as_tibble()->counts_59


### extract 57

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




### calculations

meds_dt_57%>%
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  filter(rand_date<=last_rand_date_53)%>%
  rename(month=processing_period_date)%>%
  group_by(month)%>%
  summarise(Entries=n(),
            Participants=n_distinct(study_number),
            Codes = n_distinct(prescribed_bnf_code))%>%
  mutate(extract="57")%>%
  as_tibble()->counts_57


rm(meds_dt_57)





### aggregate counts across datasets

counts_meds<-bind_rows(counts_53, counts_57, counts_59)%>%
  mutate(Dataset="Dispensing")

rm(counts_53, counts_57, counts_59)


## aggregate counts -----

counts<-counts_gp%>%bind_rows(counts_meds)

rm(counts_gp, counts_meds)

## plots -----

### 2010 onwards -----


counts%>%
  filter(month>="2010-01-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract, Dataset), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")))%>%
  ggplot(aes(month, 
             value, 
             color=extract, 
             fill=extract, 
             size=extract, 
             group=extract, 
             shape=extract))+
  geom_point(alpha=0.4, stroke=1.5)+
  geom_line(alpha=0.4, size=1)+
  facet_wrap(~key, nrow=3, scales="free_y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="red")+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="dark green")+
  labs(x="Month",
       y="Counts",
       title="Dataset stability across different extracts",
       color="Extract",
       shape="Extract",
       size="Extract",
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
  scale_x_date(date_breaks = "6 months", date_labels = "%B %Y")+
  scale_shape_manual(values= c(23, 24, 25))



### 2018 onwards -----

counts%>%
  filter(month>="2018-01-01" & month<="2021-12-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract, Dataset), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")),
         extract=fct_relevel(extract, c("59", "57", "53")),
         extract=case_when(extract == "59" ~ "July 2021",
                           extract=="57"~ "June 2021",
                           extract=="53" ~ "May 2021"),
         Dataset=fct_relevel(Dataset, c("GP", "Dispensing")))%>%  
  ggplot(aes(month, 
             value, 
             fill=extract, 
             group=extract,
             shape=extract))+
  geom_area(position="identity", 
            alpha=0.2, 
            color="black"
  )+
  geom_point(aes(shape=extract), alpha=0.8, color="black")+
  facet_grid(cols = vars(Dataset), rows=vars(key), scales="free_y",
             labeller = label_wrap_gen(width = 15),
             switch="y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="#F8766D", size=1)+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="#619CFF", size=1)+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="#00BA38", size=1)+
  labs(x="Month",
       y="Counts",
       # title="Dataset stability across different extracts (records from 2018 onwards)",
       subtitle="2018 onwards",
       color="Extract",
       shape="Extract",
       size="Extract",
       fill="Extract",
       # caption="Plot capped before 2018 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in the first extract).
       # Vertical dashed lines placed at the date of receipt of each extract"
       )+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish", size=20),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", limits=c(as.Date("2018-01-01"), NA))+
  scale_shape_manual(breaks=c("May 2021", "June 2021", "July 2021"), values= c(23, 24, 25))+
  scale_y_continuous(limits=c(0, NA))+
  scale_fill_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#619CFF", "#00BA38", "#F8766D"))+
  scale_color_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#619CFF", "#00BA38", "#F8766D"))

ggsave("Outputs/Figures/Thesis/temporal_stability_2018.png",
       last_plot(),
       units="cm",
       width=60,
       height=30,
       dpi="retina")


#### slides ------

counts%>%
  filter(month>="2018-01-01" & month<="2021-12-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract, Dataset), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")),
         extract=fct_relevel(extract, c("59", "57", "53")),
         extract=case_when(extract == "59" ~ "July 2021",
                           extract=="57"~ "June 2021",
                           extract=="53" ~ "May 2021"),
         Dataset=if_else(Dataset=="GP","GP dataset", "Dispensing dataset"),
         Dataset=fct_relevel(Dataset, c("GP dataset", "Dispensing dataset")))%>%  
  ggplot(aes(month, 
             value, 
             fill=extract, 
             group=extract,
             shape=extract))+
  geom_area(position="identity", 
            alpha=0.5, 
            color="black"
  )+
  geom_point(aes(shape=extract), alpha=0.8, color="black")+
  facet_grid(cols = vars(Dataset), 
             rows=vars(key), 
             scales="free_y",
             switch="y"
             )+
  # geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="#F8766D", size=1)+
  # geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="#619CFF", size=1)+
  # geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="#00BA38", size=1)+
  labs(x="Month",
       y="Counts",
       # title="Dataset stability across different extracts (records from 2018 onwards)",
       subtitle="June 2018 onwards",
       color="Extract",
       shape="Extract",
       size="Extract",
       fill="Extract",
       # caption="Plot capped before 2018 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in the first extract).Vertical dashed lines placed at the date of receipt of each extract"
       )+
  oxpop_blue_panel+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish", size=20),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", limits=c(as.Date("2018-01-01"), NA))+
  scale_shape_manual(breaks=c("May 2021", "June 2021", "July 2021"), values= c(23, 24, 25))+
  scale_y_continuous(limits=c(0, NA))+
  scale_fill_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#619CFF", "#00BA38", "#F8766D"))+
  scale_color_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#619CFF", "#00BA38", "#F8766D"))

ggsave("Outputs/Figures/ictmc_Slides/temporal_stability_2018_slides.png",
       last_plot(),
       width=64,
       height=30,
       units="cm",
       dpi="retina")

### 2021 onwards -------


counts%>%
  filter(month>="2021-01-01",
         month<="2021-12-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract, Dataset), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")),
         extract=fct_relevel(extract, c("59", "57", "53")),
         extract=case_when(extract == "59" ~ "July 2021",
                           extract=="57"~ "June 2021",
                           extract=="53" ~ "May 2021"),
         Dataset=fct_relevel(Dataset, c("GP", "Dispensing")))%>%  
  ggplot(aes(month, 
             value, 
             fill=extract, 
             group=extract,
             shape=extract))+
  geom_area(position="identity", 
            alpha=0.2, 
            color="black"
  )+
  geom_point(aes(shape=extract), alpha=0.8, color="black")+
  facet_grid(cols = vars(Dataset), rows=vars(key), scales="free_y",
             labeller = label_wrap_gen(width = 15),
             switch="y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="#F8766D", size=1)+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="#619CFF", size=1)+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="#00BA38", size=1)+
  labs(x="",
       y="Counts",
       # title="Retrospective dataset stability across different extracts (records with date within 2021)",
       subtitle="2021 only",
       color="Extract",
       shape="Extract",
       size="Extract",
       fill="Extract",
       # caption="The area in each panel shows monthly counts for a different metric, split by extract; the difference between areas in each month represents counts added retrospectively.\nPlot capped to entries with date within 2021 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in the first extract)"
       )+
  # theme_gray(base_size = 20)+
  # oxpop_blue_panel+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish", size=20),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1),
        panel.grid.minor = element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
  scale_shape_manual(breaks=c("May 2021", "June 2021", "July 2021"), values= c(23, 24, 25))+
  scale_y_continuous(limits=c(0, NA))+
  scale_fill_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#619CFF", "#00BA38", "#F8766D"))+
  scale_color_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#619CFF", "#00BA38", "#F8766D"))



ggsave("Outputs/Figures/Thesis/temporal_stability_2021.png",
       last_plot(),
       units="cm",
       width=60,
       height=30,
       dpi="retina")


# poster 



counts%>%
  filter(month>="2021-01-01",
         month<="2021-12-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract, Dataset), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")),
         extract=fct_relevel(extract, c("59", "57", "53")),
         extract=case_when(extract == "59" ~ "July 2021",
                           extract=="57"~ "June 2021",
                           extract=="53" ~ "May 2021"),
         Dataset=fct_relevel(Dataset, c("GP", "Dispensing")))%>%  
  ggplot(aes(month, 
             value, 
             fill=extract, 
             group=extract,
             shape=extract))+
  geom_area(position="identity", 
            # alpha=0.2, 
            color="black"
  )+
  geom_point(aes(shape=extract), alpha=0.8, color="black")+
  facet_grid(cols = vars(Dataset), rows=vars(key), scales="free_y",
             labeller = label_wrap_gen(width = 15),
             switch="y")+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="#A05935", size=1)+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="#0D6379", size=1)+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="#2A294B", size=1)+
  labs(x="",
       y="Counts",
       # title="Retrospective dataset stability across different extracts (records with date within 2021)",
       # subtitle="Vertical dashed lines show the date each extract was received",
       color="Extract",
       shape="Extract",
       size="Extract",
       fill="Extract",
       # caption="The area in each panel shows monthly counts for a different metric, split by extract; the difference between areas in each month represents counts added retrospectively.\nPlot capped to entries with date within 2021 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in the first extract)"
  )+
  # theme_minimal(base_size = 20)+
  # oxpop_blue_panel+
  theme(text = element_text(family="Mulish", size=20, color="white"))+
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1, color="white"),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_text(color="white", family="Mulish"),
        strip.text = element_text(color="white", family="Mulish"),
        panel.grid.major = element_line(color="#232325", linetype="dashed", size=0.2),
        axis.ticks = element_line(color="white"))+
  scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
  scale_shape_manual(breaks=c("May 2021", "June 2021", "July 2021"), values= c(23, 24, 25))+
  scale_y_continuous(limits=c(0, NA))+
  scale_fill_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#0D6379", "#2A294B", "#A05935"))+
  scale_color_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#0D6379", "#2A294B", "#A05935"))


ggsave("Outputs/Figures/baseline_drugs_recovery/temporal_stability_2021_poster.png",
       last_plot(),
       width=42,
       height=19.2,
       units="cm",
       dpi="retina")



#### slides -----


counts%>%
  filter(month>="2021-01-01",
         month<="2021-12-01")%>%
  rename("Distinct drug codes"=Codes)%>%
  pivot_longer(-c(month, extract, Dataset), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Entries", "Distinct drug codes")),
         extract=fct_relevel(extract, c("59", "57", "53")),
         extract=case_when(extract == "59" ~ "July 2021",
                           extract=="57"~ "June 2021",
                           extract=="53" ~ "May 2021"),
         Dataset=if_else(Dataset=="GP","GP dataset", "Dispensing dataset"),
         Dataset=fct_relevel(Dataset, c("GP dataset", "Dispensing dataset")))%>%  
  ggplot(aes(month, 
             value, 
             fill=extract, 
             group=extract,
             shape=extract))+
  geom_area(position="identity", 
            alpha=0.5, 
            color="black"
  )+
  geom_point(aes(shape=extract), alpha=0.8, color="black")+
  facet_grid(cols = vars(Dataset), 
             rows=vars(key), 
             scales="free_y",
             switch="y"
  )+
  geom_vline(aes(xintercept=as.Date(extract_59_date)), linetype="dashed", color="#F8766D", size=2)+
  geom_vline(aes(xintercept=as.Date(extract_53_date)), linetype="dashed", color="#619CFF", size=2)+
  geom_vline(aes(xintercept=as.Date(extract_57_date)), linetype="dashed", color="#00BA38", size=2)+
  labs(x="Month",
       y="Counts",
       # title="Dataset stability across different extracts (records from 2018 onwards)",
       color="Extract",
       shape="Extract",
       size="Extract",
       fill="Extract",
       # caption="Plot capped before 2018 and restricted to participants randomised on or before the 24/05/2021 (latest randomisation date for participants included in the first extract).Vertical dashed lines placed at the date of receipt of each extract"
  )+
  oxpop_blue_panel+
  theme(legend.position = "none",
        text = element_text(family="Mulish", size=20),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 30,hjust=1),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  scale_shape_manual(breaks=c("May 2021", "June 2021", "July 2021"), values= c(23, 24, 25))+
  scale_y_continuous(limits=c(0, NA))+
  scale_fill_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#619CFF", "#00BA38", "#F8766D"))+
  scale_color_manual(breaks=c("May 2021", "June 2021", "July 2021"), values=c("#619CFF", "#00BA38", "#F8766D"))

ggsave("Outputs/Figures/ictmc_Slides/temporal_stability_2021_slides.png",
       last_plot(),
       width=64,
       height=30,
       units="cm",
       dpi="retina")









### check retrospective additions -----

gp%>%count(reporting_period_end_date)%>%ggplot(aes(reporting_period_end_date, n))+geom_col()

gp%>%mutate(processed_timestamp = as.Date(processed_timestamp, format="%Y-%m-%d"))%>%count(processed_timestamp)%>%ggplot(aes(processed_timestamp, n))+geom_col()



# RESULTS: 2.1 Deriving baseline drug exposure-----------------------

## time intervals between entries ------------------

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


transformed_data_intervals_drug_groups<-rbind(p,p1)%>%
  left_join(codelist_labels)

rm(p,p1)


#### plots (ridges) -----


transformed_data_intervals_drug_groups%>%
  mutate(labels=fct_reorder(labels, desc(labels)))%>%
  ggplot(aes(x=average, fill=dataset))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180), limits=c(0, 180))+
  theme_gray(base_size=30)+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        text=element_text(family="Mulish")
        
  )+
  labs(x = "Average day difference",
       # title="Time interval between consecutive entries",
       # caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual code and participant level as the median day different between consecutive entries. Vertical lines depict medians per drug group and dataset. Entries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date", 200),
       fill="Dataset",
       y="Drug group")


ggsave("Outputs/Figures/Thesis/day_differences_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

##### poster ------
plot<-transformed_data_intervals_drug_groups%>%
  mutate(labels=fct_reorder(labels, desc(labels)))%>%
  ggplot(aes(x=average, fill=dataset))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360), limits=c(0, 360))+
  oxpop_blue_panel+
  # theme_gray(base_size=20)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank() ,       
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        legend.position = "none",
        text=element_text(size=20))+
  labs(
    # x = "Average day difference",
    # title="Time interval between consecutive entries",
    # caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual code and participant level as the median day different between consecutive entries. Vertical lines depict medians per drug group and dataset. Entries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date", 200),
    fill="Dataset",
    y="Drug group")+
  scale_fill_manual(breaks=c("GP", "Dispensing"), values = c("#A05935", "#0D6379"))

ggsave("Outputs/Figures/baseline_drugs_recovery/day_differences_main_drugs_poster.png",
       last_plot(),
       width=40,
       height=15,
       units="cm",
       dpi="retina",
       limitsize = F)

##### slides ------

transformed_data_intervals_drug_groups%>%
  mutate(labels=fct_reorder(labels, desc(labels)))%>%
  ggplot(aes(x=average, fill=dataset))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360), limits=c(0, 360))+
  theme_gray(base_size=20)+
  oxpop_blue_panel+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom"
        
  )+
  labs(x = "Average day difference",
       # title="Time interval between consecutive entries",
       # caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual code and participant level as the median day different between consecutive entries. Vertical lines depict medians per drug group and dataset. Entries with the same date were excluded so that a unique prescribing/dispensing record was retained for each distinct date", 200),
       fill="Dataset",
       y="Drug group")


ggsave("Outputs/Figures/ictmc_slides/day_differences_slides.png",
       last_plot(),
       width=60,
       height=30,
       units="cm",
       dpi="retina")






















## proportions per time interval -----


transformed_data_intervals_drug_groups%>%
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
  mutate(key=as.numeric(key))->transformed_data_intervals_drug_groups_proportions

##### plot ------
transformed_data_intervals_drug_groups_proportions%>%
  ggplot(aes(x=as.numeric(key), y=value, fill=dataset, color=dataset))+
  geom_point(aes(color=dataset))+
  geom_line(aes(color=dataset))+
  # geom_area(color="black", 
  #           alpha=0.2,
  #           position="identity")+
  # geom_point(aes(shape=dataset),
  #            color="black",
  #            alpha=0.8)+
  geom_text_repel(data=transformed_data_intervals_drug_groups_proportions%>%filter(dataset=="GP"), 
                  aes(label=paste0(value, "%")),
                  direction="y",
                  nudge_y = 5,
                  size=6)+
  geom_text_repel(data=transformed_data_intervals_drug_groups_proportions%>%filter(dataset=="Dispensing"), 
                  aes(label=paste0(value, "%")),
                  direction="y",
                  nudge_y = -5,
                  size=6)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  theme(legend.position="right",
        text=element_text(size=20, family="Mulish"))+
  labs(y="Participants (%)",
       # title="Proportions of participants with distinct average time intervals between consecutive entries",
       x="Time between consecutive records (in days)",
       # caption="Time intervals calculated as the median day difference between consecutive entries with the same drug code for the same participant\nDuplicate entries (same drug code, same participant, same date) were excluded to avoid counting duplicate entries as distinct events\nProportions refer to total number of participants with an entry for each drug group and dataset",
       fill="Dataset",
       color="Dataset",
       shape="Dataset")+
  scale_x_continuous(breaks=c(30,60,90, 180, 360))+
  scale_y_continuous(breaks=seq(0,100, by=20), expand=expansion(c(0,0.2)))


ggsave("Outputs/Figures/Thesis/time_intervals_proportions_main_drugs.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")

#### slides -----


transformed_data_intervals_drug_groups_proportions%>%
  ggplot(aes(x=as.numeric(key), y=value, fill=dataset, color=dataset))+
  geom_point(aes(color=dataset))+
  geom_line(aes(color=dataset))+
  # geom_area(color="black", 
  #           alpha=0.2,
  #           position="identity")+
  # geom_point(aes(shape=dataset),
  #            color="black",
  #            alpha=0.8)+
  geom_text_repel(data=transformed_data_intervals_drug_groups_proportions%>%filter(dataset=="GP"), 
                  aes(label=paste0(value
                                   # ,"%"
                                   ),
                      fill=NA),
                  direction="y",
                  nudge_y = 5,
                  size=6)+
  geom_text_repel(data=transformed_data_intervals_drug_groups_proportions%>%filter(dataset=="Dispensing"), 
                  aes(label=paste0(value 
                                   #,"%"
                                   ),
                      fill=NA),
                  direction="y",
                  nudge_y = -5,
                  size=6)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30))+
  theme_gray(base_size=20)+
  oxpop_blue_panel+
  theme(legend.position="none",
        legend.background = element_blank())+
  labs(y="Participants (%)",
       # title="Proportions of participants with distinct average time intervals between consecutive entries",
       x="Time between consecutive records (in days)",
       #caption="Time intervals calculated as the median day difference between consecutive entries with the same drug code for the same participant\nDuplicate entries (same drug code, same participant, same date) were excluded to avoid counting duplicate entries as distinct events\nProportions refer to total number of participants with an entry for each drug group and dataset",
       fill="Dataset",
       color="Dataset",
       shape="Dataset")+
  scale_x_continuous(breaks=c(30,60,90, 180, 360))+
  scale_y_continuous(breaks=seq(0,100, by=20), expand=expansion(c(0,0.2)))


ggsave("Outputs/Figures/ictmc_slides/time_intervals_proportions_slides.png",
       last_plot(),
       width=60,
       height=30,
       units="cm",
       dpi="retina")





## entries per month ---------------


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








t%>%
  left_join(codelist_labels, by=c("codelist"))%>%
  filter(!is.na(labels))%>%
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



















## proportions captured in each dataset --------

#### timeseries -----

#### calculations
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

transformed_data_proportions_timeperiods_datasets<-rbind(x1, x2)

rm(x1,x2)

transformed_data_proportions_timeperiods_datasets%>%distinct(study_number)%>%nrow()->participants

transformed_data_proportions_timeperiods_datasets%>%distinct(study_number)->participants_list

transformed_data_proportions_timeperiods_datasets%<>%
  mutate(flag="1")%>%
  pivot_wider(c(study_number, codelist, month), names_from=dataset, values_from = flag)%>%
  mutate(GP=replace_na(GP,"0"))%>%
  mutate(Dispensing=replace_na(Dispensing,"0"))%>%
  arrange(study_number, codelist)%>%
  mutate(study_number=as.numeric(study_number))%>%
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





#### plot -----
transformed_data_proportions_timeperiods_datasets%>%
  ungroup()%>%
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


#### barplot ----------


#### calculations
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



t%>%  
  ungroup()%>%
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












## Baseline drug exposure ---------------------

### merge randomisation dates and calculate time since randomisation -----------

## gp

gp_dt%>%
  select(study_number, date, code)%>%
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<="2021-05-31")%>% # allow 2 month lag between data reception and analysis
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
  filter(rand_date<="2021-05-31")%>% # allow 2 month lag between data reception and analysis
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


# generate flags based on different time periods
x%>%
  filter(dataset=="GP")%>%
  mutate('1 month' = as.factor(if_else(time_before_rand<=30, 1, 0)),
         '2 months' = as.factor(if_else(time_before_rand<=60, 1, 0)),
         '3 months' = as.factor(if_else(time_before_rand<=90, 1, 0)),
         '6 months' = as.factor(if_else(time_before_rand<=180, 1, 0)),
         '12 months' = as.factor(if_else(time_before_rand<=360, 1, 0)),
  )%>%
  
  rbind(
    x%>%
      filter(dataset=="Dispensing")%>%
      mutate('1 month' = as.factor(if_else(time_before_rand<=1, 1, 0)),
             '2 months' = as.factor(if_else(time_before_rand<=2, 1, 0)),
             '3 months' = as.factor(if_else(time_before_rand<=3, 1, 0)),
             '6 months' = as.factor(if_else(time_before_rand<=180, 1, 0)),
             '12 months' = as.factor(if_else(time_before_rand<=360, 1, 0)))
    
  )->baseline_flags_different_periods


baseline_flags_different_periods%>%
  ungroup()%>%
  pivot_longer(c('1 month', '2 months', '3 months', '6 months', '12 months'), names_to = "Timeframe", values_to = "Flag")%>%
  group_by(dataset, codelist, Timeframe)%>%
  summarise(Participants = n_distinct(study_number[Flag==1]))%>%
  rbind(
    
    baseline_flags_different_periods%>%
      ungroup()%>%
      pivot_longer(c('1 month', '2 months', '3 months','6 months', '12 months'), names_to = "Timeframe", values_to = "Flag")%>%
      group_by(codelist, Timeframe)%>%
      summarise(Participants = n_distinct(study_number[Flag==1]))%>%
      mutate(dataset="Either"),
    
    baseline_flags_different_periods%>%
      ungroup()%>%
      pivot_longer(c('1 month', '2 months', '3 months', '6 months', '12 months'), names_to = "Timeframe", values_to = "Flag")%>%
      filter(Flag==1)%>%
      distinct(study_number, codelist, dataset, Timeframe, Flag)%>%
      pivot_wider(names_from = dataset, values_from=Flag)%>%
      group_by(codelist, Timeframe)%>%
      summarise(Participants = n_distinct(study_number[!is.na(GP) & !is.na(Dispensing)]))%>%
      mutate(dataset="Both")
  )%>%
  ungroup()-> baseline_flags_different_periods_summarised



baseline_flags_different_periods_summarised%<>%
  mutate(key=as.integer(str_sub(Timeframe, 0, 2)))%>%
  left_join(codelist_labels)%>%
  group_by(labels)%>%
  mutate(Prop=round(Participants/total_participants*100,0))%>%
  mutate(dataset=factor(dataset, levels=c("Either", "Dispensing", "GP",  "Both")))


### plot -----

baseline_flags_different_periods_summarised%>%
  filter(dataset%in%c("Either", "GP", "Dispensing"))%>%
  ggplot(aes(key, 
             Prop, 
             fill=dataset,
             group=dataset,
             color=dataset,
             shape=dataset))+
  geom_area(position="identity", 
            alpha=0.2, 
            color="black"
  )+
  geom_point(alpha=0.8, 
             color="black"
  )+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom",
        text=element_text(family="Mulish"))+
  
  facet_wrap(~labels, scales="free_y", labeller = label_wrap_gen(width = 30))+
  scale_fill_discrete(limits= c("Either","Dispensing", "GP"),
                      breaks = c("Either","Dispensing", "GP"))+
  labs(
    # title="Participants identified using different lookback periods before randomisation",
       x="Lookback period before randomisation (in months)",
       fill="Dataset",
       color="Dataset",
       shape="Dataset",
       y="Proportion of total participants (%)",
      #  caption = str_wrap("For each record, the interval before randomisation was calculated in exact days before the randomisation date (for records in the GP dataset) or the number of calendar months predecing the month of randomisation (for records in the Dispensing dataset).
       # For the Dispensing dataset, any records occuring in the month of randomisation were excluded as it is not possible to distinguish if these occured before or after randomisation. ", 200)
      )+
  scale_y_continuous(expand=expansion(c(0,0.6)), limits=c(0, NA))+
  scale_shape_manual(values= c(23, 24, 25))+
  scale_x_reverse(breaks=c(12,6,3,2,1))




ggsave("Outputs/Figures/Thesis/baseline_timeframes_main_drugs.png",
       last_plot(),
       width=65,
       height=30,
       dpi="retina",
       limitsize=F,
       units="cm")

#### slides ------

baseline_flags_different_periods_summarised%>%
  filter(dataset%in%c("Either", "GP", "Dispensing"))%>%
  filter(codelist!="macrolides")%>%
  ggplot(aes(key, 
             Prop, 
             fill=dataset,
             group=dataset,
             color=dataset,
             shape=dataset))+
  geom_area(position="identity", 
            alpha=0.5, 
            color="black"
  )+
  geom_point(alpha=0.8, 
             color="black",
             size=3
  )+
  theme_gray(base_size=20)+
  oxpop_blue_panel+
  theme(legend.position = "none")+
  
  facet_wrap(~labels, scales="free_y", labeller = label_wrap_gen(width = 30))+
  scale_fill_discrete(limits= c("Either","Dispensing", "GP"),
                      breaks = c("Either","Dispensing", "GP"))+
  labs(
    # title="Participants identified using different lookback periods before randomisation",
       x="Lookback period before randomisation (in months)",
       fill="Dataset",
       color="Dataset",
       shape="Dataset",
       y="Proportion of total participants (%)",
       # caption = str_wrap("For each record, the interval before randomisation was calculated in exact days before the randomisation date (for records in the GP dataset) or the number of calendar months predecing the month of randomisation (for records in the Dispensing dataset). For the Dispensing dataset, any records occuring in the month of randomisation were excluded as it is not possible to distinguish if these occured before or after randomisation. ", 200)
       )+
  scale_y_continuous(expand=expansion(c(0,0.6)), limits=c(0, NA))+
  scale_shape_manual(values= c(23, 24, 25))+
  scale_x_reverse(breaks=c(12,6,3,2,1))




ggsave("Outputs/Figures/ictmc_slides/baseline_timeframes_proportions_slides.png",
       last_plot(),
       width=65,
       height=30,
       dpi="retina",
       limitsize=F,
       units="cm")


### participants captured in each dataset --------------
# 
# baseline_flags_different_periods%>%
#   filter(`3 months`==1)%>%
#   distinct(study_number, codelist, `3 months`, dataset)%>%
#   group_by(study_number, dataset, codelist)%>%
#   summarise(flag=if_else(any(`3 months` =="1"), "1", "0"))%>%
#   pivot_wider(names_from="dataset", values_from = flag)%>%
#   mutate(across(c(GP, Dispensing), ~replace_na(as.character(.), "0")))%>%
#   mutate(group = as.factor(case_when(GP=="1" & Dispensing=="1" ~ "Both",
#                                      GP=="1" & Dispensing=="0" ~ "GP",
#                                      GP=="0" & Dispensing=="1" ~ "Dispensing")))%>%
#   group_by(codelist, group)%>%
#   summarise(participants = n_distinct(study_number))%>%
#   pivot_wider(codelist, names_from = "group", values_from = "participants")%>%
#   mutate(Either=sum(GP, Dispensing, Both, na.rm = T))%>%
#   pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
#   left_join(codelist_labels)%>%
#   mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing = "Dispensing only",GP="GP only",Either = "Either dataset"))%>%
#   ungroup()%>%
#   group_by(labels)%>%
#   mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
#   mutate(labels=reorder(labels, labels))%>%
#   
#   ggplot(aes(Dataset, Participants, fill=Dataset))+
#   geom_bar(stat='identity', position="dodge")+
#   geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5)+
#   facet_wrap(~labels,
#              labeller = label_wrap_gen(width = 30),
#              scales="free_y")+
#   theme_gray(base_size=25)+
#   theme(legend.position="bottom",
#         axis.text.x = element_blank())+
#   scale_y_continuous(limits = c(0, NA),
#                      expand=expansion(c(0,0.5)))+
#   scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
#   scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
#   labs(color="Dataset",
#        title="Number of participants captured in each dataset at randomisation",
#        subtitle="Based on a 3 month lookback period",
#        caption=str_wrap("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset. 'Dispensing only' is the number of participants captured in the Dispensing dataset but not the GP dataset, and vice-versa. Proportions presented are in reference to the number of participants identified using either dataset. Calculations restricted to the time period between June 2018 and May 2021",200
#        ))
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/counts_per_dataset_barchart_main_drugs.png",
#        last_plot(),
#        width=30,
#        height=15,
#        dpi="retina")



### Comparison with CRF ----

#### process crf data -----
crf_drugs_agreement_flags<-baseline_crf%>%
  select(study_number,
         # macrolides,
         antiplatelets,
         oral_anticoagulants = oral_ac)%>%
  mutate(across(c(antiplatelets, oral_anticoagulants), ~as.factor(.)))%>%
  pivot_longer(c(antiplatelets, oral_anticoagulants), names_to = "codelist", values_to="flag")%>%
  mutate(flag = case_when(flag=="Y" ~ "1",
                          flag=="N" ~ "0",
                          is.na(flag) | flag=="U" ~ "NA"))%>%
  filter(flag!="NA")
  
#### process RCD and merge with CRF data ------
lookback_comparison_crf_joint_flags<-
  baseline_flags_different_periods%>%
  ungroup()%>%
  select(-code, -date, -rand_date, -time_before_rand)%>%
  filter(codelist %in% c(# "macrolides", 
                         "antiplatelets", "oral_anticoagulants"))%>%
  pivot_longer(c(`1 month`, `2 months`, `3 months`, `6 months`, `12 months`), names_to = "Period", values_to="flag")%>%
  mutate(study_number=as.integer(study_number))%>%
  group_by(study_number, dataset, Period, codelist)%>%
  summarise(flag=if_else(any(flag =="1"), "1", "0"))%>%
  right_join(expand.grid(study_number = crf_drugs_agreement_flags%>%distinct(study_number)%>%.[[1]],
                         codelist = c(# "macrolides", 
                                      "antiplatelets", "oral_anticoagulants"),
                         dataset = c("Dispensing", "GP"),
                         Period = c("1 month", "2 months", "3 months", "6 months", "12 months")),
             by=c("study_number", "codelist", "dataset", "Period"))%>%
  mutate(flag=replace_na(flag, "0"))%>%
  distinct(study_number, codelist, dataset, Period, flag)%>%
  arrange(study_number, codelist, Period, dataset)%>%
  pivot_wider(id_cols=c("study_number", 
                        "codelist",
                        "Period"),
                        names_from="dataset", values_from="flag")%>%
  group_by(study_number, codelist, Period)%>%
  summarise(GP = if_else(GP=="1", "1", "0"),
            Dispensing = if_else(Dispensing =="1","1", "0"),
            GP_only = if_else(GP=="1" & Dispensing =="0","1", "0"),
            Dispensing_only = if_else(GP=="0" & Dispensing =="1", "1", "0"),
            Both = if_else(GP=="1" & Dispensing =="1", "1", "0"),
            Either = if_else(GP=="1" | Dispensing=="1", "1", "0"))%>%
  left_join(crf_drugs_agreement_flags%>%
              rename(crf = flag))%>%
  filter(!is.na(crf))%>% # restrict to people where there is something recorded in the crf (note the crf data had already been restricted to people recruited in England and who could have had linkage data received)
  pivot_longer(c(GP, Dispensing, GP_only, Dispensing_only, Either, Both), names_to="dataset", values_to="RCD")%>%
  filter(dataset %in% c("GP", "Dispensing", "Either"))%>%
  ungroup()
  
  
#### run calculations -----

##### calculate agreement ----

drug_groups <- c(# "macrolides", 
                 "antiplatelets", "oral_anticoagulants")

periods<-lookback_comparison_crf_joint_flags%>%
  distinct(Period)%>%
  .[[1]]

datasets<-lookback_comparison_crf_joint_flags%>%
  distinct(dataset)%>%
  .[[1]]

kappa_crf_rcd<-list()

for (d in datasets) {
  
  for (i in drug_groups) {

    for (j in periods){
  
  lookback_comparison_crf_joint_flags%>%
    filter(dataset==d,
           codelist==i,
           Period==j)%>%
    mutate(study_number=as.character(study_number))%>%
    select(-codelist, -study_number, -Period, -dataset)%>%
    mutate(across(everything(), as.integer))->table
  
  try(k<-Kappa.test(table$crf, table$RCD))
  
  if(exists("k"))
  {
    
    kappa_crf_rcd[[d]][[i]][[j]]<-k
    
  }
  else{
    kappa_crf_rcd[[d]][[i]][[j]]<-list()
  }
  
  rm(table, k)
  }
  
  rm(i)
}
rm(d)
}


##### unlist result for table -----

kappa_crf_rcd_results<-data.frame()

for (d in datasets) {
  
  for (i in drug_groups) {
    
    for (j in periods){
  
  dataset<-d
      
  codelist<-i
  
  Period <- j
  
  
  
  try(
    table<-(data.frame(
      dataset = dataset,
      codelist = codelist,
      Period = Period,
      kappa = unlist(kappa_crf_rcd[[d]][[i]][[j]]$Result$estimate),
      CI_lower = unlist(kappa_crf_rcd[[d]][[i]][[j]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_crf_rcd[[d]][[i]][[j]]$Result$conf.int[2]),
      p_value = unlist(kappa_crf_rcd[[d]][[i]][[j]]$Result$p.value),
      judgement = unlist(kappa_crf_rcd[[d]][[i]][[j]]$Judgement)
    ))
  )
  
  try(kappa_crf_rcd_results%<>%bind_rows(table))
  
  
  rm(table, name, j)
  
    }
 rm(i)   
  }
  
  rm(d)
  
}


##### produce table     ------
   
  # format results  
  kappa_crf_rcd_results%>%
  filter(codelist!="macrolides")%>%
  left_join(codelist_labels)%>%
  mutate_when(!is.na(CI_lower), 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate_when(p_value==0.00,
              p_value="<0.001")%>%
  as_tibble()%>%
  
  # add calculations of number of people in each source
  left_join(
    lookback_comparison_crf_joint_flags%>%
       filter(codelist %in% drug_groups)%>%
       group_by(codelist, Period, dataset)%>%
       summarise(CRF_RCD = n_unique(study_number[crf=="1" & RCD=="1"]),
                 CRF_only = n_unique(study_number[crf=="1" & RCD=="0"]),
                 RCD_only = n_unique(study_number[crf=="0" & RCD=="1"]),
                 nil = n_distinct(study_number) - CRF_RCD - CRF_only - RCD_only,
                 Sn = round(CRF_RCD/(CRF_RCD+CRF_only)*100,1),
                 Sp=round(nil/(nil+RCD_only)*100,1),
                 PPV = round(CRF_RCD/(CRF_RCD+RCD_only)*100,1),
                 NPV = round(nil/(nil+CRF_only)*100,1))%>%
      ungroup(),
    by=c("codelist", "dataset", "Period"))%>%
  arrange(dataset,labels, Period)%>%
  select(Dataset=dataset,
         Label=labels,
         Period,
         CRF_RCD,
         CRF_only,
         RCD_only,
         nil,
         Sn,
         Sp,
         PPV,
         NPV,
         kappa,
         CI,
         p_value,
         judgement)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))%>%
  
  # calculate proportions and format
  rowwise() %>%
  mutate(prop = round(sum(across(4:6), na.rm = T)/sum(across(4:7), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either' = paste0(sum(across(4:6), na.rm = T),
                                       " (",
                                       prop,
                                       "%)"))%>%
  mutate(prop1 = round(sum(across(4:4))/sum(across(4:7), na.rm = T)*100,1),
         prop2 = round(sum(across(5:5))/sum(across(4:7), na.rm = T)*100,1),
         prop3 = round(sum(across(6:6))/sum(across(4:7), na.rm = T)*100,1),
         prop4 = round(sum(across(7:7))/sum(across(4:7), na.rm = T)*100,1))%>%
  mutate(prop1=if_else(is.na(prop1), 0, prop1))%>%
  mutate(prop2=if_else(is.na(prop2), 0, prop2))%>%
  mutate(prop3=if_else(is.na(prop3), 0, prop3))%>%
  mutate(prop4=if_else(is.na(prop4), 0, prop4))%>%
  mutate(CRF_RCD=paste0(CRF_RCD, " (", prop1, "%)"),
         CRF_only=paste0(CRF_only, " (", prop2, "%)"),
         RCD_only=paste0(RCD_only, " (", prop3, "%)"),
         nil=paste0(nil, " (", prop4, "%)"))%>%
  mutate(across(c(Sn, Sp, PPV, NPV), ~paste0(., "%")))%>%
  select(-c(prop, prop1, prop2, prop3, prop4))%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%
  rename('Drug group' = Label,
         K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement,
         Neither= nil)%>%
  relocate('Either', .after = 'Drug group')%>%
  relocate(Period, .after = 'Drug group')%>%
  mutate(Period = factor(Period, levels=c("1 month", "2 months", "3 months", "6 months", "12 months")))%>%
  select(-`p-value`)%>%
  arrange(Dataset, `Drug group`, Period)-> crf_rcd_rules_table


View(crf_rcd_rules_table)


crf_rcd_rules_table%>%
  ungroup()%>%
  filter(`Drug group`!="Macrolides",
         !is.na(`Drug group`))%>%
  rename(`CRF and RCD`= CRF_RCD,
         `CRF only` = CRF_only,
         `RCD only` = RCD_only)%>%
  mutate(`Drug group`=fct_relevel(`Drug group`, "Antiplatelets", "Oral anticoagulants"# ,"Macrolides"
                                  ))%>%
  arrange(`Drug group`, Period)%>%
  filter(Dataset=="Either")%>%
  select(-Dataset)%>%
  mutate(K=paste0(K, " (", `95% CI`, ")"))%>%
  select(-`95% CI`)%>%
  custom_tab(header=NULL,
             footer="Data derivation using the RCD sources based on the presence of a record in either the GP or Dispensing datasets. CRF: case-report form; RCD: routinely-collected data; CI: confidence interval")%>%
  add_header_row(values=c("", "Participants in each source (%)", "Agreement metrics"), colwidths = c(2, 5, 6))%>%
  add_header_lines("Agreement for baseline drug exposure between case-report form and routinely-collected data using different lookback periods before randomisation")%>%
  width(j=c(2:7, 1), width=3, unit="cm")%>%
  width(j=c(8:11, 1), width=2, unit="cm")%>%
  width(j=c(12, 1), width=4, unit="cm")%>%
  width(j=c(13, 1), width=4.5, unit="cm")%>%
  border(i=2, j=c(1,3), border.right = fp_border(), part="header")%>%
  border(i=3, j=3, border.right = fp_border(), part="header")%>%
  border(i=NULL, j=3, border.right = fp_border(), part="body")%>%
  border(i=3, j=c(2,7), border.right = fp_border(), part="header")%>%
  border(i=NULL, j=c(2,7), border.right = fp_border(), part="body")%>%
  border(i=c(5,10), j=NULL, border.bottom = fp_border(), part="body")%>%
  footnote(i=3,j=3, part = "header", ref_symbols=c("1"), value = as_paragraph("Sum of participants in both datasets, in CRF only, and in RCD only"))%>%
  # footnote(i=3,j=4:7, part = "header", ref_symbols=c("2"), value = as_paragraph("Proportion of all participants"))%>%
  merge_v(j=1)->table_agreement_crf_rcd_baseline
  
table_agreement_crf_rcd_baseline

save_as_docx(table_agreement_crf_rcd_baseline,
               path="Outputs/Tables/Thesis/agreement_crf_rcd_lookback.docx",
               pr_section=sect_properties)



rm(periods, Period)























# RESULTS: 2.2 Handling Dispensing temporal data -------------------------------



## trial different rules to calculate drug initiation after randomisation ---------------

### GP flags (constant) ------


as.Date("2021-05-31")-180

# GP data
gp_dt%>%
  select(study_number, date, code)%>% # select variables
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>% # join snomed codelists
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join rand dates
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  filter(date<rand_date)%>% # events happening before randomisation date
  mutate(time_before_rand = as.integer(difftime(rand_date, date, units = "days")))%>% # compute time before rand
  filter(time_before_rand<=90)%>% # include events in the 90 days prior
  mutate(dataset="GP")%>% # dataset flag
  select(study_number, code, codelist, date, rand_date, time_before_rand, dataset)%>% # trim variables
  as_tibble()->gp_baseline # store





### R1: ignore rand month, use day 1 ------

## calculate pts not at risk
# (defined as taking those drugs at baseline, i.e. within 3 months prior)

# timeframe for this is 6 months after randomisation, so we also need to first exclude people randomised after December 2020 (so 6 months extra is end of May 2021 - last month with complete data)


# Meds data
meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>% # select variables
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>% # join bnf codelists
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join rand dates
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>% # compute randomisation month
  filter(processing_period_date<rand_month)%>% # selecting events happening before the randomisation month (i.e. ignoring data in randomisation month)
  mutate(time_before_rand = abs(interval(rand_month, processing_period_date)%/%months(1)))%>% # calculate time before rand in months
  filter(time_before_rand<=3)%>% # select events happening in the 3 calendar months prior
  rename(code=prescribed_bnf_code, # renaming variables to align with gp data
         date=processing_period_date)%>%
  select(-rand_month)%>% # excluding unnecessary variable
  mutate(dataset="Dispensing")%>% # dataset flag
  select(study_number, code, codelist, date, rand_date, time_before_rand, dataset)%>% # trim variables
  as_tibble()->x2 # store


x<-rbind(gp_baseline, x2) # merge data from 2 datasets

rm(x2) # remove intermediate outputs


x%>%
  distinct(codelist, study_number) -> participants_not_at_risk 
# this contains a dataframe with participants identified as exposure at baseline (so not at risk) for each drug group
# note this is based on entries on either dataset (to reduce impact of different handling of temporal data)

rm(x) # remove intermediate output



# calculate overall drug initiation (days after rand)

# gp data
gp_dt%>%
  select(study_number, date, code)%>% # select variables
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>% # join snomed codelists
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join rand dates
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  filter(date>rand_date)%>% # restrict to events happening after randomisation date
  mutate(time_after_rand = as.integer(difftime(date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring for events at 180 days
  mutate(dataset="GP")%>% # dataset flag
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>% # trim variables
  as_tibble()->x1 # store

# meds data
meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>% # select variables
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>% # join bnf codelists
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join rand dates
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>% # compute randomisation month
  filter(processing_period_date>rand_month)%>% # selecting only events happening in the month after randomisation month (i.e. ignoring data in randomisation month)
  mutate(time_after_rand = as.integer(difftime(processing_period_date, rand_date, units = "days")))%>% # calculate days after randomisation (using first day of the month as provided)
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  rename(code=prescribed_bnf_code, # renaming variables
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>% # dataset flag
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>% # trim variables
  as_tibble()->x2 # store

x<-rbind(x1, x2) # merge intermediate outputs

rm (x2) # remove intermediate outputs



# restrict drug initiation to pts at risk for each category and produce survival dataset

drug_groups <- distinct(participants_not_at_risk, codelist)%>%.[[1]] # create list of drug groups

survival_data<-data.frame() # create empty dataframe to store survival data

for (i in drug_groups) { # for each drug group
  
  
  participants_not_at_risk%>% # take list of participants not at risk since already on drug at baseline (for each drug group)
    filter(codelist==i)%>% # trim to group of interest in each iteration
    select(study_number)%>% # select study number
    .[[1]]->participant_not_at_risk_list # extract list of participants already on drug before rand (for each group)
  
  rand_dates%>% # take randomisation dates
    filter(NationID=="EN")%>% # restrict to people randomised in England
    filter(is.na(Excluded))%>% # restrict to people not excluded (duplicates, withdrawn consent)
    filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
    filter(!study_number %in% participant_not_at_risk_list)%>% # exclude people on drug at randomisation 
    select(study_number)->participants_at_risk # list of patients at risk (for each group)
  
  
  x%>% # take combined drug initiation dataset
    select(study_number, codelist, time_after_rand, dataset)%>% # select some variables
    filter(codelist==i)%>% # select drug group for each iteration
    filter(!study_number %in% participant_not_at_risk_list)%>% # selecting only those people who are at risk of initiating drug for each group
    filter(time_after_rand<=180)%>% # right censoring at 180 days
    right_join(expand.grid(participants_at_risk%>%.[[1]], 
                           c("Dispensing", "GP"))%>%
                 rename(study_number=Var1,
                        dataset=Var2) # this code creates and right joins a dataframe that has one row for each participant and dataset, to ensure there is a censored entry also for people with no records of that drug (rather than just flags for events); i.e. it fills the gaps where there are no events
    )%>%
    arrange(study_number, dataset, time_after_rand)%>% # sorting by these variables in order
    group_by(study_number, dataset)%>% # grouping by participant and dataset
    slice_head(n=1)%>% # select first observation for each dataset (note we'd sorted by time_after_rand above)
    mutate(event=if_else(!is.na(time_after_rand), 2, 1), # apply event flags (2 if time after rand is not null, 1 if otherwise)
           time_after_rand=ifelse(event==1, 180, time_after_rand), # apply time after rand of 180 if censored
           codelist = i # apply codelist name to censored participants
    )%>%
    select(study_number, time_after_rand, dataset, event, codelist)  -> table # select the relevant variables and store in a table
  
  survival_data%<>%bind_rows(table) # add the table with the calculated survival data for this drug group to the survival dataset
  
  rm(table, i) # remove intermediate items
  
}

# DQ plot to check for distributions of the calculated time after randomisation for each drug group and dataset 
# survival_data%>%
#   filter(event=="2")%>%
#   ggplot(aes(time_after_rand, fill=dataset))+
#   geom_density()+
#   facet_wrap(~codelist)
# 
# # storing the plot for future reference
# ggsave("Outputs/Figures/baseline_drugs_recovery/time_after_rand_ignore_rand_month_first_day_density.png",
#        width=30,
#        height=15)




# add counts of people captured in both datasets for survival plots

# we will choose the first event for people who had events in both datasets, and make sure all other participants are censored

survival_data%>% # take survival data 
  group_by(codelist, study_number)%>% # group by drug group and study number
  filter(event=="2")%>% # select events only
  filter(n()>1)%>% # filter for people with more than one record (i.e. in more than one dataset)
  arrange(time_after_rand)%>% # sort by time to event
  slice_head(n=1)%>% # select first record for KM plot (which will be the event)
  ungroup()%>%
  right_join(survival_data%>%distinct(study_number, codelist), by=c("study_number", "codelist"))%>% # bring back all combinations of pts at risk for each codelist to fill gaps
  mutate(event=as.numeric(if_else(is.na(time_after_rand), "1", "2")), # apply event flag
         time_after_rand=as.numeric(replace_na(time_after_rand,180)))%>% # apply censoring date for those not captured in both datasets 
  mutate(dataset="Both") ->a # apply dataset flag and export

survival_data%<>%bind_rows(a) # store

rm(a) # remove intermediate output



# add counts in either dataset

# for each participant and codelist we just need to select the first event (which will be a drug record, or the censoring event if there is no record)

survival_data%>% # take survival data
  group_by(codelist, study_number)%>% # group by drug and participant
  arrange(time_after_rand)%>% # order by time after rand
  slice_head(n=1)%>%  # select first observation
  mutate(dataset="Either")->a # apply flag and store

survival_data%<>%bind_rows(a) # store

rm(a) # remove intermediate output



# produce survival model

fit<-survfit(
  Surv(time = time_after_rand, # specify variables for model (time, event flags, and grouping variable which in this case is the dataset)
       event) ~ dataset, 
  data=survival_data) 


survival_data%<>%left_join(codelist_labels) # join drug group labels

survival_data%<>%
  mutate(dataset=fct_relevel(dataset, "Either", "GP", "Dispensing", "Both")) # reorder dataset levels for plotting


# Kaplan-Meier plots


# ## main drup groups
# ggsurvplot_facet(fit, 
#                  fun="event", 
#                  data=survival_data,
#                  # conf.int = T, 
#                  # risk.table = T,
#                  # cumevents=T,
#                  legend.title="Dataset",
#                  # legend.labs=c("Dispensing", "GP"),
#                  title="Survival curve for drug initiation after randomisation according to each dataset",
#                  facet.by = "labels",
#                  short.panel.labs = T,
#                  # pval = T, 
#                  # pval.method = T,
#                  # scales="free_y",
#                  # nrow = 9,
# )+
#   theme_bw(base_size=15)+
#   facet_wrap(~labels, 
#              scales="free_y",
#              labeller = label_wrap_gen(width = 30)
#   )
# 
# # store plot
# ggsave("Outputs/Figures/baseline_drugs_recovery/drug_initiation_main.png",
#        last_plot(),
#        width=25,
#        height=10,
#        dpi="retina")








## agreement for initiation (binary)

survival_data%>%
  filter(dataset%in%c("GP", "Dispensing"))%>%
  mutate(flag=if_else(event=="2", 1,0))%>% # reshape event flag to fit what the package request 
  select(-event, -time_after_rand)%>% # remove variables not needed 
  pivot_wider(names_from = "dataset", values_from="flag")->t # transform to one column for each data set and store

t->t_rule1


t%>%distinct(study_number)->participants_list # list of all people included in this assessment 

t%>%distinct(study_number)%>%nrow()->participants # total number of people included in this assessment

#### calculate agreement

drug_groups <- t%>%distinct(codelist)%>%.[[1]] # select all drug groups

kappa_codelists_participants<-list() # create empty list to store results

# create list with agreement results
for (i in drug_groups) {
  
  t%>% # take binary flags data
    filter(codelist==i)%>% # select one drug group in each iteration
    mutate(study_number=as.character(study_number))%>% # reformat study number for join below
    right_join(participants_list)%>% # join list of all people (participants might not have a flag created before if they did not have data recorded during the period of analysis)
    mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  # create a 0 flag for people with no data in both datasets
    select(-codelist, -study_number)%>% # remove varibles not needed
    mutate(across(everything(), as.integer))->table # format variables as integers
  
  try(k<-Kappa.test(table$GP, table$Dispensing)) # create a K statistic for each drug group
  
  if(exists("k")) # if K exists (i.e. was possible to calculate)
  {
    
    kappa_codelists_participants[[i]]<-k # add it to the results list
    
  }
  else{
    kappa_codelists_participants[[i]]<-list() # if it does not exist, create an empty list for this drug group
  }
  
  rm(i, table, k) # remove intermediate outputs
  
}

# store agreement results in a table
kappa_codelists_participants_results<-data.frame()

for (i in drug_groups) { # for each drug group
  
  name<-i # assign the drug group to a string
  
  try(
    table<-(data.frame( # store each parameter in a separate variable in the table
      codelist = name,
      kappa = unlist(kappa_codelists_participants[[i]]$Result$estimate),
      CI_lower = unlist(kappa_codelists_participants[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_codelists_participants[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_codelists_participants[[i]]$Result$p.value),
      judgement = unlist(kappa_codelists_participants[[i]]$Judgement)
    ))
  )
  
  try(kappa_codelists_participants_results%<>%bind_rows(table)) # try to add this table to the results table
  
  
  rm(table, name, i)
  
}

# produce  binary agreement results table
kappa_codelists_participants_results%>% # take results table
  right_join(codelist_labels)%>% # join labels (right join to ensure there is one row per drug group even if there was no data)
  mutate_when(!is.na(CI_lower), # create a joint CI variable with lower and upper 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2), # round kappa
         p_value=as.character(round(p_value, 2)))%>% # round p_value
  
  mutate_when(p_value==0.00, # reformat p-value so that very small numbers are shown properly
              p_value="<0.001")%>%
  
  left_join ( # join counts of people in each of the datasets
    (t%>%
       group_by(codelist)%>%
       summarise(GP_Dispensing = n_unique(study_number[GP=="1" & Dispensing=="1"]), # counts in both dataets
                 GP_only = n_unique(study_number[GP=="1" & Dispensing=="0"]), # counts in GP only
                 Dispensing_only = n_unique(study_number[GP=="0" & Dispensing=="1"]), # counts in dispensing only
                 nil = participants - GP_Dispensing - GP_only - Dispensing_only)), # counts in neither
    by=c("codelist"))%>%
  arrange(labels)%>% # reorder rows
  select(labels, # select variables of interest
         GP_Dispensing,
         GP_only,
         Dispensing_only,
         nil,
         kappa,
         CI,
         p_value,
         judgement)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))-> codelists_agreement_participants # store reformatted table

View(codelists_agreement_participants)

# calculate overall agreement (all drug groups combined)

t%>%
  mutate(study_number=as.character(study_number))%>% # reformat study number for join below
  right_join(participants_list)%>% # join list of all people (participants might not have a flag created before if they did not have data recorded during the period of analysis)
  mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  # create a 0 flag for people with no data in both datasets
  #select(-codelist, -study_number)%>% # remove variables not needed
  mutate(across(c(GP, Dispensing), as.integer))%>%
  select(GP, Dispensing)->table

codelists_agreement_participants%<>%bind_rows(
  data.frame(
    labels="Aggregated",
    GP_Dispensing=NA,
    GP_only=NA,
    Dispensing_only=NA,
    nil=NA,
    kappa=round(Kappa.test(table$GP, table$Dispensing)$Result$estimate,2),
    CI=paste0(round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[1],2),
              " : ",
              round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[2],2)),
    p_value = Kappa.test(table$GP, table$Dispensing)$Result$p.value,
    judgement=Kappa.test(table$GP, table$Dispensing)$Judgement)%>%
    mutate(p_value=as.character(round(p_value,2)),
           p_value=if_else(p_value==0, "<0.001", p_value)))%>%
  arrange(labels)



# store results in joint agreement table (different rules)
initiation_agreement_rules_table<-
  codelists_agreement_participants%>%
  filter(labels=="Aggregated")%>%
  mutate(rand_month="ignore",
         date_rule="first_day")%>%
  select(rand_month,
         date_rule,
         kappa,
         CI)




## calculate day differences (for BA plots)

survival_data%>% # take survival data
  filter(event=="2")%>% # select events only
  group_by(study_number, codelist)%>% # group by participant and codelist
  filter(dataset%in%c("Dispensing", "GP"))%>% # remove rows for calculations of people in either or both datasets (retain only the intervals in each of the datasets)
  filter(n_distinct(dataset)>1)%>% # select people with an entry in each dataset
  ungroup()%>% # ungroup data
  select(study_number, codelist, time_after_rand, dataset)%>% # select some variables
  pivot_wider(names_from=dataset, values_from=time_after_rand)%>% # transform data so that for each participant and drug there is a column with time after rand in GP and another in Dispensing
  mutate(avg=(GP+Dispensing)/2, # calculate average between both datasets
         diff=Dispensing-GP)%>% # calculate difference between both datasets
  group_by(codelist)%>% # group by drug group
  mutate(mean_diff=mean(diff), # calculate mean difference across the entire group
         lower = mean_diff - 1.96*sd(diff), # calculate lower limit of agreement
         upper= mean_diff + 1.96*sd(diff))%>% # calculate upper limit of agreement
  left_join(codelist_labels) -> a # join codelist labels and store

# store BA data for this rule in a table that will contain all rules for plotting later on 
bland_altman_table_rules<-a%>%
  mutate(rand_month="ignore",
         date_rule="first_day")






### intraclass correlation coefficients
# using library irr
# model parameters specified based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/

# prepare data
survival_data%>% # take survival data
  filter(dataset%in%c("GP", "Dispensing"))%>% # select entries in each of the datasets
  filter(event=="2")%>% # select events only 
  select(study_number, dataset, time_after_rand, codelist)%>% # select some variables
  pivot_wider(names_from = dataset, values_from = time_after_rand)%>% # pivot time values to separate columns
  filter(!is.na(Dispensing) &!is.na(GP))->icc_data # filter for people with an event in both datasets and store

# produce icc analysis
icc(ratings = icc_data%>% # specify data
      select(-study_number, -codelist), # remove unnecessary variables
    model="twoway", # two way model as same set of raters for all subjects
    type="agreement", # agreement defined as absolute agreement as we are more interested in quantifying absolute agreement (difference in days) rather than consistency between measurements
    unit="average")->icc_result # unit measurement type is the average between both raters as we are not specifying one as best

icc_results<-list(icc_result) # store aggregated results (all classes combined) in a results list

# produce icc for each drug group 

drug_groups <- distinct(survival_data, codelist)%>%.[[1]] # create drug groups list

for (i in drug_groups) { # for each drug group
  
  
  icc(ratings = icc_data%>% # run the icc model as above
        filter(codelist==i)%>% # but filtering for one drug group in each iteration
        select(-study_number, -codelist), 
      model="twoway",
      type="agreement",
      unit="average")->icc_result
  
  icc_results[[i]]<-icc_result # store results for each drug group in the results list
  
  rm(i, icc_result)
  
}




# build table with results

icc_results_table<-data_frame() # create empty dataframe

for (i in drug_groups) { 
  
  name<-i
  
  try(
    table<-(data.frame( # this will extract the individual parameters from each element of the results list (for each drug group) and store them as new variables
      codelist = name,
      pairs = unlist(icc_results[[i]]$subjects),
      icc = unlist(icc_results[[i]]$value),
      CI_lower = unlist(icc_results[[i]]$lbound),
      CI_upper = unlist(icc_results[[i]]$ubound),
      p_value = unlist(icc_results[[i]]$p.value)
    ))
  )
  
  try(icc_results_table%<>%bind_rows(table)) # bind results to the results table
  
  
  rm(table, name, i)
  
}







# some work on the results table
data.frame(labels="Aggregated", # this creates a dataframe with the aggregated results from the first element of the results list
           pairs = unlist(icc_results[[1]]$subjects),
           icc = unlist(icc_results[[1]]$value),
           CI_lower = unlist(icc_results[[1]]$lbound),
           CI_upper = unlist(icc_results[[1]]$ubound),
           p_value = unlist(icc_results[[1]]$p.value))%>%
  bind_rows(icc_results_table%>% # joining results for  each drug group
              left_join(codelist_labels%>%select(labels, codelist))%>%
              arrange(labels))%>%
  mutate(across(3:6, ~round(., 2)))%>% # rounding
  mutate(p_value=as.character(p_value))%>% # tidying p-value structure
  mutate_when(p_value<0.001,
              p_value=as.character("<0.001"))%>% # this will reformat the p-value so that very small values are rounded for simplicity
  select(labels, pairs, icc, CI_lower, CI_upper, p_value, codelist)%>% # selecting some variables
  mutate(CI=paste0(CI_lower, " : ", CI_upper))%>% # creating a CI variable
  select(-CI_lower, -CI_upper)%>% # triming out
  as.data.frame()->icc_results_table # store



# produce flextable with results
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

# store flextable
save_as_docx(table_flex, path = "Outputs/Tables/icc_initiation_all_drugs.docx", pr_section = sect_properties)

mean_diff=mean(a$diff)
lower=mean_diff - 1.96*sd(a$diff) 
upper=mean_diff + 1.96*sd(a$diff)

# store icc values for this rule in a table that will contain all rules
icc_rules_table<-data.frame(rand_month="ignore",
                            date_rule="first_day",
                            mean_diff = mean_diff, 
                            lower_bound=lower, 
                            upper_bound=upper, 
                            icc = icc_results_table[icc_results_table$labels=="Aggregated", ]$icc,
                            icc_CI = icc_results_table[icc_results_table$labels=="Aggregated", ]$CI)



### aggregated BA plot

a%>%
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
  labs(caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, or no record in the 3 calendar months prior to the month of randomisation for the Dispensing dataset). Dispensing records occuring in the month of randomisation were ignored. Differences (vertical axis) calculated as days after randomisation in the Dispensing dataset minus GP dataset (i.e. positive differences depict events occuring in the GP dataset first). The black horizontal line depicts the mean difference across all observations, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 180))+
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





### produce agreement tables
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
  select(-codelist, -Judgement)%>%
  rename(Pairs=pairs, ICC=icc)%>%
  relocate(CI, .after=ICC)%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))

## bring back codelist groups

codelists_agreement_participants%<>%
  left_join(codelist_labels, by=c("Drug group" = "labels"))

# flextable (all drugs)

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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_all_drugs_ignore_rand_first_day.docx", pr_section = sect_properties)





codelists_agreement_participants%>%
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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_main_drugs_ignore_rand_first_day.docx", pr_section = sect_properties)


#### investigate differences in immunosuppressive drugs -------

# (requires loading of SNOMED FSN from the initial section of the script)

# 
# x%>%
#   filter(codelist=="immunosuppressive_drugs")%>%
#   distinct(study_number, dataset)%>%
#   arrange(study_number)%>%
#   group_by(study_number)%>%
#   filter(n()==1)->immunossuppresion_divergent_participants
# 
# x%>%
#   filter(codelist=="immunosuppressive_drugs",
#          dataset=="GP",
#          study_number %in% immunossuppresion_divergent_participants$study_number)%>%
#   select(study_number, code, dataset)%>%
#   bind_rows(
#     x%>%
#       filter(codelist=="immunosuppressive_drugs",
#              dataset=="Dispensing",
#              study_number %in% immunossuppresion_divergent_participants$study_number)%>%
#       select(study_number, code, dataset)
#   )%>%
#   group_by(code, dataset)%>%
#   summarise(participants=n_distinct(study_number))->immunosuppression_codes

# immunosuppression_codes%<>%
#   left_join(FSN%>%select(conceptId, term), by = c("code" = "conceptId"))

# 
# meds_dt%>%
#   distinct(prescribed_bnf_name, prescribed_bnf_code)%>%
#   rename(term=prescribed_bnf_name)%>%
#   as_tibble()->bnf_codes
# 
# immunosuppression_codes%<>%
#   left_join(bnf_codes, by=c("code"="prescribed_bnf_code"))
# 
# 
# immunosuppression_codes%<>%
#   filter(dataset=="GP")%>%
#   select(code, participants, dataset, term.x)%>%
#   rename(term=term.x)%>%
#   bind_rows(
#     immunosuppression_codes%>%
#       filter(dataset=="Dispensing")%>%
#       select(code, participants,  dataset, term.y)%>%
#       rename(term=term.y)
#   )%>%arrange(desc(participants))
# 
# 
# view(immunosuppression_codes)
# 
# write_csv(immunosuppression_codes, "Outputs/Tables/immunosuppression_codes.csv")
# 
# 
# x%>%
#   filter(codelist=="immunosuppressive_drugs")%>%
#   distinct(study_number, code, dataset)%>%
#   group_by(code, dataset)%>%
#   summarise(participants=n_distinct(study_number))->immunosuppression_codes_all
# 
# 
# immunosuppression_codes_all%<>%
#   left_join(FSN%>%select(conceptId, term), by = c("code" = "conceptId"))
# 
# 
# meds_dt%>%
#   distinct(prescribed_bnf_name, prescribed_bnf_code)%>%
#   rename(term=prescribed_bnf_name)%>%
#   as_tibble()->bnf_codes
# 
# immunosuppression_codes_all%<>%
#   left_join(bnf_codes, by=c("code"="prescribed_bnf_code"))
# 
# 
# immunosuppression_codes_all%<>%
#   filter(dataset=="GP")%>%
#   select(code, participants, dataset, term.x)%>%
#   rename(term=term.x)%>%
#   bind_rows(
#     immunosuppression_codes_all%>%
#       filter(dataset=="Dispensing")%>%
#       select(code, participants,  dataset, term.y)%>%
#       rename(term=term.y)
#   )%>%arrange(desc(participants))
# 
# 
# view(immunosuppression_codes_all)



# differences in codes:
## dispensing: inclusion of sulfasalazine, mesalazine, balsalize (not in GP extraction)
## meds: inclusion of cosentyx/secukinumab (not in BNF), some drugs that are in codelist not picked up by dispensing (but accounting for small numbers)


# so most differences are due to people not being captured rather than differences in codes; assume this is due to different patterns regarding who prescribes these drugs (hospital vs GP) and where they are collected (hospital pharmacy and not community)


rm(immunossuppresion_divergent_participants, immunosuppression_codes, immunosuppression_codes_all)

rm(x, fit, table, drug_groups, participant_not_at_risk_list, participants_at_risk, participants_not_at_risk, a, t, BA_data, partipants_list, participants, survival_data, icc_data, icc_results, kappa_codelist_participants, upper, lower, mean_diff, icc_results_table, table_flex, kappa_codelists_participants, kappa_codelists_participants_results, participants_list)




























### R2: ignoring rand month, use day 15 ---------



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


x<-rbind(gp_baseline, x2)

rm (x2)


x%>%distinct(codelist, study_number) -> participants_not_at_risk # this contains a dataframe with participants not at risk for each drug group
# note this is based on entries on either dataset (to reduce impact of different handling of temporal data)

rm(x)


## calculate  drug initiation 


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

meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>% # compute randomisation month
  filter(processing_period_date>rand_month)%>% # selecting only events happening after randomisation with some degree of certainty (following calendar month)
  mutate(processing_period_date=as.Date(paste0(str_sub(processing_period_date, 1, 8), "15")))%>% # input day 15
  mutate(time_after_rand = as.integer(difftime(processing_period_date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>%
  as_tibble()->x2

x<-rbind(x1, x2)

rm (x2)


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


# plot time after rand in each dataset and drug group

# 
# survival_data%>%
#   filter(event=="2")%>%
#   ggplot(aes(time_after_rand, fill=dataset))+
#   geom_density()+
#   facet_wrap(~codelist)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/time_after_rand_ignore_rand_day_15_density.png",
#        width=30,
#        height=15)

## overall day differences (for BA plots)

survival_data%>% # take survival data
  filter(event=="2")%>% # select events only
  group_by(study_number, codelist)%>% # group by participant and codelist
  filter(dataset%in%c("Dispensing", "GP"))%>% # remove rows for calculations of people in either or both datasets (retain only the intervals in each of the datasets)
  filter(n_distinct(dataset)>1)%>% # select people with an entry in each dataset
  ungroup()%>% # ungroup data
  select(study_number, codelist, time_after_rand, dataset)%>% # select some variables
  pivot_wider(names_from=dataset, values_from=time_after_rand)%>% # transform data so that for each participant and drug there is a column with time after rand in GP and another in Dispensing
  mutate(avg=(GP+Dispensing)/2, # calculate average between both datasets
         diff=Dispensing-GP)%>% # calculate difference between both datasets
  group_by(codelist)%>% # group by drug group
  mutate(mean_diff=mean(diff), # calculate mean difference across the entire group
         lower = mean_diff - 1.96*sd(diff), # calculate lower limit of agreement
         upper= mean_diff + 1.96*sd(diff))%>% # calculate upper limit of agreement
  left_join(codelist_labels) -> a # join codelist labels and store


# store ba results for aggregated plot with all rules
bland_altman_table_rules%<>%bind_rows(a%>%
                                        mutate(rand_month="ignore",
                                               date_rule="day_15"))



### intraclass correlation coefficients
# using library irr
# model parameters specified based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/

# prepare data
survival_data%>% # take survival data
  filter(dataset%in%c("GP", "Dispensing"))%>% # select entries in each of the datasets
  filter(event=="2")%>% # select events only 
  select(study_number, dataset, time_after_rand, codelist)%>% # select some variables
  pivot_wider(names_from = dataset, values_from = time_after_rand)%>% # pivot time values to separate columns
  filter(!is.na(Dispensing) &!is.na(GP))->icc_data # filter for people with an event in both datasets and store

# produce icc analysis
icc(ratings = icc_data%>% # specify data
      select(-study_number, -codelist), # remove unnecessary variables
    model="twoway", # two way model as same set of raters for all subjects
    type="agreement", # agreement defined as absolute agreement as we are more interested in quantifying absolute agreement (difference in days) rather than consistency between measurements
    unit="average")->icc_result # unit measurement type is the average between both raters as we are not specifying one as best

icc_results<-list(icc_result) # store aggregated results (all classes combined) in a results list

# produce icc for each drug group 

drug_groups <- distinct(survival_data, codelist)%>%.[[1]] # create drug groups list

for (i in drug_groups) { # for each drug group
  
  
  icc(ratings = icc_data%>% # run the icc model as above
        filter(codelist==i)%>% # but filtering for one drug group in each iteration
        select(-study_number, -codelist), 
      model="twoway",
      type="agreement",
      unit="average")->icc_result
  
  icc_results[[i]]<-icc_result # store results for each drug group in the results list
  
  rm(i, icc_result)
  
}




# build table with results

icc_results_table<-data_frame() # create empty dataframe

for (i in drug_groups) { 
  
  name<-i
  
  try(
    table<-(data.frame( # this will extract the individual parameters from each element of the results list (for each drug group) and store them as new variables
      codelist = name,
      pairs = unlist(icc_results[[i]]$subjects),
      icc = unlist(icc_results[[i]]$value),
      CI_lower = unlist(icc_results[[i]]$lbound),
      CI_upper = unlist(icc_results[[i]]$ubound),
      p_value = unlist(icc_results[[i]]$p.value)
    ))
  )
  
  try(icc_results_table%<>%bind_rows(table)) # bind results to the results table
  
  
  rm(table, name, i)
  
}







# some work on the results table
data.frame(labels="Aggregated", # this creates a dataframe with the aggregated results from the first element of the results list
           pairs = unlist(icc_results[[1]]$subjects),
           icc = unlist(icc_results[[1]]$value),
           CI_lower = unlist(icc_results[[1]]$lbound),
           CI_upper = unlist(icc_results[[1]]$ubound),
           p_value = unlist(icc_results[[1]]$p.value))%>%
  bind_rows(icc_results_table%>% # joining results for  each drug group
              left_join(codelist_labels%>%select(labels, codelist))%>%
              arrange(labels))%>%
  mutate(across(3:6, ~round(., 2)))%>% # rounding
  mutate(p_value=as.character(p_value))%>% # tidying p-value structure
  mutate_when(p_value<0.001,
              p_value=as.character("<0.001"))%>% # this will reformat the p-value so that very small values are rounded for simplicity
  select(labels, pairs, icc, CI_lower, CI_upper, p_value, codelist)%>% # selecting some variables
  mutate(CI=paste0(CI_lower, " : ", CI_upper))%>% # creating a CI variable
  select(-CI_lower, -CI_upper)%>% # triming out
  as.data.frame()->icc_results_table # store


mean_diff=mean(a$diff)
lower = mean_diff - 1.96*sd(a$diff)
upper= mean_diff + 1.96*sd(a$diff)

icc_rules_table%<>%bind_rows(
  data.frame(
    rand_month="ignore",
    date_rule="day_15", 
    mean_diff = mean_diff, 
    lower_bound=lower, 
    upper_bound=upper,
    icc = icc_results_table[icc_results_table$labels=="Aggregated", ]$icc,
    icc_CI = icc_results_table[icc_results_table$labels=="Aggregated", ]$CI))




# BA plot
a%>%
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
  labs(subtitle="Dispensing dates input as day 15 of each month",
       caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, or no record in the 3 calendar months prior to the month of randomisation for the Dispensing dataset). Dispensing records occuring in the month of randomisation were ignored. Differences (vertical axis) calculated as days after randomisation in the Dispensing dataset minus GP dataset (i.e. positive differences depict events occuring in the GP dataset first). The black horizontal line depicts the mean difference across all observations, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 180))+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot")

ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_ignore_rand_day_15.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")









## agreement for initiation

survival_data%>%
  filter(dataset%in%c("GP", "Dispensing"))%>%
  mutate(flag=if_else(event=="2", 1,0))%>% # reshape event flag to fit what the package request 
  select(-event, -time_after_rand)%>% # remove variables not needed 
  pivot_wider(names_from = "dataset", values_from="flag")->t # transform to one column for each data set and store

t->t_rule2


t%>%distinct(study_number)->participants_list

t%>%distinct(study_number)%>%nrow()->participants





#### calculate agreement

drug_groups <- t%>%distinct(codelist)%>%.[[1]]

kappa_codelists_participants<-list()

for (i in drug_groups) {
  
  t%>%
    filter(codelist==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  
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



# calculate overall agreement (all drug groups combined)

t%>%
  mutate(study_number=as.character(study_number))%>% # reformat study number for join below
  right_join(participants_list)%>% # join list of all people (participants might not have a flag created before if they did not have data recorded during the period of analysis)
  mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  # create a 0 flag for people with no data in both datasets
  #select(-codelist, -study_number)%>% # remove variables not needed
  mutate(across(c(GP, Dispensing), as.integer))%>%
  select(GP, Dispensing)->table

codelists_agreement_participants%<>%bind_rows(
  data.frame(
    labels="Aggregated",
    GP_Dispensing=NA,
    GP_only=NA,
    Dispensing_only=NA,
    nil=NA,
    kappa=round(Kappa.test(table$GP, table$Dispensing)$Result$estimate,2),
    CI=paste0(round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[1],2),
              " : ",
              round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[2],2)),
    p_value = Kappa.test(table$GP, table$Dispensing)$Result$p.value,
    judgement=Kappa.test(table$GP, table$Dispensing)$Judgement)%>%
    mutate(p_value=as.character(round(p_value,2)),
           p_value=if_else(p_value==0, "<0.001", p_value)))%>%
  arrange(labels)



# store results in joint agreement table (different rules)
initiation_agreement_rules_table<-rbind(initiation_agreement_rules_table,
                                        codelists_agreement_participants%>%
                                          filter(labels=="Aggregated")%>%
                                          mutate(rand_month="ignore",
                                                 date_rule="day_15")%>%
                                          select(rand_month,
                                                 date_rule,
                                                 kappa,
                                                 CI))





### produce tables
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
  select(-codelist, -Judgement)%>%
  rename(Pairs=pairs, ICC=icc)%>%
  relocate(CI, .after=ICC)%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))

## bring back codelist groups

codelists_agreement_participants%<>%
  left_join(codelist_labels, by=c("Drug group" = "labels"))


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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_all_drugs_ignore_rand_day_15.docx", pr_section = sect_properties)






codelists_agreement_participants%>%
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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_main_drugs_ignore_rand_day_15.docx", pr_section = sect_properties)

























rm(x, fit, table, drug_groups, participant_not_at_risk_list, participants_at_risk, participants_not_at_risk, a, t, BA_data, partipants_list, participants, survival_data, icc_data, icc_results, kappa_codelist_participants, upper, lower, mean_diff, icc_results_table, table_flex, kappa_codelists_participants, kappa_codelists_participants_results, participants_list, codelists_agreement_participants)





### R3: ignoring rand month, use last day ---------




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


x<-rbind(gp_baseline, x2)

rm (x2)


x%>%distinct(codelist, study_number) -> participants_not_at_risk # this contains a dataframe with participants not at risk for each drug group
# note this is based on entries on either dataset (to reduce impact of different handling of temporal data)

rm(x)



## calculate overall drug initiation 


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

rm (x2)


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



# plot time after rand in each dataset and drug group
# 
# 
# survival_data%>%
#   filter(event=="2")%>%
#   ggplot(aes(time_after_rand, fill=dataset))+
#   geom_density()+
#   facet_wrap(~codelist)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/time_after_rand_ignore_rand_last_day_density.png",
#        width=30,
#        height=15)

























































## overall day differences (for BA plots)

survival_data%>% # take survival data
  filter(event=="2")%>% # select events only
  group_by(study_number, codelist)%>% # group by participant and codelist
  filter(dataset%in%c("Dispensing", "GP"))%>% # remove rows for calculations of people in either or both datasets (retain only the intervals in each of the datasets)
  filter(n_distinct(dataset)>1)%>% # select people with an entry in each dataset
  ungroup()%>% # ungroup data
  select(study_number, codelist, time_after_rand, dataset)%>% # select some variables
  pivot_wider(names_from=dataset, values_from=time_after_rand)%>% # transform data so that for each participant and drug there is a column with time after rand in GP and another in Dispensing
  mutate(avg=(GP+Dispensing)/2, # calculate average between both datasets
         diff=Dispensing-GP)%>% # calculate difference between both datasets
  group_by(codelist)%>% # group by drug group
  mutate(mean_diff=mean(diff), # calculate mean difference across the entire group
         lower = mean_diff - 1.96*sd(diff), # calculate lower limit of agreement
         upper= mean_diff + 1.96*sd(diff))%>% # calculate upper limit of agreement
  left_join(codelist_labels) -> a # join codelist labels and store


### intraclass correlation coefficients
# using library irr
# model parameters specified based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/

# prepare data
survival_data%>% # take survival data
  filter(dataset%in%c("GP", "Dispensing"))%>% # select entries in each of the datasets
  filter(event=="2")%>% # select events only 
  select(study_number, dataset, time_after_rand, codelist)%>% # select some variables
  pivot_wider(names_from = dataset, values_from = time_after_rand)%>% # pivot time values to separate columns
  filter(!is.na(Dispensing) &!is.na(GP))->icc_data # filter for people with an event in both datasets and store

# produce icc analysis
icc(ratings = icc_data%>% # specify data
      select(-study_number, -codelist), # remove unnecessary variables
    model="twoway", # two way model as same set of raters for all subjects
    type="agreement", # agreement defined as absolute agreement as we are more interested in quantifying absolute agreement (difference in days) rather than consistency between measurements
    unit="average")->icc_result # unit measurement type is the average between both raters as we are not specifying one as best

icc_results<-list(icc_result) # store aggregated results (all classes combined) in a results list

# produce icc for each drug group 

drug_groups <- distinct(survival_data, codelist)%>%.[[1]] # create drug groups list

for (i in drug_groups) { # for each drug group
  
  
  icc(ratings = icc_data%>% # run the icc model as above
        filter(codelist==i)%>% # but filtering for one drug group in each iteration
        select(-study_number, -codelist), 
      model="twoway",
      type="agreement",
      unit="average")->icc_result
  
  icc_results[[i]]<-icc_result # store results for each drug group in the results list
  
  rm(i, icc_result)
  
}




# build table with results

icc_results_table<-data_frame() # create empty dataframe

for (i in drug_groups) { 
  
  name<-i
  
  try(
    table<-(data.frame( # this will extract the individual parameters from each element of the results list (for each drug group) and store them as new variables
      codelist = name,
      pairs = unlist(icc_results[[i]]$subjects),
      icc = unlist(icc_results[[i]]$value),
      CI_lower = unlist(icc_results[[i]]$lbound),
      CI_upper = unlist(icc_results[[i]]$ubound),
      p_value = unlist(icc_results[[i]]$p.value)
    ))
  )
  
  try(icc_results_table%<>%bind_rows(table)) # bind results to the results table
  
  
  rm(table, name, i)
  
}







# some work on the results table
data.frame(labels="Aggregated", # this creates a dataframe with the aggregated results from the first element of the results list
           pairs = unlist(icc_results[[1]]$subjects),
           icc = unlist(icc_results[[1]]$value),
           CI_lower = unlist(icc_results[[1]]$lbound),
           CI_upper = unlist(icc_results[[1]]$ubound),
           p_value = unlist(icc_results[[1]]$p.value))%>%
  bind_rows(icc_results_table%>% # joining results for  each drug group
              left_join(codelist_labels%>%select(labels, codelist))%>%
              arrange(labels))%>%
  mutate(across(3:6, ~round(., 2)))%>% # rounding
  mutate(p_value=as.character(p_value))%>% # tidying p-value structure
  mutate_when(p_value<0.001,
              p_value=as.character("<0.001"))%>% # this will reformat the p-value so that very small values are rounded for simplicity
  select(labels, pairs, icc, CI_lower, CI_upper, p_value, codelist)%>% # selecting some variables
  mutate(CI=paste0(CI_lower, " : ", CI_upper))%>% # creating a CI variable
  select(-CI_lower, -CI_upper)%>% # triming out
  as.data.frame()->icc_results_table # store


mean_diff=mean(a$diff)
lower = mean_diff - 1.96*sd(a$diff)
upper= mean_diff + 1.96*sd(a$diff)

icc_rules_table%<>%bind_rows(
  data.frame(
    rand_month="ignore",
    date_rule="last_day", 
    mean_diff = mean_diff, 
    lower_bound=lower, 
    upper_bound=upper,
    icc = icc_results_table[icc_results_table$labels=="Aggregated", ]$icc,
    icc_CI = icc_results_table[icc_results_table$labels=="Aggregated", ]$CI))


# store BA data for this rule
bland_altman_table_rules%<>%bind_rows(a%>%
                                        mutate(rand_month="ignore",
                                               date_rule="last_day"))

# BA plot
a%>%
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
  labs(subtitle="Dispensing dates input as the last day of each month",
       caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, or no record in the 3 calendar months prior to the month of randomisation for the Dispensing dataset). Dispensing records occuring in the month of randomisation were ignored. Differences (vertical axis) calculated as days after randomisation in the Dispensing dataset minus GP dataset (i.e. positive differences depict events occuring in the GP dataset first). The black horizontal line depicts the mean difference across all observations, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 180))+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot")

ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_ignore_rand_last_day.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")





## agreement for initiation

survival_data%>%
  filter(dataset%in%c("GP", "Dispensing"))%>%
  mutate(flag=if_else(event=="2", 1,0))%>% # reshape event flag to fit what the package request 
  select(-event, -time_after_rand)%>% # remove variables not needed 
  pivot_wider(names_from = "dataset", values_from="flag")->t # transform to one column for each data set and store

t->t_rule3


t%>%distinct(study_number)->participants_list

t%>%distinct(study_number)%>%nrow()->participants











#### calculate agreement

drug_groups <- t%>%distinct(codelist)%>%.[[1]]

kappa_codelists_participants<-list()

for (i in drug_groups) {
  
  t%>%
    filter(codelist==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  
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


# calculate overall agreement (all drug groups combined)

t%>%
  mutate(study_number=as.character(study_number))%>% # reformat study number for join below
  right_join(participants_list)%>% # join list of all people (participants might not have a flag created before if they did not have data recorded during the period of analysis)
  mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  # create a 0 flag for people with no data in both datasets
  #select(-codelist, -study_number)%>% # remove variables not needed
  mutate(across(c(GP, Dispensing), as.integer))%>%
  select(GP, Dispensing)->table

codelists_agreement_participants%<>%bind_rows(
  data.frame(
    labels="Aggregated",
    GP_Dispensing=NA,
    GP_only=NA,
    Dispensing_only=NA,
    nil=NA,
    kappa=round(Kappa.test(table$GP, table$Dispensing)$Result$estimate,2),
    CI=paste0(round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[1],2),
              " : ",
              round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[2],2)),
    p_value = Kappa.test(table$GP, table$Dispensing)$Result$p.value,
    judgement=Kappa.test(table$GP, table$Dispensing)$Judgement)%>%
    mutate(p_value=as.character(round(p_value,2)),
           p_value=if_else(p_value==0, "<0.001", p_value)))%>%
  arrange(labels)



# store results in joint agreement table (different rules)
initiation_agreement_rules_table<-rbind(initiation_agreement_rules_table,
                                        codelists_agreement_participants%>%
                                          filter(labels=="Aggregated")%>%
                                          mutate(rand_month="ignore",
                                                 date_rule="last_day")%>%
                                          select(rand_month,
                                                 date_rule,
                                                 kappa,
                                                 CI))


### produce tables
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
  select(-codelist, -Judgement)%>%
  rename(Pairs=pairs, ICC=icc)%>%
  relocate(CI, .after=ICC)%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))

## bring back codelist groups

codelists_agreement_participants%<>%
  left_join(codelist_labels, by=c("Drug group" = "labels"))


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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_all_drugs_ignore_rand_last_day.docx", pr_section = sect_properties)







codelists_agreement_participants%>%
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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_main_drugs_ignore_rand_last_day.docx", pr_section = sect_properties)
















rm(x, fit, table, drug_groups, participant_not_at_risk_list, participants_at_risk, participants_not_at_risk, a, t, BA_data, partipants_list, participants, survival_data, icc_data, icc_results, kappa_codelist_participants, upper, lower, mean_diff, icc_results_table, codelists_agreement_participants)


### R4: include rand month, use first day of the month  -------------------------------------------





meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  filter(processing_period_date<rand_date)%>% # selecting only events happening before randomisation 
  mutate(time_before_rand = as.integer(difftime(rand_date, processing_period_date, units = "days")))%>%
  filter(time_before_rand<=90)%>% # events happening in the 3 calendar months prior
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_before_rand, dataset)%>%
  as_tibble()->x2


x<-rbind(gp_baseline, x2)

rm(x2)


x%>%distinct(codelist, study_number) -> participants_not_at_risk # this contains a dataframe with participants not at risk for each drug group
# note this is based on entries on either dataset (to reduce impact of different handling of temporal data)

rm(x)

# calculate initiation

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


meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(processing_period_date= ceiling_date(processing_period_date, "month") - 1)%>%
  filter(processing_period_date>rand_date)%>% # selecting only events happening after randomisation 
  mutate(time_after_rand = as.integer(difftime(processing_period_date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>%
  as_tibble()->x2

x<-rbind(x1, x2)

rm (x1,x2)


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


# 
# survival_data%>%
#   filter(event=="2")%>%
#   ggplot(aes(time_after_rand, fill=dataset))+
#   geom_density()+
#   facet_wrap(~codelist)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/time_after_rand_include_rand_first_day_density.png",
#        width=30,
#        height=15)








## overall day differences (for BA plots)

survival_data%>% # take survival data
  filter(event=="2")%>% # select events only
  group_by(study_number, codelist)%>% # group by participant and codelist
  filter(dataset%in%c("Dispensing", "GP"))%>% # remove rows for calculations of people in either or both datasets (retain only the intervals in each of the datasets)
  filter(n_distinct(dataset)>1)%>% # select people with an entry in each dataset
  ungroup()%>% # ungroup data
  select(study_number, codelist, time_after_rand, dataset)%>% # select some variables
  pivot_wider(names_from=dataset, values_from=time_after_rand)%>% # transform data so that for each participant and drug there is a column with time after rand in GP and another in Dispensing
  mutate(avg=(GP+Dispensing)/2, # calculate average between both datasets
         diff=Dispensing-GP)%>% # calculate difference between both datasets
  group_by(codelist)%>% # group by drug group
  mutate(mean_diff=mean(diff), # calculate mean difference across the entire group
         lower = mean_diff - 1.96*sd(diff), # calculate lower limit of agreement
         upper= mean_diff + 1.96*sd(diff))%>% # calculate upper limit of agreement
  left_join(codelist_labels) -> a # join codelist labels and store





# store BA data for this rule
bland_altman_table_rules%<>%bind_rows(a%>%
                                        mutate(rand_month="use",
                                               date_rule="first_day"))






### intraclass correlation coefficients
# using library irr
# model parameters specified based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/

# prepare data
survival_data%>% # take survival data
  filter(dataset%in%c("GP", "Dispensing"))%>% # select entries in each of the datasets
  filter(event=="2")%>% # select events only 
  select(study_number, dataset, time_after_rand, codelist)%>% # select some variables
  pivot_wider(names_from = dataset, values_from = time_after_rand)%>% # pivot time values to separate columns
  filter(!is.na(Dispensing) &!is.na(GP))->icc_data # filter for people with an event in both datasets and store

# produce icc analysis
icc(ratings = icc_data%>% # specify data
      select(-study_number, -codelist), # remove unnecessary variables
    model="twoway", # two way model as same set of raters for all subjects
    type="agreement", # agreement defined as absolute agreement as we are more interested in quantifying absolute agreement (difference in days) rather than consistency between measurements
    unit="average")->icc_result # unit measurement type is the average between both raters as we are not specifying one as best

icc_results<-list(icc_result) # store aggregated results (all classes combined) in a results list

# produce icc for each drug group 

drug_groups <- distinct(survival_data, codelist)%>%.[[1]] # create drug groups list

for (i in drug_groups) { # for each drug group
  
  
  icc(ratings = icc_data%>% # run the icc model as above
        filter(codelist==i)%>% # but filtering for one drug group in each iteration
        select(-study_number, -codelist), 
      model="twoway",
      type="agreement",
      unit="average")->icc_result
  
  icc_results[[i]]<-icc_result # store results for each drug group in the results list
  
  rm(i, icc_result)
  
}




# build table with results

icc_results_table<-data_frame() # create empty dataframe

for (i in drug_groups) { 
  
  name<-i
  
  try(
    table<-(data.frame( # this will extract the individual parameters from each element of the results list (for each drug group) and store them as new variables
      codelist = name,
      pairs = unlist(icc_results[[i]]$subjects),
      icc = unlist(icc_results[[i]]$value),
      CI_lower = unlist(icc_results[[i]]$lbound),
      CI_upper = unlist(icc_results[[i]]$ubound),
      p_value = unlist(icc_results[[i]]$p.value)
    ))
  )
  
  try(icc_results_table%<>%bind_rows(table)) # bind results to the results table
  
  
  rm(table, name, i)
  
}







# some work on the results table
data.frame(labels="Aggregated", # this creates a dataframe with the aggregated results from the first element of the results list
           pairs = unlist(icc_results[[1]]$subjects),
           icc = unlist(icc_results[[1]]$value),
           CI_lower = unlist(icc_results[[1]]$lbound),
           CI_upper = unlist(icc_results[[1]]$ubound),
           p_value = unlist(icc_results[[1]]$p.value))%>%
  bind_rows(icc_results_table%>% # joining results for  each drug group
              left_join(codelist_labels%>%select(labels, codelist))%>%
              arrange(labels))%>%
  mutate(across(3:6, ~round(., 2)))%>% # rounding
  mutate(p_value=as.character(p_value))%>% # tidying p-value structure
  mutate_when(p_value<0.001,
              p_value=as.character("<0.001"))%>% # this will reformat the p-value so that very small values are rounded for simplicity
  select(labels, pairs, icc, CI_lower, CI_upper, p_value, codelist)%>% # selecting some variables
  mutate(CI=paste0(CI_lower, " : ", CI_upper))%>% # creating a CI variable
  select(-CI_lower, -CI_upper)%>% # triming out
  as.data.frame()->icc_results_table # store


mean_diff=mean(a$diff)
lower = mean_diff - 1.96*sd(a$diff)
upper= mean_diff + 1.96*sd(a$diff)

icc_rules_table%<>%bind_rows(
  data.frame(
    rand_month="use",
    date_rule="first_day", 
    mean_diff = mean_diff, 
    lower_bound=lower, 
    upper_bound=upper,
    icc = icc_results_table[icc_results_table$labels=="Aggregated", ]$icc,
    icc_CI = icc_results_table[icc_results_table$labels=="Aggregated", ]$CI))


# BA plot for this rule

a%>%
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
  labs(caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in either  dataset, and using first day of the month for all analyses in the Dispensing dataset). Differences (vertical axis) calculated as days after randomisation in the Dispensing dataset minus GP dataset (i.e. positive differences depict events occuring in the GP dataset first). The black horizontal line depicts the mean difference across all observations, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 180),
       subtitle="Using first day of the month in all analyses in the Dispensing dataset")+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot")



ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_use_rand_first_day.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")






#### calculate agreement


survival_data%>%
  filter(dataset%in%c("GP", "Dispensing"))%>%
  mutate(flag=if_else(event=="2", 1,0))%>% # reshape event flag to fit what the package request 
  select(-event, -time_after_rand)%>% # remove variables not needed 
  pivot_wider(names_from = "dataset", values_from="flag")->t # transform to one column for each data set and store

t->t_rule4


t%>%distinct(study_number)->participants_list

t%>%distinct(study_number)%>%nrow()->participants


drug_groups <- t%>%distinct(codelist)%>%.[[1]]

kappa_codelists_participants<-list()

for (i in drug_groups) {
  
  t%>%
    filter(codelist==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  
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

# calculate overall agreement (all drug groups combined)

t%>%
  mutate(study_number=as.character(study_number))%>% # reformat study number for join below
  right_join(participants_list)%>% # join list of all people (participants might not have a flag created before if they did not have data recorded during the period of analysis)
  mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  # create a 0 flag for people with no data in both datasets
  #select(-codelist, -study_number)%>% # remove variables not needed
  mutate(across(c(GP, Dispensing), as.integer))%>%
  select(GP, Dispensing)->table

codelists_agreement_participants%<>%bind_rows(
  data.frame(
    labels="Aggregated",
    GP_Dispensing=NA,
    GP_only=NA,
    Dispensing_only=NA,
    nil=NA,
    kappa=round(Kappa.test(table$GP, table$Dispensing)$Result$estimate,2),
    CI=paste0(round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[1],2),
              " : ",
              round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[2],2)),
    p_value = Kappa.test(table$GP, table$Dispensing)$Result$p.value,
    judgement=Kappa.test(table$GP, table$Dispensing)$Judgement)%>%
    mutate(p_value=as.character(round(p_value,2)),
           p_value=if_else(p_value==0, "<0.001", p_value)))%>%
  arrange(labels)



# store results in joint agreement table (different rules)
initiation_agreement_rules_table<-rbind(initiation_agreement_rules_table,
                                        codelists_agreement_participants%>%
                                          filter(labels=="Aggregated")%>%
                                          mutate(rand_month="use",
                                                 date_rule="first_day")%>%
                                          select(rand_month,
                                                 date_rule,
                                                 kappa,
                                                 CI))


### produce tables
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
  select(-codelist, -Judgement)%>%
  rename(Pairs=pairs, ICC=icc)%>%
  relocate(CI, .after=ICC)%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))

## bring back codelist groups

codelists_agreement_participants%<>%
  left_join(codelist_labels, by=c("Drug group" = "labels"))


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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_all_drugs_use_rand_first_day.docx", pr_section = sect_properties)






codelists_agreement_participants%>%
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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_main_drugs_use_rand_first_day.docx", pr_section = sect_properties)









































rm(x, fit, table, drug_groups, participant_not_at_risk_list, participants_at_risk, participants_not_at_risk, a, t, BA_data, partipants_list, participants, survival_data, icc_data, icc_results, kappa_codelist_participants, upper, lower, mean_diff, icc_results_table, codelists_agreement_participants)











### R5: include rand month, use day 15  -------------------------------------------




meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(processing_period_date=as.Date(paste0(str_sub(processing_period_date, 1, 8), "15")))%>% # input day 15
  filter(processing_period_date<rand_date)%>% # selecting only events happening before randomisation 
  mutate(time_before_rand = as.integer(difftime(rand_date, processing_period_date, units = "days")))%>%
  filter(time_before_rand<=90)%>% # events happening in the 90 days prior
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_before_rand, dataset)%>%
  as_tibble()->x2


x<-rbind(gp_baseline, x2)

rm (x2)


x%>%distinct(codelist, study_number) -> participants_not_at_risk # this contains a dataframe with participants not at risk for each drug group
# note this is based on entries on either dataset (to reduce impact of different handling of temporal data)

rm(x)




# calculate initiation
gp_dt%>%
  select(study_number, date, code)%>%
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  filter(date>rand_date)%>% # restrict to events happening after randomisation date
  mutate(time_after_rand = as.integer(difftime(date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  mutate(dataset="GP")%>%
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>%
  as_tibble()->x1


meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(processing_period_date=as.Date(paste0(str_sub(processing_period_date, 1, 8), "15")))%>% # input day 15
  filter(processing_period_date>rand_date)%>% # select records after randomsiation date
  mutate(time_after_rand = as.integer(difftime(processing_period_date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>%
  as_tibble()->x2

x<-rbind(x1, x2)

rm (x1,x2)


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

# plot time after rand in each dataset and drug group

# 
# survival_data%>%
#   filter(event=="2")%>%
#   ggplot(aes(time_after_rand, fill=dataset))+
#   geom_density()+
#   facet_wrap(~codelist)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/time_after_rand_include_rand_day_15_density.png",
#        width=30,
#        height=15)



## overall day differences (for BA plots)

survival_data%>% # take survival data
  filter(event=="2")%>% # select events only
  group_by(study_number, codelist)%>% # group by participant and codelist
  filter(dataset%in%c("Dispensing", "GP"))%>% # remove rows for calculations of people in either or both datasets (retain only the intervals in each of the datasets)
  filter(n_distinct(dataset)>1)%>% # select people with an entry in each dataset
  ungroup()%>% # ungroup data
  select(study_number, codelist, time_after_rand, dataset)%>% # select some variables
  pivot_wider(names_from=dataset, values_from=time_after_rand)%>% # transform data so that for each participant and drug there is a column with time after rand in GP and another in Dispensing
  mutate(avg=(GP+Dispensing)/2, # calculate average between both datasets
         diff=Dispensing-GP)%>% # calculate difference between both datasets
  group_by(codelist)%>% # group by drug group
  mutate(mean_diff=mean(diff), # calculate mean difference across the entire group
         lower = mean_diff - 1.96*sd(diff), # calculate lower limit of agreement
         upper= mean_diff + 1.96*sd(diff))%>% # calculate upper limit of agreement
  left_join(codelist_labels) -> a # join codelist labels and store




# store BA data for this rule
bland_altman_table_rules%<>%bind_rows(a%>%
                                        mutate(rand_month="use",
                                               date_rule="day_15"))







### intraclass correlation coefficients
# using library irr
# model parameters specified based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/

# prepare data
survival_data%>% # take survival data
  filter(dataset%in%c("GP", "Dispensing"))%>% # select entries in each of the datasets
  filter(event=="2")%>% # select events only 
  select(study_number, dataset, time_after_rand, codelist)%>% # select some variables
  pivot_wider(names_from = dataset, values_from = time_after_rand)%>% # pivot time values to separate columns
  filter(!is.na(Dispensing) &!is.na(GP))->icc_data # filter for people with an event in both datasets and store

# produce icc analysis
icc(ratings = icc_data%>% # specify data
      select(-study_number, -codelist), # remove unnecessary variables
    model="twoway", # two way model as same set of raters for all subjects
    type="agreement", # agreement defined as absolute agreement as we are more interested in quantifying absolute agreement (difference in days) rather than consistency between measurements
    unit="average")->icc_result # unit measurement type is the average between both raters as we are not specifying one as best

icc_results<-list(icc_result) # store aggregated results (all classes combined) in a results list

# produce icc for each drug group 

drug_groups <- distinct(survival_data, codelist)%>%.[[1]] # create drug groups list

for (i in drug_groups) { # for each drug group
  
  
  icc(ratings = icc_data%>% # run the icc model as above
        filter(codelist==i)%>% # but filtering for one drug group in each iteration
        select(-study_number, -codelist), 
      model="twoway",
      type="agreement",
      unit="average")->icc_result
  
  icc_results[[i]]<-icc_result # store results for each drug group in the results list
  
  rm(i, icc_result)
  
}




# build table with results

icc_results_table<-data_frame() # create empty dataframe

for (i in drug_groups) { 
  
  name<-i
  
  try(
    table<-(data.frame( # this will extract the individual parameters from each element of the results list (for each drug group) and store them as new variables
      codelist = name,
      pairs = unlist(icc_results[[i]]$subjects),
      icc = unlist(icc_results[[i]]$value),
      CI_lower = unlist(icc_results[[i]]$lbound),
      CI_upper = unlist(icc_results[[i]]$ubound),
      p_value = unlist(icc_results[[i]]$p.value)
    ))
  )
  
  try(icc_results_table%<>%bind_rows(table)) # bind results to the results table
  
  
  rm(table, name, i)
  
}







# some work on the results table
data.frame(labels="Aggregated", # this creates a dataframe with the aggregated results from the first element of the results list
           pairs = unlist(icc_results[[1]]$subjects),
           icc = unlist(icc_results[[1]]$value),
           CI_lower = unlist(icc_results[[1]]$lbound),
           CI_upper = unlist(icc_results[[1]]$ubound),
           p_value = unlist(icc_results[[1]]$p.value))%>%
  bind_rows(icc_results_table%>% # joining results for  each drug group
              left_join(codelist_labels%>%select(labels, codelist))%>%
              arrange(labels))%>%
  mutate(across(3:6, ~round(., 2)))%>% # rounding
  mutate(p_value=as.character(p_value))%>% # tidying p-value structure
  mutate_when(p_value<0.001,
              p_value=as.character("<0.001"))%>% # this will reformat the p-value so that very small values are rounded for simplicity
  select(labels, pairs, icc, CI_lower, CI_upper, p_value, codelist)%>% # selecting some variables
  mutate(CI=paste0(CI_lower, " : ", CI_upper))%>% # creating a CI variable
  select(-CI_lower, -CI_upper)%>% # triming out
  as.data.frame()->icc_results_table # store


mean_diff=mean(a$diff)
lower = mean_diff - 1.96*sd(a$diff)
upper= mean_diff + 1.96*sd(a$diff)

icc_rules_table%<>%bind_rows(
  data.frame(
    rand_month="use",
    date_rule="day_15", 
    mean_diff = mean_diff, 
    lower_bound=lower, 
    upper_bound=upper,
    icc = icc_results_table[icc_results_table$labels=="Aggregated", ]$icc,
    icc_CI = icc_results_table[icc_results_table$labels=="Aggregated", ]$CI))



# produce BA plot for this rule
a%>%
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
  labs(caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in either dataset, and inputing the 15th day of the month for all records in the Dispensing dataset. Differences (vertical axis) calculated as days after randomisation in the Dispensing dataset minus GP dataset (i.e. positive differences depict events occuring in the GP dataset first). The black horizontal line depicts the mean difference across all observations, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 180),
       subtitle="Inputting Dispensing date as the 15th of the month in the Dispensing dataset")+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot")



ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_use_rand_day_15.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")








#### calculate agreement

survival_data%>%
  filter(dataset%in%c("GP", "Dispensing"))%>%
  mutate(flag=if_else(event=="2", 1,0))%>% # reshape event flag to fit what the package request 
  select(-event, -time_after_rand)%>% # remove variables not needed 
  pivot_wider(names_from = "dataset", values_from="flag")->t # transform to one column for each data set and store

t->t_rule5


t%>%distinct(study_number)->participants_list

t%>%distinct(study_number)%>%nrow()->participants


drug_groups <- t%>%distinct(codelist)%>%.[[1]]

kappa_codelists_participants<-list()

for (i in drug_groups) {
  
  t%>%
    filter(codelist==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  
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

# calculate overall agreement (all drug groups combined)

t%>%
  mutate(study_number=as.character(study_number))%>% # reformat study number for join below
  right_join(participants_list)%>% # join list of all people (participants might not have a flag created before if they did not have data recorded during the period of analysis)
  mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  # create a 0 flag for people with no data in both datasets
  #select(-codelist, -study_number)%>% # remove variables not needed
  mutate(across(c(GP, Dispensing), as.integer))%>%
  select(GP, Dispensing)->table

codelists_agreement_participants%<>%bind_rows(
  data.frame(
    labels="Aggregated",
    GP_Dispensing=NA,
    GP_only=NA,
    Dispensing_only=NA,
    nil=NA,
    kappa=round(Kappa.test(table$GP, table$Dispensing)$Result$estimate,2),
    CI=paste0(round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[1],2),
              " : ",
              round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[2],2)),
    p_value = Kappa.test(table$GP, table$Dispensing)$Result$p.value,
    judgement=Kappa.test(table$GP, table$Dispensing)$Judgement)%>%
    mutate(p_value=as.character(round(p_value,2)),
           p_value=if_else(p_value==0, "<0.001", p_value)))%>%
  arrange(labels)



# store results in joint agreement table (different rules)
initiation_agreement_rules_table<-rbind(initiation_agreement_rules_table,
                                        codelists_agreement_participants%>%
                                          filter(labels=="Aggregated")%>%
                                          mutate(rand_month="use",
                                                 date_rule="day_15")%>%
                                          select(rand_month,
                                                 date_rule,
                                                 kappa,
                                                 CI))


### produce tables
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
  select(-codelist, -Judgement)%>%
  rename(Pairs=pairs, ICC=icc)%>%
  relocate(CI, .after=ICC)%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))

## bring back codelist groups

codelists_agreement_participants%<>%
  left_join(codelist_labels, by=c("Drug group" = "labels"))

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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_all_drugs_use_rand_day_15.docx", pr_section = sect_properties)






codelists_agreement_participants%>%
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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_main_drugs_use_rand_day_15.docx", pr_section = sect_properties)

























































rm(x, fit, table, drug_groups, participant_not_at_risk_list, participants_at_risk, participants_not_at_risk, a, t, BA_data, partipants_list, participants, survival_data, icc_data, icc_results, kappa_codelist_participants, upper, lower, mean_diff, icc_results_table, codelists_agreement_participants)







### R6: include rand month, use last day of the month  -------------------------------------------




meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(processing_period_date= ceiling_date(processing_period_date, "month") - 1)%>%
  filter(processing_period_date<rand_date)%>% # selecting only events happening after randomisation 
  mutate(time_before_rand = as.integer(difftime(rand_date, processing_period_date, units = "days")))%>%
  filter(time_before_rand<=90)%>% # events happening in the 90 days prior
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_before_rand, dataset)%>%
  as_tibble()->x2


x<-rbind(gp_baseline, x2)

rm (x2)


x%>%distinct(codelist, study_number) -> participants_not_at_risk # this contains a dataframe with participants not at risk for each drug group
# note this is based on entries on either dataset (to reduce impact of different handling of temporal data)

rm(x)


# calculate initiation

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

meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<=as.Date("2021-05-31")-180)%>% # censoring to people who could have completed 180 days follow-up at 31-05-21
  mutate(processing_period_date= ceiling_date(processing_period_date, "month") - 1)%>%
  filter(processing_period_date>rand_date)%>% # selecting only events happening after randomisation 
  mutate(time_after_rand = as.integer(difftime(processing_period_date, rand_date, units = "days")))%>% # calculate days after randomisation
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(dataset="Dispensing")%>%
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>%
  as_tibble()->x2

x<-rbind(x1, x2)

rm (x1,x2)



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


# plot calculations for time after rand
# survival_data%>%
#   filter(event=="2")%>%
#   ggplot(aes(time_after_rand, fill=dataset))+
#   geom_density()+
#   facet_wrap(~codelist)
# 
# ggsave("Outputs/Figures/baseline_drugs_recovery/time_after_rand_include_rand_last_day_density.png",
#        width=30,
#        height=15)



## overall day differences (for BA plots)

survival_data%>% # take survival data
  filter(event=="2")%>% # select events only
  group_by(study_number, codelist)%>% # group by participant and codelist
  filter(dataset%in%c("Dispensing", "GP"))%>% # remove rows for calculations of people in either or both datasets (retain only the intervals in each of the datasets)
  filter(n_distinct(dataset)>1)%>% # select people with an entry in each dataset
  ungroup()%>% # ungroup data
  select(study_number, codelist, time_after_rand, dataset)%>% # select some variables
  pivot_wider(names_from=dataset, values_from=time_after_rand)%>% # transform data so that for each participant and drug there is a column with time after rand in GP and another in Dispensing
  mutate(avg=(GP+Dispensing)/2, # calculate average between both datasets
         diff=Dispensing-GP)%>% # calculate difference between both datasets
  group_by(codelist)%>% # group by drug group
  mutate(mean_diff=mean(diff), # calculate mean difference across the entire group
         lower = mean_diff - 1.96*sd(diff), # calculate lower limit of agreement
         upper= mean_diff + 1.96*sd(diff))%>% # calculate upper limit of agreement
  left_join(codelist_labels) -> a # join codelist labels and store


# store BA data for this rule
bland_altman_table_rules%<>%bind_rows(a%>%
                                        mutate(rand_month="use",
                                               date_rule="last_day"))





### intraclass correlation coefficients
# using library irr
# model parameters specified based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/

# prepare data
survival_data%>% # take survival data
  filter(dataset%in%c("GP", "Dispensing"))%>% # select entries in each of the datasets
  filter(event=="2")%>% # select events only 
  select(study_number, dataset, time_after_rand, codelist)%>% # select some variables
  pivot_wider(names_from = dataset, values_from = time_after_rand)%>% # pivot time values to separate columns
  filter(!is.na(Dispensing) &!is.na(GP))->icc_data # filter for people with an event in both datasets and store

# produce icc analysis
icc(ratings = icc_data%>% # specify data
      select(-study_number, -codelist), # remove unnecessary variables
    model="twoway", # two way model as same set of raters for all subjects
    type="agreement", # agreement defined as absolute agreement as we are more interested in quantifying absolute agreement (difference in days) rather than consistency between measurements
    unit="average")->icc_result # unit measurement type is the average between both raters as we are not specifying one as best

icc_results<-list(icc_result) # store aggregated results (all classes combined) in a results list

# produce icc for each drug group 

drug_groups <- distinct(survival_data, codelist)%>%.[[1]] # create drug groups list

for (i in drug_groups) { # for each drug group
  
  
  icc(ratings = icc_data%>% # run the icc model as above
        filter(codelist==i)%>% # but filtering for one drug group in each iteration
        select(-study_number, -codelist), 
      model="twoway",
      type="agreement",
      unit="average")->icc_result
  
  icc_results[[i]]<-icc_result # store results for each drug group in the results list
  
  rm(i, icc_result)
  
}




# build table with results

icc_results_table<-data_frame() # create empty dataframe

for (i in drug_groups) { 
  
  name<-i
  
  try(
    table<-(data.frame( # this will extract the individual parameters from each element of the results list (for each drug group) and store them as new variables
      codelist = name,
      pairs = unlist(icc_results[[i]]$subjects),
      icc = unlist(icc_results[[i]]$value),
      CI_lower = unlist(icc_results[[i]]$lbound),
      CI_upper = unlist(icc_results[[i]]$ubound),
      p_value = unlist(icc_results[[i]]$p.value)
    ))
  )
  
  try(icc_results_table%<>%bind_rows(table)) # bind results to the results table
  
  
  rm(table, name, i)
  
}







# some work on the results table
data.frame(labels="Aggregated", # this creates a dataframe with the aggregated results from the first element of the results list
           pairs = unlist(icc_results[[1]]$subjects),
           icc = unlist(icc_results[[1]]$value),
           CI_lower = unlist(icc_results[[1]]$lbound),
           CI_upper = unlist(icc_results[[1]]$ubound),
           p_value = unlist(icc_results[[1]]$p.value))%>%
  bind_rows(icc_results_table%>% # joining results for  each drug group
              left_join(codelist_labels%>%select(labels, codelist))%>%
              arrange(labels))%>%
  mutate(across(3:6, ~round(., 2)))%>% # rounding
  mutate(p_value=as.character(p_value))%>% # tidying p-value structure
  mutate_when(p_value<0.001,
              p_value=as.character("<0.001"))%>% # this will reformat the p-value so that very small values are rounded for simplicity
  select(labels, pairs, icc, CI_lower, CI_upper, p_value, codelist)%>% # selecting some variables
  mutate(CI=paste0(CI_lower, " : ", CI_upper))%>% # creating a CI variable
  select(-CI_lower, -CI_upper)%>% # triming out
  as.data.frame()->icc_results_table # store


mean_diff=mean(a$diff)
lower = mean_diff - 1.96*sd(a$diff)
upper= mean_diff + 1.96*sd(a$diff)

# store icc data from this rule
icc_rules_table%<>%bind_rows(
  data.frame(
    rand_month="use",
    date_rule="last_day", 
    mean_diff = mean_diff, 
    lower_bound=lower, 
    upper_bound=upper,
    icc = icc_results_table[icc_results_table$labels=="Aggregated", ]$icc,
    icc_CI = icc_results_table[icc_results_table$labels=="Aggregated", ]$CI))



# BA plot for this rule
a%>%
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
  labs(caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in either dataset, and inputing last day of the month for all records in the Dispensing dataset). Differences (vertical axis) calculated as days after randomisation in the Dispensing dataset minus GP dataset (i.e. positive differences depict events occuring in the GP dataset first). The black horizontal line depicts the mean difference across all observations, while the dashed lines depict 95% confidence intervals. ICC: intraclass correlation coefficient", 180),
       subtitle="Converting Dispensing date to last date of the processing month (instead of first)")+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot")



ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_use_rand_last_day.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")


### calculate agreement -------

survival_data%>%
  filter(dataset%in%c("GP", "Dispensing"))%>%
  mutate(flag=if_else(event=="2", 1,0))%>% # reshape event flag to fit what the package request 
  select(-event, -time_after_rand)%>% # remove variables not needed 
  pivot_wider(names_from = "dataset", values_from="flag")->t # transform to one column for each data set and store

t->t_rule6

t%>%distinct(study_number)->participants_list

t%>%distinct(study_number)%>%nrow()->participants

drug_groups <- t%>%distinct(codelist)%>%.[[1]]

kappa_codelists_participants<-list()

for (i in drug_groups) {
  
  t%>%
    filter(codelist==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  
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

# calculate overall agreement (all drug groups combined)

t%>%
  mutate(study_number=as.character(study_number))%>% # reformat study number for join below
  right_join(participants_list)%>% # join list of all people (participants might not have a flag created before if they did not have data recorded during the period of analysis)
  mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  # create a 0 flag for people with no data in both datasets
  #select(-codelist, -study_number)%>% # remove variables not needed
  mutate(across(c(GP, Dispensing), as.integer))%>%
  select(GP, Dispensing)->table

codelists_agreement_participants%<>%bind_rows(
  data.frame(
    labels="Aggregated",
    GP_Dispensing=NA,
    GP_only=NA,
    Dispensing_only=NA,
    nil=NA,
    kappa=round(Kappa.test(table$GP, table$Dispensing)$Result$estimate,2),
    CI=paste0(round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[1],2),
              " : ",
              round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[2],2)),
    p_value = Kappa.test(table$GP, table$Dispensing)$Result$p.value,
    judgement=Kappa.test(table$GP, table$Dispensing)$Judgement)%>%
    mutate(p_value=as.character(round(p_value,2)),
           p_value=if_else(p_value==0, "<0.001", p_value)))%>%
  arrange(labels)



# store results in joint agreement table (different rules)
initiation_agreement_rules_table<-rbind(initiation_agreement_rules_table,
                                        codelists_agreement_participants%>%
                                          filter(labels=="Aggregated")%>%
                                          mutate(rand_month="use",
                                                 date_rule="last_day")%>%
                                          select(rand_month,
                                                 date_rule,
                                                 kappa,
                                                 CI))



### produce tables
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
  select(-codelist, -Judgement)%>%
  rename(Pairs=pairs, ICC=icc)%>%
  relocate(CI, .after=ICC)%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))

## bring back codelist groups

codelists_agreement_participants%<>%
  left_join(codelist_labels, by=c("Drug group" = "labels"))


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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_all_drugs_use_rand_day_last.docx", pr_section = sect_properties)







codelists_agreement_participants%>%
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


save_as_docx(table_flex, path = "Outputs/Tables/agreement_initiation_main_drugs_use_rand_last_day.docx", pr_section = sect_properties)





rm(x, fit, table, drug_groups, participant_not_at_risk_list, participants_at_risk, participants_not_at_risk, a, t, BA_data, partipants_list, participants, survival_data, icc_data, icc_results, kappa_codelist_participants, upper, lower, mean_diff, icc_results_table, codelists_agreement_participants)




### BA plots with different rules ------

# redo categories in different tables
# BA plot data
bland_altman_table_rules%<>%
  ungroup()%>%
  mutate(rand_month= if_else(rand_month=="use", "Use data in month of randomisation", "Ignore data in month of randomisation"),
         date_rule=case_when(date_rule=="day_15" ~"Input day 15",
                             date_rule=="first_day" ~ "Use day 1 (as provided)",
                             date_rule=="last_day"~"Input last day of month"))

# kappa table
initiation_agreement_rules_table%<>%
  mutate(rand_month= if_else(rand_month=="use", "Use data in month of randomisation", "Ignore data in month of randomisation"),
         date_rule=case_when(date_rule=="day_15" ~"Input day 15",
                             date_rule=="first_day" ~ "Use day 1 (as provided)",
                             date_rule=="last_day"~"Input last day of month"))

# icc table
icc_rules_table%<>%
  mutate(rand_month= if_else(rand_month=="use", "Use data in month of randomisation", "Ignore data in month of randomisation"),
         date_rule=case_when(date_rule=="day_15" ~"Input day 15",
                             date_rule=="first_day" ~ "Use day 1 (as provided)",
                             date_rule=="last_day"~"Input last day of month"))

# merge kappa and icc tables into general agreement table

agreement_rules_table<-
  icc_rules_table%>%
  left_join(initiation_agreement_rules_table, by=c("date_rule", "rand_month"))

#rm(icc_rules_table, initiation_agreement_rules_table)

# combined BA plot with different rules
bland_altman_table_rules%>%
  ungroup()%>%
  # mutate(date_rule = factor(date_rule, levels=c("Use day 1 (as provided)", "Input day 15", "Input last day of month")))%>%
  ggplot(aes(avg, diff))+
  geom_point(alpha=0.2)+
  facet_wrap(rand_month~factor(date_rule, levels=c("Use day 1 (as provided)", "Input day 15", "Input last day of month")),
             #switch = "y"
  )+
  geom_hline(data=agreement_rules_table, aes(yintercept = mean_diff)) +
  geom_hline(data=agreement_rules_table, aes(yintercept = lower_bound), color = "red", linetype="dashed") +
  geom_hline(data=agreement_rules_table, aes(yintercept = upper_bound), color = "red", linetype="dashed")+
  
  ylab("Difference Between Measurements\n(Dispensing-GP)") +
  xlab("Average Measurement")+
  theme_gray(base_size=20)+
  labs(caption = str_wrap("Calculations restricted to drug initation among participants not taking a particular drug at randomisation (defined as no record in the previous 90 days in the GP dataset, and applying different rules for the Dispensing dataset in each facet). Differences (vertical axis) calculated as days after randomisation in the Dispensing dataset minus GP dataset (i.e. positive differences depict events occuring in the GP dataset first). The black horizontal line depicts the mean difference across all observations, while the dashed lines depict 95% limits of agreement. K: Cohen's kappa; ICC: intraclass correlation coefficient", 180),
       subtitle="Each facet depicts a different derivation rule for handling of temporal data in the Dispensing dataset",
       title="Bland-Altman plots for drug initation after randomisation in the GP and Dispensing datasets")+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot")+
  scale_x_continuous(limits=c(0,200))+
  geom_label(data=agreement_rules_table, aes(x= 180, y=agreement_rules_table$mean_diff, label=paste0("Mean: ", round(agreement_rules_table$mean_diff, 2))), color="black")+
  geom_label(data=agreement_rules_table, aes(x= 180, y=agreement_rules_table$lower*1.5, label=paste0("-1.96 SD: ", round(agreement_rules_table$lower_bound, 2))), color="black")+
  geom_label(data=agreement_rules_table, aes(x= 180, y=agreement_rules_table$upper*1.5, label=paste0("+1.96 SD: ",round(agreement_rules_table$upper_bound, 2))), color="black")+
  geom_text(data=agreement_rules_table, aes(x= 180, y=160, label=paste0("K: ", kappa, " (",CI, ")\n", "ICC: ", icc, " (", icc_CI, ")")), color="black")


ggsave("Outputs/Figures/baseline_drugs_recovery/bland_altmann_rules.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")






## Explore impact of different rules to deal with date fields--------

# some drug groups as examples
drugs_rules <- c("antiplatelets",
                 "diabetes_drugs_not_insulin",
                 "ics",
                 "antibacterials",
                 # "macrolides",
                 "antifungals",
                 "immunosuppressive_drugs",
                 "Systemic steroids",
                 "oral_anticoagulants",
                 "insulin")

### baseline intervals ---------

## GP 
# NB these will be the same for all rules

## need to restrict to people who completed 6 months follow-up 2 months before extract reception
censoring_rand_date <- as.Date(last_date) - 60 - 180 # last_date is latest date of randomisation for people included in both datasets

gp_dt%>% # dataset
  select(study_number, date, code)%>% # select some variables
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>% # join codelists
  filter(codelist %in% drugs_rules)%>% # filter to drug groups of interest
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join randomisation dates
  filter(rand_date<=censoring_rand_date)%>% # allow 2 month lag between data reception and analysis and 180 days of follow-up
  mutate(time_from_rand = as.integer(difftime(rand_date, date, units = "days")))%>% # calculate time before randomization in days
  mutate(dataset="GP")%>% # apply dataset flag
  select(study_number, code, codelist, date, rand_date, time_from_rand, dataset)%>% # select some variables
  rename(original_date = date)%>%
  as_tibble()->x1 # store

## Dispensing
meds_dt%>% # dataset
  select(study_number, prescribed_bnf_code, processing_period_date)%>% # select some variables
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>% # join codelists
  filter(codelist %in% drugs_rules)%>% # filter to drug groups of interest
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join randomisation dates
  filter(rand_date<=censoring_rand_date)%>% # allow 2 month lag between data reception and analysis and 180 days of follow-up
  mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>% # extract record month from processing_period_date
  mutate(months_from_rand = interval(processing_period_date, rand_month)%/%months(1))%>% # calculate number of months before randomsiation
  rename(code=prescribed_bnf_code, # renaming variables
         original_date=processing_period_date)%>%
  select(-rand_month)%>% # excluding randomisation month (after calculations)
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, code, codelist, original_date, rand_date, months_from_rand, dataset)%>% # selecting some varibles
  as_tibble()->x2 # store


### GP flags -------

gp_flags<-x1%>%
  mutate(Baseline = ifelse(time_from_rand>=0 & time_from_rand<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(time_from_rand>=-180 & time_from_rand< 0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>% # each participant and drug can only be either baseline or initiation
  mutate(dataset="GP")%>%
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)


### Dispensing flags -------

#### Rule 1 (day 1; ignore data in month of randomisation) ------

#### without GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(date_rule_days = as.integer(difftime(rand_date, original_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R1",
         dataset="Dispensing only")->R1_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(date_rule_days = as.integer(difftime(rand_date, original_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R1",
         dataset="GP and Dispensing")->R1_with_gp











#### Rule 2 (day 15; ignore data in month of randomisation) ------

#### without GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(rule_date=as.Date(paste0(str_sub(original_date, 1, 8), "15")))%>% # APPLY RULE: INPUT DAY 15
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R2",
         dataset="Dispensing only")->R2_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(rule_date=as.Date(paste0(str_sub(original_date, 1, 8), "15")))%>% # APPLY RULE: INPUT DAY 15
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R2",
         dataset="GP and Dispensing")->R2_with_gp










#### Rule 3 (last day; ignore data in month of randomisation) ------

#### without GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(rule_date = ceiling_date(original_date, "month") - days(1))%>% # APPLY RULE: INPUT LAST DAY
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R3",
         dataset="Dispensing only")->R3_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(rule_date = ceiling_date(original_date, "month") - days(1))%>% # APPLY RULE: INPUT LAST DAY
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R3",
         dataset="GP and Dispensing")->R3_with_gp






#### Rule 4 (day 1; use data in month of randomisation) ------

#### without GP
x2%>%
  
  mutate(date_rule_days = as.integer(difftime(rand_date, original_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R4",
         dataset="Dispensing only")->R4_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  
  mutate(date_rule_days = as.integer(difftime(rand_date, original_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R4",
         dataset="GP and Dispensing")->R4_with_gp




#### Rule 5 (day 15; use data in month of randomisation) ------

#### without GP
x2%>%
  mutate(rule_date=as.Date(paste0(str_sub(original_date, 1, 8), "15")))%>% # APPLY RULE: INPUT DAY 15
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R5",
         dataset="Dispensing only")->R5_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  mutate(rule_date=as.Date(paste0(str_sub(original_date, 1, 8), "15")))%>% # APPLY RULE: INPUT DAY 15
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R5",
         dataset="GP and Dispensing")->R5_with_gp


#### Rule 6 (last day; use data in month of randomisation) ------

#### without GP
x2%>%
  mutate(rule_date = ceiling_date(original_date, "month") - days(1))%>% # APPLY RULE: INPUT LAST DAY
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R6",
         dataset="Dispensing only")->R6_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  mutate(rule_date = ceiling_date(original_date, "month") - days(1))%>% # APPLY RULE: INPUT LAST DAY
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R6",
         dataset="GP and Dispensing")->R6_with_gp


#### merge all tables ------
rules_table<-R1_no_gp%>%
  rbind(R1_with_gp)%>%
  rbind(R2_no_gp)%>%
  rbind(R2_with_gp)%>%
  rbind(R3_no_gp)%>%
  rbind(R3_with_gp)%>%
  rbind(R4_no_gp)%>%
  rbind(R4_with_gp)%>%
  rbind(R5_no_gp)%>%
  rbind(R5_with_gp)%>%
  rbind(R6_no_gp)%>%
  rbind(R6_with_gp)

### calculate proportions
rules_table%>%
  filter(codelist %in% drugs_rules)%>%
  left_join(codelist_labels)%>%
  group_by(labels, rule, dataset, Event)%>%
  summarise(Prop = round(n_distinct(study_number)/length(RECOVERY)*100,1))->rules_props

#### plot ----

rules_props%>%
  mutate(labels=factor(labels,levels=c( 
                            "Antibacterials",
                            # "Macrolides",
                            "Antifungals",
                            "Antiplatelets",
                            "Oral anticoagulants",
                            "Insulin",
                            "Diabetes drugs (non-insulin)",
                            "Inhaled corticosteroids",
                            "Immunosuppressive drugs (non-steroidal)")))%>%
  mutate(Event=case_when(Event=="Baseline" ~ "Baseline exposure",
                         Event=="Initiation" ~ "Initiation after randomisation"),
         Event=factor(Event, levels=c("Initiation after randomisation", "Baseline exposure")))%>%
  
  ggplot(aes(rule, Prop, fill=Event))+
  geom_col(position="stack", color="black")+
  geom_text(aes(label=Prop), 
            position=position_stack(vjust=0.5),
            color="black",
            size=5)+
  facet_grid(cols=vars(labels),
             rows=vars(dataset), 
             scales = "free_y", 
             labeller = label_wrap_gen(width = 15),
             switch="y"
  )+
  labs(title="Impact of different Dispensing data handling rules on baseline drug exposure and initiation",
       subtitle="Split by dataset used (Dispensing only, or GP and Dispensing)",
       y="Participants (%)",
       x="",
       fill="Event",
       caption="Rule derivation logic:\nR1-R3: ignore data in month of randomisaiton, input day 1 (R1), day 15 (R2) or last day of the month (R3)\nR4-R6: use data in month of randomisation, input day 1 (R4), day 15 (R5) or last day of the month (R6)"
  )+
  theme_gray(base_size=20)+
  theme(legend.position ="bottom",
        strip.placement="outside")+
  scale_fill_discrete(breaks=c("Baseline exposure", "Initiation after randomisation"))



ggsave("Outputs/Figures/baseline_drugs_recovery/baseline_initiation_rules_proportions.png",
       width=60,
       height=30,
       dpi="retina",
       units="cm")


## Final table (agreement metrics for initiation GP vs Dispensing)-----

t_rule1%>%
  mutate(rule="r1")%>%
  select(-labels)%>%
  rbind(t_rule2%>%mutate(rule="r2"))%>%
  rbind(t_rule3%>%mutate(rule="r3"))%>%
  rbind(t_rule4%>%mutate(rule="r4"))%>%
  rbind(t_rule5%>%mutate(rule="r5"))%>%
  rbind(t_rule6%>%mutate(rule="r6"))%>%
  group_by(rule)%>%
  summarise(GP_only=length(study_number[GP==1 & Dispensing ==0]),
                        Dispensing_only = length(study_number[GP==0 & Dispensing ==1]),
                        Both = length(study_number[GP==1 & Dispensing ==1]),
                        Neither = length(study_number[GP==0 & Dispensing ==0]))%>%
  rowwise()%>%
  mutate(sum=sum(GP_only, Both, Dispensing_only, Neither))%>%
  pivot_longer(-c(rule, sum), names_to="Group", values_to="Count")%>%
  mutate(prop=round(Count/sum*100,1))%>%
  View()
  
  


View(agreement_rules_table)


agreement_rules_table%>%
  distinct(rand_month, date_rule, .keep_all=T)%>%
  mutate(across(c(mean_diff, upper_bound, lower_bound), ~round(.,1)),
         date_rule = factor(date_rule, levels=c("Use day 1 (as provided)", "Input day 15", "Input last day of month")))%>%
  arrange(rand_month, date_rule)%>%
  transmute(`Handling of Dispensing data in month of randomisation` = rand_month,
            `Handling of Dispensing dates` = date_rule,
            `Kappa (95% CI)` = paste0(kappa, " (", CI, ")"),
            `Average day difference (95% LA)` = paste0(mean_diff, " (", lower_bound, " : ", upper_bound, ")"),
            `Intraclass correlation coefficient (95% CI)` = paste0(icc, " (", icc_CI, ")"))%>%View()
  custom_tab(header="Agreement metrics for drug initiation events using different data handling rules in the Dispensing dataset (across all drug groups)", 
             footer="CI: confidence interval; LA: limits of agreement")%>%
  merge_at(i=1:3, j=1, part="body")%>%
  merge_at(i=4:6, j=1, part="body")%>%
  align(i=NULL, j=NULL, align="left", part="all")%>%
  width(j=1, width=3)%>%
  width(j=2, width=2)%>%
  width(j=3:5, width=2)%>%
  flextable::font(fontname="Mulish")->initiation_date_rules_flextable  


save_as_docx(initiation_date_rules_flextable, path = "Outputs/Tables/Thesis/agreement_rules_final_table.docx", pr_section = sect_properties)













































# Comparison with CRF -----

## the calculations below will be the exact same as above ("Explore impact of different rules to deal with date fields") but without having to restrict to people who have completed 6 month follow-up


##### baseline intervals ---------

## GP 
# NB these will be the same for all rules

gp_dt%>% # dataset
  select(study_number, date, code)%>% # select some variables
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>% # join codelists
  filter(codelist %in% drugs_rules)%>% # filter to drug groups of interest
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join randomisation dates
  mutate(time_from_rand = as.integer(difftime(rand_date, date, units = "days")))%>% # calculate time before randomization in days
  mutate(dataset="GP")%>% # apply dataset flag
  select(study_number, code, codelist, date, rand_date, time_from_rand, dataset)%>% # select some variables
  rename(original_date = date)%>%
  as_tibble()->x1 # store

## Dispensing
meds_dt%>% # dataset
  select(study_number, prescribed_bnf_code, processing_period_date)%>% # select some variables
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>% # join codelists
  filter(codelist %in% drugs_rules)%>% # filter to drug groups of interest
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join randomisation dates
  mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>% # extract record month from processing_period_date
  mutate(months_from_rand = interval(processing_period_date, rand_month)%/%months(1))%>% # calculate number of months before randomsiation
  rename(code=prescribed_bnf_code, # renaming variables
         original_date=processing_period_date)%>%
  select(-rand_month)%>% # excluding randomisation month (after calculations)
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, code, codelist, original_date, rand_date, months_from_rand, dataset)%>% # selecting some variables
  as_tibble()->x2 # store


###### GP flags -------

gp_flags<-x1%>%
  mutate(Baseline = ifelse(time_from_rand>=0 & time_from_rand<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(time_from_rand>=-180 & time_from_rand<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>% # each participant and drug can only be either baseline or initiation
  mutate(dataset="GP")%>%
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)

rm(x1)

###### Dispensing flags -------

###### Rule 1 (day 1; ignore data in month of randomisation) ------

#### without GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(date_rule_days = as.integer(difftime(rand_date, original_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R1",
         dataset="Dispensing only")->R1_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(date_rule_days = as.integer(difftime(rand_date, original_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R1",
         dataset="GP and Dispensing")->R1_with_gp











###### Rule 2 (day 15; ignore data in month of randomisation) ------

#### without GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(rule_date=as.Date(paste0(str_sub(original_date, 1, 8), "15")))%>% # APPLY RULE: INPUT DAY 15
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R2",
         dataset="Dispensing only")->R2_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(rule_date=as.Date(paste0(str_sub(original_date, 1, 8), "15")))%>% # APPLY RULE: INPUT DAY 15
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R2",
         dataset="GP and Dispensing")->R2_with_gp










###### Rule 3 (last day; ignore data in month of randomisation) ------

#### without GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(rule_date = ceiling_date(original_date, "month") - days(1))%>% # APPLY RULE: INPUT LAST DAY
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R3",
         dataset="Dispensing only")->R3_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  filter(months_from_rand!=0)%>% # APPLY RULE: REMOVE EVENTS IN MONTH OF RANDOMISATION
  mutate(rule_date = ceiling_date(original_date, "month") - days(1))%>% # APPLY RULE: INPUT LAST DAY
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R3",
         dataset="GP and Dispensing")->R3_with_gp






###### Rule 4 (day 1; use data in month of randomisation) ------

#### without GP
x2%>%
  
  mutate(date_rule_days = as.integer(difftime(rand_date, original_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R4",
         dataset="Dispensing only")->R4_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  
  mutate(date_rule_days = as.integer(difftime(rand_date, original_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R4",
         dataset="GP and Dispensing")->R4_with_gp




###### Rule 5 (day 15; use data in month of randomisation) ------

#### without GP
x2%>%
  mutate(rule_date=as.Date(paste0(str_sub(original_date, 1, 8), "15")))%>% # APPLY RULE: INPUT DAY 15
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R5",
         dataset="Dispensing only")->R5_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  mutate(rule_date=as.Date(paste0(str_sub(original_date, 1, 8), "15")))%>% # APPLY RULE: INPUT DAY 15
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R5",
         dataset="GP and Dispensing")->R5_with_gp


###### Rule 6 (last day; use data in month of randomisation) ------

#### without GP
x2%>%
  mutate(rule_date = ceiling_date(original_date, "month") - days(1))%>% # APPLY RULE: INPUT LAST DAY
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  select(study_number, codelist, Event)%>%
  mutate(rule="R6",
         dataset="Dispensing only")->R6_no_gp # apply rule flag and dataset used and store

# with GP
x2%>%
  mutate(rule_date = ceiling_date(original_date, "month") - days(1))%>% # APPLY RULE: INPUT LAST DAY
  
  mutate(date_rule_days = as.integer(difftime(rand_date, rule_date, units="days")))%>% # compute day difference for this rule
  mutate(Baseline = ifelse(date_rule_days>=0 & date_rule_days<=90, 1, 0), # event is baseline if recorded in 90 days before rand
         Initiation = ifelse(date_rule_days>=-180 & date_rule_days<0, 1, 0))%>% # event is initiation if recorded in the 180 days after rand
  filter(Baseline==1 | Initiation == 1)%>%  # remove events not meeting above criteria
  group_by(study_number, codelist)%>% 
  summarise(Event=if_else(any(Baseline==1), "Baseline", "Initiation"))%>%# each participant and drug can only be baseline or initiation
  mutate(dataset="Dispensing")%>% # apply dataset flag
  select(study_number, codelist, Event, dataset)%>% # trim
  pivot_wider(c(study_number, codelist), names_from = dataset, values_from=Event)%>%
  
  full_join(gp_flags)%>% # join gp data
  mutate(across(c(Dispensing, GP), ~replace_na(., "0")))%>%
  group_by(study_number, codelist)%>%
  summarise(Event=if_else(GP=="Baseline" | Dispensing=="Baseline", "Baseline", "Initiation"))%>% # baseline if any are baseline, otherwise initiation
  ungroup()%>%
  mutate(rule="R6",
         dataset="GP and Dispensing")->R6_with_gp


###### merge all tables ------
rules_table<-R1_no_gp%>%
  rbind(R1_with_gp)%>%
  rbind(R2_no_gp)%>%
  rbind(R2_with_gp)%>%
  rbind(R3_no_gp)%>%
  rbind(R3_with_gp)%>%
  rbind(R4_no_gp)%>%
  rbind(R4_with_gp)%>%
  rbind(R5_no_gp)%>%
  rbind(R5_with_gp)%>%
  rbind(R6_no_gp)%>%
  rbind(R6_with_gp)



###### merge CRF and RCD flags ----

crf_drugs_agreement_flags%>%
  distinct(study_number)%>%
  nrow() # 34025 people with CRF data for meds

rules_agreement_flags_rcd_crf<-
  crf_drugs_agreement_flags%>%
  rename(crf=flag)%>%
  mutate(study_number=as.character(study_number))%>%
  filter(!is.na(crf))%>%
  left_join(
    rules_table%>%
      ungroup()%>%
      filter(study_number%in%crf_drugs_agreement_flags$study_number)%>%
      filter(codelist %in% c(# "macrolides", 
                             "antiplatelets", "oral_anticoagulants"))%>%
      filter(Event=="Baseline",
             dataset=="Dispensing only")%>%
      select(-Event, -dataset)%>%
      mutate(RCD="1")%>%
      filter(study_number %in% crf_drugs_agreement_flags$study_number)%>%
      full_join(expand.grid(rule=c("R1", "R2", "R3", "R4", "R5", "R6"),
                  codelist = c(# "macrolides", 
                    "antiplatelets", "oral_anticoagulants"),
                  study_number=as.character(crf_drugs_agreement_flags%>%distinct(study_number)%>%.[[1]])))%>%
      mutate(RCD=replace_na(RCD, "0")),
    by=c("codelist", "study_number")
    )%>%
  filter(!is.na(crf))%>%
  arrange(study_number, codelist, rule)

rules_agreement_flags_rcd_crf%>%
  count(codelist, rule, RCD, crf)%>%View()


####### calculate agreement ----

drug_groups <- c(# "macrolides", 
                 "antiplatelets", "oral_anticoagulants")

rules<-rules_agreement_flags_rcd_crf%>%
  distinct(rule)%>%
  .[[1]]

kappa_crf_rcd<-list()

for (i in drug_groups) {
    
    for (j in rules){
      
      rules_agreement_flags_rcd_crf%>%
        filter(codelist==i,
               rule==j)%>%
        mutate(study_number=as.character(study_number))%>%
        select(-codelist, -study_number, -rule)%>%
        mutate(across(everything(), as.integer))->table
      
      try(k<-Kappa.test(table$crf, table$RCD))
      
      if(exists("k"))
      {
        
        kappa_crf_rcd[[i]][[j]]<-k
        
      }
      else{
        kappa_crf_rcd[[i]][[j]]<-list()
      }
      
      rm(table, k)
    }
    
    rm(i)
  }


###### unlist result for table -----

kappa_crf_rcd_results<-data.frame()

for (i in drug_groups) {
    
    for (j in rules){
      
      codelist<-i
      
      Rule <- j
      
      
      
      try(
        table<-(data.frame(
          codelist = codelist,
          Rule = Rule,
          kappa = unlist(kappa_crf_rcd[[i]][[j]]$Result$estimate),
          CI_lower = unlist(kappa_crf_rcd[[i]][[j]]$Result$conf.int[1]),
          CI_upper = unlist(kappa_crf_rcd[[i]][[j]]$Result$conf.int[2]),
          p_value = unlist(kappa_crf_rcd[[i]][[j]]$Result$p.value),
          judgement = unlist(kappa_crf_rcd[[i]][[j]]$Judgement)
        ))
      )
      
      try(kappa_crf_rcd_results%<>%bind_rows(table))
      
      
      rm(table, name, j)
      
    }
    rm(i)   
  }
  
View(kappa_crf_rcd_results)
  

###### produce table     ------

# format results  
kappa_crf_rcd_results%>%
  left_join(codelist_labels)%>%
  mutate_when(!is.na(CI_lower), 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate_when(p_value==0.00,
              p_value="<0.001")%>%
  as_tibble()%>%
  
  # add calculations of number of people in each source
  left_join(
    rules_agreement_flags_rcd_crf%>%
      rename(Rule=rule)%>%
      filter(codelist %in% drug_groups)%>%
      group_by(codelist, Rule)%>%
      summarise(CRF_RCD = n_unique(study_number[crf=="1" & RCD=="1"]),
                CRF_only = n_unique(study_number[crf=="1" & RCD=="0"]),
                RCD_only = n_unique(study_number[crf=="0" & RCD=="1"]),
                nil = n_distinct(study_number) - CRF_RCD - CRF_only - RCD_only,
                Sn = round(CRF_RCD/(CRF_RCD+CRF_only)*100,1),
                Sp=round(nil/(nil+RCD_only)*100,1),
                PPV = round(CRF_RCD/(CRF_RCD+RCD_only)*100,1),
                NPV = round(nil/(nil+CRF_only)*100,1))%>%
      ungroup(),
    by=c("codelist", "Rule"))%>%
  arrange(labels, Rule)%>%
  select(Label=labels,
         Rule,
         CRF_RCD,
         CRF_only,
         RCD_only,
         nil,
         Sn,
         Sp,
         PPV,
         NPV,
         kappa,
         CI,
         p_value,
         judgement)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))%>%
  
  # calculate proportions and format
  rowwise() %>%
  mutate(prop = round(sum(across(3:5), na.rm = T)/sum(across(3:6), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either' = paste0(sum(across(3:5), na.rm = T),
                           " (",
                           prop,
                           "%)"))%>%
  mutate(prop1 = round(sum(across(3:3))/sum(across(3:6), na.rm = T)*100,1),
         prop2 = round(sum(across(4:4))/sum(across(3:6), na.rm = T)*100,1),
         prop3 = round(sum(across(5:5))/sum(across(3:6), na.rm = T)*100,1),
         prop4 = round(sum(across(6:6))/sum(across(3:6), na.rm = T)*100,1))%>%
  mutate(prop1=if_else(is.na(prop1), 0, prop1))%>%
  mutate(prop2=if_else(is.na(prop2), 0, prop2))%>%
  mutate(prop3=if_else(is.na(prop3), 0, prop3))%>%
  mutate(prop4=if_else(is.na(prop4), 0, prop4))%>%
  mutate(CRF_RCD=paste0(CRF_RCD, " (", prop1, "%)"),
         CRF_only=paste0(CRF_only, " (", prop2, "%)"),
         RCD_only=paste0(RCD_only, " (", prop3, "%)"),
         nil=paste0(nil, " (", prop4, "%)"))%>%
  select(-c(prop, prop1, prop2, prop3, prop4))%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%
  rename('Drug group' = Label,
         K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement,
         Neither= nil)%>%
  relocate('Either', .after = 'Drug group')%>%
  relocate(Rule, .after = 'Drug group')%>%
  arrange(`Drug group`, Rule)-> crf_rcd_rules_results_table


View(crf_rcd_rules_results_table)


crf_rcd_rules_results_table%>%
  filter(`Drug group` !="Macrolides")%>%
  # mutate(Rule = case_when(Rule=="R1" ~ "Ignore month of randomisation, use day 1",
  #                         Rule=="R2" ~ "Ignore month of randomisation, input day 15",
  #                         Rule=="R3" ~ "Include month of randomisation, input last day of month",
  #                         Rule=="R4" ~ "Include month of randomisation, use day 1",
  #                         Rule=="R5" ~ "Include month of randomisation, input day 15",
  #                         Rule=="R6" ~ "Include month of randomisation, input last day of month",
  # ))%>%
  mutate(`Date rule` = case_when(Rule=="R1" ~ "Day 1",
                                 Rule=="R2" ~ "Day 15",
                                 Rule=="R3" ~ "Last day of month",
                                 Rule=="R4" ~ "Day 1",
                                 Rule=="R5" ~ "Day 15",
                                 Rule=="R6" ~ "Last day of month"),
         `Month of randomisation` = case_when(Rule=="R1" ~ "Ignore",
                                                      Rule=="R2" ~ "Ignore",
                                                      Rule=="R3" ~ "Ignore",
                                                      Rule=="R4" ~ "Include",
                                                      Rule=="R5" ~ "Include",
                                                      Rule=="R6" ~ "Include"))%>%
  select(`Drug group`,
         `Month of randomisation`,
         `Date rule`,
         Either,
         CRF_RCD,
         CRF_only,
         RCD_only,
         Neither,
         Sn,
         Sp,
         PPV,
         NPV,
         K,
         `95% CI`,
         `p-value`,
         Judgement)%>%
  ungroup()%>%
  rename(`CRF and RCD`= CRF_RCD,
         `CRF only` = CRF_only,
         `RCD only` = RCD_only)%>%
  mutate(`Drug group`=fct_relevel(`Drug group`, "Antiplatelets", "Oral anticoagulants"
                                  # , "Macrolides"
                                  ))%>%
  arrange(`Drug group`,`Month of randomisation`,`Date rule`)%>%
  mutate(K=paste0(K, " (", `95% CI`, ")"))%>%
  select(-`p-value`, -`95% CI`)%>%
  mutate(across(c(Sn, Sp, PPV, NPV), ~paste0(., "%")))%>%
  custom_tab(header=NULL,
             footer="RCD includes Dispensing data only. CRF: case-report form; RCD: routinely-collected data; CI: confidence interval. K: Cohen’s Kappa")%>%
  add_header_row(values=c("", "Derivation rules", "Participants in each source (%)", "Agreement"), colwidths = c(1, 2, 5, 6))%>%
  add_header_lines("Agreement for baseline drug exposure between case-report form and Dispensing data using different temporal data handling rules in the Dispensing data")%>%
  width(j=c(3), width=4, unit="cm")%>%
  width(j=c(4:8, 13), width=3, unit="cm")%>%
  width(j=c(1, 14), width=4.5, unit="cm")%>%
  border(i=2, j=c(1, 2,4), border.right = fp_border(), part="header")%>%
  border(i=3, j=c(1,3,8), border.right = fp_border(), part="header")%>%
  border(i=NULL, j=c(1,3,8), border.right = fp_border(), part="body")%>%
  border(i=6, j=NULL, border.bottom = fp_border(), part="body")%>%
  border(i=12, j=NULL, border.bottom = fp_border(), part="body")%>%
  border(i=4, j=2, border.bottom = fp_border(), part="body")%>%
  border(i=10, j=2, border.bottom = fp_border(), part="body")%>%
  border(i=1:10, j=1, border.bottom = fp_border(), part="body")%>%
  
  merge_v(j=2:3, target=2:3)%>%
  merge_v(j=1)%>%
  
  align(i=NULL, j=2:3, part="body", align = "left")%>%
  footnote(i=3, 
           j=4, 
           part="header", 
           ref_symbols = c("1"),
           value=as_paragraph("Sum of participants in both datasets, in CRF only, and in RCD only
"))->table_agreement_initiation_date_rules_crf_rcd

  # footnote(i=3, 
  #          j=5:8, 
  #          part="header", 
  #          ref_symbols = c("2"),
  #          value=as_paragraph("Proportion calculated using all participants as denominator (separately from the 'Either' column)")) 

table_agreement_initiation_date_rules_crf_rcd

save_as_docx(table_agreement_initiation_date_rules_crf_rcd,
             path="Outputs/Tables/Thesis/agreement_crf_rcd_rules.docx",
               pr_section=sect_properties)






### investigate agreement for people randomised on the 1st of the month ------

rules_agreement_flags_rcd_crf%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  mutate(rand_day = str_sub(as.character(rand_date), 9,10))%>%
  filter(rand_day=="01")->rules_agreement_flags_rcd_crf_day_one

###### calculate agreement ----

drug_groups <- c(# "macrolides", 
  "antiplatelets", "oral_anticoagulants")

rules<-rules_agreement_flags_rcd_crf_day_one%>%
  distinct(rule)%>%
  .[[1]]

kappa_crf_rcd<-list()

for (i in drug_groups) {
  
  for (j in rules){
    
    rules_agreement_flags_rcd_crf_day_one%>%
      filter(codelist==i,
             rule==j)%>%
      mutate(study_number=as.character(study_number))%>%
      select(-codelist, -study_number, -rule)%>%
      mutate(across(everything(), as.integer))->table
    
    try(k<-Kappa.test(table$crf, table$RCD))
    
    if(exists("k"))
    {
      
      kappa_crf_rcd[[i]][[j]]<-k
      
    }
    else{
      kappa_crf_rcd[[i]][[j]]<-list()
    }
    
    rm(table, k)
  }
  
  rm(i)
}


###### unlist result for table -----

kappa_crf_rcd_results<-data.frame()

for (i in drug_groups) {
  
  for (j in rules){
    
    codelist<-i
    
    Rule <- j
    
    
    
    try(
      table<-(data.frame(
        codelist = codelist,
        Rule = Rule,
        kappa = unlist(kappa_crf_rcd[[i]][[j]]$Result$estimate),
        CI_lower = unlist(kappa_crf_rcd[[i]][[j]]$Result$conf.int[1]),
        CI_upper = unlist(kappa_crf_rcd[[i]][[j]]$Result$conf.int[2]),
        p_value = unlist(kappa_crf_rcd[[i]][[j]]$Result$p.value),
        judgement = unlist(kappa_crf_rcd[[i]][[j]]$Judgement)
      ))
    )
    
    try(kappa_crf_rcd_results%<>%bind_rows(table))
    
    
    rm(table, name, j)
    
  }
  rm(i)   
}

View(kappa_crf_rcd_results)


###### produce table     ------

# format results  
kappa_crf_rcd_results%>%
  left_join(codelist_labels)%>%
  mutate_when(!is.na(CI_lower), 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate_when(p_value==0.00,
              p_value="<0.001")%>%
  as_tibble()%>%
  
  # add calculations of number of people in each source
  left_join(
    rules_agreement_flags_rcd_crf_day_one%>%
      rename(Rule=rule)%>%
      filter(codelist %in% drug_groups)%>%
      group_by(codelist, Rule)%>%
      summarise(CRF_RCD = n_unique(study_number[crf=="1" & RCD=="1"]),
                CRF_only = n_unique(study_number[crf=="1" & RCD=="0"]),
                RCD_only = n_unique(study_number[crf=="0" & RCD=="1"]),
                nil = n_distinct(study_number) - CRF_RCD - CRF_only - RCD_only,
                Sn = round(CRF_RCD/(CRF_RCD+CRF_only)*100,1),
                Sp=round(nil/(nil+RCD_only)*100,1),
                PPV = round(CRF_RCD/(CRF_RCD+RCD_only)*100,1),
                NPV = round(nil/(nil+CRF_only)*100,1))%>%
      ungroup(),
    by=c("codelist", "Rule"))%>%
  arrange(labels, Rule)%>%
  select(Label=labels,
         Rule,
         CRF_RCD,
         CRF_only,
         RCD_only,
         nil,
         Sn,
         Sp,
         PPV,
         NPV,
         kappa,
         CI,
         p_value,
         judgement)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))%>%
  
  # calculate proportions and format
  rowwise() %>%
  mutate(prop = round(sum(across(3:5), na.rm = T)/sum(across(3:6), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either' = paste0(sum(across(3:5), na.rm = T),
                           " (",
                           prop,
                           "%)"))%>%
  mutate(prop1 = round(sum(across(3:3))/sum(across(3:6), na.rm = T)*100,1),
         prop2 = round(sum(across(4:4))/sum(across(3:6), na.rm = T)*100,1),
         prop3 = round(sum(across(5:5))/sum(across(3:6), na.rm = T)*100,1),
         prop4 = round(sum(across(6:6))/sum(across(3:6), na.rm = T)*100,1))%>%
  mutate(prop1=if_else(is.na(prop1), 0, prop1))%>%
  mutate(prop2=if_else(is.na(prop2), 0, prop2))%>%
  mutate(prop3=if_else(is.na(prop3), 0, prop3))%>%
  mutate(prop4=if_else(is.na(prop4), 0, prop4))%>%
  mutate(CRF_RCD=paste0(CRF_RCD, " (", prop1, "%)"),
         CRF_only=paste0(CRF_only, " (", prop2, "%)"),
         RCD_only=paste0(RCD_only, " (", prop3, "%)"),
         nil=paste0(nil, " (", prop4, "%)"))%>%
  select(-c(prop, prop1, prop2, prop3, prop4))%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%
  rename('Drug group' = Label,
         K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement,
         Neither= nil)%>%
  relocate('Either', .after = 'Drug group')%>%
  relocate(Rule, .after = 'Drug group')%>%
  arrange(`Drug group`, Rule)-> crf_rcd_rules_results_table


View(crf_rcd_rules_results_table)


crf_rcd_rules_results_table%>%
  filter(`Drug group` !="Macrolides")%>%
  # mutate(Rule = case_when(Rule=="R1" ~ "Ignore month of randomisation, use day 1",
  #                         Rule=="R2" ~ "Ignore month of randomisation, input day 15",
  #                         Rule=="R3" ~ "Include month of randomisation, input last day of month",
  #                         Rule=="R4" ~ "Include month of randomisation, use day 1",
  #                         Rule=="R5" ~ "Include month of randomisation, input day 15",
  #                         Rule=="R6" ~ "Include month of randomisation, input last day of month",
  # ))%>%
  mutate(`Date rule` = case_when(Rule=="R1" ~ "Day 1",
                                 Rule=="R2" ~ "Day 15",
                                 Rule=="R3" ~ "Last day of month",
                                 Rule=="R4" ~ "Day 1",
                                 Rule=="R5" ~ "Day 15",
                                 Rule=="R6" ~ "Last day of month"),
         `Month of randomisation` = case_when(Rule=="R1" ~ "Ignore",
                                              Rule=="R2" ~ "Ignore",
                                              Rule=="R3" ~ "Ignore",
                                              Rule=="R4" ~ "Include",
                                              Rule=="R5" ~ "Include",
                                              Rule=="R6" ~ "Include"))%>%
  select(`Drug group`,
         `Month of randomisation`,
         `Date rule`,
         Either,
         CRF_RCD,
         CRF_only,
         RCD_only,
         Neither,
         Sn,
         Sp,
         PPV,
         NPV,
         K,
         `95% CI`,
         `p-value`,
         Judgement)%>%
  ungroup()%>%
  rename(`CRF and RCD`= CRF_RCD,
         `CRF only` = CRF_only,
         `RCD only` = RCD_only)%>%
  mutate(`Drug group`=fct_relevel(`Drug group`, "Antiplatelets", "Oral anticoagulants"
                                  # , "Macrolides"
  ))%>%
  arrange(`Drug group`,`Month of randomisation`,`Date rule`)%>%
  mutate(K=paste0(K, " (", `95% CI`, ")"))%>%
  select(-`p-value`, -`95% CI`)%>%
  mutate(across(c(Sn, Sp, PPV, NPV), ~paste0(., "%")))%>%
  custom_tab(header=NULL,
             footer="RCD includes Dispensing data only. CRF: case-report form; RCD: routinely-collected data; CI: confidence interval. K: Cohen’s Kappa")%>%
  add_header_row(values=c("", "Derivation rules", "Participants in each source (%)", "Agreement"), colwidths = c(1, 2, 5, 6))%>%
  add_header_lines("Agreement for baseline drug exposure between case-report form and Dispensing data using different temporal data handling rules in the Dispensing data")%>%
  width(j=c(3), width=4, unit="cm")%>%
  width(j=c(4:8, 13), width=3, unit="cm")%>%
  width(j=c(1, 14), width=4.5, unit="cm")%>%
  border(i=2, j=c(1, 2,4), border.right = fp_border(), part="header")%>%
  border(i=3, j=c(1,3,8), border.right = fp_border(), part="header")%>%
  border(i=NULL, j=c(1,3,8), border.right = fp_border(), part="body")%>%
  border(i=6, j=NULL, border.bottom = fp_border(), part="body")%>%
  border(i=12, j=NULL, border.bottom = fp_border(), part="body")%>%
  border(i=4, j=2, border.bottom = fp_border(), part="body")%>%
  border(i=10, j=2, border.bottom = fp_border(), part="body")%>%
  border(i=1:10, j=1, border.bottom = fp_border(), part="body")%>%
  
  merge_v(j=2:3, target=2:3)%>%
  merge_v(j=1)%>%
  
  align(i=NULL, j=2:3, part="body", align = "left")%>%
  footnote(i=3, 
           j=4, 
           part="header", 
           ref_symbols = c("1"),
           value=as_paragraph("Sum of participants in both datasets, in CRF only, and in RCD only
"))->table_agreement_initiation_date_rules_crf_rcd

# footnote(i=3, 
#          j=5:8, 
#          part="header", 
#          ref_symbols = c("2"),
#          value=as_paragraph("Proportion calculated using all participants as denominator (separately from the 'Either' column)")) 

table_agreement_initiation_date_rules_crf_rcd

save_as_docx(table_agreement_initiation_date_rules_crf_rcd,
             path="Outputs/Tables/Thesis/agreement_crf_rcd_rules_day_one.docx",
             pr_section=sect_properties)

### investigate agreement for people randomised on the last day of the month ------

rules_agreement_flags_rcd_crf%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  mutate(rand_date=as.Date(rand_date, "%Y-%m-%d"))%>%
  mutate(last_day = ceiling_date(rand_date, "month") - days(1))%>%
  filter(rand_date==last_day)->rules_agreement_flags_rcd_crf_last_day

View(rules_agreement_flags_rcd_crf_last_day)

rules_agreement_flags_rcd_crf_last_day%<>%
  filter(codelist!="macrolides")%>%
  mutate(rule_aggregate = if_else(rule %in% c("R1","R2","R3"), "Ignore", "Include"))%>%
  select(-rule)%>%
  rename(rule=rule_aggregate)%>%
  group_by(study_number, codelist, rule)%>%
  summarise(crf=if_else(any(crf==1), 1,0),
            RCD=if_else(any(RCD==1), 1,0))%>%
  ungroup()


###### calculate agreement ----

drug_groups <- c(# "macrolides", 
  "antiplatelets", "oral_anticoagulants")

rules<-rules_agreement_flags_rcd_crf_last_day%>%
  distinct(rule)%>%
  .[[1]]

kappa_crf_rcd<-list()

for (i in drug_groups) {
  
  for (j in rules){
    
    rules_agreement_flags_rcd_crf_last_day%>%
      filter(codelist==i,
             rule==j)%>%
      mutate(study_number=as.character(study_number))%>%
      select(-codelist, -study_number, -rule)%>%
      mutate(across(everything(), as.integer))->table
    
    try(k<-Kappa.test(table$crf, table$RCD))
    
    if(exists("k"))
    {
      
      kappa_crf_rcd[[i]][[j]]<-k
      
    }
    else{
      kappa_crf_rcd[[i]][[j]]<-list()
    }
    
    rm(table, k)
  }
  
  rm(i)
}


###### unlist result for table -----

kappa_crf_rcd_results<-data.frame()

for (i in drug_groups) {
  
  for (j in rules){
    
    codelist<-i
    
    Rule <- j
    
    
    
    try(
      table<-(data.frame(
        codelist = codelist,
        Rule = Rule,
        kappa = unlist(kappa_crf_rcd[[i]][[j]]$Result$estimate),
        CI_lower = unlist(kappa_crf_rcd[[i]][[j]]$Result$conf.int[1]),
        CI_upper = unlist(kappa_crf_rcd[[i]][[j]]$Result$conf.int[2]),
        p_value = unlist(kappa_crf_rcd[[i]][[j]]$Result$p.value),
        judgement = unlist(kappa_crf_rcd[[i]][[j]]$Judgement)
      ))
    )
    
    try(kappa_crf_rcd_results%<>%bind_rows(table))
    
    
    rm(table, name, j)
    
  }
  rm(i)   
}

View(kappa_crf_rcd_results)


###### produce table     ------

# format results  
kappa_crf_rcd_results%>%
  left_join(codelist_labels)%>%
  mutate_when(!is.na(CI_lower), 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate_when(p_value==0.00,
              p_value="<0.001")%>%
  as_tibble()%>%
  
  # add calculations of number of people in each source
  left_join(
    rules_agreement_flags_rcd_crf_last_day%>%
      rename(Rule=rule)%>%
      filter(codelist %in% drug_groups)%>%
      group_by(codelist, Rule)%>%
      summarise(CRF_RCD = n_unique(study_number[crf=="1" & RCD=="1"]),
                CRF_only = n_unique(study_number[crf=="1" & RCD=="0"]),
                RCD_only = n_unique(study_number[crf=="0" & RCD=="1"]),
                nil = n_distinct(study_number) - CRF_RCD - CRF_only - RCD_only,
                Sn = round(CRF_RCD/(CRF_RCD+CRF_only)*100,1),
                Sp=round(nil/(nil+RCD_only)*100,1),
                PPV = round(CRF_RCD/(CRF_RCD+RCD_only)*100,1),
                NPV = round(nil/(nil+CRF_only)*100,1))%>%
      ungroup(),
    by=c("codelist", "Rule"))%>%
  arrange(labels, Rule)%>%
  select(Label=labels,
         Rule,
         CRF_RCD,
         CRF_only,
         RCD_only,
         nil,
         Sn,
         Sp,
         PPV,
         NPV,
         kappa,
         CI,
         p_value,
         judgement)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))%>%
  
  # calculate proportions and format
  rowwise() %>%
  mutate(prop = round(sum(across(3:5), na.rm = T)/sum(across(3:6), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either' = paste0(sum(across(3:5), na.rm = T),
                           " (",
                           prop,
                           "%)"))%>%
  mutate(prop1 = round(sum(across(3:3))/sum(across(3:6), na.rm = T)*100,1),
         prop2 = round(sum(across(4:4))/sum(across(3:6), na.rm = T)*100,1),
         prop3 = round(sum(across(5:5))/sum(across(3:6), na.rm = T)*100,1),
         prop4 = round(sum(across(6:6))/sum(across(3:6), na.rm = T)*100,1))%>%
  mutate(prop1=if_else(is.na(prop1), 0, prop1))%>%
  mutate(prop2=if_else(is.na(prop2), 0, prop2))%>%
  mutate(prop3=if_else(is.na(prop3), 0, prop3))%>%
  mutate(prop4=if_else(is.na(prop4), 0, prop4))%>%
  mutate(CRF_RCD=paste0(CRF_RCD, " (", prop1, "%)"),
         CRF_only=paste0(CRF_only, " (", prop2, "%)"),
         RCD_only=paste0(RCD_only, " (", prop3, "%)"),
         nil=paste0(nil, " (", prop4, "%)"))%>%
  select(-c(prop, prop1, prop2, prop3, prop4))%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%
  rename('Drug group' = Label,
         K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement,
         Neither= nil)%>%
  relocate('Either', .after = 'Drug group')%>%
  relocate(Rule, .after = 'Drug group')%>%
  arrange(`Drug group`, Rule)-> crf_rcd_rules_results_table


View(crf_rcd_rules_results_table)


crf_rcd_rules_results_table%>%
  filter(`Drug group` !="Macrolides")%>%
  # mutate(Rule = case_when(Rule=="R1" ~ "Ignore month of randomisation, use day 1",
  #                         Rule=="R2" ~ "Ignore month of randomisation, input day 15",
  #                         Rule=="R3" ~ "Include month of randomisation, input last day of month",
  #                         Rule=="R4" ~ "Include month of randomisation, use day 1",
  #                         Rule=="R5" ~ "Include month of randomisation, input day 15",
  #                         Rule=="R6" ~ "Include month of randomisation, input last day of month",
  # ))%>%
  rename(`Month of randomisation` = Rule)%>%
  select(`Drug group`,
         `Month of randomisation`,
         Either,
         CRF_RCD,
         CRF_only,
         RCD_only,
         Neither,
         Sn,
         Sp,
         PPV,
         NPV,
         K,
         `95% CI`,
         `p-value`,
         Judgement)%>%
  ungroup()%>%
  rename(`CRF and RCD`= CRF_RCD,
         `CRF only` = CRF_only,
         `RCD only` = RCD_only)%>%
  mutate(`Drug group`=fct_relevel(`Drug group`, "Antiplatelets", "Oral anticoagulants"
                                  # , "Macrolides"
  ))%>%
  arrange(`Drug group`,`Month of randomisation`,`Date rule`)%>%
  mutate(K=paste0(K, " (", `95% CI`, ")"))%>%
  select(-`p-value`, -`95% CI`)%>%
  mutate(across(c(Sn, Sp, PPV, NPV), ~paste0(., "%")))%>%
  custom_tab(header=NULL,
             footer="RCD includes Dispensing data only. CRF: case-report form; RCD: routinely-collected data; CI: confidence interval. K: Cohen’s Kappa")%>%
  add_header_row(values=c("", "Derivation rules", "Participants in each source (%)", "Agreement"), colwidths = c(1, 2, 5, 6))%>%
  add_header_lines("Agreement for baseline drug exposure between case-report form and Dispensing data using different temporal data handling rules in the Dispensing data")%>%
  width(j=c(3), width=4, unit="cm")%>%
  width(j=c(4:8, 13), width=3, unit="cm")%>%
  width(j=c(1, 14), width=4.5, unit="cm")%>%
  border(i=2, j=c(1, 2,4), border.right = fp_border(), part="header")%>%
  border(i=3, j=c(1,3,8), border.right = fp_border(), part="header")%>%
  border(i=NULL, j=c(1,3,8), border.right = fp_border(), part="body")%>%
  border(i=6, j=NULL, border.bottom = fp_border(), part="body")%>%
  border(i=12, j=NULL, border.bottom = fp_border(), part="body")%>%
  border(i=4, j=2, border.bottom = fp_border(), part="body")%>%
  border(i=10, j=2, border.bottom = fp_border(), part="body")%>%
  border(i=1:10, j=1, border.bottom = fp_border(), part="body")%>%
  
  merge_v(j=2:3, target=2:3)%>%
  merge_v(j=1)%>%
  
  align(i=NULL, j=2:3, part="body", align = "left")%>%
  footnote(i=3, 
           j=4, 
           part="header", 
           ref_symbols = c("1"),
           value=as_paragraph("Sum of participants in both datasets, in CRF only, and in RCD only
"))->table_agreement_initiation_date_rules_crf_rcd

# footnote(i=3, 
#          j=5:8, 
#          part="header", 
#          ref_symbols = c("2"),
#          value=as_paragraph("Proportion calculated using all participants as denominator (separately from the 'Either' column)")) 

table_agreement_initiation_date_rules_crf_rcd

save_as_docx(table_agreement_initiation_date_rules_crf_rcd,
             path="Outputs/Tables/Thesis/agreement_crf_rcd_rules_last_day.docx",
             pr_section=sect_properties)



# RESULTS: 2.3 Impact of using different sources and agreement --------

## Baseline drug exposure ---------------------
# 90 day lookback period; input day 15 in dispensing records, ignore data in month of randomisation
## gp

gp_dt%>%
  select(study_number, date, code)%>%
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<="2021-05-31")%>% # allow 2 month lag between data reception and analysis
  filter(date<rand_date)%>%
  mutate(time_before_rand = as.integer(difftime(rand_date, date, units = "days")))%>%
  filter(time_before_rand>=0 & time_before_rand<=90)%>%
  distinct(study_number, codelist)%>%
  mutate(dataset="GP")%>%
  mutate(flag=1)%>%
  as_tibble()->x1


## meds

meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>%
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>%
  left_join(rand_dates%>%select(study_number, rand_date))%>%
  filter(rand_date<="2021-05-31")%>% # allow 2 month lag between data reception and analysis
  filter(processing_period_date<rand_date)%>%
  # mutate(rand_month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
  # mutate(months_before_rand = abs(interval(rand_month, processing_period_date)%/%months(1)))%>% # calculate time before rand in months
  # filter(months_before_rand!=0)%>% # remove records in randomisation month
  rename(code=prescribed_bnf_code,
         date=processing_period_date)%>%
  mutate(date=as.Date(paste0(str_sub(date, 1, 8), "15")))%>% # APPLY RULE: INPUT DAY 15
  mutate(time_before_rand = as.integer(difftime(rand_date, date, units = "days")))%>% # 90 days before rand 
  filter(time_before_rand>=0 & time_before_rand<=90)%>%
  select(
    #-rand_month, 
         -time_before_rand)%>%
  mutate(dataset="Dispensing")%>%
  mutate(flag=1)%>%
  select(study_number, codelist, dataset, flag)%>%
  as_tibble()->x2


baseline_flags_table<-rbind(x1, x2)

rm(x1, x2)

### participants captured in each dataset (use-case scenario 1) --------------
baseline_flags_table%<>%
  distinct(study_number, codelist, dataset, .keep_all = T)%>%
  pivot_wider(id_cols=c(study_number, codelist), names_from = "dataset", values_from="flag")%>%
  mutate(Dispensing=replace_na(Dispensing, 0))%>%
  mutate(GP=replace_na(GP, 0))

baseline_flags_table%>%
  group_by(codelist)%>%
  summarise(GP_only=n_distinct(study_number[GP==1 & Dispensing ==0]),
            Dispensing_only = n_distinct(study_number[GP==0 & Dispensing ==1]),
            Both = n_distinct(study_number[GP==1 & Dispensing ==1]),
            Either = n_distinct(study_number[GP==1 | Dispensing ==1]))%>%
  pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(codelist_labels)%>%
  mutate(Dataset=recode(Dataset, Both = "Both datasets", Dispensing_only = "Dispensing only",GP_only="GP only",Either = "Either dataset"))%>%
  ungroup()%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either dataset"]*100,1))%>%
  mutate(labels=reorder(labels, labels))->transformed_data_proportions_datasets_baseline
  
##### plot -----
transformed_data_proportions_datasets_baseline%>%
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5)+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y")+
  theme_gray(base_size=25)+
  theme(legend.position="bottom",
        text=element_text(family="Mulish"),
        axis.text.x = element_blank(),
        axis.title.x=element_blank())+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.5)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(color="Dataset",
       # title="Number of participants captured in each dataset at randomisation",
       # subtitle="Based on a 3 month lookback period",
       # caption=str_wrap("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset. 'Dispensing only' is the number of participants captured in the Dispensing dataset but not the GP dataset, and vice-versa. Proportions presented are in reference to the number of participants identified using either dataset. Calculations restricted to the time period between June 2018 and May 2021",200       )
       )

ggsave("Outputs/Figures/Thesis/counts_per_dataset_barchart_baseline.png",
       last_plot(),
       width=30,
       height=15,
       dpi="retina")



##### slides ------

transformed_data_proportions_datasets_baseline%>%
  filter(codelist!="macrolides")%>%
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat='identity', position="dodge")+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5, color="white")+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             scales="free_y",
             ncol=5)+
  theme_gray(base_size=25)+
  oxpop_blue_panel+
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y=element_text(size=10))+
  scale_y_continuous(limits = c(0, NA),
                     expand=expansion(c(0,0.6)))+
  scale_fill_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either dataset", "Both datasets", "Dispensing only", "GP only"))+
  labs(x=NULL,
    # color="Dataset",
       # title="Number of participants captured in each dataset at randomisation",
       # subtitle="Based on a 3 month lookback period",
       # caption=str_wrap("Counts based on presence of a relevant drug code up to 90 days before randomisation in the GP dataset, or in the 3 calendar months before the calendar month of randomisation in the Dispensing dataset. 'Dispensing only' is the number of participants captured in the Dispensing dataset but not the GP dataset, and vice-versa. Proportions presented are in reference to the number of participants identified using either dataset. Calculations restricted to the time period between June 2018 and May 2021",200
       )

ggsave("Outputs/Figures/ictmc_slides/counts_per_dataset_barchart_baseline.png",
       last_plot(),
       width=60,
       height=30,
       units="cm",
       dpi="retina")




### agreement between sources ---------


baseline_flags_table%>%distinct(study_number)%>%nrow()->participants

baseline_flags_table%>%distinct(study_number)->participants_list


drug_groups <- baseline_flags_table%>%distinct(codelist)%>%.[[1]]

kappa_codelists<-list()

for (i in drug_groups) {
  
  baseline_flags_table%>%
    filter(codelist==i)%>%
    mutate(study_number=as.character(study_number))%>%
    right_join(participants_list)%>%
    mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  
    select(-codelist, -study_number)%>%
    mutate(across(everything(), as.integer))->table
  
  try(k<-Kappa.test(table$GP, table$Dispensing))
  
  if(exists("k"))
  {
    
    kappa_codelists[[i]]<-k
    
  }
  else{
    kappa_codelists[[i]]<-list()
  }
  
  rm(i, table, k)
  
}

kappa_codelists_results<-data.frame()

for (i in drug_groups) {
  
  name<-i
  
  try(
    table<-(data.frame(
      codelist = name,
      kappa = unlist(kappa_codelists[[i]]$Result$estimate),
      CI_lower = unlist(kappa_codelists[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_codelists[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_codelists[[i]]$Result$p.value),
      judgement = unlist(kappa_codelists[[i]]$Judgement)
    ))
  )
  
  try(kappa_codelists_results%<>%bind_rows(table))
  
  
  rm(table, name, i)
  
}


kappa_codelists_results%>%
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
    (baseline_flags_table%>%
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
  mutate(kappa=as.numeric(kappa))-> codelists_agreement



codelists_agreement%<>%
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




#### table ------ 


codelists_agreement%>%
  select(-codelist)%>%
  rename("Either (%)" = "Either dataset (%)",
         'Both (%)' = GP_Dispensing,
         'GP dataset only (%)' = GP_only,
         'Dispensing dataset only (%)'=Dispensing_only,
         'Neither dataset (%)'=nil)%>%
  custom_tab(header=NULL,
             footer=NULL)%>%
  width(j=1, width=6, unit="cm")%>%
  width(j=2:10, width=3, unit="cm")%>%
  # width(j=2:6, width=2, unit="cm")%>%
  flextable::font(fontname="Mulish")%>%
  align(i=NULL, j=2:10, align="center", part="all")%>%
  merge_v(j=1)%>%
  add_header_row(values=c("", "Participants in each source (%)", "Agreement"), colwidths = c(1, 5, 4), top = T)%>%
  add_header_lines("Agreement between the GP and Dispensing dataset for presence of a record at any time point")%>%
  footnote(i=3, 
           j=2, 
           part="header", 
           ref_symbols = c("1"),
           value=as_paragraph("Proportion calculated using all participants as denominator (separately from the subsequent columns)"))%>%
  footnote(i=3, 
           j=3:6, 
           part="header", 
           ref_symbols = c("2"),
           value=as_paragraph("Proportion calculated using all participants as denominator (separately from the 'Either' column)"))->table_flex

table_flex

save_as_docx(table_flex, path = "Outputs/Tables/Thesis/agreement_gp_dispensing_baseline.docx", pr_section = sect_properties)





































### agreement with CRF ------

baseline_flags_table%>%
  pivot_longer(c(GP, Dispensing), names_to = "dataset", values_to="flag")%>%
  
  right_join(expand.grid(study_number = crf_drugs_agreement_flags%>%distinct(study_number)%>%.[[1]],
                         codelist = c(# "macrolides", 
                                      "antiplatelets", "oral_anticoagulants"),
                         dataset = c("Dispensing", "GP"))%>%
               mutate(study_number=as.character(study_number)),
             by=c("study_number", "codelist", "dataset"))%>%
  mutate(flag=replace_na(flag, 0))%>%
  pivot_wider(names_from = "dataset", values_from="flag")%>%
  group_by(study_number, codelist)%>%
  summarise(GP = if_else(GP=="1", "1", "0"),
          Dispensing = if_else(Dispensing =="1","1", "0"),
          GP_only = if_else(GP=="1" & Dispensing =="0","1", "0"),
          Dispensing_only = if_else(GP=="0" & Dispensing =="1", "1", "0"),
          Both = if_else(GP=="1" & Dispensing =="1", "1", "0"),
          Either = if_else(GP=="1" | Dispensing=="1", "1", "0"))%>%
  left_join(crf_drugs_agreement_flags%>%
              mutate(study_number=as.character(study_number))%>%
              rename(crf = flag))%>%
  filter(!is.na(crf))%>% # restrict to people where there is something recorded in the crf (note the crf data had already been restricted to people recruited in England and who could have had linkage data received)
  pivot_longer(c(GP, Dispensing, GP_only, Dispensing_only, Either, Both), names_to="dataset", values_to="RCD")%>%
  filter(dataset %in% c("GP", "Dispensing", "Either"))%>%
  ungroup()->baseline_crf_rcd_flags



##### calculate agreement

drug_groups <- c(# "macrolides", 
                 "antiplatelets", "oral_anticoagulants")

datasets<-baseline_crf_rcd_flags%>%
  distinct(dataset)%>%
  .[[1]]

kappa_crf_rcd<-list()

for (d in datasets) {
  
  for (i in drug_groups) {
    
      baseline_crf_rcd_flags%>%
        filter(dataset==d,
               codelist==i)%>%
        mutate(study_number=as.character(study_number))%>%
        select(-codelist, -study_number,-dataset)%>%
        mutate(across(everything(), as.integer))->table
      
      try(k<-Kappa.test(table$crf, table$RCD))
      
      if(exists("k"))
      {
        
        kappa_crf_rcd[[d]][[i]]<-k
        
      }
      else{
        kappa_crf_rcd[[d]][[i]]<-list()
      }
      
      rm(table, k)
    
      rm(i)
      
      } 
  rm(d)

  }


##### unlist result for table

kappa_crf_rcd_results<-data.frame()

for (d in datasets) {
  
  for (i in drug_groups) {
    
      
      dataset<-d
      
      codelist<-i
      
      try(
        table<-(data.frame(
          dataset = dataset,
          codelist = codelist,
          kappa = unlist(kappa_crf_rcd[[d]][[i]]$Result$estimate),
          CI_lower = unlist(kappa_crf_rcd[[d]][[i]]$Result$conf.int[1]),
          CI_upper = unlist(kappa_crf_rcd[[d]][[i]]$Result$conf.int[2]),
          p_value = unlist(kappa_crf_rcd[[d]][[i]]$Result$p.value),
          judgement = unlist(kappa_crf_rcd[[d]][[i]]$Judgement)
        ))
      )
      
      try(kappa_crf_rcd_results%<>%bind_rows(table))
      
      
      rm(table, name, i)
      
    }
    rm(d)   
  }
  

##### produce table

# format results  
kappa_crf_rcd_results%>%
  left_join(codelist_labels)%>%
  mutate_when(!is.na(CI_lower), 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate_when(p_value==0.00,
              p_value="<0.001")%>%
  as_tibble()%>%
  
  # add calculations of number of people in each source
  left_join(
    lookback_comparison_crf_joint_flags%>%
      filter(codelist %in% drug_groups)%>%
      group_by(codelist, dataset)%>%
      summarise(CRF_RCD = n_unique(study_number[crf=="1" & RCD=="1"]),
                CRF_only = n_unique(study_number[crf=="1" & RCD=="0"]),
                RCD_only = n_unique(study_number[crf=="0" & RCD=="1"]),
                nil = n_distinct(study_number) - CRF_RCD - CRF_only - RCD_only,
                Sn = round(CRF_RCD/(CRF_RCD+CRF_only)*100,1),
                Sp=round(nil/(nil+RCD_only)*100,1),
                PPV = round(CRF_RCD/(CRF_RCD+RCD_only)*100,1),
                NPV = round(nil/(nil+CRF_only)*100,1))%>%
      ungroup(),
    by=c("codelist", "dataset"))%>%
  arrange(dataset,labels)%>%
  select(Dataset=dataset,
         Label=labels,
         CRF_RCD,
         CRF_only,
         RCD_only,
         nil,
         Sn,
         Sp,
         PPV,
         NPV,
         kappa,
         CI,
         p_value,
         judgement)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))%>%
  
  # calculate proportions and format
  rowwise() %>%
  mutate(prop = round(sum(across(3:5), na.rm = T)/sum(across(3:6), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either' = paste0(sum(across(3:5), na.rm = T),
                           " (",
                           prop,
                           "%)"))%>%
  mutate(prop1 = round(sum(across(3:3))/sum(across(3:6), na.rm = T)*100,1),
         prop2 = round(sum(across(4:4))/sum(across(3:6), na.rm = T)*100,1),
         prop3 = round(sum(across(5:5))/sum(across(3:6), na.rm = T)*100,1),
         prop4 = round(sum(across(6:6))/sum(across(3:6), na.rm = T)*100,1))%>%
  mutate(prop1=if_else(is.na(prop1), 0, prop1))%>%
  mutate(prop2=if_else(is.na(prop2), 0, prop2))%>%
  mutate(prop3=if_else(is.na(prop3), 0, prop3))%>%
  mutate(prop4=if_else(is.na(prop4), 0, prop4))%>%
  mutate(CRF_RCD=paste0(CRF_RCD, " (", prop1, "%)"),
         CRF_only=paste0(CRF_only, " (", prop2, "%)"),
         RCD_only=paste0(RCD_only, " (", prop3, "%)"),
         nil=paste0(nil, " (", prop4, "%)"))%>%
  mutate(across(c(Sn, Sp, PPV, NPV), ~paste0(., "%")))%>%
  select(-c(prop, prop1, prop2, prop3, prop4))%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%
  rename('Drug group' = Label,
         K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement,
         Neither= nil)%>%
  relocate('Either', .after = 'Drug group')%>%
  arrange(Dataset, `Drug group`)-> crf_rcd_results_table


View(crf_rcd_results_table)





crf_rcd_results_table%>%
  filter(`Drug group`!="Macrolides")%>%
  ungroup()%>%
  rename(`CRF and RCD`= CRF_RCD,
         `CRF only` = CRF_only,
         `RCD only` = RCD_only)%>%
  mutate(`Drug group`=fct_relevel(`Drug group`, "Antiplatelets", "Oral anticoagulants"
                                  # , "Macrolides"
                                  ))%>%
  mutate(Dataset = fct_relevel(Dataset, "GP", "Dispensing", "Either"))%>%
  mutate(K=paste0(K, " (", `95% CI`, ")"))%>%
  select(-`p-value`, -`95% CI`)%>%
  arrange(`Drug group`, Dataset)%>%
  relocate(Dataset,.after=`Drug group`)%>%
  custom_tab(header=NULL,
             footer="Baseline exposure derived in the RCD sources as any record in the 90 days before randomisation. CRF: case-report form; RCD: routinely-collected data; CI: confidence interval")%>%
  add_header_row(values=c("", "Participants in each source (%)", "Agreement"), colwidths = c(2, 5, 6))%>%
  add_header_lines("Impact of using different data sources on the agreement for baseline drug exposure between case-report form and routinely-collected data")%>%
  width(j=c(3:7), width=3, unit="cm")%>%
  width(j=c(2, 13), width=4.5, unit="cm")%>%
  width(j=c(8:11), width=2, unit="cm")%>%
  width(j=c(12), width=3.5, unit="cm")%>%
  border(i=2, j=c(1,3), border.right = fp_border(), part="header")%>%
  border(i=3, j=c(2,7), border.right = fp_border(), part="header")%>%
  border(i=NULL, j=c(2,7), border.right = fp_border(), part="body")%>%
  border(i=c(3,6), j=NULL, border.bottom = fp_border(), part="body")%>%
  border(j=1, i=1:6, border.bottom = fp_border(), part="body")%>%
  footnote(i=3, 
           j=3, 
           part="header", 
           ref_symbols = c("1"),
           value=as_paragraph("Proportion calculated using all participants as denominator (separately from the subsequent columns)"))%>%
  merge_v(j=1)->table_agreement_crf_rcd_merge
  # footnote(i=3, 
  #          j=4:7, 
  #          part="header", 
  #          ref_symbols = c("2"),
  #          value=as_paragraph("Proportion calculated using all participants as denominator (separately from the 'Either' column)"))

table_agreement_crf_rcd_merge  

save_as_docx(table_agreement_crf_rcd_merge,
             path="Outputs/Tables/Thesis/agreement_crf_rcd_combining_sources.docx",
               pr_section=sect_properties)









### counts at baseline using CRF vs RCD (use-case scenario 3)------

# thesis 
counts_rcd_vs_crf_baseline_plot<-
  baseline_crf_rcd_flags%>%
  mutate(across(c(crf, RCD),~as.numeric(.)))%>%
  pivot_wider(c(study_number, codelist, crf), names_from = "dataset", values_from="RCD")%>%
  rename(`Either RCD source` = Either)%>%
  group_by(codelist)%>%
  mutate(total=length(crf))%>%
  group_by(codelist, total)%>%
  summarise(across(c(crf, GP, Dispensing, `Either RCD source`), ~sum(.x)))%>%
  pivot_longer(-c(codelist, total), values_to = "Count", names_to="Source")%>%
  group_by(codelist)%>%
  mutate(prop = round(Count/total*100,1))%>%
  left_join(codelist_labels)%>%
  mutate(Source=if_else(Source=="crf", "CRF", Source))%>%
  mutate(Source=factor(Source, levels=c("CRF", "Either RCD source", "GP", "Dispensing")))%>%
  
  ggplot(aes(Source, Count, fill=Source))+
  geom_col(width=0.6)+
  facet_wrap(~labels,
             nrow=2,
             scales="free")+
  theme(legend.position = "none",
        text=element_text(size=20,
                          family="Mulish"))+
  geom_text(aes(label=paste0(Count, " (", prop, "%)")),
            vjust=-1,
            size=6)+
  scale_y_continuous(expand=expansion(c(0,0.1)))


ggsave("Outputs/Figures/Thesis/counts_rcd_vs_crf_baseline_plot.png",
       counts_rcd_vs_crf_baseline_plot,
       dpi="retina",
       width=60,
       height=40,
       units="cm")

ggsave("Outputs/Figures/Thesis/HR/counts_rcd_vs_crf_baseline_plot.tiff",
       counts_rcd_vs_crf_baseline_plot,
       dpi="retina",
       width=60,
       height=40,
       units="cm")

# slides

counts_rcd_vs_crf_baseline_plot<-
  baseline_crf_rcd_flags%>%
  mutate(across(c(crf, RCD),~as.numeric(.)))%>%
  pivot_wider(c(study_number, codelist, crf), names_from = "dataset", values_from="RCD")%>%
  rename(`Either RCD source` = Either)%>%
  group_by(codelist)%>%
  mutate(total=length(crf))%>%
  group_by(codelist, total)%>%
  summarise(across(c(crf, GP, Dispensing, `Either RCD source`), ~sum(.x)))%>%
  pivot_longer(-c(codelist, total), values_to = "Count", names_to="Source")%>%
  group_by(codelist)%>%
  mutate(prop = round(Count/total*100,1))%>%
  left_join(codelist_labels)%>%
  mutate(Source=if_else(Source=="crf", "CRF", Source))%>%
  mutate(Source=factor(Source, levels=c("CRF", "Either RCD source", "GP", "Dispensing")))%>%
  
  ggplot(aes(Source, Count, fill=Source))+
  geom_col()+
  facet_wrap(~labels,
             nrow=2)+
  oxpop_blue_panel+
  theme(legend.position = "bottom")+
  geom_text(aes(label=paste0(Count, " (", prop, "%)")),
            vjust=-1,
            color="white",
            size=6)+
  scale_y_continuous(expand = expansion(c(0,0.1)))

# slides
ggsave("Outputs/Figures/counts_rcd_vs_crf_baseline_plot.png",
       counts_rcd_vs_crf_baseline_plot,
       dpi="retina",
       width=60,
       height=30,
       units="cm")


#### venn diagram ----

antiplatelets_venn_list<-list(
  CRF=baseline_crf_rcd_flags%>%
    filter(codelist=="antiplatelets",
         crf==1)%>%
    distinct(study_number)%>%
    .[[1]],
  GP = baseline_crf_rcd_flags%>%
    filter(codelist=="antiplatelets",
           dataset=="GP",
           RCD==1)%>%
    distinct(study_number)%>%
    .[[1]],
  Dispensing = baseline_crf_rcd_flags%>%
    filter(codelist=="antiplatelets",
           dataset=="Dispensing",
           RCD==1)%>%
    distinct(study_number)%>%
    .[[1]])

anticoagulants_venn_list<-list(
  CRF=baseline_crf_rcd_flags%>%
    filter(codelist=="oral_anticoagulants",
           crf==1)%>%
    distinct(study_number)%>%
    .[[1]],
  GP = baseline_crf_rcd_flags%>%
    filter(codelist=="oral_anticoagulants",
           dataset=="GP",
           RCD==1)%>%
    distinct(study_number)%>%
    .[[1]],
  Dispensing = baseline_crf_rcd_flags%>%
    filter(codelist=="oral_anticoagulants",
           dataset=="Dispensing",
           RCD==1)%>%
    distinct(study_number)%>%
    .[[1]])


antiplatelets_venn_plot<-ggvenn(antiplatelets_venn_list, 
                  # fill_alpha= 0.2,
                  set_name_size = 5)+
  theme_void(base_size=25)+
  labs(subtitle="Antiplatelets")+
  theme(text=element_text(family="Mulish", size=20))

anticoagulants_venn_plot<-
  ggvenn(anticoagulants_venn_list, 
                                  # fill_alpha= 0.2,
                                  set_name_size = 5)+
  theme_void(base_size=25)+
  labs(subtitle="Oral anticoagulants")+
  theme(text=element_text(family="Mulish", size=20))
  
antiplatelets_venn_plot+anticoagulants_venn_plot

ggsave("Outputs/Figures/Thesis/venn_diagram_counts_rcd_vs_crf_baseline_plot.png",
       dpi="retina",
       width=40,
       height=20,
       units="cm")

##### alternative venn diagram -----

draw.venn(antiplatelets_venn_list[["CRF"]], 
          antiplatelets_venn_list[["GP"]], 
          antiplatelets_venn_list[["Dispensing"]], 
          xtitle=NULL,
          ytitle=NULL,
          ztitle=NULL,
          nr_f="Mulish",
          nr_s=3,
          title=NULL,
          subtitle=NULL,
          output="jpg",
          filename="outputs/figures/baseline_drugs_recovery/antiplatelets_venn.jpg")



draw.venn(anticoagulants_venn_list[["CRF"]], 
          anticoagulants_venn_list[["GP"]], 
          anticoagulants_venn_list[["Dispensing"]], 
          xtitle=NULL,
          ytitle=NULL,
          ztitle=NULL,
          nr_f="Mulish",
          nr_s=3,
          title=NULL,
          subtitle=NULL,
          output="jpg",
          filename="outputs/figures/baseline_drugs_recovery/anticoagulants_venn.jpg")


## Initiation ------------------


as.Date("2021-05-31")-180

drug_initiation_analysis_population<-
  rand_dates%>%select(study_number, rand_date)%>%
  filter(rand_date<=as.Date("2021-05-31")-180) # censoring to people who could have completed 180 days follow-up at 31-05-21 (two month lag before data reception)
  
# GP data
gp_dt%>%
  select(study_number, date, code)%>% # select variables
  inner_join(codelists_snomed, by=c("code"="ConceptId"))%>% # join snomed codelists
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join rand dates
  filter(study_number %in% drug_initiation_analysis_population$study_number)%>% # restrict to people who could have completed 6 month follow-up
  filter(date>rand_date)%>% # events happening after randomisation date
  mutate(time_after_rand = as.integer(difftime(date, rand_date, units = "days")))%>% # compute time after rand
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  mutate(dataset="GP")%>% # dataset flag
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>% # trim variables
  as_tibble()->x1 # store

# Meds data
meds_dt%>%  
  select(study_number, prescribed_bnf_code, processing_period_date)%>% # select variables
  inner_join(codelists_bnf, by=c("prescribed_bnf_code"="code"))%>% # join bnf codelists
  filter(study_number %in% drug_initiation_analysis_population$study_number)%>% # restrict to people who could have completed 6 month follow-up
  left_join(rand_dates%>%select(study_number, rand_date))%>% # join rand dates
  mutate(date=as.Date(paste0(str_sub(as.character(processing_period_date), 1, 8), "15")))%>% # APPLY RULE: IMPUTE DAY 15
  # mutate(rand_month=as.Date(paste0(str_sub(as.character(rand_date), 1, 8), "01")))%>% # compute randomisation month
  # mutate(months_before_rand = abs(interval(rand_month, processing_period_date)%/%months(1)))%>% # calculate time before rand in months
  # filter(months_before_rand!=0)%>% # APPLY RULE: remove events in the month of randomisation
  mutate(time_before_rand=as.integer(difftime(rand_date, date, units="days")))%>%
  mutate(time_after_rand = as.integer(difftime(date, rand_date, units = "days")))%>% # compute time after rand
  filter(time_after_rand<=180)%>% # right censoring at 180 days
  rename(code=prescribed_bnf_code # renaming variables to align with gp data
         )%>%
  #select(-rand_month)%>% # excluding unnecessary variable
  mutate(dataset="Dispensing")%>% # dataset flag
  select(study_number, code, codelist, date, rand_date, time_after_rand, dataset)%>% # trim variables
  as_tibble()->x2 # store


initiation_table_flags<-rbind(x1, x2)%>% # merge data from 2 datasets
  filter(time_after_rand>0) # remove events before rand 

rm (x1, x2) # remove intermediate outputs


baseline_flags_table%>%
  filter(study_number %in% drug_initiation_analysis_population$study_number)%>%
  distinct(codelist, study_number) -> participants_not_at_risk 
# this contains a dataframe with participants identified as exposure at baseline (so not at risk) for each drug group
# note this is based on entries on either dataset (to reduce impact of different handling of temporal data)



# restrict drug initiation to pts at risk for each category and produce survival dataset

drug_groups <- distinct(participants_not_at_risk, codelist)%>%.[[1]] # create list of drug groups

survival_data<-data.frame() # create empty dataframe to store survival data

for (i in drug_groups) { # for each drug group
  
  
  participants_not_at_risk%>% # take list of participants not at risk since already on drug at baseline (for each drug group)
    filter(codelist==i)%>% # trim to group of interest in each iteration
    select(study_number)%>% # select study number
    .[[1]]->participant_not_at_risk_list # extract list of participants already on drug before rand (for each group)
  
  drug_initiation_analysis_population%>% # take randomisation dates
    filter(!study_number %in% participant_not_at_risk_list)%>% # exclude people on drug at randomisation 
    select(study_number)->participants_at_risk # list of patients at risk (for each group)
  
  
  initiation_table_flags%>% # take combined drug initiation dataset
    select(study_number, codelist, time_after_rand, dataset)%>% # select some variables
    filter(codelist==i)%>% # select drug group for each iteration
    filter(!study_number %in% participant_not_at_risk_list)%>% # selecting only those people who are at risk of initiating drug for each group
    filter(time_after_rand<=180)%>% # right censoring at 180 days
    right_join(expand.grid(participants_at_risk%>%.[[1]], 
                           c("Dispensing", "GP"))%>%
                 rename(study_number=Var1,
                        dataset=Var2) # this code creates and right joins a dataframe that has one row for each participant and dataset, to ensure there is a censored entry also for people with no records of that drug (rather than just flags for events); i.e. it fills the gaps where there are no events
    )%>%
    arrange(study_number, dataset, time_after_rand)%>% # sorting by these variables in order
    group_by(study_number, dataset)%>% # grouping by participant and dataset
    slice_head(n=1)%>% # select first observation for each dataset (note we'd sorted by time_after_rand above)
    mutate(event=if_else(!is.na(time_after_rand), 2, 1), # apply event flags (2 if time after rand is not null, 1 if otherwise)
           time_after_rand=ifelse(event==1, 180, time_after_rand), # apply time after rand of 180 if censored
           codelist = i # apply codelist name to censored participants
    )%>%
    select(study_number, time_after_rand, dataset, event, codelist)  -> table # select the relevant variables and store in a table
  
  survival_data%<>%bind_rows(table) # add the table with the calculated survival data for this drug group to the survival dataset
  
  rm(table, i) # remove intermediate items
  
}

View(survival_data)



# add counts of people captured in both datasets for survival plots

# we will choose the first event for people who had events in both datasets, and make sure all other participants are censored

survival_data%>% # take survival data 
  group_by(codelist, study_number)%>% # group by drug group and study number
  filter(event=="2")%>% # select events only
  filter(n()>1)%>% # filter for people with more than one record (i.e. in more than one dataset)
  arrange(time_after_rand)%>% # sort by time to event
  slice_head(n=1)%>% # select first record for KM plot (which will be the event)
  ungroup()%>%
  right_join(survival_data%>%distinct(study_number, codelist), by=c("study_number", "codelist"))%>% # bring back all combinations of pts at risk for each codelist to fill gaps
  mutate(event=as.numeric(if_else(is.na(time_after_rand), "1", "2")), # apply event flag
         time_after_rand=as.numeric(replace_na(time_after_rand,180)))%>% # apply censoring date for those not captured in both datasets 
  mutate(dataset="Both") ->a # apply dataset flag and export

survival_data%<>%bind_rows(a) # store

rm(a) # remove intermediate output



# add counts in either dataset

# for each participant and codelist we just need to select the first event (which will be a drug record, or the censoring event if there is no record)

survival_data%>% # take survival data
  group_by(codelist, study_number)%>% # group by drug and participant
  arrange(time_after_rand)%>% # order by time after rand
  slice_head(n=1)%>%  # select first observation
  mutate(dataset="Either")->a # apply flag and store

survival_data%<>%bind_rows(a) # store

rm(a) # remove intermediate output



# produce survival model
# 
# fit<-survfit(
#   Surv(time = time_after_rand, # specify variables for model (time, event flags, and grouping variable which in this case is the dataset)
#        event) ~ dataset, 
#   data=survival_data) 


survival_data%<>%left_join(codelist_labels) # join drug group labels

survival_data%<>%
  mutate(dataset=fct_relevel(dataset, "Either", "GP", "Dispensing", "Both")) # reorder dataset levels for plotting


##### Kaplan-Meier plots -----
# 
# ggsurvplot_facet(fit, 
#                  fun="event", 
#                  data=survival_data,
#                  # conf.int = T, 
#                  # risk.table = T,
#                  # cumevents=T,
#                  legend.title="Dataset",
#                  # legend.labs=c("Dispensing", "GP"),
#                  title="Survival curve for drug initiation after randomisation according to each dataset",
#                  facet.by = "labels",
#                  short.panel.labs = T,
#                  # pval = T, 
#                  # pval.method = T,
#                  # scales="free_y",
#                  # nrow = 9,
# )+
#   theme_bw(base_size=15)+
#   facet_wrap(~labels, 
#              scales="free_y",
#              labeller = label_wrap_gen(width = 30)
#   )
# 
# # store plot
# ggsave("Outputs/Figures/baseline_drugs_recovery/KM_drug_initiation.png",
#        last_plot(),
#        width=25,
#        height=10,
#        dpi="retina")




## agreement for initiation (binary)

survival_data%>%
  filter(dataset%in%c("GP", "Dispensing"))%>%
  mutate(flag=if_else(event=="2", 1,0))%>% # reshape event flag to fit what the package request 
  select(-event, -time_after_rand)%>% # remove variables not needed 
  pivot_wider(names_from = "dataset", values_from="flag")->t # transform to one column for each data set and store


t%>%distinct(study_number)->participants_list # list of all people included in this assessment 

t%>%distinct(study_number)%>%nrow()->participants # total number of people included in this assessment

#### calculate agreement

drug_groups <- t%>%distinct(codelist)%>%.[[1]] # select all drug groups

kappa_codelists_participants<-list() # create empty list to store results

# create list with agreement results
for (i in drug_groups) {
  
  t%>% # take binary flags data
    filter(codelist==i)%>% # select one drug group in each iteration
    mutate(study_number=as.character(study_number))%>% # reformat study number for join below
    right_join(participants_list)%>% # join list of all people (participants might not have a flag created before if they did not have data recorded during the period of analysis)
    mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  # create a 0 flag for people with no data in both datasets
    select(-codelist, -study_number)%>% # remove varibles not needed
    mutate(across(everything(), as.integer))->table # format variables as integers
  
  try(k<-Kappa.test(table$GP, table$Dispensing)) # create a K statistic for each drug group
  
  if(exists("k")) # if K exists (i.e. was possible to calculate)
  {
    
    kappa_codelists_participants[[i]]<-k # add it to the results list
    
  }
  else{
    kappa_codelists_participants[[i]]<-list() # if it does not exist, create an empty list for this drug group
  }
  
  rm(i, table, k) # remove intermediate outputs
  
}

# store agreement results in a table
kappa_codelists_participants_results<-data.frame()

for (i in drug_groups) { # for each drug group
  
  name<-i # assign the drug group to a string
  
  try(
    table<-(data.frame( # store each parameter in a separate variable in the table
      codelist = name,
      kappa = unlist(kappa_codelists_participants[[i]]$Result$estimate),
      CI_lower = unlist(kappa_codelists_participants[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_codelists_participants[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_codelists_participants[[i]]$Result$p.value),
      judgement = unlist(kappa_codelists_participants[[i]]$Judgement)
    ))
  )
  
  try(kappa_codelists_participants_results%<>%bind_rows(table)) # try to add this table to the results table
  
  
  rm(table, name, i)
  
}

# produce  binary agreement results table
kappa_codelists_participants_results%>% # take results table
  right_join(codelist_labels)%>% # join labels (right join to ensure there is one row per drug group even if there was no data)
  mutate_when(!is.na(CI_lower), # create a joint CI variable with lower and upper 
              CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2), # round kappa
         p_value=as.character(round(p_value, 2)))%>% # round p_value
  
  mutate_when(p_value==0.00, # reformat p-value so that very small numbers are shown properly
              p_value="<0.001")%>%
  
  left_join ( # join counts of people in each of the datasets
    (t%>%
       group_by(codelist)%>%
       summarise(GP_Dispensing = n_unique(study_number[GP=="1" & Dispensing=="1"]), # counts in both dataets
                 GP_only = n_unique(study_number[GP=="1" & Dispensing=="0"]), # counts in GP only
                 Dispensing_only = n_unique(study_number[GP=="0" & Dispensing=="1"]), # counts in dispensing only
                 nil = participants - GP_Dispensing - GP_only - Dispensing_only)), # counts in neither
    by=c("codelist"))%>%
  arrange(labels)%>% # reorder rows
  select(labels, # select variables of interest
         GP_Dispensing,
         GP_only,
         Dispensing_only,
         nil,
         kappa,
         CI,
         p_value,
         judgement)%>%
  as_tibble()%>%
  mutate(kappa=as.numeric(kappa))-> codelists_agreement_participants # store reformatted table

View(codelists_agreement_participants)

# calculate overall agreement (all drug groups combined)
# 
# t%>%
#   mutate(study_number=as.character(study_number))%>% # reformat study number for join below
#   right_join(participants_list)%>% # join list of all people (participants might not have a flag created before if they did not have data recorded during the period of analysis)
#   mutate(across(c(GP, Dispensing), ~replace_na(.x, 0)))%>%  # create a 0 flag for people with no data in both datasets
#   #select(-codelist, -study_number)%>% # remove variables not needed
#   mutate(across(c(GP, Dispensing), as.integer))%>%
#   select(GP, Dispensing)->table
# 
# codelists_agreement_participants%<>%bind_rows(
#   data.frame(
#     labels="Aggregated",
#     GP_Dispensing=NA,
#     GP_only=NA,
#     Dispensing_only=NA,
#     nil=NA,
#     kappa=round(Kappa.test(table$GP, table$Dispensing)$Result$estimate,2),
#     CI=paste0(round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[1],2),
#               " : ",
#               round(Kappa.test(table$GP, table$Dispensing)$Result$conf.int[2],2)),
#     p_value = Kappa.test(table$GP, table$Dispensing)$Result$p.value,
#     judgement=Kappa.test(table$GP, table$Dispensing)$Judgement)%>%
#     mutate(p_value=as.character(round(p_value,2)),
#            p_value=if_else(p_value==0, "<0.001", p_value)))%>%
#   arrange(labels)
# 



### intraclass correlation coefficients
# using library irr
# model parameters specified based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/

# prepare data
survival_data%>% # take survival data
  filter(dataset%in%c("GP", "Dispensing"))%>% # select entries in each of the datasets
  filter(event=="2")%>% # select events only 
  select(study_number, dataset, time_after_rand, codelist)%>% # select some variables
  pivot_wider(names_from = dataset, values_from = time_after_rand)%>% # pivot time values to separate columns
  filter(!is.na(Dispensing) &!is.na(GP))->icc_data # filter for people with an event in both datasets and store

# produce icc analysis
icc(ratings = icc_data%>% # specify data
      select(-study_number, -codelist), # remove unnecessary variables
    model="twoway", # two way model as same set of raters for all subjects
    type="agreement", # agreement defined as absolute agreement as we are more interested in quantifying absolute agreement (difference in days) rather than consistency between measurements
    unit="average")->icc_result # unit measurement type is the average between both raters as we are not specifying one as best

icc_results<-list(icc_result) # store aggregated results (all classes combined) in a results list

# produce icc for each drug group 

drug_groups <- distinct(survival_data, codelist)%>%.[[1]] # create drug groups list

for (i in drug_groups) { # for each drug group
  
  
  icc(ratings = icc_data%>% # run the icc model as above
        filter(codelist==i)%>% # but filtering for one drug group in each iteration
        select(-study_number, -codelist), 
      model="twoway",
      type="agreement",
      unit="average")->icc_result
  
  icc_results[[i]]<-icc_result # store results for each drug group in the results list
  
  rm(i, icc_result)
  
}




# build table with results

icc_results_table<-data_frame() # create empty dataframe

for (i in drug_groups) { 
  
  name<-i
  
  try(
    table<-(data.frame( # this will extract the individual parameters from each element of the results list (for each drug group) and store them as new variables
      codelist = name,
      pairs = unlist(icc_results[[i]]$subjects),
      icc = unlist(icc_results[[i]]$value),
      CI_lower = unlist(icc_results[[i]]$lbound),
      CI_upper = unlist(icc_results[[i]]$ubound),
      p_value = unlist(icc_results[[i]]$p.value)
    ))
  )
  
  try(icc_results_table%<>%bind_rows(table)) # bind results to the results table
  
  
  rm(table, name, i)
  
}







# some work on the results table
data.frame(labels="Aggregated", # this creates a dataframe with the aggregated results from the first element of the results list
           pairs = unlist(icc_results[[1]]$subjects),
           icc = unlist(icc_results[[1]]$value),
           CI_lower = unlist(icc_results[[1]]$lbound),
           CI_upper = unlist(icc_results[[1]]$ubound),
           p_value = unlist(icc_results[[1]]$p.value))%>%
  bind_rows(icc_results_table%>% # joining results for  each drug group
              left_join(codelist_labels%>%select(labels, codelist))%>%
              arrange(labels))%>%
  mutate(across(3:6, ~round(., 2)))%>% # rounding
  mutate(p_value=as.character(p_value))%>% # tidying p-value structure
  mutate_when(p_value<0.001,
              p_value=as.character("<0.001"))%>% # this will reformat the p-value so that very small values are rounded for simplicity
  select(labels, pairs, icc, CI_lower, CI_upper, p_value, codelist)%>% # selecting some variables
  mutate(CI=paste0(CI_lower, " : ", CI_upper))%>% # creating a CI variable
  select(-CI_lower, -CI_upper)%>% # triming out
  as.data.frame()->icc_results_table # store



# produce flextable with results
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

table_flex

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

# store flextable
save_as_docx(table_flex, path = "Outputs/Tables/icc_initiation_agreement_gp_dispensing.docx", pr_section = sect_properties)



### produce agreement tables
# add total counts and proportions (either dataset)

codelists_agreement_participants%>%
  mutate(across(2:6, ~replace_na(.x, 0)))%>%
  rowwise() %>%
  mutate(prop = round(sum(across(2:4), na.rm = T)/sum(across(2:5), na.rm=T)*100,1))%>%
  mutate(prop=if_else(is.na(prop), 0, prop))%>%
  mutate('Either dataset' = paste0(sum(across(2:4), na.rm = T),
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
  rename("Drug group"=labels,
         K=kappa,
         '95% CI'=CI,
         'p-value' = p_value,
         Judgement=judgement)%>%
  relocate('Either dataset', .after = 'Drug group')%>%

## add icc

  left_join(icc_results_table, by=c('Drug group'="labels"))%>%
  select(-codelist, -Judgement)%>%
  rename(Pairs=pairs, ICC=icc)%>%
  relocate(CI, .after=ICC)%>%
  mutate(CI=ifelse(str_detect(string=CI, pattern="NA"), NA, CI))%>%

## bring back codelist groups

  left_join(codelist_labels, by=c("Drug group" = "labels"))->transformed_data_agreement_initiation

### table ----

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

transformed_data_agreement_initiation%>%
  select(-codelist, -Pairs)%>%
  rename('Both datasets' = GP_Dispensing,
         'GP dataset  only' = GP_only,
         'Dispensing dataset only'=Dispensing_only,
         'Neither dataset'=nil)%>%
  # rename(`Either dataset` = `Either dataset (%)`)%>%
  custom_tab(header=NULL,
             footer="Analysis right-censored at 180 days after randomisation. Drug initiation derived only for patients considered not at risk, defined as those not identified as taking a drug at randomisation based on presence of a relevant drug code up to 90 days before randomisation in either dataset. ICC: intraclass correlation coefficient; CI: confidence interval. K: Cohen's Kappa")%>%
  width(j=1, width=5, unit="cm")%>%
  width(j=c(2:6, 8, 11), width=3, unit="cm")%>%
  flextable::font(fontname="Mulish")%>%
  set_header_labels(values=list(CI="95% CI", 
                                p_value = "p-value"))%>%
  add_header_row(values = c("","Participants in each source (%)", "Agreement (fact of drug initiation)","Agreement (time after randomisation)"), colwidths = c(1,5,3,3)) %>%
  add_header_lines("Agreement for medication initiation after randomisation between the GP and Dispensing datasets")%>%
  align(i=NULL, j=2:11, align="center", part="all")%>%
  border(i=1, j=2, border.right = fp_border(color="dark grey"), part="header")%>%
  border(i=2, j=9, border.right = fp_border(color="dark grey"), part="header")%>%
  border(i=2, j=6, border.right = fp_border(color="dark grey"), part="header")%>%
  border(i=NULL, j=1, border.right = fp_border(color="dark grey"), part="body")%>%
  border(i=NULL, j=9, border.right = fp_border(color="dark grey"), part="body")%>%
  border(i=NULL, j=6, border.right = fp_border(color="dark grey"), part="body")%>%
  footnote(i=3, 
           j=2, 
           part="header", 
           ref_symbols = c("1"),
           value=as_paragraph("Proportion calculated using all participants as denominator (separately from the subsequent columns)"))%>%
  merge_v(j=1)%>%
  footnote(i=3, 
           j=3:6, 
           part="header", 
           ref_symbols = c("2"),
           value=as_paragraph("Proportion calculated using all participants as denominator (separately from the 'Either' column)"))->table_flex

table_flex

save_as_docx(table_flex, path = "Outputs/Tables/Thesis/agreement_initiation_table_gp_dispensing.docx", pr_section = sect_properties)








### plots of counts lost by not having GP data (use case scenario 2) -----------
codelists_agreement_participants%>%
  select(labels,
         Both = GP_Dispensing,
         `GP only` = GP_only,
         `Dispensing only` = Dispensing_only)%>%
  rowwise()%>%
  mutate(Either = sum(Both, `GP only`, `Dispensing only`))%>%
  pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(codelist_labels)%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either"]*100,1))%>%
  filter(Participants>0)%>%
  ungroup()%>%
  
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
  labs(
    # title="Number of participants identified as initiating a drug within 6 months after randomisation from each dataset",
       y="Participants",
       fill="",
      #  caption = str_wrap("Analysis right-censored at 180 days after randomisation. Drug initiation derived only for patients considered not at risk, defined as those not identified as taking a drug at randomisation based on presence of a relevant drug code up to 90 days before randomisation in either dataset.", 200)
      )+
  scale_fill_discrete(limits=c("Either", "Both", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either", "Both", "Dispensing only", "GP only"))+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=3)+
  scale_y_continuous(expand=expansion(c(0,0.5)))



ggsave("Outputs/Figures/Thesis/drug_initiation_proportions_datasets.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")


###### slides ------
codelists_agreement_participants%>%
  filter(labels!="Macrolides")%>%
  select(labels,
         Both = GP_Dispensing,
         `GP only` = GP_only,
         `Dispensing only` = Dispensing_only)%>%
  rowwise()%>%
  mutate(Either = sum(Both, `GP only`, `Dispensing only`))%>%
  pivot_longer(2:5, names_to="Dataset", values_to = "Participants", values_drop_na = T)%>%
  left_join(codelist_labels)%>%
  group_by(labels)%>%
  mutate(Prop = round(Participants/Participants[Dataset=="Either"]*100,1))%>%
  filter(Participants>0)%>%
  ungroup()%>%
  
  ggplot(aes(Dataset, Participants, fill=Dataset))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~labels, scales="free",
             labeller = label_wrap_gen(width = 30),
             ncol=5)+
  theme_gray(base_size=25)+
  oxpop_blue_panel+
  theme(legend.position = "none",
        text = element_text(family="Mulish"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        # strip.text.x = element_text(size = 10),
        axis.text.y=element_text(size=15))+
  labs(
    # title="Number of participants identified as initiating a drug within 6 months after randomisation from each dataset",
       y="Participants",
       fill="",
       # caption = str_wrap("Analysis right-censored at 180 days after randomisation. Drug initiation derived only for patients considered not at risk, defined as those not identified as taking a drug at randomisation based on presence of a relevant drug code up to 90 days before randomisation in either dataset.", 200)
       )+
  scale_fill_discrete(limits=c("Either", "Both", "Dispensing only", "GP only"))+
  scale_x_discrete(limits=c("Either", "Both", "Dispensing only", "GP only"))+
  geom_text(aes(label=paste0(Participants, "\n(", Prop, "%)")), vjust=-0.1, size=5, color="white")+
  scale_y_continuous(expand=expansion(c(0,0.6)))



ggsave("Outputs/Figures/ictmc_slides/drug_initiation_proportions_datasets.png",
       last_plot(),
       width=60,
       height=30,
       units="cm",
       dpi="retina")






