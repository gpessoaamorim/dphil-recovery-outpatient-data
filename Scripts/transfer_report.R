# DPhil Transfer Report------------
# 05/10/21
# Guilherme Amorim

# notes to self------

      # plots for slides
      
      # set theme_gray(base_size=25)
      # set geom_text(size=6)
      # save with ggsave("Outputs/Transfer/Figures/plot.png",dpi="retina",width=20,  height=8)

## plot structure ------
# 1. data manipulation
# 2. aesthetics and calling ploting functions
# 3. apply general theme
# 4. labeling


# load libraries------------------

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
library(tidytext) # reordering characters acording to a grouping variable (for plots)
library(ggdark) # themes for ggplot 
library(ggthemes) # themes for ggplot 

  
  
# set working directory
setwd("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY")

# Some system settings ------
options(scipen=10000) # prevent scientific notation in plots

# set custom ggplot themes ------

##  (need to set a custom theme for word documents)

## custom themes for slides
# oxpop_blue_panel<- (dark_mode(theme_fivethirtyeight(base_size = 20))+
#                  theme(plot.background = element_rect(fill = "#081c44"),
#                        panel.background = element_rect(fill = "#081c44"),
#                        text = element_text(family="Mulish",
#                                            color = "white"),
#                        panel.grid.major = element_line(color = "grey90", size=0.1, linetype = "dashed"),
#                        panel.grid.minor = element_blank(),
#                        strip.background = element_blank(),
#                        legend.position = "none",
#                        legend.background = element_blank()))
# 
# oxpop_grey_panel<- (dark_mode(theme_fivethirtyeight(base_size = 20))+
#                       theme(plot.background = element_rect(fill = "#081c44"),
#                             panel.background = element_rect(fill = "#273746"),
#                             text = element_text(family="Mulish",
#                                                 color = "white"),
#                             panel.grid.major = element_line(color = "grey90", size=0.1, linetype = "dashed"),
#                             panel.grid.minor = element_blank(),
#                             strip.background = element_blank(),
#                             legend.position = "none",
#                             legend.background = element_blank()))








# Import datasets ----------

## import GP dataset------------------

gp_unfiltered <- read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/INT08_GDPPR/0047/DP_INT08_GDPPR_Deidentified_2021-05-06_08-32-56.csv", 
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


## import Dispensing dataset------------------
meds_unfiltered <- read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/INT07_PCAREMEDS/0049/DP_INT07_PCAREMEDS_DEIDENTIFIE_2021-05-13_15-43-35.csv", 
                            col_types = cols(study_number = col_character(),
                                             bsa_prescription_id = col_character(), 
                                             paiddmd_code = col_character(),
                                             prescribeddmd_code = col_character(),
                                             processing_period_date = col_date(format = "%Y-%m-%d")))

## import randomisation dates ------------------
rand_dates <- read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/Randomisation_dates/0053/DP_Date_Of_Randomisation_2021-06-02_06-30-22.csv", col_types = cols(study_number = col_character(), randomisation_date = col_date(format = "%Y-%m-%d"),randomisation_date_2 = col_skip(), sus_extract_number = col_skip()))

## import list of withdrawn participants ------------------
withdrew<- read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/RECOVERY Consent Withdrawn 2021-10-08.xlsx", col_types = c("text", "skip", "skip", "skip", "skip", "skip", "skip", "skip","skip", "skip", "skip", "skip"))%>%.[[1]]

## load GP cluster list to retrieve clusters and code descriptions---------------
gp_cluster_lookup <-read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/GDPPR_Cluster_refset_1000230_20210914.xlsx")
 # latest version available, 14 September 2021


gp_cluster_lookup$Cluster_Desc%<>%as.factor()

  # renaming categories to make them shorter
levels(gp_cluster_lookup$Cluster_Desc)<-list('ACE' = "Angiotensin-converting enzyme (ACE) inhibitor prescription codes",
                                             'ARB' = "Angiotensin II receptor blockers (ARB) prescription codes",
                                             'AntiHT'="Antihypertensive medications",
                                             'Antipsychotics' ="Antipsychotic drug codes",
                                             'Asthma drugs'="Asthma-related drug treatment codes",
                                             'Asthma ICS' = "Asthma inhaled corticosteroids codes",
                                             'Bone sparing agents' = "Bone sparing agent drug codes",
                                             'Bone sparing therapy' = "Bone sparing therapy codes",
                                             'COPD drugs' = "Chronic obstructive pulmonary disease (COPD) drug codes",
                                             'Clopidogrel drug codes' = "Clopidogrel drug codes",
                                             'Clopidogrel prophylaxis codes' = "Clopidogrel prophylaxis codes",
                                             'Lithium stopped' = "Code for stopped lithium",
                                             'Constipation treatment' = "Constipation treatment codes",
                                             'Steroids'="Corticosteroid drug codes",
                                             'Diabetes drugs' = "Diabetes mellitus drugs codes",
                                             'Dipyridamole'="Dipyridamole prescription codes",
                                             'Epilepsy drugs' = "Drug treatment for epilepsy",
                                             'Ezetimibe' = "Ezetimibe drug codes",
                                             'Flu vaccine'="Flu vaccine drug codes",
                                             'Hypothiroidism drugs' = "Hypothyroidism treatment codes",
                                             'Immunossuppresion drugs'="Immunosuppression drugs",
                                             'Beta-blockers (licensed)'="Licensed beta-blocker prescription codes",
                                             'Lithium'="Lithium prescription codes",
                                             'Metformin'="Metformin drug codes",
                                             'Oral anticoagulation drugs'="Oral anticoagulant drug codes",
                                             'Oral anticoagulation prophylaxis'="Oral anticoagulant prophylaxis codes",
                                             'OTC salicylate'="Over the counter (OTC) salicylate codes",
                                             'Pharmacotherapy codes'="Pharmacotherapy codes",
                                             'Pharmacotherapy drug codes' = "Pharmacotherapy drug codes",
                                             'Prednisolone'="Prednisolone drug codes",
                                             'Salicylate prescription'="Salicylate prescription codes",
                                             'Severe asthma drugs'="Severe asthma drug treatment codes",
                                             'Severe immunosuppresion drugs'="Severe immunosuppression drug codes",
                                             'Statins' = "Statin codes",
                                             'Beta-blockers (unlicensed)'="Unlicensed beta-blocker prescription codes")



## load SNOMED FSN (fully-specified names) to inspect GP dataset-------
FSN <- read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/Lookup_tables/FSN.csv", 
                col_types = cols(id = col_character(), moduleId = col_character(), conceptId = col_character(), typeId = col_character(), caseSignificanceId = col_character()))




## load entire list of SNOMED concepts that ever existed -------
#(28 October 2020 release)

snomed_concepts<-read_csv("Tools/SNOMED terminology/Aggregated/rdiagnosislist/concepts.csv", col_types = cols(id = col_character(), moduleId = col_character(), definitionStatusId = col_character()))



## load entire list of SNOMED descriptions (not needed for overall script) ---------------------
#(28 October 2020 release)

snomed_descriptions<-read_csv("Tools/SNOMED terminology/Aggregated/rdiagnosislist/descriptions.csv", col_types = cols(Id = col_character(), moduleId = col_character(), conceptId = col_character(), typeId = col_character(), caseSignificanceId = col_character()))








# Load RECOVERY drug codelists ------------------
## SNOMED codelists ------------------
file.list <- list.files(path="K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Tools/Codelists/Medications_SNOMED/", full.names=T, pattern = "*.xlsx")

df = lapply(file.list, function(i){
  x=read_excel(i, sheet="Codelist")
  x$codelist=i
  x
})

codelists_snomed<-bind_rows(df)

codelists_snomed$codelist<-str_sub(codelists$codelist,117, -6)

rm(file.list,df)

## BNF codelists------------------

file.list <- list.files(path="K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Tools/Codelists/Medications_BNF/", full.names=T, pattern = "*.xlsx")

df = lapply(file.list, function(i){
  x=read_excel(i, sheet="Codelist")
  x$codelist=i
  x
})

codelists_bnf<-bind_rows(df)

codelists_bnf$codelist<-str_sub(codelists_bnf$codelist,112, -6)

rm(df, file.list)






# General descriptive summaries ------------------

## Participants and rows ------------------
nrow(gp_unfiltered) # 9912418
n_distinct(gp_unfiltered$study_number) # 32913

nrow(meds_unfiltered) # 6409490
n_distinct(meds_unfiltered$study_number) # 32922

## Remove withdrawn participants and recalculate -----------------------------

gp<-gp_unfiltered%>%
  filter(!study_number %in% withdrew)

### save filtered gp dataset ------
write.csv(gp, "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/INT08_GDPPR/0047/gp_filtered.csv", row.names=F)

rm(gp_unfiltered)




meds<-meds_unfiltered%>%
  filter(!study_number %in% withdrew)

### save filtered meds dataset ------
write_csv(meds, "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/INT07_PCAREMEDS/0049/meds_filtered.csv")


rm(meds_unfiltered)




nrow(gp) # 9908044
n_distinct(gp$study_number) # 32901

nrow(meds) # 6407818
n_distinct(meds$study_number) # 32911

rand_dates<-rand_dates%>%
  filter(!(study_number %in% withdrew))

rm(withdrew)

## !Code to load filtered gp and meds datasets --------
gp <- read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/INT08_GDPPR/0047/gp_filtered/gp_filtered.csv", 
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


meds <- read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/INT07_PCAREMEDS/0049/meds_filtered.csv", 
                 col_types = cols(study_number = col_character(),
                                  bsa_prescription_id = col_character(), 
                                  paiddmd_code = col_character(),
                                  prescribeddmd_code = col_character(),
                                  processing_period_date = col_date(format = "%Y-%m-%d")))


## Date of last patient randomised in each dataset ---------

participants_gp <- gp%>%
  distinct(study_number)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  arrange(desc(randomisation_date))

x1<-participants_gp%>%
  slice_head(n=1)%>%
  select(randomisation_date)%>%
  .[[1]]

x1

participants_meds <- meds%>%
  distinct(study_number)%>%
  left_join(rand_dates, by=c("study_number"))%>%
  arrange(desc(randomisation_date))

x2<-participants_meds%>%
  slice_head(n=1)%>%
  select(randomisation_date)

x2

### restrict meds dataset to participants randomised up until max rand date in GP dataset
participants_meds_restricted<-
  participants_meds%>%
  filter(randomisation_date<=x1)

participants_meds_restricted%>%
  nrow() # 32780

meds_restricted<-meds%>%
  filter(study_number %in% participants_meds_restricted$study_number)

meds_restricted%>%nrow() # 6389875

n_distinct(meds_restricted$study_number)# 32780

rm(meds)

## counts of overall RECOVERY participants

rand_dates%>%
  filter(!study_number%in%withdrew)%>%
  filter(randomisation_date<=x1)%>%
  nrow() # 39159

rand_dates%>%
  filter(!study_number%in%withdrew)%>%
  filter(randomisation_date<=x1)%>%
  filter(NATION=="EN")%>%
  nrow() # 35018



## Counts of prescriptions in the GP dataset -----

gp%>%
  filter(!is.na(episode_prescription) | !is.na(value1_prescription) | !is.na(value2_prescription))%>%
  nrow() # 3628110


### total RECOVERY participants randomised until then
rand_dates%>%
  filter(randomisation_date<=x1)%>%
  distinct(study_number)%>%
  nrow() # 39159 participants


a<-rand_dates%>%
  filter(randomisation_date<=x1 & NATION=="EN")%>%
  select(study_number)%>%
  .[[1]]

a%>%length() # 35018 EN participants


## Overlap in participants between datasets ------------
b<-participants_gp%>%
  select(study_number)%>%
  .[[1]] # 32913 participants

c<-participants_meds_restricted%>%
  select(study_number)%>%
  .[[1]] # 32785 participants


myCol<-brewer.pal(3, "Pastel2")[c(1,2,3)]

venn.diagram(list(a,b,c), 
             category.names = c("Trial", "GP dataset", "Dispensing dataset"), 
             filename='Outputs/Transfer/Figures/#1_participants_venn.jpg',
             fill=myCol,
             width = 8500,
             height=7000,
             cex=2,
             cat.cex=3)

# setdiff between GP and RECOVERY
y<-setdiff(b,a)
length(y) # 30


rand_dates%>%
  filter(study_number%in%y)%>%
  mutate(NATION=as.factor(NATION))%>%
  group_by(NATION)%>%
  summarize(n=n_distinct(study_number))%>%
  View()

# 14 NI, 1 SC, and 15 WL participants; 30 in total



# setdiff between Dispensing and RECOVERY
z<-setdiff(c,a)
length(z) # 95

rand_dates%>%
  filter(study_number%in%z)%>%
  View()

rand_dates%>%
  filter(study_number%in%z)%>%
  mutate(NATION=as.factor(NATION))%>%
  group_by(NATION)%>%
  summarize(n=n_distinct(study_number))%>%
  View()
# 17 NI, 1 SC, 77 WL participants (95 total, all accounted for)

rm(participants_meds, participants_gp, participants_meds_restricted,a,b,c,x1,x2,y,z)

## Entries per participant---------------------

### GP dataset---------------------
p<-gp%>%
  group_by(study_number)%>%
  summarise(entries_per_participant=n())

skim(p) # summary statistics

p1<-p%>%
  ggplot(aes(x=entries_per_participant))+
  geom_histogram(color="black", fill="white")+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  ggtitle("Entries per participant in the GP dataset")+
  xlim(c(0,7000))+
  ylim(c(0,15000))

p2<-p%>%
  ggplot(aes(x=entries_per_participant))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank())+
  xlab("Entries")+
  xlim(c(0,7000))

f2<-p1+p2+plot_layout(nrow=2, heights=c(2,1))

### Dispensing dataset---------------------

p<-meds_restricted%>%
  group_by(study_number)%>%
  summarise(entries_per_participant=n())

skim(p) # summary statistics

p1<-p%>%
  ggplot(aes(x=entries_per_participant))+
  geom_histogram(color="black", fill="white")+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  ggtitle("Entries per participant in the Dispensing dataset")+
  xlim(c(0,7000))+
  ylim(c(0,15000))

p2<-p%>%
  ggplot(aes(x=entries_per_participant))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab("Entries")+
  xlim(c(0,7000))

f3<-p1+p2+plot_layout(nrow=2, heights=c(2,1))

figure<-f2/f3

ggsave("Outputs/Transfer/Figures/Entries_per_participant.png",
       figure,
       height=10,
       dpi = 700)



rm(p,p1,p2,f2,f3, figure, myCol, rand_dates, withdrew)




# General description of data fields --------------

gp_fields<-colnames(gp)%>%
  as_tibble()%>%
  rename(Field=value)

write.csv(gp_fields, "Outputs/Transfer/Tables/GP_dataset_fields.csv", row.names = F)  

meds_fields<-colnames(meds)%>%
  as_tibble()%>%
  rename(Field=value)

write.csv(meds_fields, "Outputs/Transfer/Tables/Dispensing_dataset_fields.csv", row.names = F)  

## Sample data -------------

s1<-gp%>%
  filter(value1_prescription>0)%>%
  slice_sample(n=1)

write.csv(s1, "Outputs/Transfer/Tables/GP_dataset_sample.csv", row.names=F)

s2<-meds%>%
  slice_sample(n=1)

write.csv(s2, "Outputs/Transfer/Tables/Dispensing_dataset_sample.csv", row.names=F)

rm(s1,s2, meds_fields, gp_fields)

# Frequencies of data fields -------------


## GP dataset--------

colnames(gp)
head(gp)

gp_rows<-nrow(gp)
gp_participants<-n_distinct(gp$study_number)

### general completeness table-------

completeness_table<-
  gp%>%
  summarise_all(funs(sum(complete.cases(.))/gp_rows*100))%>%
  t()%>%
  as.data.frame()%>%
  mutate(value=V1, .keep=c("unused"))

completeness_table$field<-
  row.names(completeness_table)

completeness_table$field%<>%
  factor(levels=colnames(gp))

row.names(completeness_table)<-NULL


completeness_plot<-
  completeness_table%>%
  ggplot(aes(x=as.factor(field), y=value))+
  geom_bar(stat='identity')+
  xlab("Field")+
  ylab("Completeness rate")+
  #scale_x_discrete(guide=guide_axis(n.dodge=2))+
  theme(axis.text.x = element_text(angle = 30,hjust=1))+
  geom_text(stat='identity', aes(label=round(value, 1)), vjust=-0.5, size=4, hjust=0.5)

ggsave("Outputs/Transfer/Figures/completeness_table.png", last_plot(),
       width=10, dpi = 700)

rm(completeness_table,completeness_plot)

### sex -----
skim(gp$sex)

gp%>%
  filter(!is.na(sex))%>%
  nrow() #5799675 gender entries

gp%>%
  filter(!is.na(sex))%>%
  distinct(study_number)%>%
  nrow() #20686 participants


gp%>%
  distinct(sex)%>%
  nrow() #3 gender codes; which?

table(gp$sex, useNA = c("always"))

plot_labels<-c("Unknown", "Male")

p<-gp%>%
  distinct(study_number,sex)%>%
  ggplot(aes(sex))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  scale_x_discrete(labels=plot_labels)+
  xlab("Sex")+
  ylab("Participants")

ggsave("Outputs/Transfer/Figures/Sex_counts.png",
       # width = 10,
       p)

rm(plot_labels, p)


### lsoa-----

library(forcats) # used to reverse grouping order in some ggplots
library(raster) # handling geographical mapping data
library(rgdal) # loading mapping data
library(mapproj) # map coordinates

skim(gp$lsoa)

gp%>%
  filter(!is.na(lsoa))%>%
  nrow() #4362 entries with missing LSOAs, 

gp%>%
  distinct(lsoa)%>%
  nrow() # 18826 distinct LSOAs 

gp%>%
  filter(!is.na(lsoa))%>%
  distinct(study_number)%>%
  nrow() # covering 32883 participants

gp%>%
  filter(!is.na(lsoa))%>%
  distinct(study_number)%>%
  .[[1]] -> p 

rand_dates%>%
  filter(!study_number%in% withdrew)%>%
  filter(study_number %in% p)%>%
  group_by(NATION)%>%
  summarise(n=n())


gp%>%
  filter(is.na(lsoa))%>%
  distinct(study_number)%>%
  .[[1]] -> p1

rand_dates%>%
  filter(study_number %in% p1)%>%
  group_by(NATION)%>%
  summarise(n=n())


# 32,844 LSOAs in england (according to https://www.ons.gov.uk/methodology/geography/ukgeographies/censusgeography)
# so we have participants covering 18826/32844 = 57.9% LSOAs

#### some numbers and notes on administrative divisions --------

# according to https://www.gov.uk/guidance/local-government-structure-and-elections, in England there are:
# 55 Unitary Authorities 
# out of date now
# 26 county councils
# but two have been extinguished: Northamptonshire (April 2021) and Buckinghamshire (April 2020)
# 32 London Boroughs
# 36 metropolitan districts

# more up to date info here: https://lgiu.org/local-government-facts-and-figures-england/, which says:
# 398 principal (unitary, upper and second-tier) councils in the UK:
# England
# 24 county councils (plus 8 old counties including London and Metropolitan areas, not used nowadays - but still have codes) - these are second-tier
# 181 district councils - lower-tier
# 32 london boroughs - unitary
# 36 metropolitan councils - unitary
# 58 unitary units - unitary
# 2 sui generis authorities (city of london and isles of scilly) - unitary
# Wales
# 22 UAs - unitary
# Scotland
# 32 UAs - unitary 
# NI 
# 11 UAs - unitary

# there are 374 Local Authority Districts (LADs) (https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2021-uk-bfe/about)- one for each of the above, except English county councils - which include several LADs

# LADs in England are 309:
# 32 London Boroughs
# 58 UAs
# 36 metropolitan districts
# 181 district councils
# 2 sui generis areas
# 22 LADs in Wales, one for each UA
# 32 LADs in Scotland, one for each UA
# 11 LADs in NI, one for each UA


#### load lsoa to LAD matches (whole UK) - May 2021------
# (from https://geoportal.statistics.gov.uk/datasets/postcode-to-output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-may-2021-lookup-in-the-uk/about)
lsoa_lad<-read_csv("Tools/UK maps/OA-LSOA-MSOA-LAD-lookup/PCD_OA_LSOA_MSOA_LAD_MAY21_UK_LU.csv", col_types = cols(pcd7 = col_skip(), pcd8 = col_skip(), pcds = col_skip(), dointr = col_skip(),doterm = col_skip(), usertype = col_skip(), oa11cd = col_skip(), msoa11cd = col_skip(),msoa11nm = col_skip(), ladnmw = col_skip()))

lsoa_lad%<>%
  distinct(lsoa11cd, .keep_all=T)

lsoa_lad%>%
  distinct(ladcd)%>%
  nrow() # 377 distinct LADs, including 2 pseudo ones (L99999999 Channel Islands and M99999999 Isle of Man), and one row with NA)

lsoa_lad%>%
  filter(grepl('E', ladcd))%>%
  distinct(ladcd, .keep_all=T)%>%
  nrow() # 309 distinct LADs in England

# so we can be secure about using these maps




#### load LAD-county matches (for England) - April 2021------
# from https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-county-april-2021-lookup-in-england/explore
lad_county <- read_csv("Tools/UK maps/LAD-county-lookup/Local_Authority_District_to_County_(April_2021)_Lookup_in_England.csv", col_types = cols(FID = col_skip()))
# this list is problematic because it does not include the 58 unitary authorities 
# there are 250 LADs, corresponding to these divisions:
# 24 county councils (181 LADs)
# 36 metropolitan councils (36 LADs)
# 32  london boroughs + City of London (33 LADs)

# so for all English LADs that are not in this list (58 UAs + Scilly), the LAD itself needs to be kept for use in the map

# merge lsoa-lad-counties map

counties<-left_join(lsoa_lad, lad_county, by=c("ladcd"="LAD21CD"))

counties%<>%
  select(lsoa=lsoa11cd,
         lsoa11nm,
         ladcd,
         ladnm,
         county_code=CTY21CD,
         county_name=CTY21NM,
  )

counties%>%distinct(ladcd, .keep_all=T)%>%nrow() # we retain 377 LADs in the UK
counties%>%distinct(county_code, .keep_all=T)%>%nrow() # 33 counties; so this includes the old counties (nowadays only 24) and one NA


#### import maps of all UK counties and unitary authorities (May 2021)------------
### from https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-may-2021-uk-bgc/about
filename<-"K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/UK counties shapefile/UK/Counties_and_Unitary_Authorities_(May_2021)_UK_BGC"

# load map to shape file
poly_counties<-raster::shapefile(filename)


poly_counties@data[["CTYUA21CD"]]%>%
  as.data.frame()%>%
  distinct(.)
# 217 distinct areas in the map

poly_counties@data[["CTYUA21CD"]]%>%
  as.data.frame()%>%
  filter(grepl('E', .))
# 152 distinct areas for england; these are:
# 58 UAs - have one LAD
# 36 metropolitan councils - have one LAD
# 32 london boroughs - have one LAD
# 24 county councils - have many LADs
# 2 sui generis areas (City of London and Isles of Scilly) - have one LAD


# we want to keep LAD code for those that do not belong to a county, so we can do:
counties%<>%
  mutate(county_ua=if_else(is.na(county_code), ladcd, county_code), .keep=c("all"))

# and we need to exclude the ones in the old counties; these are:
# E11000001 Greater Manchester
# E11000002 Merseyside
# E11000003 South Yorkshire
# E11000005 West Midlands
# E11000006 West Yorkshire
# E11000007 Tyne and Wear
# E13000001 Inner London
# E13000002 Outer London

# so we'll do:
counties%<>%
  mutate(county_ua=if_else(grepl('E11|E13', county_code), ladcd, county_ua), .keep=c("all"))

# calculate counts per county, UAs or LAD (for London and metropolitan councils)
county_counts<-gp%>%
  select(study_number, code, lsoa)%>%
  left_join(counties, by="lsoa")%>%
  group_by(county_ua)%>%
  summarise(entries=n(),
            participants=n_distinct(study_number))

# calculate unique counties or UAs in the UK
counties%>%
  distinct(county_ua, .keep_all=T)%>%
  nrow()# 220; 

counties%>%
  distinct(county_ua, .keep_all=T)%>%
  filter(grepl('E', county_ua))%>%
  nrow() # 152 England

counties%>%
  distinct(county_ua, .keep_all=T)%>%
  filter(grepl('W', county_ua))%>%
  nrow() # 22 Wales

counties%>%
  distinct(county_ua, .keep_all=T)%>%
  filter(grepl('S', county_ua))%>%
  nrow() # 32 Scotland

counties%>%
  distinct(county_ua, .keep_all=T)%>%
  filter(grepl('N', county_ua))%>%
  nrow() # 11 NI

counties%>%
  distinct(county_ua, .keep_all=T)%>%
  filter(!grepl('N|E|S|W', county_ua))%>%
  nrow() # 3 others
# which are

counties%>%
  distinct(county_ua, .keep_all=T)%>%
  filter(!grepl('N|E|S|W', county_ua))%>%
  select(ladnm) # 1 NA, Pseudo (Channel Islands), Pseudo (Isle of Man) 

# so in total we have 220 = 152+22+32+11+3 counties or UAs 

# calculate unique counties or UAs in the data
n_distinct(county_counts$county_ua) # 156 different ones; 90 EN, 4 WL, 1 NA

county_counts%>%
  filter(grepl('E', county_ua))%>%
  nrow() # 151 England

county_counts%>%
  filter(grepl('W', county_ua))%>%
  nrow() # 4 Wales

county_counts%>%
  filter(!grepl('E|W', county_ua)) # 1 NA

# what's the english county/ua not in our data?

a<-county_counts%>%
  filter(grepl('E', county_ua))%>%
  select(county_ua)%>%
  .[[1]]

counties%>%
  filter(grepl('E', county_ua) & !county_ua%in%a)
# Isles of Scilly!!

# on with the work now...

#### create map plots------

# align county code field name
county_counts%<>%
  rename(CTYUA21CD=county_ua)

# housecleaning
rm(withdrew, counties, lsoa_lad, lad_county, filename, a)

# merge counts from recovery with map file
poly_counties<-merge(poly_counties, county_counts, by="CTYUA21CD")

# convert map file to a tidy format for ggplot
poly_counties_df<-broom::tidy(poly_counties)

# assign unique row names to original file to allow data retrieval (not transferred by the tidy command)
poly_counties$id<-row.names(poly_counties)

# rejoin the data for each area (lost in the tidy transformation)
# and reduce amount of data that gets dragged along to aid processing
poly_counties_df<-left_join(poly_counties_df, poly_counties@data, by="id")%>%
  select(long, lat, order, hole, piece, group, id, entries, participants)

# plot the map (for participant counts)
map1<-poly_counties_df%>%
  rename(Participants=participants)%>%
  ggplot(aes(x=long, y=lat, group=group, fill=Participants))+
  geom_polygon(colour="black", size=0.1)+
  theme_void()

# plot the map (for entry counts)
map2<-poly_counties_df%>%
  rename(Entries=entries)%>%
  ggplot(aes(x=long, y=lat, group=group, fill=Entries))+
  geom_polygon(colour="black", size=0.1)+
  theme_void()

# join both maps
map=map1+map2

# print plot
print(map)

# save map
ggsave("Outputs/Transfer/Figures/UK counties map.png",
       map)

# housecleaning
rm(county_counts, poly_counties, poly_counties_df, map, map1, map2)
gc()




### date of death------------

gp%>%
  filter(!is.na(date_of_death))%>%
  nrow()
# appears in 2,786,050 rows

gp%>%
  distinct(date_of_death)%>%
  nrow()
# 367 unique entries

gp%>%
  distinct(date_of_death, .keep_all = T)%>%
  distinct(study_number)%>%
  nrow()
# for 364 participants

x<- gp%>%
  filter(!is.na(date_of_death))%>%
  distinct(date_of_death, .keep_all=T)%>%
  mutate(year_of_death=substr(date_of_death,1,4), keep=c("unused"))%>%
  mutate(year_of_death=as.numeric(year_of_death))%>%
  group_by(year_of_death)%>%
  summarize(participants=n_distinct(study_number))

x%>%
  ggplot(aes(x=year_of_death, y=participants, group=NA))+
  geom_point()+
  geom_line()+
  geom_label_repel(stat='identity', aes(label=paste(year_of_death, ", ", participants, " participants", sep="")), vjust=1, size=6, hjust=0.5)+
  xlab("Year of death")+
  ylab("Number of participants")+
  theme_gray(base_size=25)

ggsave('Outputs/Transfer/Figures/date_of_death_plot.png', 
       last_plot(),
       height=10,
       width=20,
       dpi="retina")


### ethnic ----------------

gp%>%
  filter(!is.na(ethnic))%>%
  nrow()
# appears in 4166192 rows

gp%>%
  distinct(ethnic)%>%
  nrow()
# 18 unique entries

gp%>%
  filter(!is.na(ethnic))%>%
  distinct(study_number)%>%
  nrow()
# for 15752 participants (of 32901), 48%

plot_labels<-c("White British",
               "White Irish",
               "White - Any other White Background",
               "Mixed - White and Black Caribbean",
               "Mixed - White and Black African",
               "Mixed - White and Asian",
               "Mixed - Any other mixed background",
               "Asian or Asian British - Indian",
               "Asian or Asian British - Pakistani",
               "Asian or Asian British - Bangladeshi",
               "Asian or Asian British - Any other Asian background",
               "Black or Black British - Caribbean",
               "Black or Black British - African",
               "Black or Black British - Any other Black background",
               "Other Ethnic Groups - Chinese",
               "Other Ethnic Groups - Any other ethnic group",
               "Not stated")

ethnic_bar_chart<-
  gp%>%
  distinct(study_number,ethnic)%>%
  ggplot(aes(ethnic))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  scale_x_discrete(labels=plot_labels, 
                   #guide=guide_axis(n.dodge=2)
  )+
  theme(axis.text.x = element_text(angle = 30,hjust=1))+
  xlab("Ethnic group")+
  ylab("Participants")

ggsave('Outputs/Transfer/Figures/ethnic_bar_chart.png', last_plot(),
       height=8)


rm(ethnic_bar_chart, plot_labels)

### gp system supplier--------


gp%>%
  filter(!is.na(gp_system_supplier))%>%
  nrow()
# appears in 9908044 rows (100%)

gp%>%
  distinct(gp_system_supplier)%>%
  nrow()
# 4 unique entries

gp%>%
  filter(!is.na(gp_system_supplier))%>%
  distinct(study_number)%>%
  nrow() 
# all participants (32901)

gp_supplier_chart<-
  gp%>%
  distinct(study_number,gp_system_supplier)%>%
  mutate(gp_system_supplier=as.factor(gp_system_supplier))%>%
  ggplot(aes(gp_system_supplier))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  # theme(axis.text.x = element_text(angle = 30,hjust=1))+
  xlab("GP system supplier")+
  ylab("Participants")

ggsave('Outputs/Transfer/Figures/gp_supplier_chart.png', last_plot(),
       # height=8
)

rm(gp_supplier_chart)

#### map of gp system suppliers ----

# load lsoa to LAD map (as above)
lsoa_lad<-read_csv("Tools/UK maps/OA-LSOA-MSOA-LAD-lookup/PCD_OA_LSOA_MSOA_LAD_MAY21_UK_LU.csv", col_types = cols(pcd7 = col_skip(), pcd8 = col_skip(), pcds = col_skip(), dointr = col_skip(),doterm = col_skip(), usertype = col_skip(), oa11cd = col_skip(), msoa11cd = col_skip(),msoa11nm = col_skip(), ladnmw = col_skip()))

# select distinct LSOAs (map included lower levels such as OA so there were multiple entries for each LSOA)
lsoa_lad%<>%
  distinct(lsoa11cd, .keep_all=T) 

# load LAD to county maps
lad_county <- read_csv("Tools/UK maps/LAD-county-lookup/Local_Authority_District_to_County_(April_2021)_Lookup_in_England.csv", col_types = cols(FID = col_skip()))

# merge lsoa-lad-counties map
counties<-left_join(lsoa_lad, lad_county, by=c("ladcd"="LAD21CD"))

# select only variables of interest
counties%<>%
  select(lsoa=lsoa11cd,
         lsoa11nm,
         ladcd,
         ladnm,
         county_code=CTY21CD,
         county_name=CTY21NM,
  )

# import maps of all UK counties and unitary authorities (May 2021) (as above)
filename<-"K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/UK counties shapefile/UK/Counties_and_Unitary_Authorities_(May_2021)_UK_BGC"

# load map to shape file
poly_counties<-raster::shapefile(filename)

# keep LAD code for those LADs that do not belong to a county, so we can do:
counties%<>%
  mutate(county_ua=if_else(is.na(county_code), ladcd, county_code), .keep=c("all"))

# and we need to exclude the ones in the old counties (starting with E11 or E13)
counties%<>%
  mutate(county_ua=if_else(grepl('E11|E13', county_code), ladcd, county_ua), .keep=c("all"))

# calculate counts of participants per county (separated by supplier)
county_counts<-
  gp%>%
  select(study_number, code, gp_system_supplier,lsoa)%>%
  left_join(counties, by="lsoa")%>%
  group_by(county_ua, gp_system_supplier)%>%
  summarise(participants=n_distinct(study_number))

# align county code field name
county_counts%<>%
  rename(CTYUA21CD=county_ua)

# rearrange data format to allow one entry per county (and thus merging in step below)
county_counts%<>%
  pivot_wider(names_from=gp_system_supplier, values_from = participants)# spreading the counts 


# merge counts from recovery with map file
poly_counties<-merge(poly_counties, county_counts, by="CTYUA21CD")

# convert map file to a tidy format for ggplot
poly_counties_df<-broom::tidy(poly_counties)

# assign unique row names to original file to allow data retrieval (not transferred by the tidy command)
poly_counties$id<-row.names(poly_counties)

# rejoin the data for each area (lost in the tidy transformation)
# and reduce amount of data that gets dragged along to aid processing
poly_counties_df<-left_join(poly_counties_df, poly_counties@data, by="id")%>%
  select(long, lat, order, hole, piece, group, id, EMIS, TPP, 'Cegedim Healthcare Solutions', 'EVA Health Technologies')%>%
  pivot_longer(cols=c(EMIS, TPP, 'Cegedim Healthcare Solutions', 'EVA Health Technologies'), names_to = "supplier", values_to="participants")


# plot the map (with participant counts per suppleor and per county)
map<-poly_counties_df%>%
  ggplot(aes(x=long, y=lat, group=group, fill=participants))+
  geom_polygon(colour="black", size=0.1)+
  theme_void(base_size = 30)+
  facet_wrap(ncol = 4, facets = vars(supplier))

# print plot
print(map)

# save map
ggsave("Outputs/Transfer/Figures/gp_system_supplier_map.png",
       map,
       width = 40,
       height=10,
       dpi = 300)



rm(counties, county_counts, filename, lad_county, lsoa_lad, map, poly_counties, poly_counties_df)


### processed_timestamp------

gp%>%
  select(processed_timestamp)%>%
  head()

gp%>%
  filter(!is.na(processed_timestamp))%>%
  nrow()
# appears in 9908044 rows (100%)

gp%>%
  filter(!is.na(processed_timestamp))%>%
  distinct(study_number)%>%
  nrow() 
# all participants (32901)

processed_timestamp_plot<-
  gp%>%
  mutate(processed_timestamp=as.Date(processed_timestamp))%>%
  group_by(processed_timestamp)%>%
  summarize(participants=n_distinct(study_number),
            entries=n())%>%
  pivot_longer(-processed_timestamp, names_to = "key", values_to = "value")%>%
  ggplot(aes(x=processed_timestamp, y=value, group=key))+
  geom_point()+
  geom_line()+
  xlab("Date of processing")+
  ylab("Count")+
  facet_grid(rows=vars(key), scales = "free_y")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

ggsave("Outputs/Transfer/Figures/processed_timestamp_plot.png",
       last_plot(),
       dpi=700)

rm(processed_timestamp_plot, processing_timestamp_plot)

### reporting period end date-----------

gp%>%
  select(reporting_period_end_date)%>%
  head()

gp%>%
  distinct(reporting_period_end_date)%>%
  nrow()
# 23 distinct ones

gp%>%
  distinct(reporting_period_end_date)%>%
  arrange(reporting_period_end_date)%>%
  View()

reporting_period_end_date_plot<-
  gp%>%
  group_by(reporting_period_end_date)%>%
  summarize(participants=n_distinct(study_number),
            entries=n())%>%
  pivot_longer(-reporting_period_end_date, names_to = "key", values_to = "value")%>%
  ggplot(aes(x=reporting_period_end_date, y=value, group=key))+
  geom_point()+
  geom_line()+
  facet_grid(rows=vars(key), scales = "free_y")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  ylab("Count")+
  xlab("Reporting period end date")

ggsave("Outputs/Transfer/Figures/reporting_period_end_date.png",
       last_plot(),
       dpi=700)

rm(reporting_period_end_date_plot)

### date -----
x<- gp%>%
  mutate(date=substr(date,1,4), keep=c("unused"))%>%
  mutate(date=as.numeric(date))%>%
  group_by(date)%>%
  summarize(participants=n_distinct(study_number),
            entries=n())%>%
  pivot_longer(-date, names_to = "key", values_to = "value")

x%>%
  ggplot(aes(x=date, y=value, group=key))+
  geom_line()+
  geom_point()+
  facet_grid(rows=vars(key), scales = "free_y")+
  ylab("Count")+
  xlab("Date")+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_continuous(breaks=seq(1800,2020,by=20))

ggsave("Outputs/Transfer/Figures/date_timeseries.png",
       last_plot(),
       width=10,
       height=8,
       dpi="retina")


### record_date------

x<- gp%>%
  mutate(record_date=substr(record_date,1,4), keep=c("unused"))%>%
  mutate(record_date=as.numeric(record_date))%>%
  group_by(record_date)%>%
  summarize(participants=n_distinct(study_number),
            entries=n())%>%
  pivot_longer(-record_date, names_to = "key", values_to = "value")

x%>%
  ggplot(aes(x=record_date, y=value, group=key))+
  geom_line()+
  geom_point()+
  facet_grid(rows=vars(key), scales = "free_y")+
  ylab("Count")+
  xlab("Record date")+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_continuous(breaks=seq(1800,2020,by=20))

ggsave("Outputs/Transfer/Figures/record_date_timeseries.png",
       last_plot(),
       width=10,
       height=8,
       dpi="retina")


### relationship between date and record_date --------

# generate date difference table
date_diff_table<-
  gp%>%
  select(date, record_date)%>%
  mutate(date_difference=as.numeric(difftime(record_date, date, units="days")))

#### plot for date vs record_date-----
date_diff_table%>%
  # filter(date_difference!=0)%>%
  ggplot()+
  geom_abline(intercept = 0, slope=1, colour="dark red", size=1.1)+
  # geom_vline(xintercept = as.numeric("1900-01-01"), colour="dark red", size=1.1, na.rm=T)+
  # geom_hline(yintercept = as.numeric("1900-01-01"), colour="dark red", size=1.1, na.rm=T)+
  stat_binhex(aes(date, record_date), bins=100, colour="black")+
  scale_fill_gradientn(colours=c("light blue", "blue"), name="Frequency", na.value=NA)

ggsave("Outputs/Transfer/Figures/date_vs_record_date_scatter.png",
       last_plot())

#### plot for date_difference -----
date_diff_table1<-
  gp%>%
  select(date, record_date)%>%
  mutate(date_difference=as.numeric(difftime(record_date, date, units="days")), .keep=c("unused"))

jpeg("Outputs/Transfer/Figures/date_difference_plot.jpg",
     height = 350,
     width = 550,
     quality = 100)

smoothScatter(date_diff_table1,
              xlab="Entry row index",
              ylab="Date difference in days (record_date minus date)",
              xaxt="n")+
  abline(h=20000, lty=2)+
  abline(h=-20000, lty=2)

dev.off()

# alternative way of plotting this
date_diff_table%>%
  mutate(row=row.names(date_diff_table))%>%
  ggplot()+
  stat_binhex(aes(row, y=date_difference), bins=100, colour="black")+
  scale_fill_gradientn(colours=c("light blue", "blue"), name="Frequency", na.value=NA)

####  plots for date and record_date vs date_diff ----
plot1<-date_diff_table%>%
  # filter(!date_difference==0)%>%
  ggplot()+
  stat_binhex(aes(date, date_difference), bins=100, colour="black")+
  scale_fill_gradientn(colours=c("light blue", "blue"), name="Frequency", na.value=NA)+
  theme_bw(base_size=10)

plot2<-date_diff_table%>%
  # filter(!date_difference==0)%>%
  ggplot()+
  stat_binhex(aes(record_date, date_difference), bins=100, colour="black")+
  scale_fill_gradientn(colours=c("light blue", "blue"), name="Frequency", na.value=NA)+
  theme_bw(base_size=10)

plot=plot1/plot2

ggsave("Outputs/Transfer/Figures/record_date_and_date_vs_date_diff_plot.png",
       last_plot(),
       height = 10,
       width = 10,
       dpi="retina")


#### some calculations of specific values and thresholds------
date_diff_table%>%
  filter(date_difference=="0")%>%
  nrow() # 6222862 entries
6222862/gp_rows


date_diff_table%>%
  filter(date_difference>0)%>%
  nrow() # 757525 entries

date_diff_table%>%
  filter(date_difference<0)%>%
  nrow() # 2917143 entries

date_diff_table%>%
  filter(date_difference>20000)%>%
  nrow() # 12319 entries

date_diff_table%>%
  filter(date_difference<(-20000))%>%
  nrow() # 796 entries

gp%>%
  mutate(year=str_sub(date, 1,4))%>%
  filter(year=="1900")%>%
  nrow() # 53 entries with date 1900


gp%>%
  mutate(year=str_sub(record_date, 1,4))%>%
  filter(year=="1900")%>%
  nrow() # 13 entries with record_date 19000

dates_plot=date_plot/record_date_plot

ggsave("Outputs/Transfer/Figures/date_and_record_date.png",
       last_plot(),
       dpi=700,
       height=10)

# housecleaning
rm(date_plot,
   date_diff_table,
   record_date_plot,
   dates_plot,
   plot,
   plot1,
   plot2)

#### entries with same date but different record date --------

gp_dt<-lazy_dt(gp)

rm(gp)

x<- gp_dt%>%
  select(study_number, code, date, record_date, lsoa, gp_system_supplier)%>%
  group_by(study_number, code, date)%>%
  distinct(record_date, .keep_all=T)%>%
  add_count(study_number, code, date)%>%
  filter(n>1)%>%
  arrange(study_number, code, date)%>%
  mutate(date_diff=record_date-lag(record_date, 1L))%>%
  as_tibble()
  # 579,698

x%>%
  filter(date_diff>2|date_diff<(-2))
  # 278,493

x%>%
  arrange(desc(n))


# plot of date diff, gp system supplier, and year

x%>%
  mutate(record_year=str_sub(record_date, 1,4),
         date_diff=as.numeric(date_diff))%>%
  mutate(record_year=as.numeric(record_year))%>%
  ggplot(aes(record_year, date_diff, color=gp_system_supplier))+
  geom_point(alpha=0.5)+
  theme_gray(base_size=20)+
  theme(legend.position = "bottom")+
  guides(color=guide_legend(title="GP system supplier"))+
  labs(title="Date difference between duplicated entries per record year",
       subtitle="(based on record_date differences for the same entry)",
       y="Entries",
       x="Record year",
       caption="Capped before 1950"
       )+
  xlim(c(1950,2022))
  
ggsave("Outputs/Transfer/Figures/duplicates_date_diff_gp_supplier.png",
       last_plot(),
       height=8,
       width=20,
       dpi="retina")




# per lsoa

x%>%mutate(lsoa_diff=n_distinct(lsoa))->x1

x1%>%filter(lsoa_diff>1)%>%nrow()
# 289,270 entries have different lsoas


# per gp provider

a<-x1%>%
  filter(date_diff>2|date_diff<(-2))%>%
  mutate(record_year=str_sub(record_date, 1,4))%>%
  mutate(lsoa_diff_flag=if_else(lsoa_diff>1, "Different location","Same location"))%>%
  group_by(gp_system_supplier, record_year, lsoa_diff_flag)%>%
  summarise(n=n(),
            participants=n_distinct(study_number),
            .groups = "keep")

a%>%
  mutate(record_year=as.numeric(record_year))%>%
  rename(Entries=n,
         Participants=participants)%>%
  pivot_longer(c(Entries, Participants), names_to = "key", values_to = "value")%>%
  ggplot(aes(record_year, value, group=key, shape=key))+
  geom_line(aes(color=key))+
  geom_point(aes(color=key))+
  facet_grid(lsoa_diff_flag~gp_system_supplier)+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "right",
        legend.title = element_blank())+
  labs(title="Duplicate entries along time, per GP system supplier, and location differences",
       subtitle="(for record_date difference over 2 days only)",
       caption="Duplicate entries defined as having same study_number, code, and date, but with different record_date",
       y="Number",
       x="Record year")
  
  
ggsave("Outputs/Transfer/Figures/duplicate_records_timeseries_gp_suppliers_lsoa.png",
       last_plot(),
       heigh=8,
       width=20,
       dpi="retina")




rm(x,a,x1)


### investigate extreme / illogical dates ------

a<- gp%>%
  mutate(year=str_sub(date, 1,4),
         record_year=str_sub(record_date, 1,4))%>%
  filter(year<1920| record_year<1920)

a%>%
  filter(year<1920)%>%
  nrow() # 172


a%>%
  filter(record_year<1920)%>%
  nrow() # 12399

a%>%
  filter(year<1920 & record_year<1920)%>%
  nrow() # 78

a%>%
  filter(year<1920 & record_year>=1920)%>%
  nrow() # 94

# investigate entries with date after record_date
gp%>%
  filter(date>record_date)%>%
  select(study_number, code, date, record_date)%>%
  arrange(record_date)%>%
  View() # 757525 entries

gp%>%
  filter(record_date=="1899-12-30")%>%
  nrow() # 12117 entries with this record date

# plot of entries with date > record_date per record year and per cluster category
x<- gp%>%
  filter(date>record_date)%>%
  select(study_number, code, date, record_date)%>%
  arrange(record_date)%>%
  left_join(FSN, by = c("code"= "conceptId"))%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, date, record_date, term, Cluster_Category)%>%
  mutate(record_year=str_sub(record_date, 1,4))%>%
  group_by(record_year, Cluster_Category)%>%
  summarise(n=n())

x%>%
  mutate(record_year=as.numeric(record_year))%>%
  ggplot(aes(record_year, n, group=Cluster_Category, color=Cluster_Category))+
  geom_line()+
  geom_point()+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")+
  labs(title="Entries with date after record_date, per record year and per cluster category")+
  guides(colour = guide_legend(nrow = 4))+
  scale_x_continuous(breaks=seq(1800,2021, by=10))


ggsave("Outputs/Transfer/Figures/date_after_record_date_cluster_categories.png",
       dpi="retina",
       width=20,
       height=8)

# investigate those cases in the meds category and after 2000

## plot of date vs record_date for meds
gp%>%
  select(study_number, code, date, record_date)%>%
  arrange(record_date)%>%
  left_join(FSN, by = c("code"= "conceptId"))%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Category=="Medications")%>%
  select(study_number, code, date, record_date, term, Cluster_Category)%>%
  # mutate(date_difference=difftime(record_date, date, "days"))%>%
  ggplot(aes(record_date, date, group=NULL))+
  stat_bin_hex(colour="black")+
  geom_abline(intercept = 0, slope=1, colour="dark red", size=1.1)+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")+
  scale_fill_continuous()+
  guides(fill = guide_colorbar(barwidth = unit(15, "cm")))+
  labs(title="Record_date vs date for medications (all time)")

ggsave("Outputs/Transfer/Figures/record_date_vs_date_medications.png",
       last_plot(),
       dpi="retina",
       width=20,
       height=8)

## plot of date difference vs record_date for meds after 2000
gp%>%
  # filter(record_date>="2000-01-01")%>%
  select(study_number, code, date, record_date)%>%
  arrange(record_date)%>%
  left_join(FSN, by = c("code"= "conceptId"))%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Category=="Medications")%>%
  select(study_number, code, date, record_date, term, Cluster_Category)%>%
  mutate(date_difference=difftime(record_date, date, units=c("days")))%>%
  ggplot(aes(record_date, date_difference, group=NULL))+
  stat_bin_hex(colour="black")+
  geom_hline(yintercept = 0, colour="dark red", size=1.1)+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")+
  scale_fill_continuous()+
  guides(fill = guide_colorbar(barwidth = unit(15, "cm")))+
  labs(title="Date difference vs record_date for medications (all time)",
       y="Date difference in days (record_date - date)")


ggsave("Outputs/Transfer/Figures/date_diff_vs_record_date_medications.png",
       last_plot(),
       dpi="retina",
       width=20,
       height=8)

# investigate meds with record_date before date  and difference<0
gp%>%
  select(study_number, code, date, record_date)%>%
  arrange(record_date)%>%
  left_join(FSN, by = c("code"= "conceptId"))%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Category=="Medications")%>%
  select(study_number, code, date, record_date, term, Cluster_Desc, Cluster_Category)%>%
  mutate(date_difference=difftime(record_date, date, units=c("days")))%>%
  filter(date_difference<0)%>%
  View()

# aggregate per cluster and perform some counts
x<- gp%>%
  select(study_number, code, date, record_date)%>%
  arrange(record_date)%>%
  left_join(FSN, by = c("code"= "conceptId"))%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Category=="Medications")%>%
  select(study_number, code, date, record_date, term, Cluster_Desc, Cluster_Category)%>%
  mutate(date_difference=difftime(record_date, date, units=c("days")))%>%
  filter(date_difference<0)%>%
  mutate(record_year=str_sub(record_date, 1,4))%>%
  group_by(Cluster_Desc, record_year)%>%
  summarise(entries=n(),
            participants=n_distinct(study_number),
            average_difference=mean(date_difference))





## plot the above
x%>%
  mutate(average_day_difference=as.integer(0-average_difference))%>%
  select(-average_difference)%>%
  pivot_longer(cols=-c(Cluster_Desc, record_year), names_to="key", values_to="value")%>%
  ggplot(aes(record_year, value, group=Cluster_Desc, color=Cluster_Desc))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(key), scales = "free")+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")+
  guides(colour = guide_legend(nrow = 6))+
  labs(title="Average day difference (date - record_date), number of entries, and participants for medications with date after record_date",
       subtitle="Difference over 0 days")

ggsave("Outputs/Transfer/Figures/date_vs_record_date_meds_counts.png",
       last_plot(),
       dpi="retina",
       width=30,
       height=10)

## zoom in 2018-2020 for average difference only
x%>%
  mutate(average_difference=as.integer(0-average_difference))%>%
  filter(record_year>=2018)%>%
  pivot_longer(cols=-c(Cluster_Desc, record_year), names_to="key", values_to="value")%>%
  ggplot(aes(record_year, value, group=Cluster_Desc, color=Cluster_Desc))+
  geom_point()+
  geom_line()+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")+
  xlab("")+
  ylab("")+
  facet_wrap(vars(key), scales = "free")+  
  guides(colour = guide_legend(nrow = 6))+
  labs(title="Average day difference (date - record_date), number of entries, and participants for medications with date after record_date",
       subtitle="2018 onwards and difference over 0 days")
  

ggsave("Outputs/Transfer/Figures/date_vs_record_date_meds_2018_counts.png",
       last_plot(),
       dpi="retina",
       width=30,
       height=10)

# maybe enough of delving into this...

# housecleaning
rm(a, date_diff_table, date_diff_table1, plot, plot1, plot2, x)




## code ------

gp%>%
  skim(code)
  # 4603 missing, 0.05% of total


### compute counts per cluster category -----------------
x<- gp%>%
  select(study_number, code, date)%>%
  mutate(year=str_sub(date, 1, 4), .keep="unused")%>%
  left_join(gp_cluster_lookup, by = c("code" = "ConceptId"))%>%
  select(study_number, code, year, Cluster_Desc, Cluster_Category)%>%
  group_by(year, Cluster_Category)%>%
  summarise(entries=n(),
            participants=n_distinct(study_number),
            clusters=n_distinct(Cluster_Desc))

### plots of counts per cluster category along time (will use just entries in report) -------------
x%>%
  mutate(year=as.numeric(year))%>%
  pivot_longer(cols=c(-year, -Cluster_Category), names_to="key", values_to="value")%>%
  filter(key=="entries")%>%
  ggplot(aes(year, value, color=Cluster_Category))+
  geom_point()+
  geom_line()+
  theme_gray(base_size=20)+
  # scale_y_log10()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                             legend.position = "none")+
  guides(colour = guide_legend(nrow = 4))+
  scale_x_continuous(breaks=seq(1860,2020, by=20))+
  geom_label_repel(data = x%>%                                                                      select(year, Cluster_Category, entries)%>% 
                  mutate(year=as.numeric(year))%>%
                  group_by(Cluster_Category)%>%
                  filter(entries== max(entries)), 
                  aes(x=year, y=entries, 
                  label=paste(Cluster_Category,",", " ", year, ",", " ", entries, " ", "entries", sep=""), 
                  fill=Cluster_Category, 
                  color=NULL,
                  segment.linetype= "dashed", 
                  segment.inflect=T),
                  nudge_y = 600000, 
                  nudge_x=-100, 
                  show.legend = F, 
                  max.overlaps = 25, 
                  direction="y", 
                  force_pull = 0,
                  size=6)+
  labs(title="Timeseries of number of entries per cluster categories",
       caption = "Labels depict year with maximum number of entries per category")

ggsave("Outputs/Transfer/Figures/cluster_categories_entries_timeseries.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina",
       limitsize = F
       )


### build table with counts of different things per cluster category ----------------
cluster_category_table<-
  gp%>%
  select(study_number, code, date)%>%
  mutate(year=str_sub(date, 1, 4), .keep="unused")%>%
  left_join(gp_cluster_lookup, by = c("code" = "ConceptId"))%>%
  select(study_number, code, year, Cluster_Desc, Cluster_Category)%>%
  group_by(Cluster_Category)%>%
  summarise(entries=n(),
            entries_percent=entries/gp_rows,
            participants=n_distinct(study_number),
            participants_percent=participants/gp_participants,
            unique_codes=n_distinct(code),
            clusters=n_distinct(Cluster_Desc),
            codes_per_cluster=unique_codes/clusters,
            avg_per_participant=entries/participants)
  
### edit table
cluster_category_table_1<-
  cluster_category_table%>%
  mutate(entries_percent=round(entries_percent*100, 0))%>%
  mutate(codes_per_cluster=round(codes_per_cluster, 0))%>%
  mutate(avg_per_participant=round(avg_per_participant, 0))%>%
  mutate(participants_percent=round(participants_percent*100, 0))%>%
  rename('Cluster category' = Cluster_Category, 
         Entries=entries,
         'Entries (proportion of total)' = entries_percent,
         Participants=participants,
         'Participants (proportion of total)' = participants_percent,
         'Average number of entries per participant' = avg_per_participant,
         'Individual clusters'=clusters,
         'Unique codes' = unique_codes,
         'Average number of codes per cluster'=codes_per_cluster
         )

### save table
write_csv(cluster_category_table_1, "Outputs/Transfer/Tables/Cluster_category_counts_table.csv")

# housekeeping
rm(x,cluster_category_table, cluster_category_table_1)






### investigate NA cluster codes -----

#### matching codes to the gp cluster lookup -----

a<- gp%>%
  select(study_number, code, date)%>%
  left_join(gp_cluster_lookup, by=c("code" = "ConceptId"))%>%
  select(study_number, code, ConceptId_Description, date, Cluster_Desc, Cluster_Category)

a%>%
  filter(is.na(ConceptId_Description))%>%
  nrow()
# 410848 entries that did not match

a%>%
  filter(is.na(ConceptId_Description))%>%
  distinct(code)%>%
  nrow()
# 705 codes 

a%>%
  filter(is.na(ConceptId_Description))%>%
  distinct(study_number)%>% # select distinct participants
  nrow() # 23591 distinct participants

a%>%
  filter(is.na(ConceptId_Description))%>%
  distinct(code)%>%
  .[[1]]->x # saving individual codes to a list


# are these actually snomed codes? 

#### check snomed concept list --------

a1<-gp%>%
  select(study_number, code, date)%>%
  left_join(snomed_concepts, by=c("code" = "id"), keep=T)

a1%>%
  filter(is.na(id))%>%
  View()

a1%>%
  filter(is.na(id))%>%
  nrow()
# 387632 rows

a1%>%
  filter(is.na(id))%>%
  distinct(code)%>%
  nrow()
# 700 codes; so 5 codes are actually snomed codes; can we determine their meaning? 

#### check SNOMED FSN list ------
FSN%>%
  filter(conceptId %in% x)%>%
  distinct(conceptId, .keep_all=T)%>%
  nrow()
# we can identify 5 codes in the FSN list (agreeing with numbers above)

# they are:
FSN%>%
  filter(conceptId %in% x)%>%
  distinct(conceptId, .keep_all=T)%>%
  select(conceptId, term)%>%
  View() # non-covid vaccinations
  
  
  
FSN%>%
  filter(conceptId %in% x)%>%
  distinct(conceptId, .keep_all=T)%>%
  select(conceptId, term)%>%
  distinct(conceptId)%>%
  .[[1]]->x1  # converting these 5 to a list



# what are the remaining? they are not in the complete snomed concept list, but we can look in the full list of snomed descriptions for confirmation

#### check snomed descriptions -------

snomed_descriptions%>%
  filter(conceptId %in% setdiff(x, x1))%>%
  nrow() # 0 entries

# now matching against description Ids (version concept Ids) for double assurance

snomed_descriptions%>%
  filter(id %in% setdiff(x, x1))%>%
  nrow() # 0 entries again

# so we can be certain that these codes are NOT snomed codes

gp%>%
  filter(code%in%setdiff(x, x1))%>%
  nrow() # 387632 entries

gp%>%
  filter(code%in%setdiff(x, x1))%>%
  distinct(study_number)%>%
  nrow() # 21823 participants

gp%>%
  filter(code%in%setdiff(x, x1))%>%
  filter(is.na(episode_prescription) & is.na(value2_prescription) & is.na(value1_prescription))%>%
  nrow() # 8712 of those entries are probably not prescriptions; so majority should be prescriptions














#### non-matched codes along time ---------
a%>%
  select(study_number, code, date)%>% # some variables
  mutate(year=str_sub(date, 1,4))%>% # generate year from date
  group_by(year)%>% # group entries by year
  summarise(n=n())%>% # calculate counts of entries per year
  ggplot(aes(x=year, y=n))+ # specify variables for plot
  geom_point()+ # scatter plot
  geom_line(aes(group=NA))+ # line chart (and need to specify absence of groups)
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust=1))+ # adjust x axis text angle and position 
  labs(y="entries") # specify y axis label

ggsave("Outputs/Transfer/Figures/unmatched_codes_timeseries.png",
       last_plot()) # saving plot


#### unmatched codes across GP suppliers --------

a%>%
  select(study_number, code, date, gp_system_supplier)%>% #some variables
  mutate(gp_system_supplier=as.factor(gp_system_supplier))%>% # specify gp system supplier as a factor
  group_by(gp_system_supplier)%>% # create groups
  summarise(n=n())%>% # calculate counts per group
  ggplot(aes(x=gp_system_supplier, y=n))+ # pass variables to gg object
  geom_bar(stat = 'identity')+ # create bar chart and specify statistical transformation for length (none)
  geom_text(stat='identity', # specify statistical transformation (none)
            aes(label=round(n, 1)), # specify values for text labels (the counts)
            vjust=-0.5, size=4, hjust=0.5)+ # adjust position and size
  labs(y="entries",
       x="GP system supplier") # create x and y axis labels

ggsave("Outputs/Transfer/Figures/unmatched_codes_gp_supplier.png",
       last_plot()
)

#### map of unmatched codes---------


# load lsoa to LAD map (as above)
lsoa_lad<-read_csv("Tools/UK maps/OA-LSOA-MSOA-LAD-lookup/PCD_OA_LSOA_MSOA_LAD_MAY21_UK_LU.csv", col_types = cols(pcd7 = col_skip(), pcd8 = col_skip(), pcds = col_skip(), dointr = col_skip(),doterm = col_skip(), usertype = col_skip(), oa11cd = col_skip(), msoa11cd = col_skip(),msoa11nm = col_skip(), ladnmw = col_skip()))

# select distinct LSOAs (map included lower levels such as OA so there were multiple entries for each LSOA)
lsoa_lad%<>%
  distinct(lsoa11cd, .keep_all=T) 

# load LAD to county maps
lad_county <- read_csv("Tools/UK maps/LAD-county-lookup/Local_Authority_District_to_County_(April_2021)_Lookup_in_England.csv", col_types = cols(FID = col_skip()))

# merge lsoa-lad-counties map
counties<-left_join(lsoa_lad, lad_county, by=c("ladcd"="LAD21CD"))

# select only variables of interest
counties%<>%
  select(lsoa=lsoa11cd,
         lsoa11nm,
         ladcd,
         ladnm,
         county_code=CTY21CD,
         county_name=CTY21NM,
  )

# import maps of all UK counties and unitary authorities (May 2021) (as above)
filename<-"K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/UK counties shapefile/UK/Counties_and_Unitary_Authorities_(May_2021)_UK_BGC"

# load map to shape file
poly_counties<-raster::shapefile(filename)

# keep LAD code for those LADs that do not belong to a county, so we can do:
counties%<>%
  mutate(county_ua=if_else(is.na(county_code), ladcd, county_code), .keep=c("all"))

# and we need to exclude the ones in the old counties (starting with E11 or E13)
counties%<>%
  mutate(county_ua=if_else(grepl('E11|E13', county_code), ladcd, county_ua), .keep=c("all"))

# calculate counts of unmatched per county
county_counts<-
  a%>%
  select(study_number, code, date,lsoa)%>%
  left_join(counties, by="lsoa")%>%
  group_by(county_ua)%>%
  summarise(entries=n(),
            participants=n_distinct(study_number))

# align county code field name
county_counts%<>%
  rename(CTYUA21CD=county_ua)

# merge counts from recovery with map file
poly_counties<-merge(poly_counties, county_counts, by="CTYUA21CD")

# convert map file to a tidy format for ggplot
poly_counties_df<-broom::tidy(poly_counties)

# assign unique row names to original file to allow data retrieval (not transferred by the tidy command)
poly_counties$id<-row.names(poly_counties)

# rejoin the data for each area (lost in the tidy transformation)
# and reduce amount of data that gets dragged along to aid processing
poly_counties_df<-left_join(poly_counties_df, poly_counties@data, by="id")%>%
  select(long, lat, order, hole, piece, group, id, entries, participants)

# plot the map (for participant counts)
map1<-poly_counties_df%>%
  rename(Participants=participants)%>%
  ggplot(aes(x=long, y=lat, group=group, fill=Participants))+
  geom_polygon(colour="black", size=0.1)+
  theme_void()

# plot the map (for entry counts)
map2<-poly_counties_df%>%
  rename(Entries=entries)%>%
  ggplot(aes(x=long, y=lat, group=group, fill=Entries))+
  geom_polygon(colour="black", size=0.1)+
  theme_void()

# join both maps
map=map1+map2

# print plot
print(map)

# save map
ggsave("Outputs/Transfer/Figures/unmatched_codes_map.png",
       map)


rm(x, x1, y, a, a1, counties, county_counts, filename, lad_county, lsoa_lad, map, map1, map2, poly_counties, poly_counties_df, withdrew, x1, y)



### investigate the "To be confirmed" cluster --------
x2<-gp%>%
  left_join(gp_cluster_lookup, by=c("code"= "ConceptId"))%>%
  filter(Cluster_Category=="To be confirmed")%>%
  select(study_number, date, code, Cluster_Desc, Cluster_Category)

View(x2)

x2%>%
  nrow() # 782 

x2%>%
  distinct(code)%>%
  nrow() # 8

x2%>%
  distinct(code)%>%
  left_join(FSN, by=c("code"="conceptId"))%>%
  View()

x2%>%
  distinct(study_number)%>%
  nrow() # 567

x2%>%
  select(date)%>%
  filter(date==min(date)|date==max(date))

rm(x2, FSN)

gc()




## sensitive_code -----

### general investigation -----
gp%>%
  skim(sensitive_code)

table(gp$sensitive_code)

### plot of coding ------

gp%>%
  select(sensitive_code)%>%
  mutate(sensitive_code=as.factor(sensitive_code))%>%
  group_by(sensitive_code)%>%
  summarise(n=n(),
            prop=n/gp_rows)%>%
  ggplot(aes(sensitive_code, n, label=paste(n,", ", scales::percent(prop), sep="")))+
  geom_bar(stat="identity", width=0.5)+
  labs(title="Coding of the sensitive_code field")+
  geom_text(vjust=-0.5,
            size=6)+
  theme_gray(base_size = 25)+
  ylab("Entries")

ggsave("Outputs/Transfer/Figures/sensitive_code.png",
       last_plot(),
       height = 8,
       width = 10,
       dpi="retina")

## episode_condition -----

### general investigation -------

gp%>%
  skim(episode_condition)

# rename labels

gp$episode_condition%<>%as.factor()

levels(gp$episode_condition)<-list('Cause of death' = "D",
                                First = "F",
                                New = "N",
                                Other = "O")


### plot of coding of this field ------
gp%>%
  select(episode_condition)%>%
  group_by(episode_condition)%>%
  summarise(n=n(),
            prop=n/gp_rows)%>%
  ggplot(aes(episode_condition, n, label=paste(n,", ", scales::percent(prop), sep="")))+
  geom_bar(stat="identity")+
  labs(title="Coding of the episode_condition field")+
  geom_text(vjust=-0.5,
            size=6)+
  theme_gray(base_size = 25)+
  ylab("Entries")

ggsave("Outputs/Transfer/Figures/episode_condition_coding.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")


### plot of coding along time -----
x<-gp%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Category!="Medications")%>%
  select(episode_condition, date)%>%
  mutate(year=str_sub(date, 1,4))%>%
  group_by(episode_condition, year)%>%
  summarise(n=n())

x%>%
  mutate(year=as.numeric(year))%>%
  ggplot(aes(year, n, group=episode_condition, color=episode_condition))+
  geom_point()+
  geom_line()+
  labs(title="Coding of the episode_condition field",
       subtitle="(excluding medication clusters)")+
  theme_gray(base_size = 20)+
  ylab("Entries")+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank())+
  scale_x_continuous(breaks=seq(1860,2020, by=20))+
  facet_grid(cols=vars(episode_condition))

ggsave("Outputs/Transfer/Figures/episode_condition_timeseries.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")

### plot of coding per cluster category ------
x<-gp%>%
  select(episode_condition, code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(episode_condition, Cluster_Category)%>%
  group_by(Cluster_Category, episode_condition)%>%
  summarise(n=n())%>%
  group_by(Cluster_Category)%>%
  mutate(prop=round(n/sum(n)*100, 0))%>%
  ungroup()

x%>%
  ggplot(aes(episode_condition, n, group=Cluster_Category, fill=episode_condition))+
  geom_bar(stat="identity")+
  facet_wrap(~Cluster_Category, scales="free_y",
             labeller = label_wrap_gen(40))+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "none",
        axis.title.x=element_blank())+
  geom_text(aes(label=paste(prop, "%", sep="")), 
            size=5)+
  labs(title="Coding of the episode_condition field (per cluster category)")+
  ylab("Entries")

ggsave("Outputs/Transfer/Figures/episode_condition_per_cluster_category.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")


### plot per gp supplier ---------

gp%>%
  select(study_number, code, episode_condition, gp_system_supplier, date)%>%
  count(gp_system_supplier, episode_condition)%>%
  group_by(gp_system_supplier)%>%
  mutate(proportion=round(n/sum(n)*100,1))->t

t%>%
  ggplot(aes(episode_condition, n, fill=episode_condition))+
  geom_bar(stat='identity')+
  theme_gray(base_size = 20)+
  ylab("Entries")+
  geom_text(stat='identity', # specify statistical transformation (none)
            aes(label=paste0(n,",\n", proportion,"%")), # specify values for text labels (the counts)
            # vjust=-0.5, 
            size=5, 
            hjust=0.5)+
  facet_grid(cols=vars(gp_system_supplier))+
  theme(axis.text.x = element_text(angle = 30,hjust=1),
        legend.position = "none",
        axis.title.y = element_text(size=15))+
  labs(title="Coding of the episode_condition field, per GP system supplier")

ggsave("Outputs/Transfer/Figures/episode_condition_gp_supplier.png",
       last_plot(),
       height = 8,
       width=20,
       dpi="retina")




### investigate some individual cases for each type of entry -----
gp%>%
  filter(episode_condition=="New")%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(code, ConceptId_Description, date, Cluster_Category)%>%
  slice_sample(n=10)%>%
  View()
  
gp%>%
  filter(episode_condition=="First")%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(code, ConceptId_Description, date, Cluster_Category)%>%
  slice_sample(n=10)%>%
  View()

gp%>%
  filter(episode_condition=="Other")%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(code, ConceptId_Description, date, Cluster_Category)%>%
  slice_sample(n=10)%>%
  View()

gp%>%
  filter(episode_condition=="Cause of death")%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(code, ConceptId_Description, date, Cluster_Category)%>%
  slice_sample(n=10)%>%
  View()



# housekeeping
rm(x)







## episode_prescription ------

### general investigation ----
gp%>%
  skim(episode_prescription)

# look at some rows
gp%>%
  select(episode_prescription)%>%
  head()

# distinct entries
gp%>%
  distinct(episode_prescription)

# correctly assign as factor
gp%<>%
  mutate(episode_prescription=as.factor(episode_prescription))

# rename labels
levels(gp$episode_prescription)<-list('Acute (one-off)' = "A",
                                   'Issue of repeat' = "I")

### plot of coding frequency --------
gp%>%
  select(episode_prescription)%>%
  group_by(episode_prescription)%>%
  summarise(n=n(),
            prop=n/gp_rows)%>%
  ggplot(aes(episode_prescription, n, label=paste(n,", ", scales::percent(prop), sep="")))+
  geom_bar(stat="identity",
           width=0.5)+
  labs(title="Coding of the episode_prescription field")+
  geom_text(vjust=-0.5,
            size=6)+
  theme_gray(base_size = 25)+
  ylab("Entries")

ggsave("Outputs/Transfer/Figures/episode_prescription_coding.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")


### plot of coding per cluster category ------
x<-gp%>%
  select(episode_prescription, code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(episode_prescription, Cluster_Category)%>%
  group_by(Cluster_Category, episode_prescription)%>%
  summarise(n=n())%>%
  group_by(Cluster_Category)%>%
  mutate(prop=round(n/sum(n)*100, 0))%>%
  ungroup()

x%>%
  ggplot(aes(episode_prescription, n, group=Cluster_Category, fill=episode_prescription))+
  geom_bar(stat="identity")+
  facet_wrap(~Cluster_Category, scales="free_y",
             labeller = label_wrap_gen(35))+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "none",
        axis.title.x=element_blank(),
        strip.text = element_text(size=15))+
  geom_text(aes(label=paste(prop, "%", sep="")), 
            size=5)+
  labs(title="Coding of the episode_prescription field (per cluster category)")+
  ylab("Entries")

ggsave("Outputs/Transfer/Figures/episode_prescription_per_cluster_category.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")

### plot of coding along time (overall)-----
x<-gp%>%
  select(episode_prescription, date)%>%
  mutate(year=str_sub(date, 1,4))%>%
  group_by(episode_prescription, year)%>%
  summarise(n=n())

x%>%
  mutate(year=as.numeric(year))%>%
  ggplot(aes(year, n, group=episode_prescription, color=episode_prescription))+
  geom_point()+
  geom_line()+
  labs(title="Coding of the episode_prescription field")+
  theme_gray(base_size = 20)+
  ylab("Entries")+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
          legend.position = "bottom")+
  scale_x_continuous(breaks=seq(1860,2020, by=20))+
  facet_wrap(~episode_prescription)
  
ggsave("Outputs/Transfer/Figures/episode_prescription_timeseries.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")

### plot of coding along time (for medication clusters only) -----
x<-gp%>%
  select(episode_prescription, date, code)%>%
  mutate(year=str_sub(date, 1,4))%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Category=="Medications")%>%
  select(episode_prescription, year)%>%
  group_by(episode_prescription, year)%>%
  summarise(n=n())

x%>%
  mutate(year=as.numeric(year))%>%
  ggplot(aes(year, n, group=episode_prescription, color=episode_prescription))+
  geom_point()+
  geom_line()+
  labs(title="Coding of the episode_prescription field (for medication clusters)")+
  theme_gray(base_size = 20)+
  ylab("Entries")+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "bottom")+
  scale_x_continuous(breaks=seq(1860,2020, by=20))+
  facet_wrap(~episode_prescription)

ggsave("Outputs/Transfer/Figures/episode_prescription_timeseries_medication_clusters.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")

### plot of coding per medication cluster -----
x<-gp%>%
  select(episode_prescription, code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Category=="Medications")%>%
  select(Cluster_Desc, episode_prescription)%>%
  group_by(episode_prescription, Cluster_Desc)%>%
  summarise(n=n())%>%
  group_by(Cluster_Desc)%>%
  mutate(prop=round(n/sum(n)*100, 0))%>%
  ungroup()%>%
  arrange(Cluster_Desc)


x$Cluster_Desc<-as.character(x$Cluster_Desc)


x%>%
  ggplot(aes(episode_prescription, n, group=Cluster_Desc, fill=episode_prescription))+
  geom_bar(stat="identity")+
  facet_wrap(~Cluster_Desc,
             labeller = label_wrap_gen(35),
             scales="free_y")+
  theme_gray(base_size=16)+
  theme(axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom")+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  geom_text(aes(label=paste(prop, "%", sep="")), 
            size=5,
            vjust=-0.1)+
  labs(title="Coding of the episode_prescription field (per medication cluster)",
       subtitle="(number of entries)")+
  ylab("Entries")

ggsave("Outputs/Transfer/Figures/episode_prescription_per_medication_cluster.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")

### plot of coding for other clusters of interest -----

# reload gp_cluster_lookup because previous change to medication cluster names caused some unwanted changes in other cluster names

gp_cluster_lookup <-read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/GDPPR_Cluster_refset_1000230_20210914.xlsx")


# specify a new function to mutate at certain rows only (to allow subdividing Cluster_Desc for NAs)
mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}

x<-gp%>%
  select(episode_prescription, code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Category=="To be confirmed"|
           Cluster_Category=="Vaccinations and immunisations"|
           is.na(Cluster_Category))%>%
  left_join(FSN, by= c("code"="conceptId"))%>%
  select(code, term, episode_prescription, Cluster_Desc)%>%
  mutate(unknown_code=as.factor(if_else(is.na(term),1,0)))%>% # flag unknown codes
  mutate_when(
    (is.na(Cluster_Desc) & unknown_code=="1"), list(Cluster_Desc="NA_unknown_code"), 
    (is.na(Cluster_Desc) & unknown_code=="0"), list(Cluster_Desc="NA_known_code"))%>%
  group_by(episode_prescription, Cluster_Desc)%>%
  summarise(n=n())%>%
  group_by(Cluster_Desc)%>%
  mutate(prop=round(n/sum(n)*100, 1))%>%
  ungroup()

x%<>%
  left_join(gp_cluster_lookup, by=c("Cluster_Desc"))%>%
  select(Cluster_Desc, Cluster_Category, episode_prescription, n, prop)%>%
  distinct(Cluster_Desc, episode_prescription, .keep_all = T)


x$Cluster_Desc<-as.character(x$Cluster_Desc)

#### plot for vaccinations and immunisations -----
x%>%
  filter(Cluster_Category=="Vaccinations and immunisations")%>%
  ggplot(aes(episode_prescription, n, group=episode_prescription, fill=episode_prescription))+
  geom_bar(stat="identity")+
  facet_wrap(~Cluster_Desc,
             labeller = label_wrap_gen(35),
             scales="free_y")+
  theme_gray(base_size=16)+
  theme(axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size=10))+
  scale_y_continuous(expand=expansion(c(0,0.5)))+
  geom_text(aes(label=paste(prop, "%", sep="")), 
            size=5,
            vjust=0.1)+
  labs(title="Coding of the episode_prescription field (vaccinations and immunisations)",
       )+
  ylab("Entries")



ggsave("Outputs/Transfer/Figures/episode_prescription_vaccines.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")

#### plot for others  -----

x%>%
  filter(Cluster_Category!="Vaccinations and immunisations"|
           is.na(Cluster_Category))%>%
  ggplot(aes(episode_prescription, n, group=episode_prescription, fill=episode_prescription))+
  geom_bar(stat="identity")+
  facet_wrap(~Cluster_Desc,
             labeller = label_wrap_gen(50),
             scales="free_y")+
  theme_gray(base_size=20)+
  theme(axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size=15))+
  scale_y_continuous(expand=expansion(c(0,0.5)))+
  geom_text(aes(label=paste(prop, "%", sep="")), 
            size=5,
            vjust=0.1)+
  labs(title="Coding of the episode_prescription field (other cluster categories)",
  )+
  ylab("Entries")



ggsave("Outputs/Transfer/Figures/episode_prescription_other_clusters.png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")






### plot of mean entries per participant for each cluster -------

x<- gp%>%
  select(study_number, date, code, episode_prescription)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Category=="Medications")%>%
  select(study_number, date, code, episode_prescription, Cluster_Desc)


x1<- x%>%
  group_by(study_number, Cluster_Desc, episode_prescription)%>%
  summarise(n=n())%>%
  group_by(Cluster_Desc, episode_prescription)%>%
  summarise(mean=mean(n))

x1%>%
  ggplot(aes(episode_prescription, mean, group=Cluster_Desc, fill=episode_prescription))+
  geom_bar(stat="identity")+
  facet_wrap(~Cluster_Desc,
             labeller = label_wrap_gen(35),
             scales="free_y")+
  theme_gray(base_size=16)+
  theme(axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom")+
  geom_text(aes(label=round(mean,0)), 
            size=5)+
  scale_y_continuous(expand=expansion(c(0,0.1)))+
  labs(title="Coding of the episode_prescription field (per medication cluster)",
       subtitle="(mean entries per participant)")+
  ylab("Mean number of entries per participant")

ggsave("Outputs/Transfer/Figures/episode_prescription_per_medication_cluster(per_participant).png",
       last_plot(),
       height = 8,
       width = 20,
       dpi="retina")

### plot of mean distinct entries and proportion of participants with distrinct entries per cluster ----

x<-gp%>%
  select(study_number, code, episode_prescription, gp_system_supplier, date)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, episode_prescription, date, gp_system_supplier, Cluster_Category, Cluster_Desc)%>%
  filter(Cluster_Category == "Medications")%>%
  filter(is.na(episode_prescription)| episode_prescription!="Acute (one-off)")%>%
  group_by(study_number, Cluster_Desc, episode_prescription)%>%
  select(-date,-Cluster_Category, -code)
  

x%>%
  group_by(study_number, Cluster_Desc)%>%
  summarise(n=n_distinct(episode_prescription))%>%
  group_by(Cluster_Desc)%>%
  summarise(mean_distinct_entries_per_participant=mean(n),
            participants_with_distinct_entry=n_distinct(study_number[n>1]),
            participants_total = sum(n),
            proportion=round((participants_with_distinct_entry/participants_total*100),0))->x1



p1<-x1%>%
  mutate(Cluster_Desc=as.character(Cluster_Desc))%>%
  ggplot(aes(Cluster_Desc, mean_distinct_entries_per_participant, fill=Cluster_Desc))+
  geom_bar(stat="identity")+
  theme_gray(base_size = 20)+
  ylab("Mean number of distinct entries \n per participant")+
  geom_text(stat='identity', # specify statistical transformation (none)
            aes(label=round(mean_distinct_entries_per_participant, 2)), # specify values for text labels (the counts)
            vjust=-0.5, 
            size=5, 
            hjust=0.5)+
  ylim(c(0,2))+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size=15),
        legend.position = "none")+
  labs(title="Distinct entries for episode_prescription per medication cluster",
       subtitle="(mean per participant, and proportion of participants with distinct entries)")

p2<-x1%>%
  mutate(Cluster_Desc=as.character(Cluster_Desc))%>%
  ggplot(aes(Cluster_Desc, proportion, fill=Cluster_Desc))+
  geom_bar(stat="identity")+
  theme_gray(base_size = 20)+
  xlab("Cluster")+
  ylab("Proportion of participants \n with distinct entries")+
  geom_text(stat='identity', # specify statistical transformation (none)
            aes(label=paste0(proportion,"%")), # specify values for text labels (the counts)
            vjust=-0.5, 
            size=5, 
            hjust=0.5)+
  ylim(c(0,100))+ 
  theme(axis.text.x = element_text(angle = 30,hjust=1),
                        legend.position = "none",
        axis.title.y = element_text(size=15))
  
plot<-p1/p2

plot

ggsave('Outputs/Transfer/Figures/distinct_entries_episode_prescription.png', 
       last_plot(),
       height=10,
       width=20,
       dpi="retina")

rm(x,x1,x2, p1,p2, plot)

### example row for a participant with two distinct entries in episode_prescription for the same medication cluster ----

gp%>%
  filter(study_number=="1392975")%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Desc=="Metformin")%>%
  select(study_number, code, ConceptId_Description, episode_prescription, value1_prescription, value2_prescription, date, record_date, gp_system_supplier, lsoa)%>%
  rename('Study number' = study_number,
         Code=code,
         Description = ConceptId_Description,
         'GP system supplier' = gp_system_supplier,
          LSOA=lsoa)->t

t%<>%
  rename(Date = date,
         'Record date' = record_date)
View(t)

write_csv(t, "Outputs/Transfer/Tables/sample_episode_prescription.csv")

rm(t)

### plot per gp provider ------
gp%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, episode_prescription, gp_system_supplier, date, Cluster_Category)%>%
  filter(Cluster_Category == "Medications")%>%
  count(gp_system_supplier, episode_prescription)%>%
  group_by(gp_system_supplier)%>%
  mutate(proportion=round(n/sum(n)*100,1))->t

t%>%
  ggplot(aes(episode_prescription, n, fill=episode_prescription))+
  geom_bar(stat='identity')+
  theme_gray(base_size = 20)+
  ylab("Entries")+
  geom_text(stat='identity', # specify statistical transformation (none)
            aes(label=paste0(n,", ", proportion,"%")), # specify values for text labels (the counts)
            vjust=-0.5, 
            size=5, 
            hjust=0.5)+
  facet_grid(cols=vars(gp_system_supplier))+
  theme(axis.text.x = element_text(angle = 30,hjust=1),
        legend.position = "none",
        axis.title.y = element_text(size=15))+
  labs(title="Coding of the episode_prescription field, per GP system supplier")

ggsave("Outputs/Transfer/Figures/episode_prescription_gp_supplier.png",
       last_plot(),
       height = 8,
       width=20,
       dpi="retina")

rm(x,t,x1)

### investigate time relationship between first and issue of repeat/NAs -----
#### (STILL IN PROGRESS)-----

x<-gp%>%
  select(study_number, 
         code, 
         date, 
         episode_prescription)%>%
  left_join(gp_cluster_lookup, 
            by=c("code"="ConceptId"))%>%
  select(study_number, 
         code, 
         ConceptId_Description, 
         date,
         episode_prescription, 
         Cluster_Desc, 
         Cluster_Category)%>%
  filter(Cluster_Category=="Medications"|
         Cluster_Category=="To be confirmed"|
         Cluster_Category=="Vaccinations and immunisations"|
         is.na(Cluster_Category))%>%
  distinct(study_number, code, date, .keep_all=T)%>%
  arrange(Cluster_Category, Cluster_Desc, study_number, code, episode_prescription, date)


# x1<-x%>%
#   group_by(study_number, code)%>%
#   mutate(date_diff=date-lag(date, 1L))
  # computer not able to process this, will leave for now

rm(x, x1)


## value1_condition and value2_condition--------

### general inspection -------

a<-gp%>%
  skim(value1_condition, value2_condition)
# missing: 7477375 for value 1 (complete: 9908044 - 7477375 = 2430669; 24.5%)
# missing: 9811917 for value 2 (complete: 9908044 - 9811917 = 96127; 0.97%)

a%>%
  as.data.frame()%>%
  rename(nmissing =n_missing,
         complete = complete_rate)%>%
  mutate('Complete entries (total)' = (nrow(gp)-nmissing),
         'Complete entries (proportion)' = paste0(round(complete*100, 2),"%"),
         numeric.sd=round(numeric.sd,2),
         numeric.mean = round(numeric.mean, 2),
         .keep="unused")%>%
  select(Field=skim_variable,
         'Complete entries (total)',
         'Complete entries (proportion)',
         Mean= numeric.mean,
         "Standard deviation" = numeric.sd,
         Median = numeric.p50,
         Q1 = numeric.p25,
         Q3 = numeric.p75,
         Minimum = numeric.p0,
         Maximum = numeric.p100) ->a1
      
write_csv(a1, "Outputs/Transfer/Tables/value1_condition_and_value2_condition_table.csv")
                             

x<-gp%>%
  filter(!is.na(value1_condition)|
         !is.na(value2_condition))

rm(a, a1)

gp%>%
  mutate(value1_condition_flag = if_else(!is.na(value1_condition), 1, 0),
         value2_condition_flag = if_else(!is.na(value2_condition), 1, 0))%>%
  count(value1_condition_flag, value2_condition_flag)

### histograms -------

p1<- x%>%
  ggplot(aes(value1_condition))+
  geom_histogram()+
  labs(title="Histograms for the value1_condition and value2_condition fields",
       subtitle = "Value1_condition")+
  theme_gray(base_size = 20)+
  theme(axis.title.x = element_blank())
  
p2<- x%>%
  ggplot(aes(value2_condition))+
  geom_histogram()+
  labs(subtitle = "Value2_condition",
       caption = "Note that the x axis differs in both histograms")+
  theme_gray(base_size = 20)+
  theme(axis.title.x = element_blank())

p=p1/p2

ggsave('Outputs/Transfer/Figures/value1_condition_and_valu2_condition_histograms.png', 
       last_plot(),
       height=10,
       width=20,
       dpi="retina")

rm(p,p1,p2)

### coding along time and per supplier -------

x%<>%
  mutate(value1_condition_flag = if_else(!is.na(value1_condition), 1, 0),
         value2_condition_flag = if_else(!is.na(value2_condition), 1, 0))

x%>%
  select(date, gp_system_supplier, value1_condition_flag, value2_condition_flag)%>%
  mutate(year=str_sub(date, 1, 4))%>%
  group_by(year, gp_system_supplier)%>%
  summarise(n_value1 = sum(value1_condition_flag),
            n_value2 = sum(value2_condition_flag))%>%
  rename(value1_condition=n_value1,
         value2_condition=n_value2)%>%
  mutate(year=as.numeric(year))%>%
  pivot_longer(-c(year,gp_system_supplier), names_to = "key", values_to = "value")%>%
  ggplot(aes(year, value, color=key, groups=key))+
  geom_line()+
  geom_point()+
  facet_grid(cols=vars(gp_system_supplier),
             rows=vars(key))+
  theme_gray(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")+
  # oxpop_grey_panel+
  labs(title="Coding along time and per system supplier",
       subtitle="(presence of an entry, regardless of value)")+
  ylab("Number of entries")+
  xlab("Year")


ggsave('Outputs/Transfer/Figures/value1_condition_and_value2_condition_timeseries_suppliers.png', 
       last_plot(),
       height=10,
       width=20,
       dpi="retina")


### codes associated -----

#### value1_condition -------
x1<-x%>%
  filter(!is.na(value1_condition))
  
x1%>%
  select(code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(code, Cluster_Desc, Cluster_Category)%>%
  add_count(code, Cluster_Desc)%>%
  distinct(code, Cluster_Desc, .keep_all=T)->t1

t1%>%
  distinct(code)%>%
  nrow() # 357 codes

t1%>%
  distinct(Cluster_Desc)%>%
  nrow() # 112 clusters

t1%>%
  group_by(Cluster_Desc)%>%
  mutate(n=sum(n))%>%
  select(-code)%>%
  distinct(Cluster_Desc, .keep_all = T)%>%
  select('Cluster category' = Cluster_Category, 
         Cluster = Cluster_Desc,
         'Entries per cluster' = n)%>%
  arrange('Cluster category', desc('Entries per cluster'))%>%
  write_csv("Outputs/Transfer/Tables/value1_condition_clusters.csv")

t<-read_csv("Outputs/Transfer/Tables/value1_condition_clusters_grouped_manually.csv")

t%>%
  group_by(`General category`)%>%
  summarise(n=sum(`Entries per cluster`))%>%
  ggplot(aes(`General category`, n))+
  geom_bar(stat='identity')+
  geom_text(stat='identity', # specify statistical transformation (none)
            aes(label=n), # specify values for text labels (the counts)
            vjust=-0.5, 
            size=5, 
            hjust=0.5,
            color="black")+
  theme_gray(base_size=20)+
  labs(title="Codes associated with the value1_condition field",
       subtitle = "aggregated by bespoke general categories (i.e. not cluster categories)",
       y="Entries")+
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom",
        axis.title.x = element_blank()
        )

ggsave('Outputs/Transfer/Figures/value1_condition_categories.png', 
       last_plot(),
       height=10,
       width=20,
       dpi="retina")


# select clusters with value1_condition where the meaning of this is not immediately understood
t%>%
  filter(`General category`%in%c("COVID",
                                 "Diabetes",
                                 "Ethnicity",
                                 "Neurology assessments",
                                 "Other diagnoses",
                                 "Other physical assessments",
                                 "Other tests",
                                 "Pregnancy",
                                 "Respiratory assessments",
                                 "Risk scores",
                                 "Vaccinations"))%>%
  select(Cluster)%>%
  .[[1]]->l


# filter entries with value1_condition by this list of clusters
x1%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  filter(Cluster_Desc %in% l)%>%
  select(study_number, code, ConceptId_Description, date, value1_condition, Cluster_Desc)->t2

View(t2)

rm(t, t1,t2)

# investigate extreme values
x1%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, date, ConceptId_Description, value1_condition)%>%
  distinct(study_number, code, date, value1_condition, .keep_all=T)%>%
  arrange(desc(value1_condition))->t

View(t)



#### value2_condition -------

x2<-x%>%
  filter(!is.na(value2_condition))%>%
  distinct(code)

nrow(x2) # 19


x2%>%
    select(code)%>%
    left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
    select(code, ConceptId_Description, Cluster_Desc, Cluster_Category)%>%
    distinct(code, .keep_all=T)%>%
  arrange(Cluster_Desc, ConceptId_Description)%>%
  rename(Code=code,
         Term = ConceptId_Description,
         Cluster = Cluster_Desc,
         'Cluster category' = Cluster_Category)->t2

write_csv(t2, "Outputs/Transfer/Tables/value2_condition_codes.csv")    
rm(x2,t2)  
  
  
## value1_prescription and value2_prescription-------

### general inspection  ------

gp%>%
  skim(value1_prescription, value2_prescription)




## links -------






## Dispensing dataset -----------


colnames(meds_restricted)

skim(meds_restricted)












# Time series  -------------

## GP dataset ------------------------------
a<-gp%>%
  select(study_number, date, code)%>%
  mutate(year=substr(date,1,4), keep=c("unused"))%>%
  group_by(year)

t1<-a%>%
  summarise(participants_with_record=n_distinct(study_number))

t1<-a%>%
  summarise(entries_total=n())%>%
  add_column(participants_with_record = t1$participants_with_record)

t2<-t1%>%
  summarise(entries_per_participant=entries_total/participants_with_record)

t1<-cbind(t1,t2)

t2<-a%>%
  summarise(codes_unique=n_distinct(code))

t1<-cbind(t1,t2$codes_unique)

t1<-t1%>%
  rename(codes_unique='t2$codes_unique')

t3<-t1%>%
  summarise(codes_unique_per_participant=codes_unique/participants_with_record)

t1<-cbind(t1,t3)

# add cumulative count of participants with first record
t4<-gp%>%
  mutate(year=substr(date,1,4))%>%
  group_by(study_number)%>%
  mutate(date=as.Date(date))%>%
  filter(date==min(date))%>%
  group_by(year)%>%
  summarise(participants=n_distinct(study_number))%>%
  mutate(cumulative_participants=cumsum(participants))%>%
  select(-participants)

t<-t1%>%left_join(t4, by=c("year"))

rm(t1,t2,t3,t4)

# generate viz

p<-t%>%
  mutate(codes_unique_per_1000_participants=codes_unique_per_participant*1000, .keep=c("unused"))%>%
  rename('Unique codes' = codes_unique,
         'Unique codes (per 1000 participants)'=codes_unique_per_1000_participants,
         'Entries (per participant)'=entries_per_participant,
         'Participants with a record'=participants_with_record,
         'Cumulative participant count'=cumulative_participants,
         'Entries (total)'=entries_total)%>%
  pivot_longer(cols = c(-"year"),
               names_to = "key",
               values_to = "value")%>%
  filter(complete.cases(.))%>%
  ggplot(aes(year, value, color=key))+
  geom_point()+
  geom_line(aes(group=key))+
  theme_bw()+
  labs(y="number", title="")+
  scale_y_log10(breaks=c(10, 100, 1000, 10000,100000, 1000000, 10000000), labels=scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")+
  scale_x_discrete(guide=guide_axis(n.dodge=2))

ggsave("Outputs/Transfer/Figures/Counts along time (GP).png",
       width = 10,
       p)

rm(a,t,p)

## Dispensing dataset -------------------

b<-meds_restricted%>%
  select(study_number,paiddmd_code, processed_period)%>%
  group_by(processed_period)

t1<-b%>%
  summarise(participants_with_record=n_distinct(study_number),
            entries_total=n(),
            entries_per_participant=entries_total/participants_with_record,
            codes_unique=n_distinct(paiddmd_code),
            codes_unique_per_1000_participants=codes_unique/participants_with_record*1000)

t2<-b%>%
  select(study_number, processed_period)%>%
  group_by(study_number)%>%
  filter(processed_period==min(processed_period))%>%
  group_by(processed_period)%>%
  summarise(participants=n_distinct(study_number))%>%
  mutate(cumulative_participants=cumsum(participants))%>%
  select(-processed_period, -participants)

t<-cbind(t1,t2)

# generate viz


plot_labels<-c("April 2018",
               "May 2018",
               "June 2018",
               "July 2018",
               "August 2018",
               "September 2018",
               "October 2018",
               "November 2018",
               "December 2018",
               "January 2019",
               "February 2019",
               "March 2019",
               "April 2019",
               "May 2019",
               "June 2019",
               "July 2019",
               "August 2019",
               "September 2019",
               "October 2019",
               "November 2019",
               "December 2019",
               "January 2020",
               "February 2020",
               "March 2020",
               "April 2020",
               "May 2020",
               "June 2020",
               "July 2020",
               "August 2020",
               "September 2020",
               "October 2020",
               "November 2020",
               "December 2020",
               "January 2021",
               "February 2021")

p<-t%>%
  mutate(date=paste(processed_period, "01"), .keep=c("unused"))%>%
  rename('Unique codes' = codes_unique,
         'Unique codes (per 1000 participants)'=codes_unique_per_1000_participants,
         'Entries (per participant)'=entries_per_participant,
         'Participants with a record'=participants_with_record,
         'Cumulative participant count'=cumulative_participants,
         'Entries (total)'=entries_total)%>%
  pivot_longer(cols = c(-"date"),
               names_to = "key",
               values_to = "value")%>%
  filter(complete.cases(.))%>%
  ggplot(aes(date, value, color=key))+
  geom_point()+
  geom_line(aes(group=key))+
  theme_bw()+
  labs(y="number", title="")+
  scale_y_log10(breaks=c(10, 100, 1000, 10000,100000, 1000000, 10000000), labels=scales::comma)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position = "bottom")+
  scale_x_discrete(guide=guide_axis(n.dodge=1),
                   labels=plot_labels)

ggsave("Outputs/Transfer/Figures/Counts along time (Dispensing).png",
       width = 10,
       p)

rm(p,t,t1,t2, plot_labels)

rm(b)

gc()

# GP cluster groups ----------------------

## new clusters timeseries -----------

# select first appearance of each code
x<- gp%>%
  select(study_number, code, date)%>% # select some columns only
  mutate(year=substr(date,1,4))%>% # transform date into year
  group_by(code)%>% # group outputs by code
  filter(year==min(year))%>% # select first appearance of a code
  distinct(code,year)%>% # select each distinct code in each year
  arrange(year) # sort by year (ascending)

# join cluster lookup and select first appearance of each cluster
x<-x%>%
  left_join(gp_cluster_lookup, by=c("code"= "ConceptId"))%>% # join cluster lookup table
  group_by(Cluster_Desc)%>% # grouping by cluster names
  filter(year==min(year))%>% # select first appearance of each cluster
  distinct(Cluster_Desc, .keep_all = T)%>% # select only one entry per year for each cluster
  select(year, code, ConceptId_Description, Cluster_Desc, Cluster_Category) # select some columns only

# plot
p<-x%>%
  select(year, Cluster_Category)%>% # select variables of interest
  group_by(year, Cluster_Category)%>% # group outputs
  summarise(n=n())%>% # calculate counts per cluster category and year
  # mutate(total=sum(n))%>% # this calculated overall counts (not grouped)
  ggplot(aes(year, group=NA))+ # pass x variable to ggplot, specify no grouping variable for geom_point
  # geom_point(aes(y=total))+ # scatter plot for total counts
  # geom_line(aes(y=total))+ # line chart for total counts
  geom_point(aes(y=n, color=Cluster_Category))+ # scatter plot for counts per cluste category
  geom_line(aes(y=n, color=Cluster_Category, group=Cluster_Category))+ # add line chart for same variables
  geom_label_repel(data = x%>% # specify data for labels, which will be for the max value per cluster (need to repeat it as calcualtions above were done for the plot only and not saved as objects in the environement)
                     select(year, Cluster_Category)%>% # some variables only 
                     group_by(year, Cluster_Category)%>% # grouping as above
                     summarise(n=n())%>% # calculating counts as above
                     mutate(total=sum(n))%>% # calculating total counts per year
                     group_by(Cluster_Category)%>% # removing year grouping
                     filter(n == max(n)), # select year with max counts per cluster category
                   aes(x=year, y=n, # specify variables to be used in the labels (year for x axis, n calculated above for y position)
                       label=paste(Cluster_Category,",", "\n", year, ",", " ", n, " ", "clusters", sep=""), # speecify text in the label; want to include the name of the categoery, the count, and the year
                       fill=Cluster_Category, # fill labels with unique colours per group
                       segment.linetype= "dashed", # dashed lines connecting label to point
                       # segment.curvature = -0.1, # this allowed for angled lines
                       segment.inflect=T # adds inflection point if necessary
                   ),
                   nudge_y = 60, # nudge labels vertically
                   nudge_x=-20, # nudge labels horizontally
                   show.legend = F, # no legend for labels
                   max.overlaps = 25, # allow for a large number of overlapping labels given number of groups (otherwise wouldn't be printed)
                   # size=6, # could edit size if desired
                   direction="both", # direction of repel
                   force_pull = 0 # remove pull between label and point
  ) +    
  theme_bw(base_size=20)+ # specify B&W theme and base size
  theme(axis.text.x = element_text(angle = 90, # angle of x axis text
                                   vjust = 0.5, # vertical adjustment
                                   hjust=1), # horizontal adjustment
        legend.position = "none") # remove legend (as already in labels)

ggsave("Outputs/Transfer/Figures/GP_clusters_along_time.png",
       p,
       height=6,
       width=12)

rm(p)


## Medication group of clusters ----------
gp_cluster_lookup%>%filter(Cluster_Category=="Medications")%>%distinct(Cluster_Desc)%>%View()

a<-gp_cluster_lookup%>%filter(Cluster_Category=="Medications")%>%distinct(Cluster_Desc)%>%.[[1]]
# 36 clusters overall


x<-gp%>%
  select(study_number, code, date)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, date, Cluster_Desc, Cluster_Category)%>%
  filter(Cluster_Category=="Medications")%>%
  group_by(Cluster_Desc)%>%
  filter(date==min(date))%>%
  distinct(Cluster_Desc, .keep_all = T)%>%
  mutate(year=substr(date,1,4), .keep="unused")%>%
  arrange(year)%>%
  select(Cluster_Desc, 'First entry'=year)
# 35 clusters in the data, but 36 overall

# what is the missing cluster?
b<-x%>%
  distinct(Cluster_Desc)%>%
  .[[1]]

setdiff(a,b) # PCSK9 inhibitors

rm(a,b)

# calculate number of entries, participants per cluster

x1<-gp%>%
  select(study_number, date, code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, date, Cluster_Desc, Cluster_Category)%>%
  filter(Cluster_Category=="Medications")%>%
  group_by(Cluster_Desc)%>%
  summarise(Codes=n_distinct(code),
            Entries=n(),
            Participants=n_distinct(study_number),
            'Entries per participant'=round(Entries/Participants, digits=0))

x2<-gp%>%
  select(study_number, date, code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, date, Cluster_Desc, Cluster_Category)%>%
  filter(Cluster_Category=="Medications")%>%
  group_by(Cluster_Desc, study_number)%>%
  filter(n()>1)%>%
  arrange(date)%>%
  summarise(average=as.numeric(mean(diff(date))))%>%
  group_by(Cluster_Desc)%>%
  summarise('Mean interval between entries (in days)'=round(mean(average), digits=0))

t<-x%>%
  left_join(x1, by="Cluster_Desc")%>%
  left_join(x2, by="Cluster_Desc")%>%
  rename('Cluster' = Cluster_Desc)%>%
  arrange(Cluster)

write.csv(t, 
          "Outputs/Transfer/Tables/Medication_clusters_years.csv", 
          row.names = F)

p1<-gp%>%
  select(study_number, date, code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, date, Cluster_Desc, Cluster_Category)%>%
  filter(Cluster_Category=="Medications")%>%
  group_by(Cluster_Desc, study_number)%>%
  summarise(Entries=n())%>%
  group_by(Cluster_Desc)%>%
  ggplot(aes(x=Entries, y=fct_rev(Cluster_Desc), fill=Cluster_Desc))+
  geom_density_ridges(scale=1)+
  coord_cartesian(xlim=c(0,100))+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
  theme(legend.position = "none")+
  labs(y= "Cluster",
       x = "Entries per participant")

p2<-gp%>%
  select(study_number, date, code)%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, date, Cluster_Desc, Cluster_Category)%>%
  filter(Cluster_Category=="Medications")%>%
  group_by(Cluster_Desc, study_number)%>%
  filter(n()>1)%>%
  arrange(date)%>%
  summarise(average=as.numeric(mean(diff(date))))%>%
  group_by(Cluster_Desc)%>%
  ggplot(aes(x=average, y=fct_rev(Cluster_Desc), fill=Cluster_Desc))+
  geom_density_ridges(scale=1)+
  coord_cartesian(xlim=c(0,1500))+
  scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  labs(x = "Mean day difference between entries")


p<- p1 + p2 

ggsave("Outputs/Transfer/Figures/Meds_clusters_density.png",
       p,
       height=10,
       width=20)

rm(x, x1, x2, t, p, p1,p2)


## counts per Medication clusters timeseries----------------

a<-gp%>%
  select(study_number, date, code)%>%
  mutate(year=substr(date,1,4), keep=c("unused"))%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, year, Cluster_Desc, Cluster_Category)%>%
  filter(Cluster_Category=="Medications")%>%
  group_by(year)

t1<-a%>%
  group_by(Cluster_Desc, year)%>%
  summarise(entries_total=n(),
            participants_with_record=n_distinct(study_number),
            entries_per_participant=entries_total/participants_with_record,
            codes_unique=n_distinct(code),
            codes_unique_per_participant=codes_unique/participants_with_record)


# add cumulative count of participants with first record
t2<-gp%>%
  select(study_number, date, code)%>%
  mutate(year=substr(date,1,4), keep=c("unused"))%>%
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  select(study_number, code, year, Cluster_Desc, Cluster_Category)%>%
  filter(Cluster_Category=="Medications")%>%
  group_by(study_number, Cluster_Desc)%>%
  filter(year==min(year))%>%
  group_by(year, Cluster_Desc)%>%
  summarise(participants=n_distinct(study_number))%>%
  mutate(cumulative_participants=cumsum(participants))%>%
  select(-participants)

t<-cbind(t1, t2)

p<-t1%>%
  rename('Unique codes' = codes_unique,
         'Unique codes (per participant)'=codes_unique_per_participant,
         'Entries (per participant)'=entries_per_participant,
         'Participants with a record'=participants_with_record,
         'Entries (total)'=entries_total)%>%
  pivot_longer(cols = c(-"year", -"Cluster_Desc"),
               names_to = "key",
               values_to = "value")%>%
  mutate(year=as.integer(year))%>%
  ggplot(aes(year, value, color=key))+
  geom_point()+
  geom_line(aes(group=key))+
  theme_bw()+
  labs(y="number", title="")+
  scale_y_log10(breaks=c(10, 100, 1000, 10000,100000, 1000000, 10000000), labels=scales::comma)+
  coord_cartesian(xlim=c(2018,2022))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        # strip.text = element_text(size=7)
  )+
  facet_wrap(vars(Cluster_Desc),
             labeller = label_wrap_gen(width = 50),
             ncol=5)


ggsave("Outputs/Transfer/Figures/Medication_clusters_timeseries.png",
       p,
       height=20,
       width=30)

rm(a,p,t,t1,t2)
















# Drug groups -----------------
