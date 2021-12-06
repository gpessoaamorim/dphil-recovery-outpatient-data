# MEDS DATASET EXPLORATION



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
# library(plotly) # generate interactive plots (for inspection of individual points); commented as only needed ad hoc
library(ggh4x) # additional functions to manipulate facets in ggplot
library(ggalluvial) # river plots in ggplot
library(mmtable2) # to build tables; can be installed as remotes::install_github("ianmoran11/mmtable2")
library(gt) # to build tables
library(ggsankey) # river plots in ggplot
library(viridis)



# set working directory
setwd("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY")

# Some system settings ------
options(scipen=10000) # prevent scientific notation in plots











# Load SNOMED FSN (fully-specified names)-------
FSN <- read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/Lookup_tables/FSN.csv", 
                col_types = cols(id = col_character(), moduleId = col_character(), conceptId = col_character(), typeId = col_character(), caseSignificanceId = col_character()))



# Load data dictionary for meds data------

## formulation ------
formulation_dictionary <- read_excel("Tools/primcare_meds_reference-data_24-03-2021 (2).xlsx", 
                                     sheet = "Ref - Formulation")




# Load restricted and filtered meds dataset ----------
## filtering out patients who withdrew consent 
# and restricted to those randomised up until the last patient in the GP dataset extract 47

meds<-readRDS(file = "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Input datasets/INT07_PCAREMEDS/0049/meds.rds")

meds_rows <- nrow(meds)
meds_cols <- colnames(meds)
meds_participants <- meds%>%distinct(study_number)%>%.[[1]]


meds_dt <- lazy_dt(meds)

rm(meds)

## apply original row number --------

meds_dt%<>%
  mutate(row_number = row_number())

# General completeness table -------

completeness_table<-
  meds_dt%>%
  as_tibble()%>%
  summarise_all(funs(sum(complete.cases(.))/meds_rows*100))%>%
  t()%>%
  as.data.frame()%>%
  mutate(value=V1, .keep=c("unused"))

completeness_table$field<-
  row.names(completeness_table)

completeness_table$field%<>%
  factor(levels=meds_cols)

row.names(completeness_table)<-NULL


completeness_plot<-
  completeness_table%>%
  ggplot(aes(x=as.factor(field), y=value))+
  geom_bar(stat='identity')+
  xlab("Field")+
  ylab("Completeness rate")+
  theme_gray(base_size=20)+
  #scale_x_discrete(guide=guide_axis(n.dodge=2))+
  theme(axis.text.x = element_text(angle = 30,hjust=1))+
  geom_text(stat='identity', 
            aes(label=paste0(round(value, 1), "%")), 
            vjust=-0.5, 
            size=4)+
  labs(title="General completeness table for the Dispensing dataset")

ggsave("Outputs/Figures/dispensing_exploration/dispensing_completeness_table.png", 
       last_plot(),
       width=20, 
       height=8,
       dpi = "retina")

rm(completeness_table,completeness_plot)


# patient_age ------

meds_dt%>%
  skim(patient_age)

p1<-
  meds_dt %>%
  select(patient_age)%>%
  as_tibble()%>%
  ggplot(aes(patient_age))+
  geom_histogram(color="black")+
  theme_gray(base_size=20)+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())+
  labs(title="Patient_age",
       subtitle="Histogram")+
  xlim(0,105)



p2<-meds_dt %>%
  select(patient_age)%>%
  as_tibble()%>%
  ggplot(aes(patient_age))+
  geom_boxplot()+
  theme_gray(base_size=20)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank())+
  labs(subtitle="Boxplot",
       caption="Vertical lines in the boxplot represent median, Q1, and Q3; \nwhiskers span 1.5 times the interquartile range below Q1 and above Q3 \n(beyond which outliers are shown in black circles)")+
  xlim(0,105)


age_plot <- p1 + p2 + plot_layout(nrow=2, heights=c(2,1))

rm(p1,p2)

ggsave("Outputs/Figures/dispensing_exploration/patient_age.png", 
       last_plot(),
       width=15, 
       height=10,
       dpi = "retina")

rm(age_plot)


# patient_gender -----

meds_dt%>%
  count(patient_gender)%>%
  as_tibble()

meds_dt%>%
  select(patient_gender)%>%
  mutate(patient_gender=as.factor(patient_gender))%>%
  group_by(patient_gender)%>%
  summarise(n=n())%>%
  mutate(prop=round(n/sum(n)*100,0))%>%
  as_tibble()->x

x$patient_gender %<>%
  fct_recode(x$patient_gender,
             "Unknown/unrecorded" = "0",
             'Male' = "1",
             'Female' = "2")

plot_labels<-c("Unknown/unrecorded", "Male", "Female", "9")


x%>%
  ggplot(aes(patient_gender, n))+
  geom_bar(stat='identity')+
  geom_text(aes(label=paste0(n," (",prop, "%)")), vjust=-1, size=6)+
  scale_x_discrete(labels=plot_labels)+
  xlab("patient_gender")+
  ylab("Entries")+
  labs(title="Distribution of patient_gender")+
  theme_gray(base_size=20)

ggsave("Outputs/Figures/dispensing_exploration/patient_gender.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")



# bsa_prescription_id  -------

## compute general summary stats -------
a<- meds_dt%>%
  select(bsa_prescription_id)%>%
  skim()%>%
  as_tibble()

# 2720378 unique entries (for 6389336 rows)
# so on average each prescription has 2.34 items


## plot histogram of items per prescription
b<- meds_dt%>%
  select(bsa_prescription_id)%>%
  count(bsa_prescription_id)%>%
  as_tibble()

plot<- 
  b%>%
  ggplot(aes(n))+
  geom_histogram(bins=50)+
  theme_gray(base_size = 20)+
  labs(x="Entries",
       y="Counts",
       title="Histogram of entries per bsa_prescription_id")+
  geom_vline(aes(xintercept = median(n)), col="dark red")+
  geom_vline(aes(xintercept = mean(n)), col="dark blue")+
  geom_text(aes(x=median(n), y=900000, label=paste0("Median: ", round(median(n),1))),
            hjust=-0.1,
            size=6,
            colour="dark red")+
  geom_text(aes(x=mean(n), y=600000, label=paste0("Mean: ", round(mean(n),1))),
            hjust=-0.1,
            size=6,
            colour="dark blue")

ggsave("Outputs/Figures/dispensing_exploration/bsa_prescription_histogram.png",
       last_plot(),
       width=20,
       height=8,
       dpi="retina")

b%>%count(n)

rm(b, plot)

# eps_prescription_id -----------------


## compute general summary stats -------
a<- meds_dt%>%
  select(eps_prescription_id)%>%
  skim()%>%
  as_tibble()

# 2280880 unique entries (for 6389336 rows) - different from bsa_prescription
# so on average each prescription has 2.80 items

## plot histogram of items per prescription
b<- meds_dt%>%
  select(eps_prescription_id)%>%
  count(eps_prescription_id)%>%
  as_tibble()

plot<- 
  b%>%
  ggplot(aes(n))+
  geom_histogram(bins=50)+
  theme_gray(base_size = 20)+
  labs(x="Entries",
       y="Counts",
       title="Histogram of entries per eps_prescription_id",
       caption="Excluding 924,658 NA rows")+
  geom_vline(aes(xintercept = median(n)), col="dark red")+
  geom_vline(aes(xintercept = mean(n)), col="dark blue")+
  geom_text(aes(x=10, y=900000, label=paste0("Median: ", round(median(n),1))),
            hjust=-0.1,
            size=6,
            colour="dark red")+
  geom_text(aes(x=10, y=600000, label=paste0("Mean: ", round(mean(n),1))),
            hjust=-0.1,
            size=6,
            colour="dark blue")+
  xlim(0,13)

ggsave("Outputs/Figures/dispensing_exploration/eps_prescription_histogram.png",
       last_plot(),
       width=20,
       height=8,
       dpi="retina")

b%>%count(n)


# item_id ---------

meds_dt%>%
  select(item_id)%>%
  as_tibble()%>%
  count(item_id)->x

x%>%
  ggplot(aes(item_id, n))+
  geom_bar(stat='identity')+
  geom_text(aes(label=n), vjust=-1, size=4)+
  theme_gray(base_size=20)+
  labs(title="Distribution of item_id",
       y="Count")+
  scale_x_continuous(breaks=seq(1,32,by=1))+
  theme(panel.grid.minor = element_blank())


ggsave("Outputs/Figures/dispensing_exploration/item_id_barchart.png",
       last_plot(),
       width=20,
       height=8,
       dpi="retina")

# eps_prescription_indicator ------

meds_dt%>%
  select(eps_prescription_indicator)%>%
  group_by(eps_prescription_indicator)%>%
  summarise(n=n())%>%
  mutate(prop=round(n/sum(n)*100,0))%>%
  as_tibble()->x

plot_labels<-c("No", "Yes")

x%>%
  mutate(eps_prescription_indicator=as.factor(eps_prescription_indicator))%>%
  ggplot(aes(eps_prescription_indicator, n))+
  geom_bar(stat='identity')+
  geom_text(aes(label=paste0(n," (",prop, "%)")), vjust=-1, size=6)+
  scale_x_discrete(labels=plot_labels)+
  ylab("Entries")+
  labs(title="Distribution of eps_prescription_indicator",
       caption="Field defined as \"indicates whether prescription was processed by EPS\"")+
  theme_gray(base_size=20)

ggsave("Outputs/Figures/dispensing_exploration/eps_prescription_indicator.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")

## cross-coding with eps_prescription_id -------------

meds_dt%>%
  select(eps_prescription_id, eps_prescription_indicator)%>%
  mutate(eps_prescription_id_flag = if_else(!is.na(eps_prescription_id), 'Complete', 'Missing'))%>%
  count(eps_prescription_id_flag, eps_prescription_indicator)

## coding along time ---------

meds_dt%>%
  select(eps_prescription_indicator, processing_period_date)%>%
  count(eps_prescription_indicator, processing_period_date)%>%
  group_by(processing_period_date)%>%
  mutate(Proportion=round(n/sum(n)*100,0))%>%
  rename(Entries=n)%>%
  as_tibble()->x

x%>%
  mutate(eps_prescription_indicator=factor(eps_prescription_indicator, levels=rev(c("0","1")), labels=rev(c("No", "Yes"))))%>%
  pivot_longer(c(Entries, Proportion), names_to="key", values_to="value")%>%
  ggplot(aes(processing_period_date, value, group=eps_prescription_indicator, color=eps_prescription_indicator))+
  geom_point()+
  geom_line()+
  geom_text(aes(label=value), vjust=-1, size=4, color="black")+
  theme_gray(base_size=20)+
  theme(legend.position = "right",
        axis.title.y=element_blank())+
  labs(title="Timeseries for eps_prescription_indicator")+
  facet_wrap(~key, nrow=2, scales="free")+
  scale_y_continuous(expand=expansion(c(0,0.1)))

ggsave("Outputs/Figures/dispensing_exploration/eps_prescription_indicator_timeseries.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")


# not_dispensed_indicator ---------
meds_dt%>%
  count(not_dispensed_indicator)

plot_labels<-c("Dispensed", "Not dispensed")

p1<-meds_dt%>%
  select(not_dispensed_indicator)%>%
  count(not_dispensed_indicator)%>%
  mutate(prop=round(n/sum(n)*100,0))%>%
  as_tibble()%>%
  ggplot(aes(not_dispensed_indicator, n))+
  geom_bar(stat='identity')+
  geom_text(aes(label=paste0(n," (",prop, "%)")), vjust=-1, size=6)+
  scale_x_discrete(labels=plot_labels)+
  ylab("Entries")+
  labs(title="Distribution of not_dispensed_indicator")+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank())

ggsave("Outputs/Figures/dispensing_exploration/not_dispensed_indicator.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")

meds_dt%>%
  select(not_dispensed_indicator, bsa_prescription_id)%>%
  group_by(bsa_prescription_id)%>%
  count(not_dispensed_indicator)%>%
  pivot_wider(names_from = not_dispensed_indicator, values_from = n)%>%
  group_by(bsa_prescription_id)%>%
  mutate(Y=if_else(is.na(Y),0,Y))%>%
  mutate(Proportion=round(N/(N+Y)*100,0))%>%
  as_tibble()->x # dispensed vs non-dispensed items per prescription

x%>% 
  count(Y)%>%
  rename('Number of non-dispensed items per prescription'=Y,
         'Prescriptions'=n) ->a

write_csv(a, "Outputs/Tables/non_dispensed_items_per_prescription.csv")

rm(a,p1,x)



# prescriber_type ------

meds_dt%>%
  count(prescriber_type)%>%
  mutate(prescriber_type=as.factor(prescriber_type))%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  as_tibble()->x

plot_labels<-c("GP", "Hospital doctor", "Nurse", "Additional prescriber")

x%>%
  ggplot(aes(prescriber_type, n))+
  geom_bar(stat='identity')+
  geom_text(aes(label=paste0(n," (",prop, "%)")), vjust=-1, size=6)+
  scale_x_discrete(labels=plot_labels)+
  ylab("Entries")+
  labs(title="Distribution of prescriber_type")+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank())

ggsave("Outputs/Figures/dispensing_exploration/prescriber_type.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")

rm(x)

# cost_centre_type ----------

meds_dt%>%
  count(cost_centre_type, prescriber_type)%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  as_tibble()->x

plot_labels<-c("Hospital", "GP practice")

x%>%
  mutate(prescriber_type=as.factor(prescriber_type))%>%
  mutate(cost_centre_type = recode(cost_centre_type, '5' = "Hospital", '7' = "GP practice"),
         prescriber_type = recode(prescriber_type, '15' = "GP", '18' = "Hospital doctor", '33'="Nurse",'48'= "Additional prescriber"))%>%
  
  ggplot(aes(cost_centre_type, n, fill=prescriber_type, group=cost_centre_type))+
  geom_col(position = position_dodge2(width = 1, preserve = "single"))+
  geom_text(aes(label=paste0(n,"\n(",prop, "%)")), size=6,
            position = position_dodge2(width = 0.9, preserve='single'),
            vjust=-0.1
  )+
  ylab("Entries")+
  labs(title="Distribution of cost_centre_type",
       caption="cost_centre_type defined as the type of prescribing organisation\nproportions are within cost_centre_type",
       subtitle = "split within cost_centre_type by prescriber_type")+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        legend.position="bottom")+
  # facet_wrap(~cost_centre_type, scales="free")+
  scale_y_continuous(expand=expansion(c(0,0.1)))

ggsave("Outputs/Figures/dispensing_exploration/cost_centre_type_per_prescriber_type.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")


# cost_centre_subtype ----

meds_dt%>%
  count(cost_centre_sub_type, cost_centre_type)%>%
  group_by(cost_centre_type)%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  as_tibble()%>%
  mutate(cost_centre_type=as.factor(cost_centre_type),
         cost_centre_sub_type=as.factor(cost_centre_sub_type))%>%
  mutate(cost_centre_type = recode(cost_centre_type, 
                                   '5' = "Hospital", 
                                   '7' = "GP practice"),
         cost_centre_sub_type = recode(cost_centre_sub_type,
                                       '01' = "Walk-in centre",
                                       '02' = "Out-of-hours service",
                                       '03' = "Walk-in Centre and Out-of-hours service",
                                       '04' = "GP practice",
                                       '05' = "Health and Justice",
                                       '06' = "Private controlled drug practice",
                                       '07' = "Other",
                                       '08' = "Public health service",
                                       '09' = "Community health service",
                                       '10'= "Hospital service",
                                       '11'= "Optometry service",
                                       '12'= "Urgent and emergency care",
                                       '13'= "Hospice",
                                       '14'= "Care/nursing home"))->x

x%>%
  ggplot(aes(cost_centre_sub_type, n, fill=cost_centre_sub_type, group=cost_centre_type))+
  geom_col(position = position_dodge2(width = 1, preserve = "single"), width=0.5)+
  geom_text(aes(label=paste0(n,"\n(",prop, "%)")), size=4,
            position = position_dodge2(width = 0.9, preserve='single'),
            vjust=-0.2
  )+
  ylab("Entries")+
  labs(title="Distribution of cost_centre_sub_type",
       caption="cost_centre_sub_type defined as the subtype of prescribing organisation",
       subtitle = "per cost_centre_type")+
  theme_gray(base_size=20)+
  theme(axis.text.x=element_text(angle=30, hjust=1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none")+
  facet_wrap(~cost_centre_type, scales="free")+
  scale_y_continuous(expand=expansion(c(0,0.1)))
# scale_color_viridis()

ggsave("Outputs/Figures/dispensing_exploration/cost_centre_sub_type_per_cost_centre_type.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")

# dispensed_pharmacy_type -------

meds_dt%>%
  count(dispensed_pharmacy_type, cost_centre_type)%>%
  group_by(cost_centre_type)%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  as_tibble()%>%
  mutate(cost_centre_type=as.factor(cost_centre_type),
         dispensed_pharmacy_type=as.factor(dispensed_pharmacy_type))%>%
  mutate(cost_centre_type = recode(cost_centre_type, 
                                   '5' = "Hospital", 
                                   '7' = "GP practice"),
         dispensed_pharmacy_type = recode(dispensed_pharmacy_type,
                                          '7' = "GP practice",
                                          '8' =  "Contractor"))->x

x%>%
  ggplot(aes(cost_centre_type, n, fill=dispensed_pharmacy_type))+
  geom_col(position = position_dodge2(width = 1, preserve = "single"))+
  geom_text(aes(label=paste0(n,"\n(",prop, "%)")), size=4,
            position = position_dodge2(width = 0.9, preserve='single'),
            vjust=-0.2
  )+
  ylab("Entries")+
  labs(title="Distribution of dispensed_pharmacy_type",
       caption="dispensed_pharmacy_type defined as the type of dispensing organisation",
       subtitle = "per cost_centre_type")+
  theme_gray(base_size=20)+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="right")+
  facet_wrap(~cost_centre_type, scales="free")+
  scale_y_continuous(expand=expansion(c(0,0.1)))


ggsave("Outputs/Figures/dispensing_exploration/dispensed_pharmacy_type_per_cost_centre_type.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")


# out_of_hours_indicator -------

meds_dt%>%
  count(out_of_hours_indicator)%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  as_tibble()%>%
  mutate(out_of_hours_indicator=as.factor(out_of_hours_indicator))%>%
  mutate(out_of_hours_indicator = recode(out_of_hours_indicator, 
                                   '0' = "No"))->x

x%>%
  ggplot(aes(out_of_hours_indicator, n))+
  geom_col(position = position_dodge2(width = 1, preserve = "single"), width=0.5)+
  geom_text(aes(label=paste0(n,"\n(",prop, "%)")), size=6,
            position = position_dodge2(width = 0.9, preserve='single'),
            vjust=-0.2
  )+
  ylab("Entries")+
  labs(title="Distribution of out_of_hours_indicator")+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="right")+
  scale_y_continuous(expand=expansion(c(0,0.1)))


ggsave("Outputs/Figures/dispensing_exploration/out_of_hours_indicator.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")

# private_prescription_indicator ---------------
meds_dt%>%
  count(private_prescription_indicator)%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  as_tibble()%>%
  mutate(private_prescription_indicator=as.factor(private_prescription_indicator))%>%
  mutate(private_prescription_indicator = recode(private_prescription_indicator, 
                                         '0' = "No"))->x

x%>%
  ggplot(aes(private_prescription_indicator, n))+
  geom_col(position = position_dodge2(width = 1, preserve = "single"), width=0.5)+
  geom_text(aes(label=paste0(n,"\n(",prop, "%)")), size=6,
            position = position_dodge2(width = 0.9, preserve='single'),
            vjust=-0.2
  )+
  ylab("Entries")+
  labs(title="Distribution of private_prescription_indicator")+
  theme_gray(base_size=20)+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="right")+
  scale_y_continuous(expand=expansion(c(0,0.1)))


ggsave("Outputs/Figures/dispensing_exploration/private_prescription_indicator.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")


# prescribed_formulation -----------------
meds_dt%>%
  count(prescribed_formulation)%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  left_join(formulation_dictionary, by=c("prescribed_formulation" = "FormulationCode"))%>%
  select(prescribed_formulation, FormulationCodeDescription, n, prop)%>%
  as_tibble()->x


x%>%
  filter(prop>1)%>%
  mutate(FormulationCodeDescription=reorder(FormulationCodeDescription, n))%>%
  ggplot(aes(FormulationCodeDescription, fill=FormulationCodeDescription, n))+
  geom_col(position = position_dodge2(width = 1, preserve = "single"), width=0.5)+
  geom_text(aes(label=paste0(n," (",prop, "%)")), size=6,
            position = position_dodge2(width = 0.9, preserve='single'),
            hjust=-0.1
  )+
  ylab("Entries")+
  labs(title="Distribution of prescribed_formulation",
       caption = "Capped to formulations representing >=1% of the total entries")+
  theme_gray(base_size=20)+
  theme(legend.position="null",
        axis.title.y=element_blank(),
        legend.title = element_blank())+
  scale_y_continuous(expand=expansion(c(0,0.1)))+
  coord_flip()


ggsave("Outputs/Figures/dispensing_exploration/prescribed_formulation_over_1percent.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")

x%>%
  mutate(FormulationCodeDescription=reorder(FormulationCodeDescription, -n))%>%
  filter(prop<1)%>%
  mutate(group=if_else(between(n,1000,60000),1,2))->x1
  
x1%>%
  filter(group==1)%>%
  mutate(FormulationCodeDescription=reorder(FormulationCodeDescription, n))%>%
  ggplot(aes(FormulationCodeDescription, fill=FormulationCodeDescription, n))+
  geom_col(position = position_dodge2(width = 1, preserve = "single"), width=0.5)+
  geom_text(aes(label=paste0(n," (",prop, "%)")), size=5,
            position = position_dodge2(width = 0.9, preserve='single'),
            hjust=-.1)+
  ylab("Entries")+
  labs(caption = "Capped to formulations representing <1% of total and at least 1000 entries")+
  theme_gray(base_size=18)+
  theme(legend.position="null",
        axis.title.y=element_blank(),
        legend.title = element_blank()
        )+
  scale_y_continuous(expand=expansion(c(0,0.2)))+
  coord_flip() ->p1

x1%>%
  filter(group==2)%>%
  mutate(FormulationCodeDescription=reorder(FormulationCodeDescription, n))%>%
  ggplot(aes(FormulationCodeDescription, fill=FormulationCodeDescription, n))+
  geom_col(position = position_dodge2(width = 1, preserve = "single"), width=0.5)+
  geom_text(aes(label=paste0(n," (",prop, "%)")), size=5,
            position = position_dodge2(width = 0.9, preserve='single'),
            hjust=-.1)+
  ylab("Entries")+
  labs(caption = "Capped to formulations representing less than 1000 entries")+
  theme_gray(base_size=18)+
  theme(legend.position="null",
        axis.title.y=element_blank(),
        legend.title = element_blank()
        )+
  scale_y_continuous(expand=expansion(c(0,0.2)))+
  coord_flip() ->p2

p <- p1+p2 + plot_annotation(
  title="Distribution of prescribed_formulation"
)&
  theme(plot.title = element_text(size=30))


ggsave("Outputs/Figures/dispensing_exploration/prescribed_formulation_less_1percent.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")

rm(p,p1,p2,x,x1)


# prescribed_medicine_strength ----------

meds_dt%>%
  distinct(prescribed_medicine_strength)%>%
  as_tibble()%>%
  View()

meds_dt%>%
  select(prescribed_bnf_name, prescribed_medicine_strength)%>%
  as_tibble()%>%
  View()
  
meds_dt%>%
  select(prescribed_bnf_name, prescribed_medicine_strength)%>%
  filter(str_detect(prescribed_bnf_name, "statin"))%>%
  distinct()%>%
  as_tibble()%>%
  View()

meds_dt%>%
  select(prescribed_bnf_name, prescribed_medicine_strength)%>%
  filter(str_detect(prescribed_bnf_name, "inhaler"))%>%
  distinct()%>%
  as_tibble()%>%
  View()

# prescribed_quantity ---------
meds_dt%>%
  select(prescribed_quantity)%>%
  as_tibble()->x

x%>%
  ggplot(aes(prescribed_quantity))+
  geom_histogram(bins=100)

p1<- x%>%
  ggplot(aes(prescribed_quantity))+
  geom_histogram(color="black", bins=100)+
  theme_gray(base_size=20)+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())+
  xlim(0,1000)



p2<-x%>%
  ggplot(aes(prescribed_quantity))+
  geom_boxplot()+
  theme_gray(base_size=20)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank())+
  labs(caption="Plots capped at 1,000\nVertical lines in the boxplot represent median, Q1, and Q3; whiskers span 1.5 times the interquartile range below Q1 and above Q3 (beyond which outliers are shown in black circles)")+
  xlim(0,1000)


p <- p1 + p2 + 
  plot_layout(nrow=2, heights=c(2,1)) +
  plot_annotation(title="Prescribed_quantity")&
  theme(plot.title=element_text(size=30))

ggsave("Outputs/Figures/dispensing_exploration/prescribed_quantity_hist_boxplot.png", 
       last_plot(),
       width=20, 
       height=10,
       dpi = "retina")

rm(p,p1,p2, x)


## per formulation -----------------

meds_dt%>%
  select(prescribed_quantity, prescribed_formulation)%>%
  left_join(formulation_dictionary, by=c("prescribed_formulation" = "FormulationCode"))%>%
  select(prescribed_formulation, FormulationCodeDescription, prescribed_quantity)%>%
  as_tibble()->x


## group formulations ------------------------

oral_non_liquid<-c(
        "0002", # buccal tablet
        "0004", # capsule
        "0005", # chewable tablet
        "0010", # dispersible tablet
        "0012", # effervescent granules,
        "0013", # effervescent powder
        "0014", # effervescent tablet
        "0020", # gastro-resistant tablet
        "0021", # gastro-resistant granules,
        "0022", # Gastro-resistant tablet
        "0024", # granules
        "0037", # medicated chewing gum
        "0039", # modified-release capsule
        "0041", # modified-release granules
        "0042", # modified-release tablet
        "0052", # oral lyophilisate
        "0053", # orodispersible table
        "0063", # soluble tablet,
        "0067", # sublingual tablet,
        "0069", # tablet
        "0044", # muco-adhesive buccal tablet
        "0165"  # orodispersible film
        )

oral_liquids <- c("0035", # liquid,
                  "0048", # oral emulsion
                  "0050", # oral solution
                  "0051", # oral suspension
                  "0066", # sublingual spray
                  "0118", # gastrointestinal liquid
                  "0127", # oromucosal solution
                  "0148"  # oral drops
                  )

respiratory <-c("0029", # inhalation powder
                "0030", # inhalation vapour
                "0045", # nebuliser liquid
                "0061", # pressurized inhalation
                "0120", # powder for nebuliser solution
                "0189"  # inhalation solution
                )

injectables<- c("0090", # solution for injection
                "0091", # suspension for injection,
                "0093", # powder solution for injection
                "0095", # powder and solvent for solution for injection
                "0096", # powder and solvent for suspension for injection
                "0121", # solution for infusion
                "0123", # powder for solution for infusion
                "0125", # infusion
                "0171", # powder and solvent for prolonged-release suspension for injection
                "0172"  # prolonged-release suspension for injection)
                )
                
topicals<-c("0001", # bath additive
            "0007", # cream
            "0018", # foam
            "0023", # gel
            "0027", # impregnated dressing
            "0034", # irrigation
            "0036", # lozenge
            "0038", # medicated nail lacquer
            "0040", # modified-release drops
            "0043", # mouthwash
            "0046", # ointment
            "0054", # paint
            "0055", # paste
            "0060", # powder,
            "0062", # shampoo,
            "0064", # spray
            "0065", # stick
            "0071", # transdermal patch
            "0074", # wash
            "0111", # medical plaster
            "0129", # tablet for cutaneous solution
            "0135", # eye drops
            "0136", # ear drops
            "0137", # nasal drops
            "0139", # irrigation solution
            "0141", # cutaneous emulsion
            "0149", # eye ointment
            "0150", # nasal ointment
            "0152", # dental gel
            "0153", # eye gel
            "0154", # oral gel
            "0155", # oromucosal gel
            "0156", # vaginal gel
            "0157", # ear/eye drops solution
            "0158", # ear/eye/nose drops solution
            "0160", # cutaneous solution
            "0162", # intravesical solution
            "0168"  # nasal gel
            )
            
implants<- c("0026", # implant
             "0057", # pessary
             "0076", # intrauterine device
             "0130"  # vaginal delivery system
             )

rectal <- c("0016", # enema
            "0068", # suppository
            "0151"  # rectal ointment)
            )

unspecified<-c("0075", # not applicable
          "-")

formulation_dictionary%<>%
  mutate(formulation_group = case_when(
    FormulationCode %in% implants ~ "implants",
    FormulationCode %in% injectables ~ "injectables",
    FormulationCode %in% oral_liquids ~ "oral_liquids",
    FormulationCode %in% oral_non_liquid ~ "oral_non_liquid",
    FormulationCode %in% unspecified ~ "unspecified",
    FormulationCode %in% rectal ~ "rectal",
    FormulationCode %in% respiratory ~ "respiratory",
    FormulationCode %in% topicals ~ "topicals"))
    # note that formulation_dictionary will have formulations with no group since they do not appear in our data (about 100)

rm(implants, injectables, oral_liquids, oral_non_liquid, rectal, respiratory, topicals, unspecified)

# investigate NA and not applicable formulations -------
meds_dt%>%
  
  select(study_number, prescribed_bnf_code, prescribed_formulation, processing_period_date)%>%
  
  filter(prescribed_formulation == '-' | prescribed_formulation == "0075")%>%
  
  mutate(prescribed_formulation = recode(prescribed_formulation, '-' = "Unknown", '0075' = "Not applicable"))%>%
  
  group_by(prescribed_formulation, processing_period_date)%>%
  
  summarise(Entries=n(),
            Participants = n_distinct(study_number))%>%
  
  pivot_longer(c(Entries, Participants), names_to = "key", values_to = "value")%>%
  
  as_tibble()->x

x%>%
  ggplot(aes(processing_period_date, value, group=prescribed_formulation, color=prescribed_formulation))+
  geom_line()+
  geom_point()+
  facet_wrap(~key, nrow=2, scales="free")+
  labs(title="Timeseries of \"unknown\" and \"not applicable\" prescribed_formulation along time",
       y="",
       x="Processing period date")+
  theme_gray(base_size=20)


ggsave("Outputs/Figures/dispensing_exploration/timeseries_not_applicable_unknown_prescribed_formulation.png", 
       last_plot(),
       width=20, 
       height=8,
       dpi = "retina")  

rm(x)

# explore formulation groups --------

## counts --------
meds_dt%>%
  select(study_number, 
         processing_period_date, 
         prescribed_bnf_code, 
         prescribed_bnf_name,
         prescribed_formulation,
         prescribed_quantity)%>%
  left_join(formulation_dictionary, by=c("prescribed_formulation" = "FormulationCode"))%>%
  group_by(formulation_group)%>%
  summarise(entries=n(),
            entries_proportion = round(n()/meds_rows*100,1),
            participants = n_distinct(study_number),
            participants_proportion = round(n_distinct(study_number)/length(meds_participants)*100,1))%>%
  as_tibble()-> a
  
  
p1<-a%>%
  mutate(formulation_group = recode(formulation_group,
                                    'unspecified' = "Unspecified",
                                    'topicals' = "Topicals",
                                    'respiratory' = "Respiratory",
                                    'rectal' = "Rectal",
                                    'oral_non_liquid' = "Oral formulations (non-liquid)",
                                    'oral_liquids' = "Oral formulations (liquid)",
                                    'injectables' = "Injectables",
                                    'implants' = "Implants"))%>%
  mutate(formulation_group=reorder(formulation_group, entries))%>%
  ggplot(aes(entries, formulation_group, fill=formulation_group))+
  geom_bar(stat='identity')+
  geom_text(aes(label=paste0(entries,"\n(",entries_proportion, "%)")), hjust=-0.1, size=6)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30,hjust=1),
        axis.title.y = element_blank())+
  labs(x="Entries",
       y = "Formulation group")+
  scale_x_continuous(expand=expansion(c(0,0.2)))


p2<-a%>%
  mutate(formulation_group = recode(formulation_group,
                                    'unspecified' = "Unspecified",
                                    'topicals' = "Topicals",
                                    'respiratory' = "Respiratory",
                                    'rectal' = "Rectal",
                                    'oral_non_liquid' = "Oral formulations (non-liquid)",
                                    'oral_liquids' = "Oral formulations (liquid)",
                                    'injectables' = "Injectables",
                                    'implants' = "Implants"))%>%
  mutate(formulation_group=reorder(formulation_group, entries))%>%
  ggplot(aes(participants, formulation_group, fill=formulation_group))+
  geom_bar(stat='identity')+
  geom_text(aes(label=paste0(participants,"\n(",participants_proportion, "%)")), hjust=-0.1, size=6)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30,hjust=1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  labs(x="Participants",
       y = "Formulation group",
       caption = "Note: participant counts and proportions are not mutually exclusive")+
  scale_x_continuous(expand=expansion(c(0,0.2)))
  
p <- p1+p2 + plot_annotation(
  title="Frequency of coding per broad formulation group"
)&
  theme(plot.title = element_text(size=30))


ggsave("Outputs/Figures/dispensing_exploration/frequencies_prescribed_formulation_group.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")

rm(p,p1,p2,x,x1)
  

## entries per participant -----

meds_dt%>%
  select(study_number, 
         processing_period_date, 
         prescribed_bnf_code, 
         prescribed_bnf_name,
         prescribed_formulation,
         prescribed_quantity)%>%
  left_join(formulation_dictionary, by=c("prescribed_formulation" = "FormulationCode"))%>%
  group_by(formulation_group, study_number)%>%
  summarise(entries_per_participant=n())%>%
  group_by(formulation_group)%>%
  mutate(entries=sum(entries_per_participant))%>%
  as_tibble() ->a

# summary table
x<-a%>%group_by(formulation_group)%>%skim(entries_per_participant)

write_csv(x, "Outputs/Tables/entries_per_participant_formulation_groups.csv")

# plot
a%>%
  mutate(formulation_group = recode(formulation_group,
                                    'unspecified' = "Unspecified",
                                    'topicals' = "Topicals",
                                    'respiratory' = "Respiratory",
                                    'rectal' = "Rectal",
                                    'oral_non_liquid' = "Oral formulations (non-liquid)",
                                    'oral_liquids' = "Oral formulations (liquid)",
                                    'injectables' = "Injectables",
                                    'implants' = "Implants"))%>%
  mutate(formulation_group=reorder(formulation_group, entries))%>%
  ggplot(aes(entries_per_participant, y = formulation_group, fill=formulation_group))+
  geom_density_ridges(scale=0.8)+
  # geom_text(aes(label=entries_per_participant), hjust=-0.1, size=6)+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30,hjust=1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Formulation group",
       caption = "Density distributions are grouped per formulation\nCapped at 200",
       title = "Entries per participant, per formulation group")+
  scale_x_continuous(expand=expansion(c(0,0.2)), limits = c(0,200))

ggsave("Outputs/Figures/dispensing_exploration/entries_per_participant_formulation_group.png", 
       last_plot(),
       width=15, 
       height=10,
       dpi = "retina")

rm(p,p1,p2,x,x1)

### investigate some outliers ----

a%>%filter(entries_per_participant>200)->x

View(x)

rm(x)

### monthly prescriptions per formulation group ----

meds_dt%>%
  select(study_number, processing_period_date, prescribed_bnf_code, prescribed_formulation)%>%
  left_join(formulation_dictionary, by = c("prescribed_formulation" = "FormulationCode"))%>%
  count(study_number, processing_period_date, formulation_group)%>%
  group_by(study_number, formulation_group)%>%
  summarise(monthly_prescriptions = median(n))%>%
  as_tibble()->a

a%>%
  mutate(formulation_group = recode(formulation_group,
                                    'unspecified' = "Unspecified",
                                    'topicals' = "Topicals",
                                    'respiratory' = "Respiratory",
                                    'rectal' = "Rectal",
                                    'oral_non_liquid' = "Oral formulations (non-liquid)",
                                    'oral_liquids' = "Oral formulations (liquid)",
                                    'injectables' = "Injectables",
                                    'implants' = "Implants"))%>%
  ggplot(aes(monthly_prescriptions, formulation_group, fill=formulation_group))+
  geom_histogram(bins=50, alpha=0.7, color="black")+
  scale_x_continuous(limits=c(0,30),
                     breaks = seq(0,30, by=2)
                     )+
  facet_wrap(~formulation_group, scales="free_x")+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.x=element_blank())+
  labs(title="Histograms of number of monthly prescriptions per patient and per formulation group",
       caption = "Plots capped at 30\nCounts represent number of patients\nPatient-level summaries calculated based on median")

ggsave("Outputs/Figures/dispensing_exploration/monthly_prescriptions_per_formulation_group.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")



## prescribed_quantity  -----

meds_dt%>%
  select(study_number, prescribed_formulation, prescribed_quantity)%>%
  left_join(formulation_dictionary, by = c("prescribed_formulation" = "FormulationCode"))%>%
  as_tibble()->a

a%>%
  filter(prescribed_quantity<=1000)%>%
  mutate(formulation_group = recode(formulation_group,
                                    'unspecified' = "Unspecified",
                                    'topicals' = "Topicals",
                                    'respiratory' = "Respiratory",
                                    'rectal' = "Rectal",
                                    'oral_non_liquid' = "Oral formulations (non-liquid)",
                                    'oral_liquids' = "Oral formulations (liquid)",
                                    'injectables' = "Injectables",
                                    'implants' = "Implants"))%>%
  ggplot(aes(prescribed_quantity, group=formulation_group, fill=formulation_group))+
  geom_histogram(bins=50,color="black", alpha=0.7)+
  facet_wrap(~formulation_group, scales = "free")+
  theme_gray(base_size=20)+
  theme(legend.position = "none",
        axis.title.x=element_blank())+
  labs(title="Histograms of prescribed_quantity per formulation group",
       caption = "Plots capped at 1000\nCounts represent number of entries")

ggsave("Outputs/Figures/dispensing_exploration/prescribed_quantity_per_formulation_group.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")


rm(a,x)

# prescribed_supplier_name ------

meds_dt%>%
  count(prescribed_supplier_name)%>%
  arrange(desc(n))%>%
  as_tibble()->a
  

a%>%
  slice_head(n=20)%>%
  mutate(prescribed_supplier_name=reorder(prescribed_supplier_name, n))%>%
  ggplot(aes(prescribed_supplier_name, n, fill=prescribed_supplier_name))+
  geom_bar(stat='identity')+
  coord_flip()+
  geom_text(aes(label=n), size=6, hjust=-0.1)+
  theme_gray(base_size=20)+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y = element_blank())+
  labs(title="Number of entries for top 20 suppliers")+
  scale_y_continuous(expand=expansion(c(0,0.1)))

ggsave("Outputs/Figures/dispensing_exploration/prescribed_supplier_name.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")  
  
rm(a)

# paid fields ------
## paid_bnf_code vs prescribed ----

meds_dt%>%
  select(processing_period_date, prescribed_bnf_code, prescribed_bnf_name, prescribed_supplier_name, paid_bnf_code, paid_bnf_name, prescribed_formulation, row_number)%>%
  filter(prescribed_bnf_code != paid_bnf_code)%>%
  left_join(formulation_dictionary, by = c("prescribed_formulation" = "FormulationCode"))%>%
  as_tibble()->a

a%>%
  count(processing_period_date, formulation_group)%>%
  mutate(formulation_group = recode(formulation_group,
                                    'unspecified' = "Unspecified",
                                    'topicals' = "Topicals",
                                    'respiratory' = "Respiratory",
                                    'rectal' = "Rectal",
                                    'oral_non_liquid' = "Oral formulations (non-liquid)",
                                    'oral_liquids' = "Oral formulations (liquid)",
                                    'injectables' = "Injectables",
                                    'implants' = "Implants"))%>%
  ggplot(aes(processing_period_date, n, color=formulation_group))+
  geom_line()+
  geom_point()+
  facet_wrap(~formulation_group)+
    theme_gray(base_size=20)+
  theme(legend.position="none",
        axis.title.x=element_blank())+
  labs(title="Timeseries of divergent entries (between prescribed and paid BNF code)",
       subtitle="Per formulation group",
       y="Entries")
  
ggsave("Outputs/Figures/dispensing_exploration/prescribed_vs_paid_bnf_code_timeseries_per_formulation_group.png", 
       last_plot(),
       width=25, 
       height=10,
       dpi = "retina")  

a%>%
  select(row_number)%>%
  .[[1]]->divergent_rows

rm(a)

## paid_bnf_name -------

meds_dt%>%
  select(processing_period_date, prescribed_bnf_code, prescribed_bnf_name, prescribed_supplier_name, paid_bnf_code, paid_bnf_name, prescribed_formulation, row_number)%>%
  filter(prescribed_bnf_name != paid_bnf_name)%>%
  as_tibble()->a

a%>%
  filter(!row_number%in%divergent_rows)%>%
  nrow()

## paiddmd_code ----------

meds_dt%>%
  select(prescribeddmd_code, prescribed_bnf_name, prescribed_supplier_name, paiddmd_code, paid_bnf_name, row_number)%>%
  filter(prescribeddmd_code != paiddmd_code)%>%
  as_tibble()->a

a%>%
  filter(!row_number%in%divergent_rows)%>%
  nrow()


a%>%
  filter(!row_number%in%divergent_rows)%>%
  View()

## paid_formulation ------
meds_dt%>%
  select(prescribed_formulation, prescribed_bnf_name, prescribed_supplier_name, paid_formulation, paid_bnf_name, row_number)%>%
  filter(prescribed_formulation != paid_formulation)%>%
  as_tibble()->a

nrow(a)

## paid_quantity -----
meds_dt%>%
  select(prescribed_quantity, prescribed_bnf_name, prescribed_supplier_name, paid_quantity, paid_bnf_name, row_number)%>%
  filter(prescribed_quantity != paid_quantity)%>%
  as_tibble()%>%
  nrow()
  # 474506 different


meds_dt%>%
  select(prescribed_bnf_name, prescribed_quantity, prescribed_formulation, prescribed_supplier_name, paid_bnf_name, paid_quantity, row_number)%>%
  filter(prescribed_quantity != paid_quantity)%>%
  as_tibble()->a

View(a)




## paid_drug_strength
meds_dt%>%
  select(prescribed_medicine_strength, prescribed_bnf_name, prescribed_supplier_name, paid_drug_strength, paid_bnf_name, row_number)%>%
  filter(prescribed_medicine_strength != paid_drug_strength)%>%
  as_tibble()->a
  
nrow(a)


# generate entire list of dmd codes -----
meds_dt%>%
  distinct(prescribeddmd_code, paiddmd_code, .keep_all=T)%>%
  select(prescribed_bnf_code, prescribed_bnf_name, prescribeddmd_code, paid_bnf_name, paid_bnf_code, paiddmd_code)%>%
  as_tibble()->dispensing_codes

dispensing_codes%<>%
  left_join(FSN, by=c("prescribeddmd_code" = "conceptId"))%>%
  select(prescribed_bnf_code, prescribed_bnf_name, prescribeddmd_code, prescribeddmd_term=term, paid_bnf_code, paid_bnf_name, paiddmd_code)%>%
  left_join(FSN, by=c("paiddmd_code" = "conceptId"))%>%
  select(prescribed_bnf_code, prescribed_bnf_name, prescribeddmd_code, prescribeddmd_term, paid_bnf_code, paid_bnf_name, paiddmd_code, paiddmd_term=term)
