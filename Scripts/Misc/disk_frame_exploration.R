# disk.frame exploration --------------
install.packages("disk.frame")

# load libraries -----
library(dplyr)
library(disk.frame)

# system setup----
# hardcode temporary directory within QNAP
tempdir <- function() { "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_RECOVERY/Temporary directory" }


# package setup -----
# setup multiple workers (in different CPU cores)
setup_disk.frame()

# allow data of any size to be trasmited between workers
options(future.globals.maxSize = Inf)

# load data ------
meds_df<-as.disk.frame(meds, # specify the data frame to be converted 
                       outdir=file.path(tempdir(), "meds.df"), # specify storage location
                       overwrite = T)

rm(meds)

meds_df


# perform some manipulations
meds_df%>%
  srckeep(c("study_number", "processing_period_date", "prescribed_bnf_code", "prescribed_bnf_name"))%>%
  mutate(chapter = str_sub(prescribed_bnf_code, 1,2))%>%
  select(study_number, chapter, prescribed_bnf_name, processing_period_date)%>%
  group_by(chapter)%>%
  summarise(Entries=n(),
            Participants = n_distinct(study_number))%>%
  collect()%>%
  group_by(chapter)%>%
  summarise(Entries=sum(Entries),
            Participants= sum(Participants),
            Participants_proportion = round(n_distinct(study_number)/participants_total_meds*100, 1))%>%
  as_tibble()->x1
