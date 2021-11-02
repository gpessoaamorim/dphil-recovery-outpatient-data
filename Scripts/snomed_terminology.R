# LOAD SNOMED TERMINOLOGY -------

# libraries ------
library(Rdiagnosislist)
library(tidyverse)
# https://cran.r-project.org/web/packages/Rdiagnosislist/index.html 


# load full release (Oct 2021) -------
SNOMED <- loadSNOMED(
  c("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/uk_sct2cl_32.5.0_20210929000001Z/SnomedCT_InternationalRF2_PRODUCTION_20210131T120000Z/Full/Terminology",
    "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/uk_sct2cl_32.5.0_20210929000001Z/SnomedCT_UKClinicalRF2_PRODUCTION_20210929T000001Z/Full/Terminology",
    "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/uk_sct2cl_32.5.0_20210929000001Z/SnomedCT_UKEditionRF2_PRODUCTION_20210929T000001Z/Full/Terminology",
    "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/uk_sct2dr_32.5.0_20210929000001Z/SnomedCT_UKDrugRF2_PRODUCTION_20210929T000001Z/Full/Terminology"),
  active_only = F
)


# load snapshot release (Oct 2021) -------
# if required
# SNOMED_SNAPSHOT<-loadSNOMED(
#   c("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/uk_sct2cl_32.5.0_20210929000001Z/SnomedCT_InternationalRF2_PRODUCTION_20210131T120000Z/Snapshot/Terminology",
#     "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/uk_sct2cl_32.5.0_20210929000001Z/SnomedCT_UKClinicalRF2_PRODUCTION_20210929T000001Z/Snapshot/Terminology",
#     "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/uk_sct2cl_32.5.0_20210929000001Z/SnomedCT_UKEditionRF2_PRODUCTION_20210929T000001Z/Snapshot/Terminology",
#     "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/SNOMED terminology/uk_sct2dr_32.5.0_20210929000001Z/SnomedCT_UKDrugRF2_PRODUCTION_20210929T000001Z/Snapshot/Terminology"),
#   active_only = F
# )

# save complete concepts file-----
concepts<-SNOMED[["CONCEPT"]]%>%
  as.tibble()

write_csv(concepts, "Tools/SNOMED terminology/Aggregated/rdiagnosislist/concepts.csv")

# save complete descriptions file -------
descriptions<-SNOMED[["DESCRIPTION"]]%>%
  as.tibble()

write_csv(descriptions, "Tools/SNOMED terminology/Aggregated/rdiagnosislist/descriptions.csv")


# generate FSN file --------------
FSN <- as_tibble(SNOMED[["DESCRIPTION"]])%>%
  filter(typeId=="900000000000003001")%>%
  group_by(conceptId)%>%
  filter(effectiveTime==max(effectiveTime))%>%
  ungroup()

# save FSN table ---------------
write_csv(FSN, "Tools/SNOMED terminology/Lookup_tables/FSN.csv")

