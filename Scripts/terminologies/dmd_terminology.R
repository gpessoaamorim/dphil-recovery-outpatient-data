# dmd terminology -------

library(readxl)
library(tidyverse)

# load files

dmd_vtm <- read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/dm+d/dmd_vtm.xlsx", col_types = c("text", "text", "skip", "skip", "skip", "skip"))

dmd_vmpp <- read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/dm+d/dmd_vmpp.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "skip", "skip", "skip", "skip"))

dmd_vmp <- read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/dm+d/dmd_vmp.xlsx", 
                      col_types = c("text", "text", "text", 
                                    "skip", "skip", "skip", "skip", "skip", 
                                    "skip", "skip", "skip", "skip", "skip", 
                                    "skip", "skip", "skip", "skip", "skip", 
                                    "skip", "skip", "skip", "skip", "skip", 
                                    "skip"))

dmd_amp <- read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/dm+d/dmd_amp.xlsx", 
                      col_types = c("text", "text", "skip", 
                                    "text", "skip", "skip", "skip", "skip", 
                                    "skip", "skip", "skip", "skip", "skip", 
                                    "skip", "skip", "skip", "skip", "skip"))

dmd_ampp <- read_excel("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/dm+d/dmd_ampp.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "skip", "skip", "skip", "skip", 
                                     "skip", "skip"))

# align column names and genrate code_type field
dmd_vtm%<>%
  mutate(code_type = "VTM")%>%
  select(code_type, code=VTMID, description=NM)

dmd_vmpp%<>%
  mutate(code_type="VMPP")%>%
  select(code_type, code=VPPID, description=NM)

dmd_vmp%<>%
  mutate(code_type="VMP")%>%
  select(code_type, code=VPID, description=NM)

dmd_ampp%<>%
  mutate(code_type="AMPP")%>%
  select(code_type, code=APPID, description=NM)

dmd_amp%<>%
  mutate(code_type="AMP")%>%
  select(code_type, code=APID, description=DESC)

# joint table

dmd_code_types<-rbind(dmd_vtm, dmd_vmpp, dmd_vmp, dmd_ampp, dmd_amp)

write_csv(dmd_code_types, "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/dm+d/dmd_code_types_lookup.csv")


