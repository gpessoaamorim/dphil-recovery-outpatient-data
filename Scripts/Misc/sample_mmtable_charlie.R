# sample code for mmtable

library(tidyverse)
library(mmtable2)

t1<-gp_dt%>% # input dataset that is stored as a data_table (dt)
  select(study_number, 
         code, 
         date, 
         gp_system_supplier, 
         episode_prescription, 
         value1_prescription, 
         value2_prescription)%>% # selecting some columns from the input dataset
  
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>% # joining code clusters for drug groupings
  
  filter(Cluster_Category=="Medications")%>% # filtering for drug clusters only 
  
  as_tibble()%>% # converting data table to tibble so it can fit an mmtable input
  
  group_by(Cluster_Desc)%>% # grouping by cluster
  
  summarise(Entries=n(), 
            Participants=n_distinct(study_number),
            'Entries per participant' = Entries/Participants,
            'Unique codes' = n_distinct(code),
            'Earliest record' = min(as.numeric(str_sub(date,1,4))))%>% # creating some summaries
  
  pivot_longer(-c(Cluster_Desc), # pivoting wide to long format since this is better for mmtable and ggplot; code will pivot every column except the cluster name
               names_to="key",  # column names transformed into a new "key" column
               values_to="value")%>%  # values in each column transferred to a new "value" column for each key
  
  mmtable(value, # first argument specifies what variable should be shown (in my case it is the "value" for each of the "keys")
          table_name = "")+ # removing table name
  
  header_top(key)+ # specify the column names (for my case it is the "key" which contains the name of the columns)
  
  header_left(Cluster_Desc) # grouping to be applied on the left side




t2<-gp_dt%>% # the structure of the code here is the same as before but had to calculate separately 
 
   select(study_number, code, date, gp_system_supplier, episode_prescription, value1_prescription, value2_prescription)%>%
  
  left_join(gp_cluster_lookup, by=c("code"="ConceptId"))%>%
  
  filter(Cluster_Category=="Medications")%>%
  
  group_by(Cluster_Desc, study_number)%>%
  
  filter(n()>1)%>%
  
  arrange(date)%>%
  
  summarise(average=as.numeric(mean(diff(date))))%>%
  
  group_by(Cluster_Desc)%>%
  
  summarise('Mean interval between entries (in days)'=round(mean(average), digits=0))%>%
  
  pivot_longer(-Cluster_Desc, names_to = "key", values_to="value")%>%
  
  as_tibble()%>%
  
  mmtable(value, table_name = "")+
  
  
  header_left(Cluster_Desc)

t<-t1+t2 # this binds the two tables produced above; colums are automatically ordered alphabetically
