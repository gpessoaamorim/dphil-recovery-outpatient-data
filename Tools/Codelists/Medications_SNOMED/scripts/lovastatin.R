# lovastatin codes
# 04/10/21

library(tidyverse)
library(stringr)
library(readr)

# load terminology

FSN <- read_csv("FSN.csv", col_types = cols(X1 = col_skip(), 
                                            conceptId = col_character()))

human_read_rels <- read_csv("human.read.rels.csv", 
                            col_types = cols(X1 = col_skip(), sourceId = col_character(), 
                                             typeId = col_character(), destinationId = col_character()))

relationships_active <- read_csv("relationships.active.csv", 
                                 col_types = cols(X1 = col_skip(), id = col_character(), 
                                                  effectiveTime = col_character(), 
                                                  active = col_factor(levels = c("0", 
                                                                                 "1")), moduleId = col_character(), 
                                                  sourceId = col_character(), destinationId = col_character(), 
                                                  typeId = col_character(), characteristicTypeId = col_character(), 
                                                  modifierId = col_character()))

##################################################################################
# Extraction - rule 1: has a relationship specifying an ingredient and destination term contains "Lovastatin"

## build list of relationships of interest
ingredient.rel.list <- c(127489000, # "has active ingredient"
              10362701000001108, # "has AMP"
              732943007,# "has basis of strenght substance"
              10363001000001101, #"Has NHS dm+d (dictionary of medicines and devices) basis of strength substance"
              762949000, # "has precise active ingredient"
              10362801000001104, # "has specific active ingredient"
              10362601000001103, # "has VMP"
              738774007, # "is modification of"
              8652801000001103, # "is pack of"
              116680003) # "is a"

## apply filter to generate destination terms
lovastatin.dest.list <- human_read_rels %>%
  filter(
    str_detect(destination.term, "Lovastatin|lovastatin"))%>%
  select(destinationId)%>%
  distinct(destinationId)%>%
  .[[1]]
# all terms used to depict a relationship referring to lovastatin 

## apply rule 
lovastatin.codelist<- relationships_active %>%
  filter(
    typeId %in% ingredient.rel.list & 
    destinationId %in% lovastatin.dest.list) %>% 
  distinct(sourceId)%>% 
  left_join(FSN, by=c("sourceId"="conceptId"))%>%
  rename(conceptId=sourceId)

View(lovastatin.codelist)

write.csv(lovastatin.codelist, "codelists/lovastatin_dmd.csv",row.names=F)

rm(list = ls())
