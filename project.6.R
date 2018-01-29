rm(list = ls())

library(EML)
library(rmarkdown)
library(RPostgreSQL) 
library(dplyr)
library(data.table)
library(tools)
library(methods)
library(xlsx)

#read all the functions
source("../posgre.r")
source("../datatable.r")
source("../dataset.r")


#read a user info code that has all the postgres credential info
source("../user_info.r")

# input postgreSQL table
posgre<-posgrefun(dbname   = dbname,
                  host     = host,
                  user     = user,
                  password = password)

meta <- posgre$meta
fact<-posgre$fact
unit <- posgre$unit
creator <- posgre$creator
keyword <- posgre$keyword
entities <- posgre$entities
dataset <- posgre$dataset
method <- posgre$method
geo <- posgre$geo
tempo <- posgre$tempo

#----------------------
#input datatable
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

datasetid=6

datatable1<-datatablefun(datasetid       = datasetid,
                        entity          = 1)

otherentity1<-datatablefun(datasetid       = datasetid,
                            entity          = 2)
#-----------------------------
#generate EML
eml<-datasetfun(datasetid       = datasetid,
                dataTable       = datatable1,
                otherentity     = otherentity1)

#-----------------------------

#eml_validate(eml)

write_eml(eml, "XML_6.xml")

