library(readxl)
library(tidyverse)
library(RSQLite)
library(DBI)

DB1 <- read_excel("Top MA Donors 2016-2020(2).xlsx")
Con_All = read_excel("Top MA Donors 2016-2020(2).xlsx",sheet = "Direct Contributions & JFC Dist")
JFC = read_excel("Top MA Donors 2016-2020(2).xlsx",sheet = "JFC Contributions (DO NOT SUM W")


Con_All['contrib'] = gsub(Con_All$contrib,pattern = ", ",replacement = ",")
Con_All['contrib'] = gsub(Con_All$contrib,pattern = "\\s\\w*",replacement = "")


Contribution = Con_All %>% select(cycle,contribid,fam,date,amount,recipid,type,fectransid,cmteid) %>% distinct()
Contributor = Con_All %>% select(contribid,fam,contrib,City,State,Zip,Fecoccemp,orgname,lastname) %>% distinct()
Recipient = Con_All %>% select(recipid,recipient,party,recipcode) %>% distinct()
Organization = Con_All %>% select(orgname,ultorg) %>% distinct() %>% na.omit()

Contrib = dbConnect(SQLite(),"Kerui_Cao.sqlite")
dbWriteTable(Contrib,"Contribution",Contribution)
dbWriteTable(Contrib,"Contributor",Contributor)
dbWriteTable(Contrib,"Recipient",Recipient)
dbWriteTable(Contrib,"Organization",Organization)
