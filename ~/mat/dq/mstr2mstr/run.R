if(!require(data.table)) {install.packages('data.table');library(data.table)}
if(!require(stringdist)) {install.packages('stringdist');library(stringdist)}
if(!require(magrittr)) {install.packages('magrittr');library(magrittr)}

jstorm<-fread(rev(dir(pattern='JSTOR_Global_AllTitlesArchive',recursive=T))[1])$publication_title %>% toupper()
load('master2wok.RData')
wokm<-master2wok$SO
library(stringdist)
system.time(m<-amatch(jstorm,wokm))
m2m<-data.table(jix=which(!is.na(m)),jstor=jstorm[which(!is.na(m))],wix=m[!is.na(m)],wok=wokm[m[!is.na(m)]])
save(m2m,file='m2m.RData')
