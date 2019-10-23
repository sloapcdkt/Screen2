
##############################################
##build archive for pollscreen and metscreen##
##############################################

##load data from 2005-2014, clean up
#AQS Query: AMP501, O6 079, ALL 1 hour data, 2005 01 01 to 2016 12 31
#unzip("AMP501_1647566.zip", list = TRUE)
arch<-read.table(unzip("AMP501_1737693.zip")[1], sep = "|") # load AQS data dump (AMP501)
arch<-arch[,-c(1:4, 8, 14:28)]                # remove unwanted columns
names(arch)<-c("site", "param", "poc", "unit", "method", "date", "time", "value")
arch<-arch[-which(arch$param==81102),]     # get rid of pm10std
#arch<-arch[-which(arch$dur==7),]          # get rid of 24-hr averages (only want hourly)
#arch<-arch[-which(arch$dur=="H"),]        # get rid of 5-min averages (only want hourly)
#arch<-arch[,-4]                            # get rid of duration column, now that it's all hourly

arch$site[which(arch$site == 8002)]<-8001  # merge old and new atas
arch$site[which(arch$site == 8002)]<-8001  # merge old and new atas

arch<-arch[-which(arch$site == 5),]        # get rid of paso
arch<-arch[-which(arch$site == 2002),]     # get rid of slo
arch<-arch[-which(arch$site == 2006),]     # get rid of slo

#create site id
arch$siteid<-paste(arch$site, arch$param, arch$poc, arch$method, sep="-")

#rm some more unneeded site/monitors
arch<-arch[-which(arch$siteid=="2001-42601-1-35"),]
arch<-arch[-which(arch$siteid=="2001-42602-1-35"),]
arch<-arch[-which(arch$siteid=="2001-42603-1-35"),]
arch<-arch[-which(arch$siteid=="2001-44201-1-19"),]
arch<-arch[-which(arch$siteid=="2004-61103-1-20" & arch$unit==11),]

#figure out units...
table(arch$siteid, arch$unit) # units are consistent

#remove unneeded params
arch<-arch[,c("site", "param", "date", "time", "value")]

#prepare for long->short foramt change...
arch$date<-paste(arch$date, arch$time)
arch$date<-as.POSIXct(arch$date, tz="UTC", format="%Y%m%d %H:%M")
arch$siteid<-paste(arch$site, arch$param, sep="-")
arch<-arch[,c("siteid", "date", "value")]

#long to short...
arch<-reshape2::dcast(arch, date~siteid, value.var="value")

#make data.frames by site....

rename <- function(d) {
  params <- c("so2", "no", "no2", "nox", "o3", "wsv", "wdv", "sigt", "amt", "bam10", "bam25")
  codes <- c(42401, 42601, 42602, 42603, 44201, 61103, 61104, 61106, 62101, 85101, 88101)
  
  x <- names(d)[-ncol(d)]
  x <- sapply(x, function(a) strsplit(a, "-", fixed = TRUE)[[1]][2])
  x <- sapply(x, function(a) params[match(a, codes)])
  x <- c(x, "date")
  
  return(x)
}

make.archive <- function(d, site,
                         sites =  c("atas", "red", "carr", "morro", "grov", 
                                    "cdf", "mesa", "nrp", "oso"),
                         codes =  c(8001, 8005, 8006, 3001, 2001,
                                    2007, 2004, 4002, 9001)) {
  
  # check inputs
  if(!site%in%sites) {stop("'site' not in 'sites' list")}
  if(length(sites)!=length(codes)) {stop("Sites and codes not the same length.")}
  
  # extract data
  x <- d[, grep(paste0(codes[match(site, sites)],"-"), names(d))]
  x$date <- d$date
  names(x) <- rename(x)
  
  # write file
  write.csv(x, file=paste0("arch-", site, ".csv"), row.names=FALSE)
}

lapply(c("atas", "red", "carr", "morro", "grov", "cdf", "mesa", "nrp", "oso"),
       function(a) make.archive(arch, a))

# clean up
rm(arch, rename, make.archive)
