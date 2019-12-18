
#############################################
## check for and install needed libraries  ##
#############################################

# needed packages:
libs <- c( "dplyr", "rmarkdown", "openair", "shiny")

# packages that are needed but not installed:
libs <- libs[!libs %in% installed.packages()]

# install missing packages; clean-up
if(length(libs) > 0) {
  install.packages(libs)
}
rm(libs)

#################################
##combined 1-Level data screen ##
#################################

screen<-function(filename=NULL, bam10="", bam25="",
                 comment="", html=NULL, ignore=FALSE){
  
  ## interactive data input if empty call (a la "screen()" )
  ## see http://rmarkdown.rstudio.com/developer_parameterized_reports.html
  if(is.null(filename)) {
    unlink(paste0(getwd(),"/data/screen.html"))
    rmarkdown::render("screen.Rmd", output_dir = paste0(getwd(), "/html"), 
                      envir = new.env(), quiet = TRUE, params = "ask")
    shell.exec(paste0(getwd(),"/html/screen.html"))
    
  } else { ## otherwise...
    
    #what to call html file:
    if(is.null(html)){
      html<-paste("html/", strsplit( filename, ".csv" , fixed=T)[[1]],".html", sep="")
    } else {
      html<-paste("html", html, sep="/")
    }
  
    rmarkdown::render("screen.Rmd", output_file=html, envir = new.env(), quiet = TRUE,
                    params = list(filename = filename,
                                  directory = "H: drive",
                                  bam10 = bam10,
                                  bam25 = bam25,
                                  comment = comment,
                                  ignore = FALSE))
    
    shell.exec(paste(getwd(),html, sep="/"))
  }
}

########################
## meteorology screen ##
########################

metscreen<-function(filename=NULL, comment="", html=NULL, ignore=F){
  
  ## interactive data input if empty call (a la "metscreen()" )
  ## 
  if(is.null(filename)) {
    unlink(paste0(getwd(),"/data/metscreen.html"))
    rmarkdown::render("metscreen.Rmd", output_dir = paste0(getwd(), "/html"), 
                      envir = new.env(), quiet = TRUE, params = "ask")
    shell.exec(paste0(getwd(),"/html/metscreen.html"))
  } else {
    #what to call html file:
    if(is.null(html)){
      html<-paste("html/", strsplit( filename, ".csv" , fixed=T)[[1]],"-met.html", sep="")
    } else {
      html<-paste("html", html, sep="/")
    }
    rmarkdown::render("metscreen.Rmd", output_file=html, envir = new.env(), quiet = TRUE,
                      params = list(filename = filename,
                                    directory = "H: drive",
                                    comment = "",
                                    ignore = FALSE))
    shell.exec(paste(getwd(),html, sep="/"))
  }
}

#####################
## pollutant screen##
#####################
pollscreen<-function(filename=NULL, logscale=TRUE, comment="", 
                     html=NULL,  ignore=FALSE){

  ## interactive data input if empty call (a la "metscreen()" )
  ## 
  if(is.null(filename)) {
    unlink(paste0(getwd(),"/data/pollscreen.html"))
    rmarkdown::render("pollscreen.Rmd", output_dir = paste0(getwd(), "/html"), 
                      envir = new.env(), quiet = TRUE, params = "ask")
    shell.exec(paste0(getwd(),"/html/pollscreen.html"))
  } else {
    #what to call html file:
    if(is.null(html)){
      html<-paste("html/", strsplit( filename, ".csv" , fixed=T)[[1]],"-poll.html", sep="")
    } else {
      html<-paste("html", html, sep="/")
    }
    rmarkdown::render("pollscreen.Rmd", output_file=html, envir = new.env(), quiet = TRUE,
                      params = list(filename = filename,
                                    directory = "H: drive",
                                    logscale=logscale,
                                    comment = "",
                                    ignore = FALSE))
    shell.exec(paste(getwd(),html, sep="/"))
  }
}

#######################
## exceedence report ##
#######################
exceedences<-function(filename=NULL, comment="", html=NULL, ignore=F){
  
  ## interactive data input if empty call (a la "exceedences()" )
  ## 
  if(is.null(filename)) {
    unlink(paste0(getwd(),"/data/exceedences.html"))
    rmarkdown::render("exceedences.Rmd", output_dir = paste0(getwd(), "/html"), 
                      envir = new.env(), quiet = TRUE, params = "ask")
    shell.exec(paste0(getwd(),"/html/exceedences.html"))
  } else {
    #what to call html file:
    if(is.null(html)){
      html<-paste("html/", strsplit( filename, ".csv" , fixed=T)[[1]],"-exceed.html", sep="")
    } else {
      html<-paste("html", html, sep="/")
    }
    rmarkdown::render("exceedences.Rmd", output_file=html, envir = new.env(), quiet = TRUE,
                      params = list(filename = filename,
                                    directory = "H: drive",
                                    comment = "",
                                    ignore = FALSE))
    shell.exec(paste(getwd(),html, sep="/"))
  }
  
}

#####################
## Run all at once ##
#####################

# only BasicDataExportReport.csv on Desktop allowed
# BAM screens not tested.

allscreens <- function(filename=NULL, bam10="", bam25="",
                       comment="", ignore=FALSE, logscale=TRUE){
    
  # Only accept default filename and location, for now
  if(!is.null(filename)) {stop("Only default filename and location are allowed.")}
  
  html <- NULL
  dir <- paste0(getwd(), "/html")
  
  # Run and launch screens:
  rmarkdown::render("screen.Rmd", output_file=html, output_dir = dir, envir = new.env(), quiet = TRUE,
                    params = list(filename = "BasicDataExportReport.csv",
                                  directory = "Desktop",
                                  bam10 = bam10,
                                  bam25 = bam25,
                                  comment = comment,
                                  ignore = ignore))
  
  shell.exec(paste(getwd(), "html/screen.html", sep="/"))
  
  rmarkdown::render("pollscreen.Rmd", output_file=html, output_dir = dir, envir = new.env(), quiet = TRUE,
                    params = list(filename = "BasicDataExportReport.csv",
                                  directory = "Desktop",
                                  comment = comment,
                                  ignore = ignore,
                                  logscale = logscale))
  
  shell.exec(paste(getwd(), "html/pollscreen.html", sep="/"))
  
  rmarkdown::render("metscreen.Rmd", output_file=html, output_dir = dir, envir = new.env(), quiet = TRUE,
                    params = list(filename = "BasicDataExportReport.csv",
                                  directory = "Desktop",
                                  comment = comment,
                                  ignore = ignore))
  
  shell.exec(paste(getwd(), "html/metscreen.html", sep="/"))
  
  rmarkdown::render("exceedences.Rmd", output_file=html, output_dir = dir, envir = new.env(), quiet = TRUE,
                    params = list(filename = "BasicDataExportReport.csv",
                                  directory = "Desktop",
                                  comment = comment,
                                  ignore = ignore))
  
  shell.exec(paste(getwd(), "html/exceedences.html", sep="/"))
}


#################
## calibration ##
#################
calibrations<-function(filename=NULL, comment="", html=NULL){
 
  ## interactive data input if empty call (a la "calibrations()" )
  ## 
  if(is.null(filename)) {
    unlink(paste0(getwd(),"/data/calibrations.html"))
    rmarkdown::render("calibrations.Rmd", output_dir = paste0(getwd(), "/html"), 
                      envir = new.env(), quiet = TRUE, params = "ask")
    shell.exec(paste0(getwd(),"/html/calibrations.html"))
  } else {
    #what to call html file:
    if(is.null(html)){
      html<-paste("html/", strsplit( filename, ".csv" , fixed=T)[[1]],"-cals.html", sep="")
    } else {
      html<-paste("html", html, sep="/")
    }
    rmarkdown::render("calibrations.Rmd", output_file=html, envir = new.env(), quiet = TRUE,
                      params = list(filename = filename,
                                    directory = "H: drive",
                                    comment = ""))
    shell.exec(paste(getwd(),html, sep="/"))
  }
  
}

####################################
## Generate Gas Precision Strings ##
####################################

##new format (QA transactions)
prec<-function(filename="AQSReport.txt",strings=NULL,thresh=5){
  
  #what to call file:
  if(is.null(strings)){
    strings <- paste0("html/", filename)
    } else {
    strings <- paste("html", strings, sep="/")
  }
  
  
  #filename<-paste("data", filename, sep="/")
  #strings<-paste("html", strings, sep="/")
  
  #checks:
  #filename?
  dir <- paste0(getwd(), "/data/")
  
  if(filename %in% list.files(dir) == FALSE ) {
    stop(paste("File", filename, "not found in", dir), call.=FALSE) 
  }
    
  #read file
  cl<-c(rep("character",13), rep("numeric", 2))
  f<-read.table(paste0(dir, filename), header=FALSE, sep="|",colClasses=cl)
  id<-which(is.na(f$V15)| f$V15==0)
  f<-f[-id,]
  id<-which(abs((f$V14-f$V15)/f$V15)>=thresh/100)
  true<-f$V15
  f$V15<-as.character(f$V15)
  f$V15[id]<-paste(f$V15[id], "<---")
  header<-data.frame(c(paste("## Input file:", filename), 
                       paste("## Flagging threshold:", thresh, "%"),
                       paste("## Generated on:", date())))
  write.table(header, file=strings, quote=FALSE, row.names=FALSE, col.names=FALSE)
  write.table(f, file=strings, quote=FALSE, sep="|", na="", row.names=FALSE, col.names=FALSE, append=TRUE)
  f$percent.diff<-round(-100*(true-f$V14)/true,2)
  f$param<-"???"
  f$param[which(f$V8==42602)]<-"NO2"
  f$param[which(f$V8==42401)]<-"SO2"
  f$param[which(f$V8==44201)]<-"O3"
  row.names(f)<-1:dim(f)[1]
  
  # print df: need to override default max print length
  old.opt <- getOption("max.print")  # save oringal
  options(max.print = nrow(f)*ncol(f)+1000)
  print(f)
  options(max.print = old.opt)        # reset original
  
  # print summary
  cat("\n")
  cat(nrow(f),"strings written to",paste(getwd(), strings, sep="/"),"\n")
  cat("Unique site codes: ")
  cat(unique(f$V7))
  cat("\nUnique parameter codes: ")
  cat(paste(unique(f$V8), collapse=" "),"\n")
  dates<-strptime(f$V10, "%Y%m%d", tz="America/Los_Angeles")
  cat(paste("Date Range:", min(dates),"to", max(dates)),"\n")
  cat(paste("Strings exceeding % diff threshold (", thresh, "%) : ", sep=""))
  cat(paste(id, collapse=" " ))
}

###################################################
## Generate BAM Precision Strings from Excel Dump##
###################################################

bamprec<-function(filename="BAM.txt", strings="bam_strings.txt",thresh=2){
  
  #what to call file:
  if(is.null(strings)==TRUE){
    if (filename=="AQSReport.txt") {
      strings<-"html/strings.txt"
    } else {
      strings<-paste("html/", filename, sep="")
    }
  } else {
    strings<-paste("html", strings, sep="/")
  }
  
  filename<-paste("data", filename, sep="/")
  #strings<-paste("html", strings, sep="/")
  
  #checks:
  #filename?
  if(filename %in% list.files(recursive=T) == FALSE ) stop(paste("File", filename, "not found in", getwd()))  
  
  cl<-c(rep("character",12), rep("numeric", 6))
  f<-read.table(filename, header=FALSE, sep="|",colClasses=cl)
  id<-which(f$V13==0)
  if(length(id)!=0) {f<-f[-id,]}
  id<-which(abs((f$V13-f$V15)/f$V13)>=thresh/100)
  f$V16<-NA; f$V16[id]<-"<---"
  f$V1<-"QA" ; f$V17<-"Flow Rate Verification" 
  n<-names(f)
  f<-f[,c(1,2,17,16, 3:7, 12, 8,11,10,13,15 )]
  names(f)<-n[1:15]
  f<-f[order(f$V8, f$V10),]
  header<-data.frame(c(paste("## Input file:", filename), 
                       paste("## Flagging threshold:", thresh, "%"),
                       paste("## Generated on:", date())))
  write.table(header, file=strings, quote=FALSE, row.names=FALSE, col.names=FALSE)
  write.table(f, file=strings, quote=FALSE, sep="|", na="", row.names=FALSE, col.names=FALSE, append=TRUE)
  f$percent.diff<-round(100*(f$V14-f$V15)/f$V15,2)
  f$param<-"???"
  f$param[which(f$V8==88101)]<-"PM2.5"
  f$param[which(f$V8==81102)]<-"PM10"
  row.names(f)<-1:dim(f)[1]
  
  # print df: need to override default max print length
  old.opt <- getOption("max.print")  # save oringal
  options(max.print = nrow(f)*ncol(f)+1000)
  print(f)
  options(max.print = old.opt)        # reset original
  
  cat("\n")
  cat(nrow(f),"strings written to",strings,"\n")
  cat("Unique site codes: ")
  cat(unique(f$V7))
  cat("\nUnique parameter codes: ")
  cat(paste(unique(f$V8), collapse=" "),"\n")
  dates<-strptime(f$V10, "%Y%m%d", tz="America/Los_Angeles")
  cat(paste("Date Range:", min(dates),"to", max(dates)),"\n")
  cat(paste("Strings exceeding % diff threshold (", thresh, "%) : ", sep=""))
  cat(paste(id, collapse=" " ))
}


###################################################
## Generate BAM Precision Strings Interactively  ##
###################################################

bamprec2<-function(strings = "bam_strings2.txt", append=FALSE){
  
  #suppress warning messages
  options(warn = -1)
  
  #check input:
  if(!is.logical(append)) stop("Append must be TRUE or FALSE", call. = FALSE)
  
  #Initial warning and instructions
  if(append==FALSE){
    warn<-paste0("Welcome to the interactive BAM precision string generator.\n", "The file ", 
                 strings, ", if present, will be overwritten.\nPress [esc] to abort or [enter] to continue. ")
  } else {
    warn<-paste0("Welcome to the interactive BAM precision string generator.\n", "The file ", 
                 strings, ", if present, will be updated.\nPress [esc] to abort or [enter] to continue. ")
  }
  
  cat("\n")
  readline(warn)
  cat("\n")
  
  #set directory for strings, and remove existing file, if append==FALSE.
  strings<-paste0("html/", strings)
  if(append==FALSE) unlink(strings)
  
  #initialize some variables...
  site<-""
  type<-""
  date<-""
  more<-"y"
  
  #data for checking inputs, assigning POCs
  d<-data.frame(site=c(8002, 8002, 2007, 2007, 2004, 2004, 4002, 9001),
                type=c(10, 2.5, 10, 2.5, 10, 2.5, 10, 10),
                poc =c(3,3,2,1,3,1,2,1))
  
  #get user input for first string
  while(site==""){
    site<-readline(prompt="Enter AQS site code and press [enter] ")
    if((site %in% d$site)==F) {
      cat("Site code not recognized as one with a BAM.\n")
      site<-""
    }
  }
  
  cat("\n")
  while(type != 10 & type != 2.5){
    type<-readline(prompt="Enter 10 for PM10 or 2.5 for PM2.5 and press [enter] ")
    if((type %in% c(2.5, 10))==F) {
      cat("PM type not recognized.\n")
      type<-""
    } else {
      if((type %in% d$type[which(d$site==site)])==FALSE) {
        cat("Invalid BAM type for this site.\n")
        type<-""
      }
    }
  }
  
  cat("\n")
  while(date==""){
    date<-readline(prompt="Enter date in YYYYMMDD format and press [enter] ")
    b<-as.numeric(date)
    if(is.na(b) | nchar(date)!=8) {
      cat("Date format is not correct.\n")
      date<-""
    } else {
      b<-strptime(date, "%Y%m%d", tz="America/Los_Angeles")
      if(is.na(b)) {
        cat("Date format is not correct.\n")
        date<-""
      }
    } 
  }
  
  cat("\n")
  flow<-readline(prompt="Enter measured flow rate and press [enter] ")
  
  #figure out POC, pollutant code, method
  poll<-ifelse(type==10, 81102, 88101)
  poc<-d$poc[which(d$site==site & d$type==type)]
  method<-ifelse(type==10, 122, 170)
  
  
  #generate record for upload
  rec<-paste("QA", "I", "Flow Rate Verification", "", "06", "079", site, poll, poc, date, "1",
             method, 118, 16.7, flow, sep="|")
  write.table(rec, file=strings, quote=FALSE, sep="|", na="", row.names=FALSE, col.names=FALSE, append=TRUE)
  
  #are there more strings?
  cat("\n")
  more<-tolower(readline(prompt="Press [enter] to enter another record or any letter to finish. "))
  date<-""
  cat("\n")
  
  # for additional strings....  
  
  while(more==""){
    newsite <- readline(prompt=paste0("Current AQS site code is ", site, 
                                      ". Press [enter] to accept or enter a new site code. "))
    if(newsite != "") {
      site <- newsite
      if((site %in% d$site)==F) {
        cat("Site code not recognized as one with a BAM.\n")
        site<-""
      }
    }
    while(site==""){
      site<-readline(prompt="Enter AQS site code and press [enter] ")
      if((site %in% d$site)==F) {
        cat("Site code not recognized as one with a BAM.\n")
        site<-""
      }
    }
    
    cat("\n")
    
    newtype <- readline(prompt=paste0("Current monitor is ", type, 
                                      ". Press [enter] to accept or enter a new monitor type. "))
    
    if(newtype != "") {
      type <- newtype
      if((type %in% c(2.5, 10))==F) {
        cat("PM type not recognized.\n")
        type<-""
      } else {
        if((type %in% d$type[which(d$site==site)])==FALSE) {
          cat("Invalid BAM type for this site.\n")
          type<-""
        }
      }
    }
    while(type != 10 & type != 2.5){
      type<-readline(prompt="Enter 10 for PM10 or 2.5 for PM2.5 and press [enter] ")
      if((type %in% c(2.5, 10))==F) {
        cat("PM type not recognized.\n")
        type<-""
      } else {
        if((type %in% d$type[which(d$site==site)])==FALSE) {
          cat("Invalid BAM type for this site.\n")
          type<-""
        }
      }
    }
    
    cat("\n")
    while(date==""){
      date<-readline(prompt="Enter date in YYYYMMDD format and press [enter] ")
      b<-as.numeric(date)
      if(is.na(b) | nchar(date)!=8) {
        cat("Date format is not correct.\n")
        date<-""
      } else {
        b<-strptime(date, "%Y%m%d", tz="America/Los_Angeles")
        if(is.na(b)) {
          cat("Date format is not correct.\n")
          date<-""
        }
      } 
    }
    
    cat("\n")
    flow<-readline(prompt="Enter measured flow rate and press [enter] ")
    
    #figure out POC, pollutant code, method
    poll<-ifelse(type==10, 81102, 88101)
    poc<-d$poc[which(d$site==site & d$type==type)]
    method<-ifelse(type==10, 122, 170)
    
    
    #generate record for upload
    rec<-paste("QA", "I", "Flow Rate Verification", "", "06", "079", site, poll, poc, date, "1",
               method, 118, 16.7, flow, sep="|")
    write.table(rec, file=strings, quote=FALSE, sep="|", na="", row.names=FALSE, col.names=FALSE, append=TRUE)
    
    #are there more strings?
    cat("\n")
    more<-tolower(readline(prompt="Press [enter] to enter another record or any letter to finish. "))
    date<-""
    cat("\n")
  }
  
  
  ## generate summary
  f<-read.table(strings, sep="|")
  f$percent.diff<-round(100*(f$V14-f$V15)/f$V15,2)
  f$param<-"???"
  f$param[which(f$V8==88101)]<-"PM2.5"
  f$param[which(f$V8==81102)]<-"PM10"
  
  cat("Summary of strings: \n")
  cat("\n")
  
  print(f)
  cat("\n")
  cat(nrow(f),"strings written to",paste(getwd(), strings, sep="/"),"\n")
  cat("Unique site codes: ")
  cat(unique(f$V7))
  cat("\nUnique parameter codes: ")
  cat(paste(unique(f$V8), collapse=" "),"\n")
  dates<-strptime(f$V10, "%Y%m%d", tz="America/Los_Angeles")
  cat(paste("Date Range:", min(dates),"to", max(dates)),"\n")
  
  ## reset warnings
  options(warn = 0)
}

#################
## autocals    ##
#################

## Temporary (hopefully) replacement for AV Calibration Graph Report,
## which is currently not working. 

## just like "calibrations" function, but allows several sites in one file.

autocals <- function(){
  
  ## interactive data input if empty call (a la "calibrations()" )
  ## 
#  if(is.null(filename)) {
    unlink(paste0(getwd(),"/data/autocals.html"))
    rmarkdown::render("autocals.Rmd", output_dir = paste0(getwd(), "/html"), 
                      envir = new.env(), quiet = TRUE, params = "ask")
    shell.exec(paste0(getwd(),"/html/autocals.html"))
  # } else {
  #   #what to call html file:
  #   if(is.null(html)){
  #     html<-paste("html/", strsplit( filename, ".csv" , fixed=T)[[1]],"-cals.html", sep="")
  #   } else {
  #     html<-paste("html", html, sep="/")
  #   }
  #   rmarkdown::render("calibrations.Rmd", output_file=html, envir = new.env(), quiet = TRUE,
  #                     params = list(filename = filename,
  #                                   directory = "H: drive",
  #                                   comment = ""))
  #   shell.exec(paste(getwd(),html, sep="/"))
  # }
  
}

