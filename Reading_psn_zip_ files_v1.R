################################################################################################
# Script to read *.psn.zip files and extract positions and positioning errors with 1 min time resolution
#
# Input files: *.psn.zip fiules in a folder "D:/SCINDA/all data/.." or replace by correct address
# Input folder may contain sub-folders
#
# Output files: "PSN_data_YYYY_mMM.dat", YYYY - year, MM - month's number - psn data as in the original SCINDA files
# Output files: "eqTime_PSN_data_YYYY_mMM.dat", YYYY - year, MM - month's number  - psn data with 1 min time resolution
# (missing time epochs are identified and missing data are as NA)
#
# Output files are saved in the current R working folder (check by getwd())
# Output files header: time.stamp	year	month	day	sec.f.midnight	h	m	s	mean.x	mean.y	mean.z	sigma.x	sigma.y	sigma.z
#
################################################################################################

Read.SCINDA.PSN.files_SaveOnly <- function(dir.list) {
  #dir.list - list of folders with *.scn.zip files
  require(pracma) # function strfind
  FileExtension <- ".psn.gz"
  century <- 2000
  options(digits = 8)
  
  Make.tmp.dataframe <- function() {
    my.dataframe <- data.frame(year = as.numeric(NA),
                               month = as.numeric(NA),
                               day = as.numeric(NA),
                               sec.f.midnight = as.numeric(NA),
                               h = as.numeric(NA),
                               m = as.numeric(NA),
                               s = as.numeric(NA),
                               time.stamp = as.POSIXct(strptime("2000-01-01 00:00:00", format = "%Y-%m-%d %H:%M"), tz = "GMT"),
                               mean.x = as.numeric(NA),
                               mean.y = as.numeric(NA),
                               mean.z = as.numeric(NA),
                               sigma.x = as.numeric(NA),
                               sigma.y  = as.numeric(NA),
                               sigma.z = as.numeric(NA))
  }
  
  Gaps.PSN <- function(Input.DF, year, month) {
    l.day <- DOYall_hours$CY.length[month]
    if (year == 2016) {l.day <- DOYall_hours$LY.length[month]}
    month.string <- paste0(month)
    if (month < 10) {month.string <- paste0("0", month.string)}
    
    Needed.Length <- l.day*24*60
    Actual.Length <- length(Input.DF[,1])
    if (Needed.Length != Actual.Length) {
      print("gaps fixing...")
      string.1st.day <- paste0(year,"-",month.string,"-01 00:00:00")
      string.last.day <- paste0(year,"-",month.string,"-",l.day, " 23:59:00")
      Full.Time.series <- seq.POSIXt(as.POSIXct(strptime(string.1st.day, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT"),
                                     as.POSIXct(strptime(string.last.day, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT"),
                                     "1 min")
      Output.DF <- data.frame(time.stamp = Full.Time.series)
      Output.DF <- merge(Output.DF, Input.DF, by = "time.stamp", all.x = T)
      print("... done!")
    } else {
      Output.DF <- Input.DF
    } 
    
    return(Output.DF)
  }
  
  Length.dir.list <- length(dir.list)
  
  
  TMP.SCN.time <- c(year = as.numeric(NA),
                    month = as.numeric(NA),
                    day = as.numeric(NA),
                    sec.f.midnight = as.numeric(NA),
                    h = as.numeric(NA),
                    m = as.numeric(NA),
                    s = as.numeric(NA))
  time.stamp = as.POSIXct(strptime("2000-01-01 00:00:00", format = "%Y-%m-%d %H:%M"), tz = "GMT")
  TMP.SCN.data <- c(x = as.numeric(NA),
                    y = as.numeric(NA),
                    z = as.numeric(NA))
  SCN.data <- Make.tmp.dataframe()
  
  Out.List.DF <- list(list(name = "name1", ds = SCN.data))
  N.Out.DF <- 1
  
  orig.dir <- getwd()
  
  #setting counts for Current year & month
  setwd (dir.list[1])
  Input.File.Names <- list.files(pattern = FileExtension)
  myFile <- Input.File.Names[1]
  myLines <- readLines(myFile)
  Length.myLines <- length(myLines)
  a <- vector("integer",length = Length.myLines)
  
  for (i in 1:Length.myLines) {
    a[i] <- !is.null(strfind(myLines[i],"T"))
  }
  
  N.TimeLines <- which(a == 1)
  rm(a)
  
  TMP.SCN.time[1:4] <- as.numeric(strsplit(myLines[N.TimeLines[1]], " ")[[1]][-1])
  TMP.SCN.time[1] <- century + TMP.SCN.time[1]
  Current.Year <- TMP.SCN.time[1]
  Current.Month <- TMP.SCN.time[2]
  
  if (Current.Month <= 9) {
    Current.Month.str <- paste0("0",Current.Month)
  } else {
    Current.Month.str <- paste0(Current.Month)
  }
  DF.name <- paste0(orig.dir,"/","PSN_data_",Current.Year,"_m",Current.Month.str,".dat")
  write.table(t(colnames(SCN.data)), DF.name, col.names = F, row.names = F, quote = F, sep = "\t")
  
  if (T) {
    for (N.dir in 1:Length.dir.list) {
      setwd (dir.list[N.dir])
      Input.File.Names <- list.files(pattern = FileExtension)
      
      for (ff in 1:length(Input.File.Names)) { #reading 1 file
        if (substr(Input.File.Names[ff],1,2) != "-") {
          print(paste("file #",ff," out of ",length(Input.File.Names)))
          myFile <- Input.File.Names[ff]
          myLines <- readLines(myFile)
          Length.myLines <- length(myLines)
          
          a <- vector("integer",length = Length.myLines)
          for (i in 1:Length.myLines) {
            a[i] <- !is.null(strfind(myLines[i],"T"))
          }
          N.TimeLines <- which(a == 1)
          rm(a)
          Length.TimeLines <- length(N.TimeLines)
          N.TimeLines[Length.TimeLines+1] <- Length.myLines+1
          
          for (n.tm in 1:Length.TimeLines) { #reading all lines
            if (is.null(strfind(myLines[N.TimeLines[n.tm]],"T -20"))) {# end checking for "T -20"
              TMP.SCN.time[1:4] <- as.numeric(strsplit(myLines[N.TimeLines[n.tm]], " ")[[1]][-1])
              TMP.SCN.time[1] <- century + TMP.SCN.time[1]
              TMP.SCN.time[5] <- floor(TMP.SCN.time[4]/60/60)
              TMP.SCN.time[6] <- floor(TMP.SCN.time[4]/60 - TMP.SCN.time[5]*60)
              TMP.SCN.time[7] <- TMP.SCN.time[4] - (TMP.SCN.time[5]*60 + TMP.SCN.time[6])*60
              tmp.str <- paste0(TMP.SCN.time[1],"-",
                                TMP.SCN.time[2],"-",
                                TMP.SCN.time[3]," ",
                                TMP.SCN.time[5],":",
                                TMP.SCN.time[6],":",
                                TMP.SCN.time[7])
              time.stamp <- as.POSIXct(strptime(tmp.str, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")
              
              if ((Current.Year != TMP.SCN.time[1]) | (Current.Month != TMP.SCN.time[2])) { #opening new month file
                Current.Year <- TMP.SCN.time[1]
                Current.Month <- TMP.SCN.time[2]
                if (Current.Month <= 9) {
                  Current.Month.str <- paste0("0",Current.Month)
                } else {
                  Current.Month.str <- paste0(Current.Month)
                }
                DF.name <- paste0(orig.dir,"/","PSN_data_",Current.Year,"_m",Current.Month.str,".dat")
                write.table(t(colnames(SCN.data)), DF.name, col.names = F, row.names = F, quote = F, sep = "\t")
              } # end opening new month file
              
              i.tmp <- 1
              if ((N.TimeLines[n.tm+1] - N.TimeLines[n.tm]) > 1) {# check for nodata error
                for (n.dt in (N.TimeLines[n.tm]+1):(N.TimeLines[n.tm+1]-1)) {
                  a <- strsplit(myLines[n.dt], " ")[[1]]
                  TMP.SCN.data <- as.numeric(a[which(a != "")])
                  SCN.data[i.tmp,1:7] <- TMP.SCN.time
                  SCN.data[i.tmp,8] <- time.stamp
                  SCN.data[i.tmp,9:11] <- (TMP.SCN.data[1:3])
                  i.tmp <- i.tmp+1
                } # end reading data for the same timestamp
              } else {
                TMP.SCN.data <- rep(as.numeric(NA),3)
                SCN.data[i.tmp,1:7] <- TMP.SCN.time
                SCN.data[i.tmp,8] <- time.stamp
                SCN.data[i.tmp,9:14] <- as.numeric(NA)
              } # end check nodata error
              
              if (sum(is.na(SCN.data[,9])) != length(SCN.data[,9])) {
                SCN.data[1,9:11] <- colMeans(SCN.data[,9:11], na.rm = T)
                SCN.data[1,12:14] <- apply(SCN.data[,9:11], 2, sd, na.rm = T)
              }
              
              write.table(SCN.data[1,], DF.name, col.names = F, row.names = F, quote = F, sep = "\t", append = T)
              SCN.data <- Make.tmp.dataframe()
              
            } else {# end checking for "T -20"
              print("T -20 error at...")
              print(n.tm)
              print(myLines[N.TimeLines[n.tm]])
            }
          } # end reading all lines
          
          rm(myLines)
        }
      } #end reading 1 file
      
    } #end dir.list
    
    setwd(orig.dir)
  }
  
  FileTemplate <- paste0("^PSN_data_")
  Input.File.Names <- list.files(pattern = FileTemplate)
  for (ff in 1:length(Input.File.Names)) {
    Data <- read.delim(Input.File.Names[ff])
    Data$time.stamp <-  as.POSIXct(strptime(Data$time.stamp, format = "%Y-%m-%d %H:%M"), tz = "GMT")
    
    month <- Data$month[1]
    year <- Data$year[1]
    Full.Data <- Gaps.PSN(Data,year, month)
    colnames(Full.Data) <- colnames(Data)[c(8,1:7,9:14)]
    F.name <- paste0("eqTime_",Input.File.Names[ff])
    write.table(Full.Data, F.name, col.names = T, row.names = F, quote = F, sep = "\t")
  }
}

############################### data folders
# if the original folders structure is kept -> un-comment following lines:
# selected.years <- 2014
# selected.months <- "12-Dec"
# My.folders <- paste0("D:/SCINDA/all data/",
#                    selected.years,
#                    "/")
# My.folders1 <- paste0(My.folders,selected.months)
# 
# selected.years <- rep(c(2015:2018), each = 12)
# selected.months <- c("01-Jan",
#                      "02-Feb",
#                      "03-Mar",
#                      "04-Apr",
#                      "05-May",
#                      "06-Jun",
#                      "07-Jul",
#                      "08-Aug",
#                      "09-Sep",
#                      "10-Oct",
#                      "11-Nov",
#                      "12-Dec")
# My.folders <- paste0("D:/SCINDA/all data/",
#                    selected.years,
#                    "/")
# My.folders <- paste0(My.folders,selected.months)
# My.folders1 <- c(My.folders1, My.folders)
# 
# selected.years <- rep(2019, 2)
# selected.months <- c("01-Jan",
#                      "02-Feb")
# My.folders <- paste0("D:/SCINDA/all data/",
#                    selected.years,
#                    "/")
# My.folders <- paste0(My.folders,selected.months)
# My.folders1 <- c(My.folders1, My.folders)
# 
# My.folders <- My.folders1
# rm(My.folders1)

# if all *.scn.zip files are in one folder -> un-comment following line:
# My.folders <- "D:/SCINDA/all data"
My.folders <- "D:/Anna Work/My Work/Data/Sun_CR/Ionosphere/SCINDA/SCN files as DAT Test"


suppressWarnings(Read.SCINDA.PSN.files_SaveOnly(My.folders))


rm(My.folders)