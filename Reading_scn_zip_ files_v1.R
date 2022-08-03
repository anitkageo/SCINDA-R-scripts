################################################################################################
# Script to read *.scn.zip files
#
# Input files: *.scn.zip files in a folder "D:/SCINDA/all data/.." or replace by correct address
# Input folder may contain sub-folders
#
# Output files: "SCN_data_YYYY_mMM.dat", YYYY - year, MM - month's number 
# Output files are saved in the current R working folder (check by getwd())
# Output files header: year	month	day	sec.f.midnight	h	m	s	time.stamp	az	el	L1S4	pc.samp.l1	L2S4	pc.samp.l2	TECP	TECf	ROTI	TECR	t.since.last.cycle.slip	PRN
#
################################################################################################


Read.SCINDA.files_SaveOnly <- function(dir.list) {
  #dir.list - list of folders with *.scn.zip files
  require(pracma) 
  FileExtension <- ".scn.gz"
  century <- 2000
 
  Make.tmp.dataframe <- function() {
    my.dataframe <- data.frame(year = as.numeric(NA),
                               month = as.numeric(NA),
                               day = as.numeric(NA),
                               sec.f.midnight = as.numeric(NA),
                               h = as.numeric(NA),
                               m = as.numeric(NA),
                               s = as.numeric(NA),
                               time.stamp = as.POSIXct(strptime("2000-01-01 00:00:00", format = "%Y-%m-%d %H:%M"), tz = "GMT"),
                               az = as.numeric(NA),
                               el = as.numeric(NA),
                               L1S4 = as.numeric(NA),
                               pc.samp.l1 = as.numeric(NA),
                               L2S4  = as.numeric(NA),
                               pc.samp.l2 = as.numeric(NA),
                               TECP = as.numeric(NA),  
                               TECf = as.numeric(NA),
                               ROTI = as.numeric(NA),
                               TECR = as.numeric(NA),
                               t.since.last.cycle.slip = as.numeric(NA),  
                               PRN = as.numeric(NA))
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
  TMP.SCN.data <- c(az = as.numeric(NA),
                    el = as.numeric(NA),
                    L1S4 = as.numeric(NA),
                    pc.samp.l1 = as.numeric(NA),
                    L2S4  = as.numeric(NA),
                    pc.samp.l2 = as.numeric(NA),
                    TECP = as.numeric(NA),  
                    TECf = as.numeric(NA),
                    ROTI = as.numeric(NA),
                    TECR = as.numeric(NA),
                    t.since.last.cycle.slip = as.numeric(NA),  
                    PRN = as.numeric(NA))
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
  DF.name <- paste0(orig.dir,"/","SCN_data_",Current.Year,"_m",Current.Month.str,".dat")
  write.table(t(colnames(SCN.data)), DF.name, col.names = F, row.names = F, quote = F, sep = "\t")

  for (N.dir in 1:Length.dir.list) {
    setwd (dir.list[N.dir])
    print(dir.list[N.dir])
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
              DF.name <- paste0(orig.dir,"/","SCN_data_",Current.Year,"_m",Current.Month.str,".dat")
              write.table(t(colnames(SCN.data)), DF.name, col.names = F, row.names = F, quote = F, sep = "\t")
            } # end opening new month file
            
            if ((N.TimeLines[n.tm+1] - N.TimeLines[n.tm]) > 1) {# check for nodata error
              for (n.dt in (N.TimeLines[n.tm]+1):(N.TimeLines[n.tm+1]-1)) {
                 a <- strsplit(myLines[n.dt], " ")[[1]]
                TMP.SCN.data <- as.numeric(a[which(a != "")])
                SCN.data[1,1:7] <- TMP.SCN.time
                SCN.data[1,8] <- time.stamp
                SCN.data[1,9:20] <- TMP.SCN.data
                write.table(SCN.data, DF.name, col.names = F, row.names = F, quote = F, sep = "\t", append = T)
              } # end reading data for the same timestamp
            } else {
              TMP.SCN.data <- rep(as.numeric(NA),12)
              SCN.data[1,1:7] <- TMP.SCN.time
              SCN.data[1,8] <- time.stamp
              SCN.data[1,9:20] <- TMP.SCN.data
              write.table(SCN.data, DF.name, col.names = F, row.names = F, quote = F, sep = "\t", append = T)
            } # end check nodata error
          } else {# end checking for "T -20"
            print(n.tm)
            print(myLines[N.TimeLines[n.tm]])
          }
        } # end reading all lines
        
        rm(myLines)
      }
    } #end reading 1 file
    
  } #end dir.list
  
  write.table(SCN.data, DF.name, col.names = F, row.names = F, quote = F, sep = "\t", append = T)

  setwd(orig.dir) 
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

Read.SCINDA.files_SaveOnly(My.folders)

rm(My.folders)