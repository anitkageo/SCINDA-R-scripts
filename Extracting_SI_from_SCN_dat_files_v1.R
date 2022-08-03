################################################################################################
# Script to read SCN_data_YYYY_mMM.dat files and extract scintillation indices (SI) L1S4 and ROTI data
#
# SCINDA SI data are cleaned:
# - only GPS satellites, 
# - only satellites with elevation >= 0º (el = 0 - missing/eroneous data),
# - only satellites with ROTI > 0 (ROTI = 0 - missing/eroneous data),
# - missing time epochs are identified and missing data are set as NA

#
# Input files: SCN_data_YYYY_mMM.dat files in a folder "D:/SCINDA/.." or replace by correct address
# Input folder must NOT contain sub-folders
#
# Output files: "eqTime_SI_SCN_data_YYYY_mMM.dat", YYYY - year, MM - month's number - 
# all SI data from all available GPS satellites for 1 min time intervals
#
# Output files are saved in the current R working folder (check by getwd())
# Output files header:time.stamp	az	el	L1S4	ROTI	PRN
#
################################################################################################

# un-comment following line or replace with correct address of a folder with SCN_dat_*.dat files:
# Input.folder <- "D:/SCINDA/"

Input.folder.SI <- "D:/Anna Work/My Work/Data/Sun_CR/Ionosphere/SCINDA/SCN files as DAT Test"


Saving.SI.data <- function(Input.folder) {
  
  Gaps.SI <- function(Input.DF, year, month) {
    aver.DF <- aggregate(data.frame(tmp = Input.DF[,2]),
                         data.frame(time.stamp = cut(Input.DF$time.stamp, 
                                                     "1 min")),
                         FUN = "mean",
                         na.rm = T)
    aver.DF$time.stamp <- as.character(aver.DF$time.stamp)
    aver.DF$time.stamp <- as.POSIXct(strptime(aver.DF$time.stamp, format = Time.Format), tz = "GMT")
    
    l.day <- DOYall_hours$CY.length[month]
    if (year == 2016) {l.day <- DOYall_hours$LY.length[month]}
    month.string <- paste0(month)
    if (month < 10) {month.string <- paste0("0", month.string)}
    
    Needed.Length <- l.day*24*60
    Actual.Length <- length(aver.DF[,1])
    if (Needed.Length != Actual.Length) {
      print("gaps fixing...")
      string.1st.day <- paste0(year,"-",month.string,"-01 00:00:00")
      string.last.day <- paste0(year,"-",month.string,"-",l.day, " 23:59:00")
      Full.Time.series <- seq.POSIXt(as.POSIXct(strptime(string.1st.day, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT"),
                                     as.POSIXct(strptime(string.last.day, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT"),
                                     "1 min")
      n <- which(!is.element(Full.Time.series,aver.DF$time.stamp))
      Output.DF <- data.frame(time.stamp = Full.Time.series[n],
                              az = as.numeric(NA),
                              el = as.numeric(NA),
                              L1S4 = as.numeric(NA),
                              ROTI = as.numeric(NA),
                              PRN = as.numeric(NA))
      Output.DF <- rbind(Output.DF, Input.DF)
      Output.DF <- Output.DF[order(Output.DF$time.stamp),]
      print("... done!")
    } else {
      Output.DF <- Input.DF
    } 
    
    return(Output.DF)
  }
  
  Input.File.Names <- list.files(path = Input.folder)
  N.files <- length(Input.File.Names)
  for (nf in 1:N.files) {
    print(Input.File.Names[nf])
    Data <- read.delim(paste0(Input.folder,"/",Input.File.Names[nf]))
    Data$time.stamp <-  as.POSIXct(strptime(Data$time.stamp, format = "%Y-%m-%d %H:%M"), tz = "GMT")
    if (length(Data$PRN) > sum(is.na(Data$PRN))) {
      Data <- Data[which(Data$el > 0),]
      Data <- Data[which(Data$PRN <= 37),]
      Data$ROTI[which(Data$ROTI == 0)] <- NA
      month <- as.numeric(substr(Input.File.Names[nf],16,17))
      year <- as.numeric(substr(Input.File.Names[nf],10,13))
      Full.Data <- Full.Data <- Data[,c(8:11,17,20)]#Gaps.SI(Data[,c(8:11,17,20)],year,month)
    } else {
      Full.Data <- Data[,c(8:11,17,20)]
    }
    
    Out.File.name <- paste0("eqTime_SI_",Input.File.Names[nf])
    write.table(Full.Data, Out.File.name,
                col.names = T, row.names = F,
                quote = F, sep = "\t",)
  }
}

Saving.SI.data(Input.folder.SI)

