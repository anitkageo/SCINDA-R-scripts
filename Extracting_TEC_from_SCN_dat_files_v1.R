################################################################################################
# Script to read SCN_data_YYYY_mMM.dat files and extract TEC data
#
# SCINDA TEC data are cleaned:
# - only GPS satellites, 
# - only satellites with elevation >= 15º,
# - missing time epochs are identified and missing data are set as NA
#
# and means for 1 min time intervals (for all selected satellites) are calculated
#
# Input files: SCN_data_YYYY_mMM.dat files in a folder "D:/SCINDA/.." or replace by correct address
# Input folder must NOT contain sub-folders
#
# Output files: "SCN_TEC_1min_YYYY_mMM.dat", YYYY - year, MM - month's number - 
# TEC data from all available GPS satellites for 1 min time intervals
#
# Output files are saved in the current R working folder (check by getwd())
# Output files header:time.stamp	TECR (relative/uncalibrated TEC, in arb. u)
#
################################################################################################

Clean.TEC <- function(Input.DF) {
  clean.data <- Input.DF[1,]
  clean.data[1,] <- NA
  for (prn.N in 1:37) {
    L <-  length(Input.DF[which(Input.DF$PRN == prn.N),1])
    if (L > 3) {
      c <- hist(Input.DF$TECR[Input.DF$PRN == prn.N], plot = F)
      a <- c$counts
      b <- c$breaks
      n <- which(ceiling(a/L*100) >= 4 )
      TEC_thr <- b[n[1]]
      clean.data <- rbind(clean.data,Input.DF[which((Input.DF$PRN == prn.N) &
                                                      (Input.DF$TECR >= TEC_thr)),])
    }
  }
  clean.data <- clean.data[-1,]
  clean.data.sorted <- clean.data[order(clean.data$time.stamp, clean.data$PRN),]
  row.names(clean.data.sorted) <- c(1:length(clean.data.sorted[,1]))
  return(clean.data.sorted)
}

Gaps <- function(Input.DF,year, month) {
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
  } else {
    Output.DF <- Input.DF
  } 
  
  return(Output.DF)
}

Means.TEC <- function(Input.DF,
                      Param,
                      Aver.Time = "1 min", 
                      Time.Format = "%Y-%m-%d %H:%M") {
  
  aver.DF <- aggregate(data.frame(TECR = Input.DF[,Param]),
                       data.frame(time.stamp = cut(Input.DF$time.stamp, 
                                                   Aver.Time)),
                       FUN = "mean",
                       na.rm = T)
  aver.DF$TECR <- round(aver.DF$TECR, 2)
  aver.DF$time.stamp <- as.character(aver.DF$time.stamp)
  aver.DF$time.stamp <- as.POSIXct(strptime(aver.DF$time.stamp, format = Time.Format), tz = "GMT")
  aver.DF$TECR[which(is.nan(aver.DF$TECR))] <- NA
  return(aver.DF)
}

# un-comment following line or replace with correct address of a folder with SCN_dat_*.dat files:
# Input.File.SCN <- "D:/SCINDA/"

Input.File.SCN <- list.files(path = My.folders.SCN,pattern = ".dat")

for (ff in 1:length(Input.File.SCN)) {
  year <- substr(Input.File.SCN[ff], 10,13)
  month <- as.numeric(substr(Input.File.SCN[ff],16,17))
  print(paste(year, month))
  
  DF <- read.delim(paste0(My.folders.SCN,"/",Input.File.SCN[ff]))
  DF$time.stamp <- as.POSIXct(strptime(DF$time.stamp, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")
  if (sum(is.na(DF$TECR)) != length(DF$TECR)) {
    DF <- DF[which(DF$PRN <= 37),] # only GPS
    DF <- DF[which(DF$el >= 15),] # only elevation >= 15º
    
    clean.DF <- Clean.TEC(DF)
    print("cleaned")
    
    DF.1min <- Means.TEC(clean.DF,
                         "TECR",
                         "1 min")
    DF.1min <- Gaps(DF.1min, year,month)
    print("... done!")
    
    month.string <- paste0(month)
    if (month < 10) {month.string <- paste0("0", month.string)}
    
    Output.File.Name <- paste0("SCN_TEC_1min_",year,"_m",month.string,".dat")
    write.table(DF.1min, Output.File.Name,
                col.names = T, row.names = F,
                quote = F, sep = "\t")
    print("saved")

    rm(DF)
    suppressWarnings(rm(clean.DF))
    rm(DF.1min)
  } else {
    print("no data for this month!")
    DF.1min <- DF[,c("time.stamp", "TECR")]
    month.string <- paste0(month)
    if (month < 10) {month.string <- paste0("0", month.string)}
    
    Output.File.Name <- paste0("SCN_TEC_1min_",year,"_m",month.string,".dat")
    write.table(DF.1min, Output.File.Name,
                col.names = T, row.names = F,
                quote = F, sep = "\t")
    print("saved")
    
    rm(DF)
    rm(DF.1min)
  }
} # end reading 1 file
rm(Input.File.SCN)