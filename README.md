# SCINDA-R-scripts
R scripts to read SCINDA output files and extract ionospheric parameters (TEC, SI, positioning errors)

# R scripts ro read SCINDA output files #.scn and #.psn and extract TEC, S4, ROTI and position:

Reading_scn_zip_ files_v1.R
Extracting_TEC_from_SCN_dat_files_v1.R
Extracting_SI_from_SCN_dat_files_v1.R

Reading_psn_zip_ files_v1.R

# NB: before using scripts inport to the R Environment the file "DOYall_hours_length.txt". This file contain formation
on the names of the months and the length of the months for the leap and normal years, whic is used by scripts.

##########################################################################################################

## Reading #.scn files and extraction of TEC, S4, ROTI values with 1 min time resolution

### 1. use Reading_scn_zip_ files_v1.R to read #.scn.zip files and save the content as "SCN_data_YYYY_mMM.dat" files
 (YYYY - year, MM - month's number)

Input files: #.scn.zip fiules in a folder "D:/SCINDA/all data/.." or replace by correct address
Input folder may contain sub-folders

Output files: "SCN_data_YYYY_mMM.dat", YYYY - year, MM - month's number 
Output files are saved in the current R working folder (check by getwd())

Output files header: 
year	
month	
day	
sec.f.midnight (seconds from midnight)	
h (hour)
m (minute)	
s (second)
time.stamp (format = "%Y-%m-%d %H:%M:%S)
az (azimuth of satellite, in degrees) 
el (elevation of satellite, in degrees) 
L1S4 (Scintillation intensity index on L1) 
pc.samp.l1 (% samples taken compared to number expected, 0-100%)
L2S4 (Scintillation intensity index on L2)
pc.samp.l2 (% samples taken compared to number expected, 0-100%) 
TECP (differential pseudorange, in TECU) 
TECF (differential carrier phase, in TECU) 
ROTI (Std of rate of change of TEC over one minute)
TECR (relative (uncalibrated) TEC, in TECU) 
t.since.last.cycle.slip (time since last time slip, minutes) 
PRN (pseudorandom noise satellite identifier)

##################################

### 2a. use Extracting_TEC_from_SCN_dat_files_v1.R to read SCN_data_YYYY_mMM.dat files to extract TEC data with 1 min time resolution

This scripts reads and cleans SCINDA TEC data:
- only GPS satellites are used, 
- only satellites with elevation >= 15º are used
- missing time epochs are identified and missing data are set as NA

Afterward,the TEC means for 1 min time intervals (for all selected satellites) are calculated

Input files: SCN_data_YYYY_mMM.dat files in a folder "D:/SCINDA/.." or replace by correct address
Input folder must NOT contain sub-folders

Output files: "SCN_TEC_1min_YYYY_mMM.dat", YYYY - year, MM - month's number - TEC data from all available GPS satellites for 1 min time intervals
Output files are saved in the current R working folder (check by getwd())

Output files header:
time.stamp	
TECR (relative/uncalibrated TEC, in arb. u)

##################################

### 2b. use Extracting_SI_from_SCN_dat_files_v1.R to read SCN_data_YYYY_mMM.dat files to extract extract scintillation indices (SI) L1S4 and ROTI data 
with 1 min time resolution

SCINDA SI data are cleaned:
- only GPS satellites, 
- only satellites with elevation >= 0º (el = 0 - missing/eroneous data),
- only satellites with ROTI > 0 (ROTI = 0 - missing/eroneous data),
- missing time epochs are identified and missing data are set as NA

Input files: SCN_data_YYYY_mMM.dat files in a folder "D:/SCINDA/.." or replace by correct address
Input folder must NOT contain sub-folders

Output files: "eqTime_SI_SCN_data_YYYY_mMM.dat", YYYY - year, MM - month's number - 
all SI data from all available GPS satellites for 1 min time intervals

Output files are saved in the current R working folder (check by getwd())

Output files header:
time.stamp	
az
el
L1S4
ROTI
PRN


##########################################################################################################
## Reading #.psn files and extraction of positions and positioning errors with 1 min time resolution

### 1. use Reading_psn_zip_ files_v1.R to read #.psn.zip files and save the content as "PSN_data_YYYY_mMM.dat" files
 (YYYY - year, MM - month's number)

Input files: #.psn.zip fiules in a folder "D:/SCINDA/all data/.." or replace by correct address
Input folder may contain sub-folders

Output files: "PSN_data_YYYY_mMM.dat", YYYY - year, MM - month's number - psn data as in the original SCINDA files
Output files: "eqTime_PSN_data_YYYY_mMM.dat", YYYY - year, MM - month's number  - psn data with 1 min time resolution 
(missing time epochs are identified and missing data are as NA)

Output files are saved in the current R working folder (check by getwd())

Output files header: 
time.stamp	
year	
month	
day	
sec.f.midnight (seconds from midnight)	
h (hour)
m (minute)	
s (second)
mean.x (mean latitude for the 1 min time interval, all available measurements)	
mean.y (mean longitude for the 1 min time interval, all available measurements)	
mean.z (mean height for the 1 min time interval, all available measurements)
sigma.x (standard deviation for latitude for the 1 min time interval, all available measurements)	
sigma.y (standard deviation for longitude for the 1 min time interval, all available measurements)	
sigma.z (standard deviation for height for the 1 min time interval, all available measurements)
