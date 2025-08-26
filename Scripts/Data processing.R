library(stringr)
library(tuneR)
# Discovered a mislabeled sample: Noise_R1025_20220508_090003_2905
# BoxPaths <- list.files('/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustics Gibbon PAM_04_03_23',
#                        recursive = T, full.names = T)

BoxPaths <-c("/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustics Gibbon PAM_04_03_23/SD 003/R1060_000/R1060_2023-03-16/R1060_20230316_060002.wav",
  "/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustics Gibbon PAM_04_03_23/SD 003/R1060_000/R1060_2023-03-22/R1060_20230322_060002.wav",
  "/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustic Gibbon PAM_27_03_24/SD004/R1025_000/R1025_2024-02-20/R1025_20240220_060002.wav",
  "/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustic Gibbon PAM 27_09_22/SD25/R1060_000/R1060_2022-09-15/R1060_20220915_060002.wav",
  "/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustic Gibbon PAM_27_03_24/SD006/R1062_000/R1062_2024-01-14/R1062_20240114_060002.wav",
  "/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustic Gibbon PAM_27_03_24/SD006/R1062_000/R1062_2024-03-17/R1062_20240317_060002.wav",
  "/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustic Gibbon PAM_27_03_24/SD006/R1062_000/R1062_2024-03-17/R1062_20240317_080003.wav",
  "/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustics Gibbon PAM_04_03_23/SD 003/R1060_000/R1060_2023-01-23/R1060_20230123_060002.wav")

# Read in selection tables
SelectionTables <- list.files('/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/data/Male Gibbon Calls Selection Tables',
           full.names = T,recursive = T)

SelectionTablesShort <- basename(SelectionTables)

SelectionTablesShort <-
  str_split_fixed(SelectionTablesShort,pattern = 'R1',n=2)[,2]

SelectionTablesShort <-
  paste('R1',SelectionTablesShort,sep='')

SelectionTablesShort <-
  str_split_fixed(SelectionTablesShort,pattern = '.Table',n=2)[,1]

OutputDir <- 'data/acousticdata/male/'

for(a in 1:length(SelectionTables)){
  TempTable <- read.delim(SelectionTables[a])
  TempTable$FileName <- SelectionTablesShort[a]
  TempTable <- subset(TempTable, Notes =='H')
 WavName <-  BoxPaths[which(str_detect(BoxPaths,SelectionTablesShort[a]))]

 TempWav <- readWave(WavName)

 short.sound.files <- lapply( 1:nrow(TempTable),
                              function(b)
                                extractWave(
                                  TempWav,
                                  from = TempTable$Begin.Time..s.[b],
                                  to = TempTable$End.Time..s.[b],
                                  xunit = c("time"),
                                  plot = F,
                                  output = "Wave"
                                ))


 for(c in 1:length(short.sound.files)){

   writeWave(short.sound.files[[c]],paste(OutputDir,'/',
                                          TempTable$Notes[c],'_',SelectionTablesShort[a],'_',c, '.wav', sep=''),
             extensible = F)
 }

}
