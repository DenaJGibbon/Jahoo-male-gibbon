library(stringr)
library(tuneR)

BirdNETOutput <- list.files("/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/data/BirdNETOutput/",
           recursive = T, full.names = T)


# Efficiently read and combine using lapply with error handling
BirdNETOutputCombinedTest <- do.call(rbind, lapply(seq_along(BirdNETOutput), function(a) {
  cat("Processing file:", a, "/n")
  tryCatch({
    TempFile <- read.delim(BirdNETOutput[a])
    TempFile$TempName <- str_split_fixed(basename(BirdNETOutput[a]), pattern = '.BirdNET', n = 2)[,1]
    return(TempFile)
  }, error = function(e) {
    cat("Error in file:", BirdNETOutput[a], " - Skipping./n")
    return(NULL)
  })
}))

# Check results
head(BirdNETOutputCombinedTest)

BirdNETOutputCombinedTestSubset <-
  subset(BirdNETOutputCombinedTest, Species.Code!= 'nocall' &
           Confidence > 0.9)

unique(BirdNETOutputCombinedTestSubset)
nrow(BirdNETOutputCombinedTestSubset)
table(BirdNETOutputCombinedTestSubset$Species.Code)
hist(BirdNETOutputCombinedTestSubset$Confidence)

UniqueFiles <- unique((BirdNETOutputCombinedTestSubset$Begin.Path))

for(e in 1:length(UniqueFiles)){
 TempDF <-  subset(BirdNETOutputCombinedTestSubset,
         Begin.Path==UniqueFiles[e])

 TempWav <- readWave(UniqueFiles[e])

 WavNameShrt <- str_split_fixed(basename(UniqueFiles[e]),pattern = '.wav',n=2)[,1]

 uniquesignals <- unique(TempDF$Species.Code)

 for(f in 1:length(uniquesignals)){

 TempDFSubset <-  subset(TempDF,
                          Species.Code==uniquesignals[f])
 Dir1 <- paste('/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/data/BirdNETdetects/', '/',uniquesignals[f],'/',sep = '')
 dir.create(Dir1,recursive = T)

 short.sound.files <- lapply( 1:nrow(TempDFSubset),
                              function(g)
                                extractWave(
                                  TempWav,
                                  from = TempDFSubset$Begin.Time..s.[g],
                                  to = TempDFSubset$End.Time..s.[g],
                                  xunit = c("time"),
                                  plot = F,
                                  output = "Wave"
                                ))


 for(h in 1:length(short.sound.files)){

   writeWave(short.sound.files[[h]],paste(Dir1,'/',WavNameShrt,'_',
                                          TempDFSubset$Confidence[h],'_',
                                          uniquesignals[f],'_',WavNameShrt,'_',h, '.wav', sep=''),
             extensible = F)
 }

 }
}
