library(gibbonNetR)
library(seewave)

FullFiles <- list.files("/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/data/acousticdata_forResNet",
           recursive = TRUE, full.names = T)

for(a in 1:length(FullFiles)){
 TempName <- FullFiles[a]
 TempWave <-  readWave(TempName)

 if(str_detect(TempName,'male Nomascus gabriellae _ male crested gibbon')==TRUE){
 TempDur <- duration(TempWave)
 DurSequence <- seq(0,TempDur-1,1)
 } else {
  TempDur <- duration(TempWave)
 DurSequence <- seq(0,TempDur,1)
 }
 for(b in 1: (length(DurSequence) -1)){

   tempShorWav <-
     cutw(TempWave,from=DurSequence[b],to=DurSequence[b+1],output = 'Wave')

   TempNameNew <- sub("\\.wav$", paste('_',b,".wav",sep=''), TempName)
   TempNameNew <- str_replace(TempNameNew,pattern = '/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/data/acousticdata_forResNet/',
               replace='/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/data/acousticdata_forResNet1s/')

   writeWave(tempShorWav,TempNameNew,extensible = F)
}
}

# Create spectrogram images from your labeled .wav files
spectrogram_images(
  trainingBasePath = "/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/data/acousticdata_forResNet1s/",
  outputBasePath = "/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/data/acousticdata_images1s/",
  minfreq.khz = 0.5,
  maxfreq.khz = 3.0,
  random = FALSE,
  splits = c(0.6, 0.2, 0.2), # Adjust training, validation, and test split as needed
  new.sampleratehz = "NA"
)


# Location of spectrogram images for training
input.data.path <-  'data/acousticdata_images1s/'

# Location of spectrogram images for testing
test.data.path <- 'data/acousticdata_images1s/test/'

# User specified training data label for metadata
trainingfolder.short <- 'jahooMF'

# Specify the architecture type
architecture <- c('resnet50') # Choose 'alexnet', 'vgg16', 'vgg19', 'resnet18', 'resnet50', or 'resnet152'

# We can specify the number of epochs to train here
epoch.iterations <- c(5)

# Function to train a multi-class CNN
gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture =architecture,
                            learning_rate = 0.001,
                            class_weights = c(0.4, 0.4,  0.2),
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            early.stop = "yes",
                            output.base.path = "/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/ResNet_model/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "noise")


'/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/testdata'


# Specify model path
ModelPath <- '/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/ResNet_model/_jahooMF_multi_unfrozen_TRUE_/_jahooMF_5_resnet50_model.pt'

Labels <- dput(list.files(test.data.path <- 'data/acousticdata_images1s/test/'))

# Deploy trained model over sound files
deploy_CNN_multi(
  clip_duration = 1,
  architecture = "resnet50",
  output_folder = '/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/ResNet_automatedetection/images/',
  output_folder_selections = '/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/ResNet_automatedetection/selections/',
  output_folder_wav = '/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/ResNet_automatedetection/wavs/',
  detect_pattern = NA,
  top_model_path = ModelPath,
  path_to_files = '/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/testdata',
  downsample_rate = "NA",
  save_wav = T,
  class_names = Labels,
  noise_category = "noise",
  single_class = FALSE,
  threshold = .1,
  max_freq_khz = 3
)
