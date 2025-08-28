library(ggpubr)
library(ggplot2)

# Define IoU function
iou_interval <- function(a_start, a_end, b_start, b_end) {
  inter <- max(0, min(a_end, b_end) - max(a_start, b_start))
  union <- (a_end - a_start) + (b_end - b_start) - inter
  if (union <= 0) return(NA_real_)
  inter / union
}

ManualAnnotations <- list.files('/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/testdata_annotations',
           full.names = T)


# ------------------------------- ResNet analysis -------------------------------

# IoU for 1-D time intervals (same helper used elsewhere)
iou_interval <- function(a_start, a_end, b_start, b_end) {
  inter <- max(0, min(a_end, b_end) - max(a_start, b_start))
  union <- (a_end - a_start) + (b_end - b_start) - inter
  if (union <= 0) return(NA_real_)
  inter / union
}

# Safe mean helpers (avoid warnings when vectors are empty / all zeros)
safe_mean_all  <- function(x) if (length(x)) mean(x) else NA_real_
safe_mean_hits <- function(x) {
  if (!length(x) || all(x == 0)) return(NA_real_)
  mean(x[x > 0])
}

# ResNet file list --------------------------------------------------------------
ResNet50Output <- list.files(
  '/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/ResNet_automatedetectionfull/selections',
  full.names = TRUE
)

# ------------------------------ First sound file -------------------------------
# Manual annotations (file 1)
ManualAnnotationDF <- read.delim(ManualAnnotations[1])

## ------------------------------ Males (file 1) --------------------------------
ManualAnnotationDFM <- subset(ManualAnnotationDF, Class == 'M')
ResNet50DFM <- read.delim(ResNet50Output[7])

IoUlist_M1 <- vector("list", length = nrow(ManualAnnotationDFM))
for (a in seq_len(nrow(ManualAnnotationDFM))) {
  # Manual event times
  man_start <- ManualAnnotationDFM$Begin.Time..s.[a]
  man_end   <- ManualAnnotationDFM$End.Time..s.[a]

  # Any intersection counts as overlap: (res_start < man_end) & (res_end > man_start)
  overlaps <- with(ResNet50DFM,
                   Begin.Time..s. < man_end & End.Time..s. > man_start)

  if (!any(overlaps)) {
    IoU <- 0  # missed detection -> IoU = 0 (keeps mean inclusive of misses)
  } else {
    # Keep your original behavior: use the FIRST overlapping ResNet event
    # (If you prefer the BEST IoU, see the note below)
    res_sub  <- ResNet50DFM[overlaps, , drop = FALSE]
    res_start <- res_sub$Begin.Time..s.[1]
    res_end   <- res_sub$End.Time..s.[1]
    IoU <- iou_interval(man_start, man_end, res_start, res_end)
  }
  IoUlist_M1[[a]] <- IoU
}

mean_IoU_incl_misses_M1 <- safe_mean_all(unlist(IoUlist_M1))  # misses as 0
mean_IoU_hits_only_M1   <- safe_mean_hits(unlist(IoUlist_M1)) # matches only
ManualAnnotationDFM$BestIoU_ResNet <- unlist(IoUlist_M1)

ResNetM1_misses <- mean_IoU_incl_misses_M1
ResNetM1_hits <- mean_IoU_hits_only_M1
cat(sprintf("\n[File 1 • Males] Mean IoU (incl. misses=0): %.3f\n", mean_IoU_incl_misses_M1))
cat(sprintf("[File 1 • Males] MeanIoU_hits: %.3f\n",  mean_IoU_hits_only_M1))

## ----------------------------- Females (file 1) -------------------------------
ManualAnnotationDFF <- subset(ManualAnnotationDF, Class == 'F')
ResNet50DFF <- read.delim(ResNet50Output[2])

IoUlist_F1 <- vector("list", length = nrow(ManualAnnotationDFF))
for (a in seq_len(nrow(ManualAnnotationDFF))) {
  man_start <- ManualAnnotationDFF$Begin.Time..s.[a]
  man_end   <- ManualAnnotationDFF$End.Time..s.[a]

  overlaps <- with(ResNet50DFF,
                   Begin.Time..s. < man_end & End.Time..s. > man_start)

  if (!any(overlaps)) {
    IoU <- 0
  } else {
    res_sub  <- ResNet50DFF[overlaps, , drop = FALSE]
    res_start <- res_sub$Begin.Time..s.[1]
    res_end   <- res_sub$End.Time..s.[1]
    IoU <- iou_interval(man_start, man_end, res_start, res_end)
  }
  IoUlist_F1[[a]] <- IoU
}

mean_IoU_incl_misses_F1 <- safe_mean_all(unlist(IoUlist_F1))
mean_IoU_hits_only_F1   <- safe_mean_hits(unlist(IoUlist_F1))
ManualAnnotationDFF$BestIoU_ResNet <- unlist(IoUlist_F1)

cat(sprintf("\n[File 1 • Females] Mean IoU (incl. misses=0): %.3f\n", mean_IoU_incl_misses_F1))
cat(sprintf("[File 1 • Females] MeanIoU_hits: %.3f\n",  mean_IoU_hits_only_F1))

# ------------------------------ Second sound file ------------------------------
# Manual annotations (file 2)
ManualAnnotationDF <- read.delim(ManualAnnotations[2])

## ------------------------------ Males (file 2) --------------------------------
ManualAnnotationDFM <- subset(ManualAnnotationDF, Class == 'M')
ResNet50DFM <- read.delim(ResNet50Output[10])

IoUlist_M2 <- vector("list", length = nrow(ManualAnnotationDFM))
for (a in seq_len(nrow(ManualAnnotationDFM))) {
  man_start <- ManualAnnotationDFM$Begin.Time..s.[a]
  man_end   <- ManualAnnotationDFM$End.Time..s.[a]

  overlaps <- with(ResNet50DFM,
                   Begin.Time..s. < man_end & End.Time..s. > man_start)

  if (!any(overlaps)) {
    IoU <- 0
  } else {
    res_sub  <- ResNet50DFM[overlaps, , drop = FALSE]
    res_start <- res_sub$Begin.Time..s.[1]
    res_end   <- res_sub$End.Time..s.[1]
    IoU <- iou_interval(man_start, man_end, res_start, res_end)
  }
  IoUlist_M2[[a]] <- IoU
}

mean_IoU_incl_misses_M2 <- safe_mean_all(unlist(IoUlist_M2))
mean_IoU_hits_only_M2   <- safe_mean_hits(unlist(IoUlist_M2))
ManualAnnotationDFM$BestIoU_ResNet <- unlist(IoUlist_M2)

cat(sprintf("\n[File 2 • Males] Mean IoU (incl. misses=0): %.3f\n", mean_IoU_incl_misses_M2))
cat(sprintf("[File 2 • Males] MeanIoU_hits: %.3f\n",  mean_IoU_hits_only_M2))

## ----------------------------- Females (file 2) -------------------------------
ManualAnnotationDFF <- subset(ManualAnnotationDF, Class == 'F')
ResNet50DFF <- read.delim(ResNet50Output[5])

IoUlist_F2 <- vector("list", length = nrow(ManualAnnotationDFF))
for (a in seq_len(nrow(ManualAnnotationDFF))) {
  man_start <- ManualAnnotationDFF$Begin.Time..s.[a]
  man_end   <- ManualAnnotationDFF$End.Time..s.[a]

  overlaps <- with(ResNet50DFF,
                   Begin.Time..s. < man_end & End.Time..s. > man_start)

  if (!any(overlaps)) {
    IoU <- 0
  } else {
    res_sub  <- ResNet50DFF[overlaps, , drop = FALSE]
    res_start <- res_sub$Begin.Time..s.[1]
    res_end   <- res_sub$End.Time..s.[1]
    IoU <- iou_interval(man_start, man_end, res_start, res_end)
  }
  IoUlist_F2[[a]] <- IoU
}

mean_IoU_incl_misses_F2 <- safe_mean_all(unlist(IoUlist_F2))
mean_IoU_hits_only_F2   <- safe_mean_hits(unlist(IoUlist_F2))
ManualAnnotationDFF$BestIoU_ResNet <- unlist(IoUlist_F2)

cat(sprintf("\n[File 2 • Females] Mean IoU (incl. misses=0): %.3f\n", mean_IoU_incl_misses_F2))
cat(sprintf("[File 2 • Females] MeanIoU_hits: %.3f\n",  mean_IoU_hits_only_F2))

mean_IoU_incl_misses_F2

resNetvals <- cbind.data.frame(mean_IoU_incl_misses_M1,mean_IoU_hits_only_M1,
                 mean_IoU_incl_misses_F1,mean_IoU_hits_only_F1,
                 mean_IoU_incl_misses_M2,mean_IoU_hits_only_M2,
                 mean_IoU_incl_misses_F2,mean_IoU_hits_only_F2)
# ---------------------------- BirdNET analysis ----------------------------

# IoU for 1-D time intervals
iou_interval <- function(a_start, a_end, b_start, b_end) {
  inter <- max(0, min(a_end, b_end) - max(a_start, b_start))
  union <- (a_end - a_start) + (b_end - b_start) - inter
  if (union <= 0) return(NA_real_)
  inter / union
}

# Best IoU for a manual event vs BirdNET 3-s tiles by taking the UNION
# of all BirdNET tiles that overlap the manual event.
best_iou_for_manual <- function(man_start, man_end, bird_df) {
  if (nrow(bird_df) == 0) return(0)
  overlaps <- with(bird_df,
                   `Begin.Time..s.` < man_end & `End.Time..s.` > man_start)
  if (!any(overlaps)) return(0)
  bird_start <- min(bird_df$`Begin.Time..s.`[overlaps])
  bird_end   <- max(bird_df$`End.Time..s.`[overlaps])
  iou_interval(man_start, man_end, bird_start, bird_end)
}

# Safe summary helpers
safe_mean_all  <- function(x) if (length(x)) mean(x) else NA_real_
safe_mean_hits <- function(x) {
  if (!length(x) || all(x == 0)) return(NA_real_)
  mean(x[x > 0])
}

# ------------------------------ Read in tables ----------------------------
BirdNETdetections <- list.files(
  '/Users/denaclink/Desktop/RStudioProjects/Jahoo-male-gibbon/BirdNET_automateddetection',
  full.names = TRUE
)

# --------------------------- BirdNET first sound file ---------------------
# Manual annotations (file 1)
ManualAnnotationDF <- read.delim(ManualAnnotations[1])

## Males
ManualAnnotationDFM <- subset(ManualAnnotationDF, Class == "M")
BirdNETDF  <- read.delim(BirdNETdetections[2])
BirdNETDFM <- subset(BirdNETDF, Common.Name == " male crested gibbon")

IoUlistBirdNETM <- mapply(
  best_iou_for_manual,
  ManualAnnotationDFM$`Begin.Time..s.`,
  ManualAnnotationDFM$`End.Time..s.`,
  MoreArgs = list(bird_df = BirdNETDFM)
)

mean_IoU_incl_misses_M1 <- safe_mean_all(IoUlistBirdNETM)          # misses as 0
mean_IoU_hits_only_M1   <- safe_mean_hits(IoUlistBirdNETM)          # matches only
ManualAnnotationDFM$BestIoU_BirdNET <- IoUlistBirdNETM

cat(sprintf("\n[File 1 • Males] Mean IoU (incl. misses=0): %.3f\n", mean_IoU_incl_misses_M1))
cat(sprintf("[File 1 • Males] MeanIoU_hits: %.3f\n", mean_IoU_hits_only_M1))

## Females
ManualAnnotationDFF <- subset(ManualAnnotationDF, Class == "F")
BirdNETDFF <- subset(BirdNETDF, Common.Name == " female crested gibbon")

IoUlistBirdNETF <- mapply(
  best_iou_for_manual,
  ManualAnnotationDFF$`Begin.Time..s.`,
  ManualAnnotationDFF$`End.Time..s.`,
  MoreArgs = list(bird_df = BirdNETDFF)
)

mean_IoU_incl_misses_F1 <- safe_mean_all(IoUlistBirdNETF)
mean_IoU_hits_only_F1   <- safe_mean_hits(IoUlistBirdNETF)
ManualAnnotationDFF$BestIoU_BirdNET <- IoUlistBirdNETF

cat(sprintf("\n[File 1 • Females] Mean IoU (incl. misses=0): %.3f\n", mean_IoU_incl_misses_F1))
cat(sprintf("[File 1 • Females] MeanIoU_hits: %.3f\n", mean_IoU_hits_only_F1))

# --------------------------- BirdNET second sound file --------------------
# Manual annotations (file 2)
ManualAnnotationDF <- read.delim(ManualAnnotations[2])

## Males
ManualAnnotationDFM <- subset(ManualAnnotationDF, Class == "M")
BirdNETDF  <- read.delim(BirdNETdetections[5])
BirdNETDFM <- subset(BirdNETDF, Common.Name == " male crested gibbon")

IoUlistBirdNETM <- mapply(
  best_iou_for_manual,
  ManualAnnotationDFM$`Begin.Time..s.`,
  ManualAnnotationDFM$`End.Time..s.`,
  MoreArgs = list(bird_df = BirdNETDFM)
)

mean_IoU_incl_misses_M2 <- safe_mean_all(IoUlistBirdNETM)
mean_IoU_hits_only_M2   <- safe_mean_hits(IoUlistBirdNETM)
ManualAnnotationDFM$BestIoU_BirdNET <- IoUlistBirdNETM

cat(sprintf("\n[File 2 • Males] Mean IoU (incl. misses=0): %.3f\n", mean_IoU_incl_misses_M2))
cat(sprintf("[File 2 • Males] MeanIoU_hits: %.3f\n", mean_IoU_hits_only_M2))

## Females
ManualAnnotationDFF <- subset(ManualAnnotationDF, Class == "F")
BirdNETDFF <- subset(BirdNETDF, Common.Name == " female crested gibbon")

IoUlistBirdNETF <- mapply(
  best_iou_for_manual,
  ManualAnnotationDFF$`Begin.Time..s.`,
  ManualAnnotationDFF$`End.Time..s.`,
  MoreArgs = list(bird_df = BirdNETDFF)
)

mean_IoU_incl_misses_F2 <- safe_mean_all(IoUlistBirdNETF)
mean_IoU_hits_only_F2   <- safe_mean_hits(IoUlistBirdNETF)
ManualAnnotationDFF$BestIoU_BirdNET <- IoUlistBirdNETF

cat(sprintf("\n[File 2 • Females] Mean IoU (incl. misses=0): %.3f\n", mean_IoU_incl_misses_F2))
cat(sprintf("[File 2 • Females] MeanIoU_hits: %.3f\n", mean_IoU_hits_only_F2))


BirdNETvals <- cbind.data.frame(mean_IoU_incl_misses_M1,mean_IoU_hits_only_M1,
                               mean_IoU_incl_misses_F1,mean_IoU_hits_only_F1,
                               mean_IoU_incl_misses_M2,mean_IoU_hits_only_M2,
                               mean_IoU_incl_misses_F2,mean_IoU_hits_only_F2)



# Combine into a table ----------------------------------------------------
# Nice table from BirdNETvals and resNetvals (each is a one-row data.frame)
# Columns expected like:
#   mean_IoU_incl_misses_M1, mean_IoU_hits_only_M1,
#   mean_IoU_incl_misses_F1, mean_IoU_hits_only_F1,
#   mean_IoU_incl_misses_M2, mean_IoU_hits_only_M2,
#   mean_IoU_incl_misses_F2, mean_IoU_hits_only_F2

library(dplyr)
library(tidyr)

# Helper: reshape one model's row into tidy long format, then parse fields
tidy_iou <- function(df, model_label) {
  df %>%
    mutate(.row = 1) %>%
    pivot_longer(-.row, names_to = "key", values_to = "value") %>%
    select(-.row) %>%
    # if names accidentally have ".1" from previous cbinds, strip them
    mutate(key = sub("\\.\\d+$", "", key)) %>%
    extract(
      key,
      into = c("metric", "sex", "file"),
      regex = "^mean_IoU_(incl_misses|hits_only)_(M|F)(\\d)$",
      remove = TRUE
    ) %>%
    mutate(
      Model = model_label,
      File  = paste("File", file),
      Sex   = sex,
      Metric = dplyr::recode(metric,
                             incl_misses = "MeanIoU_misses",
                             hits_only   = "MeanIoU_hits"
      ),
      .keep = "unused"
    ) %>%
    select(Model, File, Sex, Metric, value)
}

# Build tidy tables for each model
bird_tidy <- tidy_iou(BirdNETvals, "BirdNET")
res_tidy  <- tidy_iou(resNetvals,  "ResNet50")

# Combine models and present wide (two metric columns)
iou_table <- bind_rows(bird_tidy, res_tidy) %>%
  pivot_wider(
    names_from  = Metric,
    values_from = value
  ) %>%
  arrange(File, Sex, Model) %>%
  mutate(
    `MeanIoU_misses` = round(`MeanIoU_misses`, 3),
    `MeanIoU_hits` = round(`MeanIoU_hits`, 3)
  )

# View the nice table
iou_table

library(dplyr)
library(tidyr)
library(ggplot2)

# 1) reshape once
iou_long <- iou_table %>%
  pivot_longer(c(MeanIoU_misses, MeanIoU_hits),
               names_to = "Metric", values_to = "IoU") %>%
  mutate(
    Metric = recode(Metric,
                    MeanIoU_misses = "Mean IoU (misses=0)",
                    MeanIoU_hits   = "Mean IoU (hits only)"),
    Sex  = factor(Sex,  levels = c("F","M")),
    File = factor(File, levels = c("File 1","File 2"))
  )

ggplot(iou_long, aes(x = Sex, y = IoU, fill = Model)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_text(aes(label = sprintf("%.2f", IoU)),
            position = position_dodge(0.75), vjust = -0.4, size = 3) +
  facet_grid(Metric ~ File) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, .05))) +
  labs(x = NULL, y = "Mean IoU", fill = "Model") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")+scale_fill_manual(values = c('darkorange','darkgreen'))
