

library(dplyr)
library(tibble)

getwd()
setwd("/Users/sakshirathi/ColorectalTumourClassification")


################ OA training sample ##########################


OAmaf$vaf_sg <- CRCTumorOA1maf$vaf

data_OA1 <- OAmaf %>%
  filter(logLikelihood > 4 , logLikelihood != 'inf', 0.01 < vaf, vaf < 0.2) %>%
  mutate(label = if_else(vaf_sg > 0.15 & vaf_sg > 0 , "true_variant", "technical_artifact")) %>%
  select (-c(vaf, vaf_sg))


OAmaf$vaf_sg <- CRCTumorOA2maf$vaf

data_OA2 <- OAmaf %>%
  filter(logLikelihood > 4 , logLikelihood != 'inf', 0.01 < vaf, vaf < 0.2) %>%
  mutate(label = if_else(vaf_sg > 0.15 & vaf_sg > 0 , "true_variant", "technical_artifact")) %>%
  select (-c(vaf, vaf_sg))
  
OAmaf$vaf_sg <- CRCTumorOA3maf$vaf

data_OA3 <- OAmaf %>%
  filter(logLikelihood > 4 , logLikelihood != 'inf', 0.01 < vaf, vaf < 0.2) %>%
  mutate(label = if_else(vaf_sg > 0.15 & vaf_sg > 0 , "true_variant", "technical_artifact")) %>%
  select (-c(vaf, vaf_sg))

OAmaf$vaf_sg <- CRCTumorOA4maf$vaf

data_OA4 <- OAmaf %>%
  filter(logLikelihood > 4 , logLikelihood != 'inf', 0.01 < vaf, vaf < 0.2) %>%
  mutate(label = if_else(vaf_sg > 0.15 & vaf_sg > 0 , "true_variant", "technical_artifact")) %>%
  select (-c(vaf, vaf_sg))

dim(data_OA1)
table(data_OA1$label)
prop.table(table(data_OA1$label))
View(data_OA1)

dim(data_OA2)
table(data_OA2$label)
prop.table(table(data_OA2$label))
View(data_OA2)

dim(data_OA3)
table(data_OA3$label)
prop.table(table(data_OA3$label))
View(data_OA3)

dim(data_OA4)
table(data_OA4$label)
prop.table(table(data_OA4$label))
View(data_OA4)

write.csv(data_OA4, file = "data_OA4.csv", row.names = FALSE)

############## OB testing sample ##################

OBmaf$vaf_sg <- CRCTumorOB1maf$vaf

data_OB <- OBmaf %>%
  filter(logLikelihood > 4 , logLikelihood != 'inf', 0.01 < vaf, vaf < 0.2) %>%
  mutate(label = if_else(vaf_sg > 0.15 & vaf_sg > 0 , "true_variant", "technical_artifact")) %>%
  select (-c(vaf, vaf_sg))
  
dim(data_OB)
table(data_OB$label)
prop.table(table(data_OB$label))
write.csv(data_OB, file = 'data_OB.csv', row.names = FALSE)

################ UA sample #####################

UAmaf$vaf_sg <- CRCTumorUA_Lmaf$vaf

data_UA <- UAmaf %>%
  filter(logLikelihood > 4 , logLikelihood != 'inf', 0.01 < vaf, vaf < 0.2) %>%
  mutate(label = if_else(vaf_sg > 0.15 & vaf_sg > 0 , "true_variant", "technical_artifact")) %>%
  select (-c(vaf, vaf_sg))
  

dim(data_UA)

table(data_UA$label)

prop.table(table(data_UA$label))

write.csv(data_UA, file = 'data_UA.csv', row.names = FALSE)

  
############## UB sample ######################

UBmaf$vaf_sg <- CRCTumorUB_Lmaf$vaf

data_UB <- UBmaf %>%
  filter(logLikelihood > 4 , logLikelihood != 'inf', 0.01 < vaf, vaf < 0.2) %>%
  mutate(label = if_else(vaf_sg > 0.15 & vaf_sg > 0 , "true_variant", "technical_artifact")) %>%
  select (-c(vaf, vaf_sg))

dim(data_UB)

table(data_UB$label)

prop.table(table(data_UB$label))

write.csv(data_UB, file = 'data_UB.csv', row.names = FALSE)

################ Merge all data ######################


Data <- rbind(data_OA4,data_OB,data_UA,data_UB)

Data <- data.frame(Data)

View(Data)

dim(Data)

table(Data$label)

prop.table(table(Data$label))

write.csv(Data, file = 'Data.csv', row.names = FALSE)







