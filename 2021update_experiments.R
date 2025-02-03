library(sf)
library(data.table)
library(readxl)

shp_files <- st_read("data/Region Shapes - 2021/SA1_2021_AUST.shp")
b <- as.data.table(shp_files)
b <- b[!SA1_CODE21 == "ZZZZZZZZZZZ"]

ind <- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Indicators_derived/ejapp_indicators.csv")


#pops <- readxl::read_xls("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Indicators_derived/livability_melbourne_2021_sa1.csv")
#popmelb <- pops[, c("SA1_CODE", "tot_p_p_2021")]

c <- b[, .(SA1_CODE = as.numeric(SA1_CODE21), SA4_NAME = SA4_NAME21, STE_CODE = as.integer(STE_CODE21), STE_NAME = STE_NAME21)]
#c$SA1_CODE <- as.integer(c$SA1_CODE)
#c <- merge(c, auo, by = c("SA1_CODE"), all.x=T)
#c <- merge(c, popmelb, by = c("SA1_CODE"), all.x=T)
d <- b[, .(SA2_CODE = as.integer(SA2_CODE21), SA2_NAME = SA2_NAME21, SA4_NAME = SA4_NAME21, STE_CODE = as.integer(STE_CODE21), STE_NAME = STE_NAME21)]
d <- merge(d, ind, by = c("SA2_CODE", "STE_CODE", "STE_NAME", "SA4_NAME"), all.x=T)
#d <- merge(d, ind, by = c("SA2_CODE", "STE_CODE", "STE_NAME", "SA4_NAME"), all.x=T)
d <- unique(d)

e <- b[, .(SA3_CODE = as.integer(SA3_CODE21), SA4_NAME = SA4_NAME21, STE_CODE = as.integer(STE_CODE21), STE_NAME = STE_NAME21)]

#e <- merge(e, d[, c("SA2_CODE", "usual_resident_population")], by = c("SA2_CODE"), all.x=T)
e <- unique(e)
#ep <- e[,.(SA4_NAME, STE_CODE, STE_NAME, usual_resident_population = sum(usual_resident_population, na.rm=T)), by = c("SA3_CODE")]
#ep <- unique(ep)


f <- b[, .(SA4_CODE = as.integer(SA4_CODE21), SA4_NAME = SA4_NAME21, STE_CODE = as.integer(STE_CODE21), STE_NAME = STE_NAME21)]
f <- unique(f)

# f <- na.omit(f)
# f <- merge(f, d[, c("SA2_CODE", "usual_resident_population")], by = c("SA2_CODE"), all.x=T)
#
# fp <- f[,.(STE_CODE, STE_NAME, usual_resident_population = sum(usual_resident_population, na.rm=T)), by = c("SA4_NAME")]
# fp <- unique(fp)

write.csv(as.data.frame(c), "data/indicators/sa1 - Indicators.csv")
write.csv(as.data.frame(unique(d)), "data/indicators/sa2 - Indicators.csv")
write.csv(as.data.frame(unique(e)), "data/indicators/sa3 - Indicators.csv")
write.csv(as.data.frame(unique(f)), "data/indicators/sa4 - Indicators.csv")

csv_File <- builtinData[["sa2"]][["indicators"]]
sa2_csvId <- grep("^SA2_", names(csv_File), value = TRUE)
if (any(grepl("CODE", sa2_csvId))) {
  csv_File[['csvId']] <- csv_File[[sa2_csvId[grepl("CODE", sa2_csvId)]]]
}
csv_File <- merge(csv_File, builtinData$sa2$indicators, by.x = "csvId", by.y = "SA1_CODE", all.x = TRUE)
cols_sa1 <- c("SA1_CODE")

#<- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Indicators_derived/ejapp_indicators.csv")

# ind <- unlist(ind$irsd_score)
# rowSums(ind[,c("age_0_5", "irsd_score")])



k <- as.data.table(csv_File)
j <- k[STE_NAME == "Outside Australia"]

setnames(b, "SA1_CODE21", "SA1_CODE")

m <-as.data.frame(nall$sa2$indicators)


#ind <- na.omit(ind)
ind <- merge(d[STE_CODE == 2], ind, by = c("SA2_CODE", "STE_CODE", "STE_NAME"))
ind <- unique(ind)
ind$SA1_CODE <- as.integer(ind$SA1_CODE)
write.csv(ind, "C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Indicators_derived/ejapp_indicators.csv")

m <- as.data.frame(ind)
setnames(m, "SA2_MAIN", "SA2_CODE")#, "pm25_weighted_mean","age_0_5",,"X", "no2_weighted_mean", "life_expectancy" ,  "age_65_over""irsd_score",

o <- as.data.frame(nall$sa3$indicators)

p <- as.data.frame(nall$sa2$indicators)



write.csv(ind, "C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Indicators_derived/ejapp_indicators.csv")

nall$sa1$indicators <- b
nall$sa2$indicators <- m
nall$sa3$indicators <- o
nall$sa4$indicators <- p


saveRDS(nall, "data/builtinData.rds")

a <- read.csv("data/sa1 - data (NLAS).csv")

auo <- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Indicators_derived/livability_melbourne_2021_sa1.csv")
n(unique(auo$SA1_MAIN))
setnames(auo, "SA1_MAIN", "SA1_CODE")
auo$State <- "VIC"
auo$STE_CODE <- "2"
auo$STE_NAME <- "Victoria"
write.csv(auo, "C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Indicators_derived/livability_melbourne_2021_sa1.csv")

pairs.panels(ind,
             smooth = FALSE,
             scale = FALSE,
             density = TRUE,
             ellipses = TRUE,
             method = "pearson",
             pch = 21,
             lm = FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = "lightblue",
             stars = TRUE,
             rug = TRUE,
             ci = TRUE,
             cex = 1,
             cex.axis = 1.5,
             cex.labels = 2.5)



