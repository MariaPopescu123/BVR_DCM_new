#inflow and outflow 

#if first time running this script run these lines
#d1 <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/454/8/5edb79c2676d7b351d9eec184799c7dd")
write.csv(d1, "CSVs/raw_discharge.csv", row.names = FALSE)

d1 <- read.csv("CSVs/raw_discharge.csv")

d2 <- d1|>
  filter(Reservoir == "BVR")

#ope it would appear we only have data for 2019