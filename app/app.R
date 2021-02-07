# Libraries
# ADD THEM HERE

# Interested in rows: TYPE OF PRODUCER,ENERGY SOURCE equal to Total Electric Power Industry,Total
# [You] should convert the STATE, TYPE OF PRODUCER, and ENERGY SOURCE to categorical values
# setwd("/home/jovyan/work/CS-424_Project-01/app")
setwd("/Users/mziminski/Developer/School Projects/cs424/CS-424_Project-01/app")
data <- read.csv(file = "annual_generation_state.csv", TRUE, sep = ",")
# Get header column names
colnames(data)

# Reformat the header names
colnames(data)[colnames(data) == "TYPE.OF.PRODUCER"] <- "TYPE_OF_PRODUCER"
colnames(data)[colnames(data) == "ENERGY.SOURCE"] <- "ENERGY_SOURCE"
colnames(data)[colnames(data) == "GENERATION..Megawatthours."] <- "GENERATION"
# Reformat the GENERATION col from char to dbl
data$GENERATION <- as.numeric(gsub(",", "", data$GENERATION))
# fix Mislabeled US-TOTAL (US-Total)
data$STATE <- toupper(data$STATE)
# factorize categories
data$STATE <- as.factor(data$STATE)
data$TYPE_OF_PRODUCER <- as.factor(data$TYPE_OF_PRODUCER)
data$ENERGY_SOURCE <- as.factor(data$ENERGY_SOURCE)
# Remove missIdentified & negative data
data <- subset(data, data$STATE != "  ")
data <- subset(data, data$GENERATION > 0)
# Remove references to Other, Other Gases, Other Biomass, Pumped Storage
# to_drop <- c("Other", )
data <- subset(data, data$ENERGY_SOURCE != "Other")
data <- subset(data, data$ENERGY_SOURCE != "Other Gases")
data <- subset(data, data$ENERGY_SOURCE != "Other Biomass")
data <- subset(data, data$ENERGY_SOURCE != "Pumped Storage")
droplevels(data)
levels(data$ENERGY_SOURCE)


# Coouldn't find 3 states with missing identifyers

# Mislabeled US-TOTAL: 38888-38959, 40907-40980, 42963-43035, 45016-45087, 47087-47159, 49184-49256, 51259-51368, 53416-53489, 

