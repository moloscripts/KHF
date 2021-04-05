# Libraries ####
library(easypackages)
libraries("tidyverse", "dlookr","rKenyaCensus","skimr","moments","outliers","arsenal")
data("DataCatalogue")

# Data ####
KenyanHospitalsRAW <- read.csv("Data/2021Hospitals.csv", stringsAsFactors = T)
KenyanHospitalsRAW <- KenyanHospitalsRAW %>%
  rename(`Facility Type` = Facility_Type, 
         `Bed Capacity` = Bed_Capacity,
         `Facility Name` = Facility_Name)

# Kenya County Dataset, remove the Total count observation
kenyaPop <- V1_T2.2
KenyaCountyPop <- kenyaPop %>%
  filter(County != "Total") %>%
  arrange(desc(County))
View(KenyaCountyPop)
KenyaCountyPop$Observation <- 1:nrow(KenyaCountyPop)

CountyBedCapacity <- KenyanHospitals %>%
  select(`Bed Capacity`, County) %>%
  group_by(County) %>%
  summarise(`Bed Capacity` = sum(`Bed Capacity`)) %>%
  arrange(desc(County))
CountyBedCapacity$Observation <- 1:nrow(CountyBedCapacity)

# Merged DF with total Ke County Population and Bed Capacity per County
BedCapacity <- merge(CountyBedCapacity, KenyaCountyPop, by ="Observation")
BedCapacity <- BedCapacity %>%
  select(County.y, Male, Female, Intersex, Total, `Bed Capacity`) %>%
  rename(County = County.y)
  
# Bed Capacity Per 1000 Population
BedCapacity <- BedCapacity %>%
  mutate(`Bed Capacity Per 1000 Population` = round((`Bed Capacity`*1000)/Total, 2))

# Visual of Bed Capacity per 1000 Pop
ggplot(BedCapacity, aes(reorder(County,`Bed Capacity Per 1000 Population`), `Bed Capacity Per 1000 Population`)) +
  geom_col(fill="#f48b29") +
  labs(x="") +
  geom_text(aes(label=`Bed Capacity Per 1000 Population`), vjust=1, size=3) +
  theme_minimal() +
  coord_flip()

# Data Health ####
dim(KenyanHospitalsRAW)
str(KenyanHospitalsRAW)
length(unique((KenyanHospitalsRAW$`Facility Name`))) # Number of unique observations in the column
KenyanHospitalsRAW$`Facility Name`[duplicated(KenyanHospitalsRAW$`Facility Name`)] # Check duplicates of health facility Names
KenyanHospitals <- KenyanHospitalsRAW[!duplicated(KenyanHospitalsRAW$`Facility Name`), ] # Remove the duplicates

dim(KenyanHospitals)
str(KenyanHospitals)


# Descriptive Analysis ####
describe(KenyanHospitals)
sum(KenyanHospitals$`Bed Capacity`)
max(KenyanHospitals$`Bed Capacity`)
min(KenyanHospitals$`Bed Capacity`)
skewness(KenyanHospitals$`Bed Capacity`)
kurtosis(KenyanHospitals$`Bed Capacity`)
outlier(KenyanHospitals$`Bed Capacity`)

# Remove 'Major' Outliers. Which are 3
FinalKenyanHospitals <- KenyanHospitals %>%
  filter(`Bed Capacity` < 700)

kurtosis(FinalKenyanHospitals$`Bed Capacity`)
skim(FinalKenyanHospitals)


## Univariate Analysis ####

### Distribution of Bed Capacity ####
ggplot(FinalKenyanHospitals, aes(`Bed Capacity`)) + 
  geom_histogram(aes( y = ..density..), color="red", fill="#9ede73") + 
  stat_function(fun = dnorm, args = list(mean = mean(FinalKenyanHospitals$`Bed Capacity`),
                                         sd = sd(FinalKenyanHospitals$`Bed Capacity`))) + 
  theme_minimal()

qqnorm(FinalKenyanHospitals$`Bed Capacity`)
qqline(FinalKenyanHospitals$`Bed Capacity`)


### DF Count of Health Facility Types Country Wide ####
HF_Count <- KenyanHospitals %>%
  group_by(`Facility Type`) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percent = round(count/sum(count) * 100,1))
class(HF_Count)

#### Visual
KenyanHospitals %>%
  group_by(`Facility Type`) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percent = round(count/sum(count) * 100,1)) %>%
  ggplot(aes(x=reorder(`Facility Type`, count), y=count)) + 
  geom_col(fill="#ffe268") +
  labs(x="") +
  coord_flip() + 
  theme_minimal()


### DF Count of Various Levels 
HF_levels <- KenyanHospitals %>%
  group_by(Level) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(`Percentage (%)` = round(count/sum(count) * 100,1))


