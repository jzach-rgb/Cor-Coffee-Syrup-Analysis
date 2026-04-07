## TBD ##
# Things to do:
#   Find out which milk is best with UBE and lav (DONE)
#   Find out differnce in sales between Hot vs Iced (DONE)
#   Find out when Ube and lavender sell the most (COMPLETE)
      # Date (DONE)
      # Time (DONE)
#   Find out how many people from last year are repeat drinkers of lavender(DONE)
#   Compare it with sales from last year & Trends over time in sales from before(DONE)
#   Compare ube and lav sales and revenue to revenue without ube and lav (DONE)
#   Are the customers that order special Syrup differnt kinds of customers (DONE)
# Are the customers that order special Syrup differnt kinds of customers (DONE)

# Do everything again but with pistachio incorporated(7/7 COMPLETE) 
# Replace R Plots with ggplots (5/5 COMPLETE)

# Add chi sqr test for milk prefrence (DONE)
# Add “Is temperature preference independent of flavor?” Hypo test (DONE)
# What predicts whether someone chooses Ube, Lavender, or Pistachio ? (DONE)


# Lavender is labled as Lavnder latte in 2021-2022  use unique(df20212022$Item) ********************************************************
# 

#if van sales decrease w lav would overall sales decrease
library(tidyverse)
install.packages("patchwork")
library(patchwork)
install.packages("nnet")
library(nnet)

getwd()
setwd("C:/Users/Joshu/OneDrive/Desktop/CorCoffee")

df <- read.csv("ubelav.csv")
df

## Cleaning the data
df[0,]
head(df)
# Date                  Time                  Time.Zone             Category              Item                  Qty                   Price.Point.Name     
# SKU                   Modifiers.Applied     Gross.Sales           Discounts             Net.Sales             Tax                   Transaction.ID       
# Payment.ID            Device.Name           Notes                 Details               Event.Type            Location              Dining.Option        
# Customer.ID           Customer.Name         Customer.Reference.ID Unit                  Count                 Itemization.Type      Fulfillment.Note     
# Channel               Token                 Card.Brand            PAN.Suffix 



dfclean <- df[,c("Date", "Time", "Item", "Qty", "Net.Sales", "Category", "Modifiers.Applied", "PAN.Suffix")]
dfclean
sum(is.na(dfclean))

dfclean$Item[236]
unique(dfclean$Item) ## Types of drinks we've sold Feb-Mar
# [1] "Latte"           "Cafe Au Lait"    "Cappuccino"      "Taos Bar"       
# [5] "Tea"             "Mocha"           "Chai Latte"      "LiaP Cookie"    
# [9] "Americano"       "Matcha Latte"    "Flat White"      "Cold Brew"      
# [13] "Coffee"          "Cortado"         "London Fog"      "Lemonade"       
# [17] "Hot Chocolate"   "Espresso"        "Macchiato"       "Soda"           
# [21] "Red Eye"         "Breve Special 😘" "Kind Bar"        "Flowers"        
# [25] "Matcha lemonade"

NumLatteSales <- sum(dfclean$Item == 'Latte')
NumMatchaSales <- sum(dfclean$Item == 'Matcha Latte')


## Creating a dataframe, results to keep track of sold ube and lav by drink ##
results <- dfclean %>% 
  group_by(Item) %>% 
  summarise(
    Ube = sum(Qty[grepl("Ube", Modifiers.Applied, ignore.case = TRUE)]),
    Lavender = sum(Qty[grepl("Lavender", Modifiers.Applied, ignore.case = TRUE)]),
    Pistachio = sum(Qty[grepl("Pistachio", Modifiers.Applied, ignore.case = TRUE)])
  ) %>% 
  arrange(desc(Ube + Lavender + Pistachio))

## creating Ube vs Latte Sales by Drink (Feb - Mar) ##
results_long <- pivot_longer(results,
                             cols = c("Ube", "Lavender", "Pistachio"),
                              names_to = "Flavor",
                             values_to = "Count")

results_long_filtered <- results_long[results_long$Count > 0, ]


ggplot(results_long_filtered, aes(x = Item, y = Count, fill = Flavor)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Ube" = "purple", "Lavender" = "#4FC3F7", "Pistachio" = "lightgreen")) +
      labs(title = "Syrup Sales by Drink (Feb - Mar)",
           x = "Drink", y = "Count") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## What milk goes the best with Ube and Lav??? ##
unique(dfclean$Modifiers.Applied)

# List of Milks:
# Whole
# Oat
# Almond
# Soy
# Coconut
# Half&Half
dfclean$Modifiers.Applied[!grepl("Whole|Oat|Almond|Soy|Coconut|Half&Half", dfclean$Modifiers.Applied, ignore.case = TRUE)] <- 
  paste(dfclean$Modifiers.Applied[!grepl("Whole|Oat|Almond|Soy|Coconut|Half&Half", dfclean$Modifiers.Applied, ignore.case = TRUE)], "Whole")

milkNames <- c("Whole", "Oat", "Almond", "Soy", "Coconut", "Half&Half")
syrupFalvors <- c("Ube", "Lavender", "Pistachio")
milks <- matrix(0, nrow = length(milkNames), ncol = length(syrupFalvors))

for(i in 1:length(milkNames))
{
  for(j in 1:length(syrupFalvors))
  {
    milks[i,j] <- sum(dfclean$Qty[
                      grepl(milkNames[i], dfclean$Modifiers.Applied, ignore.case = TRUE) 
                      & grepl(syrupFalvors[j], dfclean$Modifiers.Applied, ignore.case = TRUE)])
  }
}

colnames(milks) <- syrupFalvors
rownames(milks) <- milkNames
milks

milkdf <- as.data.frame(milks)
milkdf$Milk <- rownames(milkdf)
milk_long <- pivot_longer(milkdf, 
                          cols = c("Ube", "Lavender", "Pistachio"),
                          names_to = "Flavor",
                          values_to = "Count")
ggplot(milk_long, aes(x = Milk, y = Count, fill = Flavor)) +
      geom_col(position = "dodge") +
  scale_fill_manual(values = c("Ube" = "purple", "Lavender" = "#4FC3F7", "Pistachio" = "lightgreen")) +
  labs(title = "Milk Usage by Flavor", 
       x = "Milk", y = "Count") +
      theme_minimal()


## whole milk and Oat milk are the most preferred milks
Tmilks <- t(milks)

#           Whole   Oat    Almond   Soy   Coconut   Half&Half
# Ube        106    89     12       1     5         0
# Lavender    46    31      8       2     0         0
# Pistachio   47    28      8       0     1         0
milks_prop <- prop.table(Tmilks, margin = 2) ## Proportion of what milk goes best with each flavor
round(milks_prop, 3)

#           Whole      Oat    Almond   Soy     Coconut     Half&Half
# Ube       0.540      0.601  0.429    0.333   0.833       NaN
# Lavender  0.228      0.209  0.286    0.667   0.000       NaN
# Pistachio 0.233      0.189  0.286    0.000   0.167       NaN

syrup_prop <- prop.table(Tmilks, margin = 1) ## Proportion of what Syrup goes best with what milk
round(syrup_prop, 3)
#              Whole    Oat    Almond   Soy     Coconut   Half&Half
# Ube          0.505    0.412  0.056    0.005   0.023     0
# Lavender     0.529    0.356  0.092    0.023   0.000     0
# Pistachio    0.560    0.333  0.095    0.000   0.012     0


milkpropdf <- as.data.frame(t(milks_prop))
milkpropdf$Milk <- rownames(milkpropdf)
milkproplonger <- pivot_longer(milkpropdf,
                               cols = c("Ube", "Lavender", "Pistachio"),
                               names_to = "Flavor",
                               values_to = "Proportion")


ggplot(milkproplonger, aes(x = Milk, y = Proportion, fill = Flavor)) +
        geom_col(position = "fill") +
        scale_fill_manual(values = c("Ube" = "purple", "Lavender" = "#4FC3F7", "Pistachio" = "lightgreen")) +
        labs(title = "Milk Composition by Flavor",
             x = "Milk", y = "Proportion") +
  scale_y_continuous(breaks = seq(0,1, by = 0.10)) +
        theme_minimal()

Tmilks
milksclean <- milks[rowSums(milks) > 0,]
chisq.test(milksclean)
fisher.test(milksclean)


## Sales difference of Hot vs Iced ##

icedUbe <- sum(dfclean$Qty[grepl("Iced", dfclean$Modifiers.Applied , ignore.case = TRUE) &
                     grepl("Ube", dfclean$Modifiers.Applied , ignore.case = TRUE)])
hotUbe <- sum(dfclean$Qty[grepl("Hot", dfclean$Modifiers.Applied , ignore.case = TRUE) &
                    grepl("Ube", dfclean$Modifiers.Applied , ignore.case = TRUE)])


icedLav <- sum(dfclean$Qty[grepl("Iced", dfclean$Modifiers.Applied , ignore.case = TRUE) &
                     grepl("Lavender", dfclean$Modifiers.Applied , ignore.case = TRUE)])
hotLav <- sum(dfclean$Qty[grepl("Hot", dfclean$Modifiers.Applied , ignore.case = TRUE) &
                    grepl("Lavender", dfclean$Modifiers.Applied , ignore.case = TRUE)])

icedPis <- sum(dfclean$Qty[grepl("Iced", dfclean$Modifiers.Applied , ignore.case = TRUE) &
                             grepl("Pistachio", dfclean$Modifiers.Applied , ignore.case = TRUE)])
hotPis <- sum(dfclean$Qty[grepl("Hot", dfclean$Modifiers.Applied , ignore.case = TRUE) &
                            grepl("Pistachio", dfclean$Modifiers.Applied , ignore.case = TRUE)])

totalUbeTemp <- c(Hot = hotUbe, iced = icedUbe)
totalLavTemp <- c(Hot = hotLav, iced = icedLav)
totalPisTemp <- c(Hot = hotPis, iced = icedPis)

temp_mat <- matrix(c(icedUbe, hotUbe,
                     icedLav, hotLav, 
                     icedPis, hotPis), nrow = 2, ncol = 3, byrow = FALSE)
rownames(temp_mat) <- c("Iced", "Hot")
colnames(temp_mat) <- c("Ube", "Lavender", "Pistachio")

tempdf <- as.data.frame(temp_mat)
tempdf$Temp <- rownames(tempdf)
templonger <- pivot_longer(tempdf,
                       cols = c("Ube", "Lavender", "Pistachio"),
                       names_to = "Flavor",
                       values_to = "Count"
                       )
countTemp <- ggplot(templonger, aes(x = Temp, y = Count, fill = Flavor)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Ube" = "purple", "Lavender" = "#4FC3F7", "Pistachio" = "lightgreen")) +
  labs(title = "Temperature preference Counts of Syrups",
       x = "Temperature", y = "Count") +
  scale_y_continuous(breaks = seq(0,200, by = 10)) +
  theme_minimal()





temp_prop <- prop.table(temp_mat, margin = 2)
round(temp_prop,3)

temppropdf <- as.data.frame(temp_prop)
temppropdf$Temp <-rownames(temppropdf)
tempproplonger <- pivot_longer(temppropdf,  
                               cols = c("Ube", "Lavender", "Pistachio"),
                               names_to = "Flavor",
                               values_to = "Proportion")

proptemp <- ggplot(tempproplonger, aes(x = Temp, y = Proportion, fill = Flavor)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Ube" = "purple", "Lavender" = "#4FC3F7", "Pistachio" = "lightgreen")) +
  labs(title = "Temperature Preference Proportions of Syrups",
       x = "Temperature", y = "Proportion") +
  scale_y_continuous(breaks = seq(0,1, by = 0.1)) +
  theme_minimal()

countTemp + proptemp

chisq_result <- chisq.test(temp_mat)
chisq_result$expected

# Expected counts: All above 5
# Ube Lavender Pistachio
# Iced 145.95868  61.1157  53.92562
# Hot   57.04132  23.8843  21.07438


# Ube Lavender Pistachio
# Iced 155       59        47
# Hot   48       26        28
# Pearson's Chi-squared test
# 
# data:  temp_mat
# X-squared = 5.4192, df = 2, p-value = 0.06656



## When does Ube and Lavender sell the most?? (DATE WISE) ##
dfclean$Month <- as.numeric(substr(dfclean$Date, 6, 7))

UbeFebDay <- as.numeric(substr(dfclean$Date
                               [dfclean$Month == 2 & 
                                grepl("Ube", dfclean$Modifiers.Applied, ignore.case = TRUE) ], 9, 10)) # Raw data of sales made in February containing Ube

UbeMarDay <- as.numeric(substr(dfclean$Date
                               [dfclean$Month == 3 & 
                                grepl("Ube", dfclean$Modifiers.Applied, ignore.case = TRUE) ], 9, 10)) # Raw data of sales made in March containing Ube

LavMarDay <- as.numeric(substr(dfclean$Date
                               [dfclean$Month == 3 &
                                grepl("Lavender", dfclean$Modifiers.Applied, ignore.case = TRUE) ], 9, 10)) # Raw data of sales made in March containing Lavender (No Feb because Lav came out in March)

PisMarDay <- as.numeric(substr(dfclean$Date
                               [dfclean$Month == 3 &
                                   grepl("Pistachio", dfclean$Modifiers.Applied, ignore.case = TRUE) ], 9, 10)) # Raw data of sales made in March containing Pistachio (No Feb because Pis came out in March)



monthcount <- table(dfclean$Month) # Total sales by month

Ubefebdaycount <- table(UbeFebDay, dnn = "Ube Sales Made in Febuary") #Ube sales by month
Ubemardaycount <- table(UbeMarDay, dnn = "Ube Sales Made in March")

Lavmardaycount <- table(LavMarDay, dnn = "Lavender Sales Made in March")

Pismardaycount <- table(PisMarDay, dnn = "Pistachio Sales Made in March")


days <- 1:31
# Collecting number of times an ube product was sold on a day in Feb and Mar
TFebUbeDC <- Ubefebdaycount[as.character(days)]
TMarUbeDC <- Ubemardaycount[as.character(days)]

TFebUbeDC[is.na(TFebUbeDC)]<- 0
TMarUbeDC[is.na(TMarUbeDC)]<- 0


names(TMarUbeDC) <- days

# Collecting number of times a Lavender product was sold on a day in Mar
#Total (Month) (Syrup) Daily Count
TMarLavDC <- Lavmardaycount[as.character(days)]

TMarLavDC[is.na(TMarLavDC)]<- 0

names(TMarLavDC) <- days


# Collecting number of times a Pistachio product was sold on a day in Mar
TMarPisDC <- Pismardaycount[as.character(days)]
TMarPisDC[is.na(TMarPisDC)]<- 0
names(TMarPisDC) <- days


#Cumulatives of feb and mar of Ube and mar of Lav
cSumUbeFeb <- cumsum(TFebUbeDC)
cSumUbeMar <- cumsum(TMarUbeDC)
cSumLavMar <- cumsum(TMarLavDC)
cSumPisMar <- cumsum(TMarPisDC)


MarchDf <- data.frame(Day = 1:31, Ube = cSumUbeMar, Lavender = cSumLavMar, Pistachio = cSumPisMar)
march_long <- pivot_longer(MarchDf, 
                           cols = c("Ube", "Lavender", "Pistachio"),
                           names_to = "Flavor",
                           values_to = "CumulativeSales")


## Plot of Ube and Lavender in March
plot2 <- ggplot(march_long, aes(x = Day, y = CumulativeSales, color = Flavor)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) + 
  scale_color_manual(values =  c("Ube" = "purple", "Lavender" = "#4FC3F7", "Pistachio" = "lightgreen")) +
  labs(title = "Cumulative Syrup Sales in March",
       x = "Day of Month",
       y = "Cumulative Sales") +
  theme_minimal()

FebDF <- data.frame(Day = 1:31, Ube = cSumUbeFeb)
feb_long <- pivot_longer(FebDF,
                         cols = "Ube",
                         names_to = "Flavor",
                         values_to = "CumlativeSales")

## Plot Ube in Febuary
plot1 <- ggplot(feb_long, aes(x = Day, y = CumlativeSales, color = Flavor)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) +
  scale_color_manual(values = ("Ube" = "purple")) +
  labs(title = "Cumulative Spring Syrup sales in Febuary",
       x = "Day of Month",
       y = "Cumulative Sales")+
  ylim(0,115) +
  theme_minimal()

plot1 + plot2

## When does Ube and Lavender sell the most?? (TIME WISE) ##

dfclean$hour <- as.numeric(substr(dfclean$Time, 1, 2))
corHours <- table(dfclean$hour)

dfclean$hour
ubeHours <- dfclean$hour[grepl("Ube", dfclean$Modifiers.Applied, ignore.case = TRUE)]
lavHours <- dfclean$hour[grepl("Lavender", dfclean$Modifiers.Applied, ignore.case = TRUE)]
pisHours <- dfclean$hour[grepl("Pistachio", dfclean$Modifiers.Applied, ignore.case = TRUE)]



ube_df <- data.frame(hour = ubeHours)
lav_df <- data.frame(hour = lavHours)
pis_df <- data.frame(hour = pisHours)
hourdf <- data.frame(
                  hour = c(ubeHours, lavHours, pisHours),
                   Flavor = c(rep("Ube", length(ubeHours)), rep("Lavender", length(lavHours)), rep("Pistachio", length(pisHours)))
                  )
ggplot(hourdf, aes(x = hour, fill = Flavor, color = Flavor)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("Ube" = "purple", "Lavender" = "#4FC3F7", "Pistachio" = "lightgreen")) +
  scale_color_manual(values = c("Ube" = "purple", "Lavender" = "#4FC3F7", "Pistachio" = "lightgreen")) +
  labs(title = "Spring Syrup Sales by Hour",
       x = "Hour of Day",
       y = "Density") +
  theme_minimal()

################################################################
# What percent of customers from previous years are buying coffee?

# load data
df20242025 <- read.csv("item2024-2025.csv")
df20232024 <- read.csv("item2023-2024.csv")
df20222023 <- read.csv("item2022-2023.csv")
df20212022 <- read.csv("item2021-2022.csv")

lavendercount2026 <- sum(grepl("Lavender", dfclean$Modifiers.Applied, ignore.case = TRUE)) # 95
lavendercount2025 <- sum(grepl("Lavender", df20242025$Modifiers.Applied, ignore.case = TRUE)) # 868
lavendercount2024 <- sum(grepl("Lavender", df20232024$Modifiers.Applied, ignore.case = TRUE)) #782
lavendercount2023 <- sum(grepl("Lavender", df20222023$Modifiers.Applied, ignore.case = TRUE)) # 441
sum(grepl("Lavender", df20212022$Modifiers.Applied, ignore.case = TRUE)) # 0
# Looking through the data, we only started selling lavender from March 2023

unique(df20212022$Item)

# Decding on what Variable to use:
sum(df$Customer.ID == "") # contains 2189 empty obs
sum(df$Customer.Reference.ID == "") # contains 3000 empty obs
sum(df$PAN.Suffix == "")  # contains 174 empty obs
nrow(df) # out of 3002 obs total:
# CID has 27% of customers
# CRID has 0.006% of customes
# PAN suffix has 94.20% of customers
#Use PAN (potential probelm customer uses differnt cards)

panLav2023 <- df20222023$PAN.Suffix[grepl("Lavender", df20222023$Modifiers.Applied, ignore.case = TRUE)]
panLav2024 <- df20232024$PAN.Suffix[grepl("Lavender", df20232024$Modifiers.Applied, ignore.case = TRUE)]
panLav2025 <- df20242025$PAN.Suffix[grepl("Lavender", df20242025$Modifiers.Applied, ignore.case = TRUE)]
panLav2026 <- dfclean$PAN.Suffix[grepl("Lavender", dfclean$Modifiers.Applied, ignore.case = TRUE)]

sum(is.na(panLav2023)) # 28 NA
sum(is.na(panLav2024)) # 0 NA
sum(is.na(panLav2025)) # 0 NA
sum(is.na(panLav2026)) # 0 NA

sum(panLav2023 == "") # 0 ""
sum(panLav2024 == "") # 79 ""
sum(panLav2025 == "") # 70 ""
sum(panLav2026 == "") # 12 ""

# data cleaning
panLav2023 <- panLav2023[panLav2023 != ""]
panLav2024 <- panLav2024[panLav2024 != ""]
panLav2025 <- panLav2025[panLav2025 != ""]
panLav2026 <- panLav2026[panLav2026 != ""]
panLav2023 <- formatC(panLav2023, width = 4, flag = "0") # Because there was a formating change in PAN numbers in 2023, need to add leading 0's to numbers that have less than 4 numebrs

# Who came back for Lavender?
length(intersect(panLav2023,panLav2024)) # 45 people from 2023 tried the lavender again in 2024
length(intersect(panLav2024,panLav2025)) # 64 people from 2024 tried the lavender again in 2025
length(intersect(panLav2025,panLav2026)) # 11 people from 2025 tried the lavender again in 2026 (although there is still about a month left in the semester)

length(intersect(intersect(panLav2023, panLav2024), panLav2025)) # 10 people came for 2023, 2024, and 2025
length(intersect(intersect(intersect(panLav2023, panLav2024), panLav2025), panLav2026)) # No one has come back for the years

length(intersect(panLav2023, panLav2024)) / length(panLav2023) * 100  # 10.86% of 2023 customers who returned in 2024
length(intersect(panLav2024, panLav2025)) / length(panLav2024) * 100  # 9.10% of 2024 who returned in 2025
length(intersect(panLav2025, panLav2026)) / length(panLav2025) * 100  # 1.38% of 2025 who returned in 2026

# Graphing data
return_df <- data.frame(Year = c("2023-2024", "2024-2025", "2025-2026"),
                        Returning = c(length(intersect(panLav2023,panLav2024)),
                                      length(intersect(panLav2024,panLav2025)), 
                                      length(intersect(panLav2025,panLav2026))),
                        Total = c(
                          length(panLav2023),
                          length(panLav2024),
                          length(panLav2025)),
                        RetentionRate = c(length(intersect(panLav2023, panLav2024)) / length(panLav2023) * 100,
                                          length(intersect(panLav2024, panLav2025)) / length(panLav2024) * 100,
                                          length(intersect(panLav2025, panLav2026)) / length(panLav2025) * 100)
                        )

returningP1 <- ggplot(return_df, aes(x = Year, y = Returning, group = 1)) +
                geom_line(linewidth = 1.2, color = "#4FC3F7") +
                geom_point(size = 3, color = "#4FC3F7") +
                ylim(10,70) +
                ylab("Number of Customers Returning from Previous Year") +
                labs(title = "Returning Lavender Customers Year after Year") +
                theme_minimal()

returningp2 <- ggplot(return_df, aes(x = Year, y = RetentionRate, group = 1)) +
                geom_line(linewidth = 1.2, color = "#4FC3F7") +
                geom_point(size = 3, color = "#4FC3F7") +
                ylab("Percent of Customers Returning from Previous Year (in %)") +
                labs(title = "Lavender Retention Rate (Year after Year)") +
                theme_minimal()
returningP1 + returningp2

panUbe2026 <- dfclean$PAN.Suffix[grepl("Ube", dfclean$Modifiers.Applied, ignore.case = TRUE)]
panPis2026 <- dfclean$PAN.Suffix[grepl("Pistachio", dfclean$Modifiers.Applied, ignore.case = TRUE)]

length(intersect(panLav2025,panUbe2026)) / length(panLav2025) #The percent of people that bought Lavender in 2025 that bough ube in 2026 is 3.383459%
length(intersect(panLav2025,panPis2026)) / length(panLav2025) #The percent of people that bought Lavender in 2025 that bough ube in 2026 is 1.00%
length(intersect(panLav2025,panLav2026)) / length(panLav2025) #The percent of people that bought Lavender in 2025 that bough Lavender in 2026 is 1.378446%

# People who bought lavender in 2025, show up with Ube in 2026, but NOT lavender in 2026
true_switchers <- intersect(panLav2025, panUbe2026)
true_switchers <- true_switchers[!true_switchers %in% panLav2026]
length(true_switchers)  # 2.8% of people that bought lavender in 2025 switched completely to Ube in 2026 


true_switchersPis <- intersect(panLav2025, panPis2026)
true_switchersPis <- true_switchersPis[!true_switchersPis %in% panLav2026]
length(true_switchersPis) # 0.63% of people that bought Lavender in 2025 switched compleltyly to Pistachio in 2026


#   Compare it with sales from last year & Trends over time in sales from before 

lavendercountsYeardf <- data.frame(Year = c("2023", "2024", "2025", "2026"),
                                   Counts = c(lavendercount2023,lavendercount2024,lavendercount2025,lavendercount2026))

ggplot(lavendercountsYeardf, aes(x = Year, y = Counts)) +
    geom_col(fill = "#4FC3F7") +
    labs(title = "Sales Trends of Lavender (2023-2026)",
        x = "Years", y = "Counts",
        caption = "*2026 data represents February-March only, full season runs through May") +
      scale_y_continuous(breaks = seq(0,1000, by = 50)) +
      theme_minimal()

#   Compare ube and lav sales and revenue to revenue without ube and lav

TotalRev <- sum(as.numeric(substr(dfclean$Net.Sales, 2, length(dfclean$Net.Sales))), na.rm = TRUE)


UbeLavPisRev <- dfclean$Net.Sales[grepl("Ube|Lavender|Pistachio", dfclean$Modifiers.Applied, ignore.case = TRUE)]
NoULPRev <- dfclean$Net.Sales[!grepl("Ube|Lavender|Pistachio", dfclean$Modifiers.Applied, ignore.case = TRUE)]


UbeLavPisRev <- as.numeric(substr(UbeLavPisRev, 2, length(UbeLavPisRev)))
NoULPRev <- as.numeric(substr(NoULPRev, 2, length(NoULPRev)))

SumSyrupsRev <- sum(UbeLavPisRev, na.rm = TRUE)
SumNoSyrupsRev<- sum(NoULPRev, na.rm = TRUE)

RevDF <- data.frame(
  Category = c("Syrup", "Non Spring Syrup"),
  Revenue = c(SumSyrupsRev, SumNoSyrupsRev)
)
ggplot(RevDF, aes(x = "", y = Revenue, fill = Category))+
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Spring Syrups Revenue") +
  theme_void()

SumSyrupsRev / TotalRev * 100 # Ube, Lavender, and Pistachio drinks account for 15.45% of the revenue at cor during the spring

# Customers spend $5.33 per transaction on drinks with Spring syrups 
# Customers spend $4.54  per transaction on drinks without Spring syrups 
mean(UbeLavPisRev, na.rm = TRUE) # 5.33
mean(NoULPRev, na.rm = TRUE) # 4.54






# Are the customers that order special Syrup differnt kinds of customers
dfclean$cleanNetSales <- as.numeric(substr(dfclean$Net.Sales, 2, length(dfclean$Net.Sales)))

dfclean$SyrupGroup <- ifelse(grepl("Ube|Lavender|Pistachio", dfclean$Modifiers.Applied, ignore.case = TRUE), 
                             "Syrup", "Non Syrup")
anova_result <- aov(cleanNetSales ~ SyrupGroup, data = dfclean)

plot(anova_result, 2)
kruskal.test(cleanNetSales ~ SyrupGroup, data = dfclean)
library(car)
leveneTest(cleanNetSales ~ SyrupGroup, data = dfclean)


summary(anova_result) #P-value of 9.73e-14 obtained. Transactions that contain Ube, Lavander, or Pistachio have a signifincatly higher value than transcations that don't

# What are the average transaction value on the days of Customers who have ordered Lavender and Ube before, but didn't vs all the non spring syrup customers

# Get unique PAN suffixes of syrup buyers
syrupPANs <- unique(dfclean$PAN.Suffix[grepl("Ube|Lavender|Pistachio", dfclean$Modifiers.Applied, ignore.case = TRUE)])

# Get all transactions by those customers that DON'T contain syrup
syrupCustomerNoSyrup <- dfclean[dfclean$PAN.Suffix %in% syrupPANs & 
                                  !grepl("Ube|Lavender|Pistachio", dfclean$Modifiers.Applied, ignore.case = TRUE), ]

# Compare mean transaction value
mean(syrupCustomerNoSyrup$cleanNetSales, na.rm = TRUE) # Customers that have gotten Ube or Lavender but for their drinks that don't inlcude them spend $3.91 on average
mean(NoULPRev, na.rm = TRUE)  # Customers that don't get Ube or Lavender spend $4.54 on average
#Since the syrupCustomerNoSyrup spend less, this means they don't usually spend more on average but the price is high most likely because of the syrup overcharge






# What predicts whether someone chooses Ube, Lavender, or Pistachio ?
dfclean$hour <- as.numeric(substr(dfclean$Time, 1, 2))
syrupdf <- dfclean[grepl("Ube|Lavender|Pistachio", dfclean$Modifiers.Applied, ignore.case =  TRUE),]

syrupdf$Flavor <- ifelse(grepl("Ube", syrupdf$Modifiers.Applied, ignore.case = TRUE), "Ube",
                          ifelse(grepl("Lavender", syrupdf$Modifiers.Applied, ignore.case = TRUE), "Lavender",
                                 "Pistachio"))
syrupdf$Flavor <- as.factor(syrupdf$Flavor)
syrupdf$Flavor <- relevel(syrupdf$Flavor, ref = "Ube")

syrupdf$IsIced <- ifelse(grepl("Iced", syrupdf$Modifiers.Applied, ignore.case = TRUE), 1, 0)

syrupdf$Milk <- case_when(
  grepl("Oat", syrupdf$Modifiers.Applied, ignore.case = TRUE) ~ "Oat",
  grepl("Almond", syrupdf$Modifiers.Applied, ignore.case = TRUE) ~ "Almond",
  grepl("Coconut", syrupdf$Modifiers.Applied, ignore.case = TRUE) ~ "Coconut",
  grepl("Soy", syrupdf$Modifiers.Applied, ignore.case = TRUE) ~ "Soy",
  grepl("Half&Half", syrupdf$Modifiers.Applied, ignore.case = TRUE) ~ "Half&Half",
  TRUE ~ "Whole"
)

syrupdf$Milk <- as.factor(syrupdf$Milk)



model <- multinom(Flavor ~ IsIced + hour + Milk, data = syrupdf)
summary(model)
z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- (1 - pnorm(abs(z), 0, 1)) * 2
p_values

#Conclusions: 
# The newer a Syrup is the more it will sell (Ube, lavender, and pistachio sales growth)
# Scarscity maintains demand because lavender remained to be pretty popular of a syrup
# Neither of the syrups attract inherentyl high spending custoemrs 
# The syurps do well because the customers want a treat and something in the season



## Below are Elenas requests

#LF count towards vanilla 
#if lf has another syrup replaces van 

dfmarch <- read.csv("marchsales.csv")
LF <- df[dfmarch$Item == "London Fog",]
LF$Item

LFHZC <- sum(grepl("Hazelnut", LF, ignore.case = TRUE))
LFCG <- sum(grepl("Ganache", LF, ignore.case = TRUE))
LFUC <- sum(grepl("Ube", LF, ignore.case = TRUE))
LFLC <- sum(grepl("Lavender", LF, ignore.case = TRUE))
LFPC <- sum(grepl("Pistachio", LF, ignore.case = TRUE))

hotchoc <- sum(grepl("Hot Chocolate", dfmarch, ignore.case = TRUE))

vancount <- sum(grepl("Vanilla", dfmarch$Modifiers.Applied, ignore.case = TRUE)) + (length(LF) - (LFHZC + LFCG+ LFUC+ LFLC+ LFPC))
hzlcount <- sum(grepl("Hazelnut", dfmarch$Modifiers.Applied, ignore.case = TRUE)) + LFHZC
cgcount <- sum(grepl("Ganache", dfmarch$Modifiers.Applied, ignore.case = TRUE)) + sum(dfclean$Item == "Mocha") + LFCG
ubecount <- sum(grepl("Ube", dfmarch$Modifiers.Applied, ignore.case = TRUE)) + LFUC
lavcount <- sum(grepl("Lavender", dfmarch$Modifiers.Applied, ignore.case = TRUE)) + LFLC
piscount <- sum(grepl("Pistachio", dfmarch$Modifiers.Applied, ignore.case = TRUE)) + LFPC



allSyrupdf <- data.frame(Syrup = c("Vanilla", "Hazelnut", "Ganache", "Ube", "Lavender", "Pistachio"),
                         Counts = c(vancount, hzlcount, cgcount, ubecount, lavcount, piscount))



syrupsdflonger <- pivot_longer(allSyrupdf,
                             cols = c("Syrup", "Counts"),
                             names_to = "Flavor",
                             values_to = "Count")


 ggplot(allSyrupdf, aes(x = Syrup, y = Counts)) +
   geom_col(position = "dodge") +
  geom_line(linewidth = 1.2, color = "#4FC3F7") +
  geom_point(size = 3, color = "#4FC3F7") +
  ylab("Counts") +
   scale_y_continuous(breaks = seq(0,400, by = 50)) +
  labs(title = "Counts of Sales of Syurps") +
  theme_minimal()

 
 hazeldf <- read_csv("hazel.csv")
 sum(grepl("Hazelnut", hazeldf$`Modifiers Applied`, ignore.case = TRUE))

 sum(grepl("Hazelnut", hazeldf$`Modifiers Applied`, ignore.case = TRUE)) / nrow(hazeldf)
 lavcount / nrow(dfclean) 

 maysales <- read_csv("maysales.csv")     
 sum(grepl("Hazelnut", maysales$`Modifiers Applied`, ignore.case = TRUE)) / nrow(maysales)
 sum(grepl("Lavender", maysales$`Modifiers Applied`, ignore.case = TRUE)) / nrow(maysales)
 nrow(maysales) 
 
 
 library(knitr)
 
 df <- data.frame(
   Syrup = c("Ube", "Pistachio", "Lavender"),
   Switchers = c(27, 8, 11),
   Lavender2025SwitchRate = c("3.38%", "1.00%", "1.38%")
 )
 
 kable(df)
 