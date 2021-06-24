# Do Firearms Directly Impact Homicide Rates in Brazil?
# Vinicius Verza

###### Required Libraries ######

# Installing
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('DataExplorer')
install.packages('Cairo')
install.packages('tidyverse', dependencies=T)
install.packages('lubridate')
install.packages('gridExtra')
install.packages('grid')

# Running
library('dplyr')
library('ggplot2')
library('ggthemes')
library('DataExplorer')
library('Cairo')
library('tidyverse')
library('lubridate')
library('gridExtra')
library('grid')

###### Setting working directory ######
setwd("C:/Users/%username%/Desktop")

###### Opening the databases ######
total <- read.csv(file="homicidios.csv", head=TRUE, sep=",")
armas <- read.csv(file="homicidios-por-armas-de-fogo.csv", head=TRUE, sep=",")

###### > CLEANING DATASET < ######

###### First Look ######
head(Homicides)
nrow(Homicides)
summary(Homicides)

###### Ordering Columns by State ######
total <- total[order(total[,'nome']), ]
armas <- armas[order(armas[,'nome']), ]

###### Renaming columns ######
total <- total %>% rename('cod1' = 'cod', 'State' = 'nome', 'Year' = 'período', 'TotalRate' = 'valor')
armas <- armas %>% rename('cod2' = 'cod', 'Name2' = 'nome', 'Year2' = 'período', 'FirearmsRate' = 'valor')

###### Binding Datasets Together ######
combined <- cbind(total, armas)
print(combined)

###### Dropping Irrelevant Columns ######
Homicides <- select (combined,-c(cod1, cod2, Name2, Year2))
head(Homicides)

###### Saving Merged Files ######
write.csv(Homicides, file="Homicides.csv", row.names=FALSE)

###### Checking 'Zero' Values ######
Homicides[Homicides$TotalRate == 0,]
Homicides[Homicides$FirearmsRate == 0,]

###### Subseting TO State to Calculate Median From Populated Values ######
TOstateTotal <- Homicides$TotalRate[c(1025:1053)]
print(TOstateTotal)
TOstateArms <- Homicides$FirearmsRate[c(1025:1053)]
print(TOstateArms)

###### Calculating mMdian Number for TO State [Total and Firearms Rates] ######
ArmsMedian <- median(TOstateArms)
TotalMedian <- median(TOstateTotal)

###### Replacing 0 values from TO state to TO Median value ######
Homicides$TotalRate[c(1015:1024)] <- TotalMedian
Homicides$FirearmsRate[c(1015:1024)] <- ArmsMedian

###### Saving Cleaned Unique Dataset to Store Changes ######
write.csv(Homicides, file="Homicides.csv", row.names=FALSE)

###### Creating Regional Subsets ######

# States by Region
North <- subset(Homicides, State == 'AM' | State == 'RR'| State == 'AP' | State == 'PA' | State == 'TO' | State == 'RO' | State == 'AC')
Northeast <- subset(Homicides, State == 'MA' | State == 'PI'| State == 'CE' | State == 'RN' | State == 'PE' | State == 'PB' | State == 'SE' | State == 'AL' | State == 'BA')
Midwest <- subset(Homicides, State == 'MT' | State == 'MS'| State == 'GO'| State == 'DF')
Southeast <- subset(Homicides, State == 'SP' | State == 'RJ'| State == 'ES'| State == 'MG')
South <- subset(Homicides, State == 'PR' | State == 'RS'| State == 'SC')

# Regions by State
AllRegions <- Homicides %>%
  mutate(Region = case_when(
    State == 'AM' | State == 'RR'| State == 'AP' | State == 'PA' | State == 'TO' | State == 'RO' | State == 'AC' ~ "North",
    State == 'MA' | State == 'PI'| State == 'CE' | State == 'RN' | State == 'PE' | State == 'PB' | State == 'SE' | State == 'AL' | State == 'BA' ~ "Northeast",
    State == 'SP' | State == 'RJ'| State == 'ES'| State == 'MG' ~ 'Southeast',
    State == 'PR' | State == 'RS'| State == 'SC' ~ 'South',
    State == 'MT' | State == 'MS'| State == 'GO'| State == 'DF' ~ "Midwest")) %>%
  group_by(Region, Year) %>%
  summarize(TotalHomicides = sum(TotalRate), FirearmsHomicides = sum(FirearmsRate))

# Total Yearly by Region
NorthTotal <- subset(AllRegions, Region == 'North')
NortheastTotal <- subset(AllRegions, Region == 'Northeast')
MidwestTotal <- subset(AllRegions, Region == 'Midwest')
SoutheastTotal <- subset(AllRegions, Region == 'Southeast')
SouthTotal <- subset(AllRegions, Region == 'South')


###### > INSIGHTS AND ANALYSIS < ######

###### Insight 1 ######

# Correlation Between Total Homicides and Firearms Homicides
HomicidesCorrelation <- cor(Homicides$TotalRate,Homicides$FirearmsRate)
print(HomicidesCorrelation)


###### Insight 2 ######

# Plot 1 - Total Homicides and Firearms-only Homicides Compared
PLOT1 <- ggplot(AllRegions, aes(x = Year, y = TotalHomicides, color = Region)) +
  geom_line(aes(x=Year,y=FirearmsHomicides))+
  geom_line(size = 2) +
  ylab('Homicides') +
  scale_x_continuous(breaks = seq(1979, 2017, 1), 
                     limits=c(1979, 2017)) +
  scale_y_continuous(breaks = seq(0, 28000, 3000), 
                     limits=c(0, 28000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # angled year text
  ggtitle("Total Homicides and Firearms-only Homicides", subtitle = "Thick Line - Total Homicide Rate | Thin Line: Firearms-only Homicide Rate")

# Saving Plot 1
ggsave(PLOT1, filename = "plot1.pdf", 
       width = 10, height = 6, device = cairo_pdf)

# Plot 2 - Non-Firearms against Firearms-only Homicides Compared
PLOT2 <- ggplot(AllRegions, aes(x = Year, y = TotalHomicides-FirearmsHomicides, color = Region)) +
  geom_line(aes(x=Year,y=FirearmsHomicides))+
  geom_line(size = 2) +
  ylab('Homicides') +
  scale_x_continuous(breaks = seq(1979, 2017, 1), 
                     limits=c(1979, 2017)) +
  scale_y_continuous(breaks = seq(0, 22500, 3000), 
                     limits=c(0, 22500)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # angled year text
  ggtitle("Non-Firearms against Firearms-only Homicides", subtitle = "Thick Line - Homicide Rate without Firearms | Thin Line: Firearms-only Homicide Rate")

# Saving Plot 2
ggsave(PLOT2, filename = "plot2.pdf", 
       width = 10, height = 6, device = cairo_pdf)


###### Insight 3 ######

# Plot 3 - Correlation plot between Total Rate and Firearms
PLOT3 <- ggplot(Homicides) +
  aes(x = Year, y = TotalRate, colour = State, size = FirearmsRate) +
  geom_point() +
  scale_color_viridis_d(option = "plasma") +
  theme_minimal() +
  facet_wrap(vars(State), scales = "free_y") +
  ggtitle("Homicide Rates per State with Firearms Correlation")

# Saving plot 3
ggsave(PLOT3, filename = "plot3.pdf", 
       width = 14, height = 10, device = cairo_pdf)

###### Insight 4 ######

# Percentage Growth by Region and mean calculation before and after 2003

## MIDWEST ##

# Midwest Total
MWHomicidesGrowth = MidwestTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = TotalHomicides - lag(TotalHomicides),
         Rate_percent = (Diff_growth / Diff_year)/TotalHomicides * 100)
# Mean Total Growth Before and After 2003
MWTB2003 <- mean(MWHomicidesGrowth$Rate_percent[MWHomicidesGrowth$Year>=1988 & MWHomicidesGrowth$Year<=2002])
MWTA2003 <- mean(MWHomicidesGrowth$Rate_percent[MWHomicidesGrowth$Year>=2003 & MWHomicidesGrowth$Year<=2017])
# Midwest Firearms Only
MWFirearmsGrowth = MidwestTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = FirearmsHomicides - lag(FirearmsHomicides),
         Rate_percent = (Diff_growth / Diff_year)/FirearmsHomicides * 100)
# Mean Firearms Growth Before and After 2003
MWFB2003 <- mean(MWFirearmsGrowth$Rate_percent[MWFirearmsGrowth$Year>=1988 & MWFirearmsGrowth$Year<=2002])
MWFA2003 <- mean(MWFirearmsGrowth$Rate_percent[MWFirearmsGrowth$Year>=2003 & MWFirearmsGrowth$Year<=2017])

## NORTH ##

# North Total
NOHomicidesGrowth = NorthTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = TotalHomicides - lag(TotalHomicides),
         Rate_percent = (Diff_growth / Diff_year)/TotalHomicides * 100)
# Mean Total Growth Before and After 2003
NOTB2003 <- mean(NOHomicidesGrowth$Rate_percent[NOHomicidesGrowth$Year>=1988 & NOHomicidesGrowth$Year<=2002])
NOTA2003 <- mean(NOHomicidesGrowth$Rate_percent[NOHomicidesGrowth$Year>=2003 & NOHomicidesGrowth$Year<=2017])
# North Firearms Only
NOFirearmsGrowth = NorthTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = FirearmsHomicides - lag(FirearmsHomicides),
         Rate_percent = (Diff_growth / Diff_year)/FirearmsHomicides * 100)
# Mean Firearms Growth Before and After 2003
NOFB2003 <- mean(NOFirearmsGrowth$Rate_percent[NOFirearmsGrowth$Year>=1988 & NOFirearmsGrowth$Year<=2002])
NOFA2003 <- mean(NOFirearmsGrowth$Rate_percent[NOFirearmsGrowth$Year>=2003 & NOFirearmsGrowth$Year<=2017])

## NORTHEAST ##

# Northeast Total
NEHomicidesGrowth = NortheastTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = TotalHomicides - lag(TotalHomicides),
         Rate_percent = (Diff_growth / Diff_year)/TotalHomicides * 100)
# Mean Total Growth Before and After 2003
NETB2003 <- mean(NEHomicidesGrowth$Rate_percent[NEHomicidesGrowth$Year>=1988 & NEHomicidesGrowth$Year<=2002])
NETA2003 <- mean(NEHomicidesGrowth$Rate_percent[NEHomicidesGrowth$Year>=2003 & NEHomicidesGrowth$Year<=2017])
# Northeast Firearms Only
NEFirearmsGrowth = NortheastTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = FirearmsHomicides - lag(FirearmsHomicides),
         Rate_percent = (Diff_growth / Diff_year)/FirearmsHomicides * 100)
# Mean Firearms Growth Before and After 2003
NEFB2003 <- mean(NEFirearmsGrowth$Rate_percent[NEFirearmsGrowth$Year>=1988 & NEFirearmsGrowth$Year<=2002])
NEFA2003 <- mean(NEFirearmsGrowth$Rate_percent[NEFirearmsGrowth$Year>=2003 & NEFirearmsGrowth$Year<=2017])

## SOUTHEAST ##

# Southeast Total
SEHomicidesGrowth = SoutheastTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = TotalHomicides - lag(TotalHomicides),
         Rate_percent = (Diff_growth / Diff_year)/TotalHomicides * 100)
# Mean Total Growth Before and After 2003
SETB2003 <- mean(SEHomicidesGrowth$Rate_percent[SEHomicidesGrowth$Year>=1988 & SEHomicidesGrowth$Year<=2002])
SETA2003 <- mean(SEHomicidesGrowth$Rate_percent[SEHomicidesGrowth$Year>=2003 & SEHomicidesGrowth$Year<=2017])
# Southeast Firearms Only
SEFirearmsGrowth = SoutheastTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = FirearmsHomicides - lag(FirearmsHomicides),
         Rate_percent = (Diff_growth / Diff_year)/FirearmsHomicides * 100)
# Mean Firearms Growth Before and After 2003
SEFB2003 <- mean(SEFirearmsGrowth$Rate_percent[SEFirearmsGrowth$Year>=1988 & SEFirearmsGrowth$Year<=2002])
SEFA2003 <- mean(SEFirearmsGrowth$Rate_percent[SEFirearmsGrowth$Year>=2003 & SEFirearmsGrowth$Year<=2017])

## SOUTH ##

# South Total
SOHomicidesGrowth = SouthTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = TotalHomicides - lag(TotalHomicides),
         Rate_percent = (Diff_growth / Diff_year)/TotalHomicides * 100)
# Mean Total Growth Before and After 2003
SOTB2003 <- mean(SOHomicidesGrowth$Rate_percent[SOHomicidesGrowth$Year>=1988 & SOHomicidesGrowth$Year<=2002])
SOTA2003 <- mean(SOHomicidesGrowth$Rate_percent[SOHomicidesGrowth$Year>=2003 & SOHomicidesGrowth$Year<=2017])
# South Firearms Only
SOFirearmsGrowth = SouthTotal %>%
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year), 
         Diff_growth = FirearmsHomicides - lag(FirearmsHomicides),
         Rate_percent = (Diff_growth / Diff_year)/FirearmsHomicides * 100)
# Mean Firearms Growth Before and After 2003
SOFB2003 <- mean(SOFirearmsGrowth$Rate_percent[SOFirearmsGrowth$Year>=1988 & SOFirearmsGrowth$Year<=2002])
SOFA2003 <- mean(SOFirearmsGrowth$Rate_percent[SOFirearmsGrowth$Year>=2003 & SOFirearmsGrowth$Year<=2017])


# Creating Variables to Combine into a Dataframe for a Total
Region <- c('Midwest', 'North', 'Northeast', 'Southeast', 'South')
TotalGrowthDif <- c(MWTB2003-MWTA2003, NOTB2003-NOTA2003, NETB2003-NETA2003, SETB2003-SETA2003, SOTB2003-SOTA2003)
TotalGrowthDif <- round(TotalGrowthDif, digits = 2)
GunsGrowthDif <- c(MWFB2003-MWFA2003, NOFB2003-NOFA2003, NEFB2003-NEFA2003, SEFB2003-SEFA2003, SOFB2003-SOFA2003)
GunsGrowthDif <- round(GunsGrowthDif, digits = 2)

# Creating Dataframe with Results
GrowthDifferencePerRegion <- data.frame(Region,TotalGrowthDif,GunsGrowthDif)
colnames(GrowthDifferencePerRegion) <- c("Region", "Total Homicides\nGrowth Difference (%)",  "Total Firearms-Only Homicides\nGrowth Difference (%)")

# Ploting Results
GrowthDifferenceTable <- grid.table(GrowthDifferencePerRegion)

# Saving Table
ggsave(grid.table(GrowthDifferencePerRegion), filename="GrowthDifferencePerRegion.png")

# Combining all Firearm Growth Results
FirearmsGrowthAllRegions <- rbind(MWFirearmsGrowth, NOFirearmsGrowth, NEFirearmsGrowth, SOFirearmsGrowth, SEFirearmsGrowth)

# Combining all Total Growth Results
TotalGrowthAllRegions <- rbind(MWHomicidesGrowth, NOHomicidesGrowth, NEHomicidesGrowth, SOHomicidesGrowth, SEHomicidesGrowth)

# Plot 4 -  Firearms Growth Results

PLOT4 <- ggplot(FirearmsGrowthAllRegions) +
  aes(x = Year, y = Rate_percent, colour = Region) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1979, 2017, 1), 
                     limits=c(1979, 2017)) +
  scale_y_continuous(breaks = seq(-18, 42, 3), 
                     limits=c(-18, 42)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Year", y = "Percentage (%)", title = "Firearms-only Homicides Growth", subtitle = "Total Growth in Firearms-only Homicides per Region, in %") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Saving Plot 4
ggsave(PLOT4, filename = "plot4.pdf", 
       width = 10, height = 6, device = cairo_pdf)

# Plot 5 - Total Growth Results

PLOT5 <- ggplot(TotalGrowthAllRegions) +
  aes(x = Year, y = Rate_percent, colour = Region) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1979, 2017, 1), 
                     limits=c(1979, 2017)) +
  scale_y_continuous(breaks = seq(-15, 30, 3), 
                     limits=c(-15, 30)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Year", y = "Percentage (%)", title = "Total Homicides Growth", subtitle = "Total Growth in Homicides per Region, in %") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Saving Plot 5
ggsave(PLOT5, filename = "plot5.pdf", 
       width = 10, height = 6, device = cairo_pdf)
