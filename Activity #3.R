# Setup ----

install.packages(c("ggplot2", "dplyr", "lubridate"))
library(ggplot2)
library(dplyr)
library(lubridate)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

tempAnom <- read.csv("/cloud/project/activity03/climate-change.csv")

# Prompt #1

colnames(datCO2)[4] <- "CO2"

datCO2$Entity <- as.factor(datCO2$Entity)

US <- datCO2 %>%
  filter(Entity == "United States")

plot(US$Year, US$CO2,
     type = "b",
     xlab = "Year",
     ylab = expression("Fossil Fuel Emissions (Billions of Tones of CO"[2]),
     pch = 19,
     yaxt = "n")
axis(2, seq(0, 6000000000, b = 2000000000),
     seq(0, 6, by = 2), las = 2)


ggplot(US, aes(x=Year, y=CO2))+
  geom_point()+
  geom_line()+
  labs(x="Year", y="US Fossil Fuel CO2 Emissions (Tons CO2)")+
  theme_classic()

# Prompt #2

NorthA <- datCO2 %>%
  filter(Entity == "United States" |
           Entity == "Mexico" |
           Entity =="Canada")

ggplot(NorthA,
       aes(x=Year, y = CO2, color=Entity))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("red", "royalblue", "darkgoldenrod3"))


tempAnom$date <- ymd(tempAnom$Day)

# plot(tempAnom$date, tempAnom$temperature_anomaly)

# Homework ----

# Question #1

Nordics <- datCO2 %>%
  filter(Entity == "Sweden" |
           Entity == "Finland" |
           Entity =="Norway" |
           Entity =="Iceland" |
           Entity =="Denmark")

# Subsrcipted C02 in this axis label
ggplot(Nordics,
       aes(x=Year, y = CO2, color=Entity))+
  geom_line()+
  scale_color_manual(values=c("maroon", "skyblue", "royalblue", "red", "yellow"))+
  labs(x="Year", y=expression("Fossil Fuel Emssions - Tons of CO"[2]))

# Question #2

worldCO2 <- datCO2 %>%
  group_by(Year) %>%
  summarise(total_emissions = sum(CO2))

ggplot(worldCO2,
       aes(x=Year, y = total_emissions))+
  geom_line()+
  labs(title=expression("World Fossil Fuel Emssions - Tons of CO"[2]), x="Year", y=expression("CO"[2]))

ggplot(tempAnom,
       aes(x=date, y = temperature_anomaly))+
  geom_line()+
  labs(title="World Temperature Anomaly (°C)", x="Year", y="Temperature Anomaly")

# Question #3

fossilFuelEmis <- read.csv("/cloud/project/global-fossil-fuel-consumption.csv")

fossilFuelEmis$Entity <- as.factor(fossilFuelEmis$Entity)

stacked <- stack(fossilFuelEmis[, c("Gas", "Oil", "Coal")])
stackedFuels <- cbind(fossilFuelEmis[ , c("Entity", "Code", "Year")], stacked)
names(stackedFuels)[4:5] <- c("Consumption", "Fuel")

stackedFuels <- stackedFuels[, c("Year", "Fuel", "Consumption")]

ggplot(stackedFuels,
       aes(x=Year, y = Consumption, color=Fuel))+
  geom_line()+
  scale_color_manual(values=c("gray", "blue", "black"))+
  labs(title="Global Fossil Fuel Consumption: \n(Measured in Terawatt-Hours of Primary Energy)",
       x="Year", y="Consumption")+
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000, 40000, 50000))+
  scale_x_continuous(breaks = c(1800, 1850, 1900, 1950, 2000, 2024))

