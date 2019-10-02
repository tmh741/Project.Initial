library(tidyverse)
library(magrittr)
library(esquisse)
library(stringr)
library(gridExtra)

#Read in Haley's data
Lowelldata <- read.csv("UNH Export for Haley Lowell 55932 - UNH Export for Haley Lowell 55932.csv")

#Remove rows and rename columns
names <- Lowelldata[8,]

Lowell.df <- Lowelldata[9:nrow(Lowelldata),]

colnames(Lowell.df) <- c(t(names))

#Convert appropriate variables to numeric values
Lowell.df %<>% separate(Timestamp, into = c("Date","TimeStamp","AM/PM"), sep = " ")

Lowell.df$Velocity <- type.convert(Lowell.df$Velocity)

Lowell.df$Odometer <- type.convert(Lowell.df$Odometer)

Lowell.df$Seconds <- type.convert(Lowell.df$Seconds)

Lowell.df$'Player Load' <- type.convert(Lowell.df$'Player Load')

# What to do about acceleration?

## Breakdown of Heart Rates
ggplot(Lowell.df) +
  aes(x = `Heart Rate`) +
  geom_bar(fill = "#0c4c8a") +
  labs(y = "Occurrences", title = "Number of Counted Heart Rates") +
  theme_minimal()

unique(Lowell.df$"Heart Rate")

# Histograms
ggplot(Lowell.df) +
  aes(x = Velocity) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Velocity", y = "Frequency", title = "Velocity over Time") +
  theme_minimal()

ggplot(Lowell.df) +
  aes(x = Odometer) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Distance Traveled", y = "Frequency", title = "Odometer over Time") +
  theme_minimal()

ggplot(Lowell.df) +
  aes(x = `Player Load`) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(y = "Count", title = "Player Load over Time") +
  theme_minimal()

# Player Load over time
ggplot(Lowell.df) +
  aes(x = Seconds, y = `Player Load`, colour = Odometer) +
  geom_point(size = 1L) +
  scale_color_viridis_c(option = "plasma") +
  labs(x = "Time (seconds)", y = "Player Load", title = "Player Load with Time", subtitle = "Player = Haley", color = "Odometer") +
  theme_minimal()

plotHaley <- ggplot(Lowell.df) +
  aes(x = Seconds, y = `Player Load`, colour = Odometer) +
  geom_point(size = 1L) +
  scale_color_viridis_c(option = "plasma") +
  labs(x = "Time (seconds)", y = "Player Load", title = "Player Load with Time", subtitle = "Player = Haley", color = "Odometer") +
  theme_minimal()

#Read Liz's data
Lizdata <- read.csv("UNH Export for Elizabeth Ryan 68501exp.csv")

names <- Lizdata[8,]

Liz.df <- Lizdata[9:nrow(Lizdata),]

colnames(Liz.df) <- c(t(names))

#Convert appropriate variables to numeric values
Liz.df %<>% separate(Timestamp, into = c("Date","TimeStamp","AM/PM"), sep = " ")

Liz.df$Velocity <- type.convert(Liz.df$Velocity)

Liz.df$Odometer <- type.convert(Liz.df$Odometer)

Liz.df$Seconds <- type.convert(Liz.df$Seconds)

Liz.df$Latitude <- type.convert(Liz.df$Latitude)

Liz.df$Longitude <- type.convert(Liz.df$Longitude)


Liz.df$'Player Load' <- type.convert(Liz.df$'Player Load')

ggplot(Liz.df) +
  aes(x = `Heart Rate`) +
  geom_bar(fill = "#0c4c8a") +
  labs(y = "Occurrences", title = "Number of Counted Heart Rates") +
  theme_minimal()

unique(Liz.df$"Heart Rate")

# Histograms
ggplot(Liz.df) +
  aes(x = Velocity) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Velocity", y = "Frequency", title = "Velocity over Time") +
  theme_minimal()

ggplot(Liz.df) +
  aes(x = Odometer) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Distance Traveled", y = "Frequency", title = "Odometer over Time") +
  theme_minimal()

ggplot(Liz.df) +
  aes(x = `Player Load`) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(y = "Count", title = "Player Load over Time") +
  theme_minimal()

# Player Load over time
ggplot(Liz.df) +
  aes(x = Seconds, y = `Player Load`, colour = Odometer) +
  geom_point(size = 1L) +
  scale_color_viridis_c(option = "plasma") +
  labs(x = "Time (seconds)", y = "Player Load", title = "Player Load with Time", subtitle = "Player = Liz", color = "Odometer") +
  theme_minimal()

plotLiz <- ggplot(Liz.df) +
  aes(x = Seconds, y = `Player Load`, colour = Odometer) +
  geom_point(size = 1L) +
  scale_color_viridis_c(option = "plasma") +
  labs(x = "Time (seconds)", y = "Player Load", title = "Player Load with Time", subtitle = "Player = Liz", color = "Odometer") +
  theme_minimal()

ggplot(Liz.df) +
  aes(x = Seconds, y = Odometer, colour = `Player Load`) +
  geom_point(size = 1L) +
  scale_color_viridis_c(option = "plasma") +
  labs(x = "Time (seconds)", y = "Odometer", title = "Odometer with Time", subtitle = "Player = Liz", color = "Player Load") +
  theme_minimal()

ggplot(Liz.df) +
  aes(x = Seconds, y = Velocity) +
  geom_point(size = 1L) +
  labs(x = "Time (seconds)", y = "Velocity", title = "Velocity with Time") +
  theme_minimal()


grid.arrange(plotLiz,plotHaley,ncol=2)


ggplot(Liz.df) +
  aes(x = Odometer, y = `Player Load`) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  labs(x = "Distance Travelled", y = "Player Load", title = "Odometer vs Player Load", subtitle = "Player = Liz") +
  theme_minimal()

ggplot(Liz.df) +
  aes(x = Latitude, y = Longitude) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  labs(title = "Lat vs Long", subtitle = "Player = Haley") +
  theme_minimal()