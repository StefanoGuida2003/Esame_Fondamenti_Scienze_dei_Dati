library(dbscan)
library(plyr)
library(gifski)
library(gganimate)
library(dplyr)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(readxl)

videogameDataset <- read_excel("VideoGame_FinalUpdate.xlsx")
critic <- videogameDataset[!is.na(videogameDataset$Critic_Score), "Critic_Score"]
critic <- as.numeric(critic)

dbscan <- dbscan(videogameDataset, eps = 0.5, minPts = 10)

print(dbscan$cluster)

plot(critic, videogameDataset$Global_Sales, col = dbscan$cluster)

gamesGenre_Year <- videogameDataset %>% 
  group_by(Genre, Year_of_Release) %>% 
  summarize(n=n())
gamesGenre_Year$Year_of_Release <- as.numeric(gamesGenre_Year$Year_of_Release)

no_crit_score <- videogameDataset %>%
  filter(!is.na(Critic_Score))

boxPl <- ggplot(no_crit_score, aes(x = Genre, y = Critic_Score)) +
  geom_boxplot(fill = "#b97bf1") + 
  xlab("Generi") +
  ylab("Voto Critico")
boxPl

istogram <- ggplot(videogameDataset, aes(x = Genre, y = Global_Sales)) +
  geom_bar(stat="identity", fill="#b97bf1") +
  labs(x = "Genere", y = "Vendite Globali")

sales_by_year <- videogameDataset %>%
  group_by(Year_of_Release) %>%
  summarise(NA_Sales = sum(NA_Sales , na.rm = TRUE), 
            EU_Sales = sum(EU_Sales), 
            JP_Sales = sum(JP_Sales), 
            Other_Sales = sum(Other_Sales),
            Global_Sales = sum(Global_Sales))

line <- ggplot(sales_by_year, aes(x = Year_of_Release, y = Global_Sales)) +
  geom_line(aes(y = NA_Sales, colour = "NA")) +
  geom_line(aes(y = EU_Sales, colour = "EU")) +
  geom_line(aes(y = JP_Sales, colour = "JP")) +
  geom_line(aes(y = Other_Sales, colour = "Other")) +
  labs(title = "Sales of Video Games", x = "Year", y = "Sales") +
  guides(colour = guide_legend(title = "Region"))
line

yearInterval <- gamesGenre_Year %>%
  filter(Year_of_Release >= 1977 & Year_of_Release <= 1985)

graph1 <- ggplot(yearInterval, aes(x=Genre, y=n, fill=Year_Interval)) + 
  geom_bar(stat = "identity" , fill = "#595959") +
  ggtitle("Andamento del mercato 1977-1985") +
  xlab("Generi") +
  ylab("Numero Videogames") +
  theme(legend.position = "right")
graph1

yearInterval <- gamesGenre_Year %>%
  filter(Year_of_Release >= 1986 & Year_of_Release <= 1993)

graph2 <- ggplot(yearInterval, aes(x=Genre, y=n, fill=Year_Interval)) + 
  geom_bar(stat = "identity" , fill = "#f93b3e") +
  ggtitle("Andamento del mercato 1986-1993") +
  xlab("Generi") +
  ylab("Numero Videogames") +
  theme(legend.position = "right")
graph2

yearInterval <- gamesGenre_Year %>%
  filter(Year_of_Release >= 1994 & Year_of_Release <= 2002)

graph3 <- ggplot(yearInterval, aes(x=Genre, y=n, fill=Year_Interval)) + 
  geom_bar(stat = "identity" , fill = "#f77700") +
  ggtitle("Andamento del mercato 1994-2002") +
  xlab("Generi") +
  ylab("Numero Videogames") +
  theme(legend.position = "right")
graph3

yearInterval <- gamesGenre_Year %>%
  filter(Year_of_Release >= 2003 & Year_of_Release <= 2011)

graph4 <- ggplot(yearInterval, aes(x=Genre, y=n, fill=Year_Interval)) + 
  geom_bar(stat = "identity" , fill = "#ffe518") +
  ggtitle("Andamento del mercato 2003-2011") +
  xlab("Generi") +
  ylab("Numero Videogames") +
  theme(legend.position = "right")
graph4

yearInterval <- gamesGenre_Year %>%
  filter(Year_of_Release >= 2012 & Year_of_Release <= 2017)

graph5 <- ggplot(yearInterval, aes(x=Genre, y=n, fill=Year_Interval)) + 
  geom_bar(stat = "identity" , fill = "#8dff18") +
  ggtitle("Andamento del mercato 2012-2017") +
  xlab("Generi") +
  ylab("Numero Videogames") +
  theme(legend.position = "right")
graph5


