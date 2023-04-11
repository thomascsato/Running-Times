library(tidyverse)
running_times <- read_csv("runningtimes.csv")

str(running_times)
unique(running_times$Place)
# Imagine being one of the top times in history at the olympics and getting 20th place

running_times %>%
  filter(Place == 20)
# Runs the 824th fastest time in the Olympics EVER - 20th place in the event. Interesting

fastest <- running_times %>%
  count(Country) %>%
  arrange(desc(n))
fastest

# Seems like Kenya has the most top runners, with surprisingly USA following closely behind
# I would have expected Ethiopia to be higher than USA I think.  OR perhaps not, but at 
# least closer to the US than it is, with a 1265 sum differential.

unique(running_times$Event)

countries <- running_times %>%
  filter(Country == c("KEN", "USA", "ETH", "JAM")) %>%
  group_by(Event, Country) %>%
  summarize(n = n())

running_times$Event <- as.factor(running_times$Event)
running_times$Event <- factor(running_times$Event, levels = c("100 m", "200 m", "400 m", "800 m", "1500 m",
                                              "5000 m", "10,000 m", "Half marathon", "Marathon"))

ggplot(countries) +
  geom_col(aes(x = Event, y = n, fill = Country), position = "fill") +
  coord_flip() +
  ylab("Proportion of Top Times for Each Event") +
  ggtitle("Proportions of Top 4 Countries Represented in Each Running Event")

View(running_times)

running_times <- running_times %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-")

running_times$Year <- as.integer(running_times$Year)

ggplot(running_times) +
  geom_bar(aes(Month), fill = "blue") +
  facet_wrap(~ Event) +
  theme_bw() +
  ggtitle("Distributions of Each Event by Month")
# Interesting to note how bimodal the distribution becomes when you go to 
# The half marathon and marathon distances.

ggplot(running_times) +
  geom_bar(aes(Month, fill = Gender), position = position_dodge(preserve = "single")) +
  facet_wrap(~ Event) +
  theme_bw() +
  ggtitle("Distributions of Each Event by Month, by Gender")
# Wanted to see if it varied by gender, but the distributions looks pretty much the same

running_times %>%
  count(Event, Gender)
# They all have around 2000 observations for each event, 1000 for each gender

ggplot(running_times) +
  geom_bar(aes(Year)) +
  facet_wrap(~ Event) +
  theme_bw() +
  ggtitle("Distributions of Each Event by Year") +
  ylab("Count") +
  theme(panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(10, 20, 10, 10))
# It is crazy to me how certain of the years have close to 200 observations in a given bucket.
# Also interesting to not the amount of missing values in some years
# Interestingly, from 400 m to 5000 m, the distributions seem to be fairly uniform, but in the tails of the events, people seem to be getting faster...

marathon <- running_times %>%
  filter(Event == "Marathon") %>%
  count(Year) %>%
  arrange(desc(n))
sum(marathon$n)
# It seems about right tho...

View(marathon %>% arrange(Year))
# Upon observation, it seems that the gaps in the graph have something to do with the bucket width or something like that (???)

unique(running_times$Name)
# There are 2903 unique names in this dataset of 18244 observations
18244 / 2903
# So on average, if you have a top 1000 time in your event, chances are you have 6.3 top times as well

running_times %>%
  count(Name) %>%
  arrange(desc(n)) %>%
  head(n = 10)

top <- data.frame(City = character(), n = integer(), Event = character())
for (i in seq(1:length(unique(running_times$Event)))) {
  df <- running_times %>%
    filter(Event == unique(running_times$Event)[i]) %>%
    count(City) %>%
    arrange(desc(n))
  top[nrow(top) + 1,] <- c(df[1,], levels(unique(running_times$Event))[i])
}
# Not sure if there is a more efficient way to produce this dataframe.....

top$Event <- factor(top$Event, levels = c("100 m", "200 m", "400 m", "800 m", "1500 m",
                                          "5000 m", "10,000 m", "Half marathon", "Marathon"))

ggplot(top) +
  geom_col(aes(Event, n, fill = City)) +
  theme_bw() +
  ggtitle("Highest Quantity City for Top Times in Each Event") +
  geom_text(aes(Event, n, label = City), vjust = -0.5) +
  ylab("Number of Top Times") +
  theme(legend.position = "none")
