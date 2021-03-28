library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(gridExtra)
library(corrr)
library(GGally)
library(car)

data<-read_csv("2020.csv")
data <- data %>% select(-c(4:13, 10))
names(data) <- c("Country", "Region", "Score", "Economy", "Family", "Health", "Freedom", "Generosity", "Trust", "Dystopia + Residuals")

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
names(df) <- c("Country", "Gdp_billions", "Code")
countries <- data[ ,c("Country", "Score")]
df$Country[df$Country == "Gambia, The"] <- "Gambia"
df$Country[df$Country == "Hong Kong"] <- "Hong Kong S.A.R. of China"
df$Country[df$Country == "Taiwan"] <- "Taiwan Province of China"
df$Country[df$Country == "Korea, South"] <- "South Korea"
df$Country[df$Country == "Congo, Republic of the"] <- "Congo (Brazzaville)"
df$Country[df$Country == "Congo, Democratic Republic of the"] <- "Congo (Kinshasa)"
df$Country[df$Country == "Cote d'Ivoire"] <- "Ivory Coast"
df$Country[df$Country == "Burma"] <- "Myanmar"
df$Country[df$Country == "West Bank"] <- "Palestinian Territories"

df1 <- countries %>% full_join(df, by = "Country")
na <- df1[is.na(df1$Score),]
#df1$Score[is.na(df1$Score)] <- 0

l <- list(color = toRGB("gray85"), width = 0.2)

g <- list(
  lonaxis = list(range = c(-170, 179)),
  lataxis = list(range = c(-55, 100)),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  showframe = FALSE,
  showcountries = TRUE, 
  countrycolor = toRGB("gray"),
  showcoastlines = FALSE,
  bgcolor = toRGB("light blue"),
  projection = list(type = 'Mercator'))

p <- plot_geo(df1) %>%
  add_trace(z = ~Score, color = ~Score, colors = 'Reds', 
            text = ~Country, locations = ~Code, marker = list(line = l),
            hovertemplate = "%{z}<br>%{text}<extra></extra>"
  ) %>%
  add_trace(data=na, marker = list(color = toRGB("gray85")), mode = "markers", opacity = 0,
            text = ~Country, locations = ~Code, hovertemplate = "NA<br>%{text}<extra></extra>"
  ) %>%
  colorbar (title = list(text = "<b>Hapiness Score<br> </b>", font = list(size=14, color='red')),
           limits=c(0,8), tickfont=list(size=13, color='red'), len = 0.5) %>%
  layout (title = list(text = "<b>World Happiness Score-2020</b>", x = 1.5, 
          font = list(size=20, color = "#A52A2A")), 
          margin = list(l=50, r=50, b=100, t=100, pad=4),
          geo = g)
show(p)


data2 <- data
unique(data2$Region)

North <- c("Central and Eastern Europe", 
           "Western Europe", 
           "North America and ANZ",
           "Commonwealth of Independent States",
           "East Asia")
South <- c("Middle East and North Africa",  
           "Latin America and Caribbean",
           "Southeast Asia",                    
           "Sub-Saharan Africa",
           "South Asia")

data2 <- data2 %>%
  mutate('NS' = ifelse(data2$Region %in% North, "North", "South"))

data3 <- data2 %>% 
  select(NS, Region, Score) %>% 
  group_by(Region, NS) %>%
  summarize(Score=sum(Score), .groups = 'keep')%>%
  arrange(NS)

data3$Region <- factor(data3$Region, levels = c(North, South))

ggplot(data = data3, aes(x="", y=Score, fill = Region, group = NS)) +
  geom_bar(width = 1, stat="identity") +
  scale_fill_manual(values=c("#7442c8", "#0000ff", "#6600ff", "#9932cc", "#8b00ff", "#ffff00", "#ceff1d", "#ffdc33", "#ffcf40", "#fde910")) +
  coord_polar("y") +
  theme_minimal() +
  labs(title="Happiness Score - Regionwise for 2020") +
  theme(legend.position = "right", 
        legend.title = element_text(face="bold", color="#cb5b10", size = 13), 
        legend.text = element_text(face="bold", color="#cb5b10", size = 11),
        plot.title = element_text(face="bold", size = 20, hjust = 1, color = '#c73d28'),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = NULL)

datasmall <- data2 %>%
  head(10)

datasmall2 <- data2 %>%
  tail(10)

datasmall2$Country[datasmall2$Country == "Central African Republic"] <- "CAR"

g1 <- ggplot(data = datasmall, aes(x=reorder(Country, Score), y=Score, fill = Score)) + 
  geom_bar(stat="identity", width = 0.5, position = 'dodge', color = "grey") +
  coord_flip() +
  theme_minimal()+
  scale_fill_gradientn(name = "Score", colours=c("#ffcbdb", "#ffb8c2", "#ff8597"))+
  geom_text(aes(label = round(`Score`, 2), y = `Score` - 0.5), size=4, colour= "white") +
  labs(title = "Top-10 Happiest Countries 2020") +
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5, vjust = 1, color = '#de5d83'),
        axis.text.x = element_text(face="bold", color="#fc6c85", size = 11, vjust = 2),
        axis.text.y = element_text(face="bold", color="#fc6c85", size = 14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face="bold", color="#fc6c85", size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(seq(0, 8, 0.5)))


g2 <- ggplot(data = datasmall2, aes(x=reorder(Country, -Score), y=Score, fill = Score)) + 
  geom_bar(stat="identity", width = 0.5, position = 'dodge', color = "grey") +
  coord_flip() +
  theme_minimal()+
  scale_fill_gradientn(name = "Score", colours=c("#ccccff", "#a496f2", "#7b68ee"))+
  geom_text(aes(label = round(`Score`, 2), y = `Score` - 0.5), size=4, colour= "white") +
  labs(title = "Top-10 Unhappiest Countries 2020") +
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5, vjust = 1, color = '#6b5ccc'),
        axis.text.x = element_text(face="bold", color="#8000ff", size = 11, vjust = 2),
        axis.text.y = element_text(face="bold", color="#8000ff", size = 14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face="bold", color="#8000ff", size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(seq(0, 8, 0.5)))

grid.arrange(g1,g2, ncol=1, nrow=2)
                   
t.test(Score ~ NS, data = data2, paired = FALSE)

data.cut <- data %>% select(-c(1:2, 10))

data.cut %>%
  select(Score, Economy, Family, Health, Freedom, Generosity, Trust) %>%
  psych::corr.test(adjust = "BH")

data.cut %>%
  select(Score, Economy, Family, Health, Freedom, Generosity, Trust) %>%
  psych::corr.test(adjust = "BH", method = "spearman")

norm_func <- function(x) {
  YJ <- car::powerTransform(x ~ 1, family = "yjPower") 
  lambdaYJ <- YJ$lambda
  trans <- car::yjPower(U = x, lambda = lambdaYJ)
  return (trans)
}

data_norm <- as_tibble(apply(data.cut, 2, norm_func))

hist(data.cut$Score)
hist(data_norm$Score)
hist(data.cut$Economy)
hist(data_norm$Economy)
hist(data.cut$Family)
hist(data_norm$Family)
hist(data.cut$Generosity)
hist(data_norm$Generosity)
hist(data.cut$Freedom)
hist(data_norm$Freedom)
hist(data.cut$Health)
hist(data_norm$Health)
hist(data.cut$Trust)
hist(data_norm$Trust)

cor.data <- data.cut %>% correlate() %>% focus(`Score`)
cor.data %>% 
  mutate(term = factor(term, levels = term[order(-`Score`)])) %>% 
  ggplot(aes(x = term, 
             y = `Score`)) +
  geom_bar(stat = "identity", fill = "#cf59b1") +
  labs(title = "Hapiness Score vs 6 Factors Correlation Coefficients") +
  ylab("Correlation with Score") +
  xlab("Factor") + 
  geom_text(aes(label = round(`Score`, 2), y = `Score` -0.03), size=4, colour= "snow1") +
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5, vjust = 1, color = '#00a1db'),
        axis.text.x = element_text(face="bold", color="#008cf0", size = 12),
        axis.text.y = element_text(face="bold", color="#008cf0", size = 12),
        axis.title.y = element_text(face="bold", color="#cf59b1", size = 14),
        axis.title.x = element_text(face="bold", color="#cf59b1", size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#fceef2"))

cor.data <- data_norm %>% correlate() %>% focus(`Score`)
cor.data %>% 
  mutate(term = factor(term, levels = term[order(-`Score`)])) %>% 
  ggplot(aes(x = term, 
             y = `Score`)) +
  geom_bar(stat = "identity", fill = "#cf59b1") +
  labs(title = "Hapiness Score vs 6 Factors Correlation Coefficients") +
  ylab("Correlation with Score") +
  xlab("Factor") + 
  geom_text(aes(label = round(`Score`, 2), y = `Score` -0.03), size=4, colour= "snow1") +
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5, vjust = 1, color = '#00a1db'),
        axis.text.x = element_text(face="bold", color="#008cf0", size = 12),
        axis.text.y = element_text(face="bold", color="#008cf0", size = 12),
        axis.title.y = element_text(face="bold", color="#cf59b1", size = 14),
        axis.title.x = element_text(face="bold", color="#cf59b1", size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#fceef2"))


data.cut2 <- data.cut %>%
  gather(-`Score`, key = "var", value = "Factor Score")

data.cut2$var <- factor(data.cut2$var,levels=c("Economy", "Family", "Health", "Freedom", "Generosity", "Trust"))

ggplot(data = data.cut2, aes(x = `Factor Score`, y = `Score`)) +
  labs(title = "Hapiness Score vs 6 Factors Correlation") +
  ylab("Hapiness Score") +
  facet_wrap(~ var, scales = "free") +
  geom_point(aes(color=data.cut2$Country), size = 2, shape=21, fill='#8ac8ff', color='black') +
  geom_smooth(method = 'lm', se= FALSE, color='#cf59b1') + 
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5, vjust = 1, color = '#00a1db'),
        strip.text = element_text(face="bold", color="#cf59b1", size=16),
        axis.text.x = element_text(face="bold", color="#008cf0", size = 12),
        axis.text.y = element_text(face="bold", color="#008cf0", size = 12),
        axis.title.y = element_text(face="bold", color="#cf59b1", size = 16),
        axis.title.x = element_text(face="bold", color="#cf59b1", size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#fceef2"),
        strip.background = element_rect(fill="#fee1ec"))

data_norm2 <- data_norm %>%
  gather(-`Score`, key = "var", value = "Factor Score")

data_norm2$var <- factor(data_norm2$var,levels=c("Economy", "Family", "Health", "Freedom", "Generosity", "Trust"))

ggplot(data = data_norm2, aes(x = `Factor Score`, y = `Score`)) +
  labs(title = "Hapiness Score vs 6 Factors Correlation") +
  ylab("Hapiness Score") +
  facet_wrap(~ var, scales = "free") +
  geom_point(aes(color=data_norm2$Country), size = 2, shape=21, fill='#8ac8ff', color='black') +
  geom_smooth(method = 'lm', se= FALSE, color='#cf59b1') + 
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5, vjust = 1, color = '#00a1db'),
        strip.text = element_text(face="bold", color="#cf59b1", size=16),
        axis.text.x = element_text(face="bold", color="#008cf0", size = 12),
        axis.text.y = element_text(face="bold", color="#008cf0", size = 12),
        axis.title.y = element_text(face="bold", color="#cf59b1", size = 16),
        axis.title.x = element_text(face="bold", color="#cf59b1", size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#fceef2"),
        strip.background = element_rect(fill="#fee1ec"))

lm_func <- function(data, x) {
  fv <- data %>% lm(Score ~ x,.) %>% fitted.values()
  return (fv)
}

p1 <- plot_ly(
  alpha = .8,
  data      = data2,
  type      = "scatter",
  mode      = "markers",
  name      = "County", 
  text      = ~Country,
  hoverinfo = "text",
  colors    = "inferno",
  color     = ~Score
) %>% 
  add_trace(x = ~Economy, y = ~Score) %>% 
  add_lines(x = ~Economy, y = lm_func(data2, data2$Economy), mode = "lines", name = "Regression line", hoverinfo = "skip") %>%
  colorbar (title = "<b>Hapiness Score<br> </b>",
            titlefont = list(size=16, color='#cb5b10'),
            tickfont=list(size=12, color='red'), limits = c(0,8), len = 0.5) %>%
  layout (title = list(text = "<b>Score vs Economy</b>", font = list(size=20, color = "#cb5b10")), 
          font = list(size=13, color = "#cb5b10"),
          margin = list(l=100, r=100, b=100, t=100, pad=3),
          xaxis=list(title = "Factor Score", titlefont=list(color='red'), tickfont=list(color='red'), showline = TRUE),
          yaxis=list(title = "Hapiness Score", titlefont=list(color='red'), tickfont=list(color='red')))
show(p1)

p2 <- plot_ly(
  alpha = .8,
  data      = data2,
  type      = "scatter",
  mode      = "markers",
  name      = "County", 
  text      = ~Country,
  hoverinfo = "text",
  colors    = "inferno",
  color     = ~Score
) %>% 
  add_trace(x = ~Family, y = ~Score) %>% 
  add_lines(x = ~Family, y = lm_func(data2, data2$Family), mode = "lines", name = "Regression line", hoverinfo = "skip") %>%
  colorbar (title = "<b>Hapiness Score<br> </b>",
            titlefont = list(size=16, color='#cb5b10'), 
            tickfont=list(size=12, color='red'), limits = c(0,8), len = 0.5) %>%
  layout (title = list(text = "<b>Score vs Family</b>", font = list(size=20, color = "#cb5b10")), 
          font = list(size=13, color = "#cb5b10"),
          margin = list(l=100, r=100, b=100, t=100, pad=3),
          xaxis=list(title = "Factor Score", titlefont=list(color='red'), tickfont=list(color='red'), showline = TRUE),
          yaxis=list(title = "Hapiness Score", titlefont=list(color='red'), tickfont=list(color='red')))
show(p2)

p3 <- plot_ly(
  alpha = .8,
  data      = data2,
  type      = "scatter",
  mode      = "markers",
  name      = "County", 
  text      = ~Country,
  hoverinfo = "text",
  colors    = "inferno",
  color     = ~Score
) %>% 
  add_trace(x = ~Health, y = ~Score) %>% 
  add_lines(x = ~Health, y = lm_func(data2, data2$Health), mode = "lines", name = "Regression line", hoverinfo = "skip") %>%
  colorbar (title = "<b>Hapiness Score<br> </b>",
            titlefont = list(size=16, color='#cb5b10'),
            tickfont=list(size=12, color='red'), limits = c(0,8), len = 0.5) %>%
  layout (title = list(text = "<b>Score vs Health</b>", font = list(size=20, color = "#cb5b10")), 
          font = list(size=13, color = "#cb5b10"),
          margin = list(l=100, r=100, b=100, t=100, pad=3),
          xaxis=list(title = "Factor Score", titlefont=list(color='red'), tickfont=list(color='red'), showline = TRUE),
          yaxis=list(title = "Hapiness Score", titlefont=list(color='red'), tickfont=list(color='red')))
show(p3)

# Название одинокой точки на оси y близко к 0 не отображается (связано с особенностями отображения в данном режиме), поэтому создадим ей "компаньона" с максимально близкими координатами и аналогичным названием.
data3 <- data2
data3[nrow(data3) + 1,] <- list("Afghanistan", "South Asia", 2.5671, 0.3007058, 0.3564338, 0.2660515, 0, 0.1352347, 0.001225785, 1.507236, "South")


p4 <- plot_ly(
  alpha = .8,
  data      = data3,
  type      = "scatter",
  mode      = "markers",
  name      = "County", 
  text      = ~Country,
  hoverinfo = "text",
  colors    = "inferno",
  color     = ~Score
) %>% 
  add_trace(x = ~Freedom, y = ~Score) %>%
  add_lines(x = ~Freedom, y = lm_func(data3, data3$Freedom), mode = "lines", name = "Regression line", hoverinfo = "skip") %>%
  colorbar (title = "<b>Hapiness Score<br> </b>",
            titlefont = list(size=16, color='#cb5b10'), 
            tickfont=list(size=12, color='red'), limits = c(0,8), len = 0.5) %>%
  layout (title = list(text = "<b>Score vs Freedom</b>", font = list(size=20, color = "#cb5b10")), 
          font = list(size=13, color = "#cb5b10"),
          margin = list(l=100, r=100, b=100, t=100, pad=3), 
          # hovermode = "x",
          xaxis=list(title = "Factor Score", titlefont=list(color='red'), tickfont=list(color='red'), showline = TRUE),
          yaxis=list(title = "Hapiness Score", titlefont=list(color='red'), tickfont=list(color='red')))
show(p4)


p5 <- plot_ly(
  alpha = .8,
  data      = data2,
  type      = "scatter",
  mode      = "markers",
  name      = "County", 
  text      = ~Country,
  hoverinfo = "text",
  colors    = "inferno",
  color     = ~Score
) %>% 
  add_trace(x = ~Generosity, y = ~Score) %>% 
  add_lines(x = ~Generosity, y = lm_func(data2, data2$Generosity), mode = "lines", name = "Regression line", hoverinfo = "skip") %>%
  colorbar (title = "<b>Hapiness Score<br> </b>",
            titlefont = list(size=16, color='#cb5b10'), 
            tickfont=list(size=12, color='red'), limits = c(0,8), len = 0.5) %>%
  layout (title = list(text = "<b>Score vs Generosity</b>", font = list(size=20, color = "#cb5b10")), 
          font = list(size=13, color = "#cb5b10"),
          margin = list(l=100, r=100, b=100, t=100, pad=3),
          xaxis=list(title = "Factor Score", titlefont=list(color='red'), tickfont=list(color='red'), showline = TRUE),
          yaxis=list(title = "Hapiness Score", titlefont=list(color='red'), tickfont=list(color='red')))
show(p5)

p6 <- plot_ly(
  alpha = .8,
  data      = data2,
  type      = "scatter",
  mode      = "markers",
  name      = "County", 
  text      = ~Country,
  hoverinfo = "text",
  colors    = "inferno",
  color     = ~Score
) %>% 
  add_trace(x = ~Trust, y = ~Score) %>% 
  add_lines(x = ~Trust, y = lm_func(data2, data2$Trust), mode = "lines", name = "Regression line", hoverinfo = "skip") %>%
  colorbar (title = "<b>Hapiness Score<br> </b>",
            titlefont = list(size=16, color='#cb5b10'), 
            tickfont=list(size=12, color='red'), limits = c(0,8), len = 0.5) %>%
  layout (title = list(text = "<b>Score vs Trust</b>", font = list(size=20, color = "#cb5b10")), 
          font = list(size=13, color = "#cb5b10"),
          margin = list(l=100, r=100, b=100, t=100, pad=3),
          xaxis=list(title = "Factor Score", titlefont=list(color='red'), tickfont=list(color='red'), showline = TRUE),
          yaxis=list(title = "Hapiness Score", titlefont=list(color='red'), tickfont=list(color='red')))
show(p6)

model = lm(Score ~ Economy, data_norm)
summary(model)

model = lm(Score ~ Family, data_norm)
summary(model)

model = lm(Score ~ Health, data_norm)
summary(model)

model = lm(Score ~ Freedom, data_norm)
summary(model)

model = lm(Score ~ Generosity, data_norm)
summary(model)

model = lm(Score ~ Trust, data_norm)
summary(model)


