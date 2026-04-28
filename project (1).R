setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
corruption <- read.csv('corruption.csv')
cost <- read.csv('cost_of_living.csv')
richcont <- read.csv('richest_countries.csv')
tourism <- read.csv('tourism.csv')
un <- read.csv('unemployment.csv')
ec <- read.csv("Economic Indicators And Inflation.csv")
bank <- read_xlsx("WorldBank.xlsx")
df <- full_join(corruption, cost)
df <- full_join(df, richcont)
df <- full_join(df, tourism)
df <- full_join(df, un)



#cor(df$corruption_index, df$annual_income) #-0.9166564
#cor(df$corruption_index, df$purchasing_power_index) #-0.6616726
#cor(df$annual_income, df$cost_index)#0.6805984
#cor(df$annual_income, df$purchasing_power_index)#0.7472385
#cor(df$cost_index, df$purchasing_power_index)#0.7091937

bank %>% 
  ggplot(aes(x = `Individuals using the Internet (% of population)`, 
                          y = `Life expectancy at birth (years)`, color = Region)) +
  geom_point(alpha = 0.5, size = 1.2) +
  geom_smooth(color = "black", se = FALSE, linewidth = 1.5) +
  theme_minimal()+ 
  labs(title = "Internet Usage vs. Life Expectancy",
                      x = "Internet Usage (% of population)",
                      y = "Life Expectancy",
                      colour = "Region") + 
  scale_color_viridis_d(option = "D")+
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    text = element_text(size = 12),
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_text(face = "bold")
  )





#bank %>% 
#  ggplot(aes(x = `Birth rate, crude (per 1,000 people)`, y = `Life expectancy at birth (years)`)) +
#  geom_point(color = "orange", alpha = 0.4) + 
#  geom_smooth(method = "lm", color = "black", se = FALSE) +
#  theme_minimal() + 
#  labs(title = "Birth rate VS Life Expectancy",
#       x = "Birth rate", 
#       y = "Life Expectancy")

bank %>% 
  filter(!is.na(`GDP per capita (USD)`)) %>% 
  filter(!is.na(`Life expectancy at birth (years)`)) %>% 
  with(cor(`GDP per capita (USD)`, `Life expectancy at birth (years)`))

bank %>% 
  filter(Year > 1999) %>% 
  ggplot(aes(x = `Life expectancy at birth (years)`, y = log(`GDP per capita (USD)`), colour = Region, size = `Population density (people per sq. km of land area)`)) + 
  geom_point(alpha = 0.4) + theme_minimal() +
  labs(title = "GDP per capita VS Life Expectancy",
       subtitle = "In years 2000-2018",
       x = "Life Expectancy",
       y = "GDP per capita",
       colour = "Region",
       size = "Population density") + 
  scale_color_viridis_d(option = "D")+
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 15, face = "bold"),
    panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
    panel.grid.minor = element_blank(), 
    legend.title = element_text(face = "bold")
  )


#bank %>% 
#  group_by(Year, Region) %>% 
#  summarise(mean = mean(`GDP per capita (USD)`, na.rm = T)) %>% 
#  ggplot(aes(x = Year ,fill = Region)) + geom_area(aes(y = mean), alpha = 0.7) + 
#  labs(title = "GDP per capita during 1960-2018",
#       y = "GDP per capita")+ theme_classic()+
#  theme(
#    text = element_text(size = 13),
#    plot.title = element_text(size = 17, face = "bold"),
#    panel.grid= element_line(colour = "grey80", linewidth = 0.3 ),
#    legend.title = element_text(face = "bold")
#  )

#bank %>% 
#  ggplot(aes(x = `Birth rate, crude (per 1,000 people)`, y = `Life expectancy at birth (years)`)) +
#  geom_point(color = "orange", alpha = 0.4) + 
#  geom_smooth(method = "lm", color = "black", se = FALSE) +
#  theme_classic() + 
#  labs(title = "Birth rate VS Life Expectancy",
#       x = "Birth rate", 
#       y = "Life Expectancy")+ 
#  theme(
#    text = element_text(size = 13),
 #   plot.title = element_text(size = 17, face = "bold"),
#    panel.grid= element_line(colour = "grey80", linewidth = 0.3 ),
#    legend.title = element_text(face = "bold")
#  )

df <- left_join(df, bank, by = c("country" = "Country Name"))

df %>% 
  ggplot(aes(x = corruption_index, y = receipts_in_billions)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Impact of Tourist Attractiveness on Corruption",
    x = "Corruption Index",
    y = "Receipts (Billions)") + 
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 15, face = "bold"),
    panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
    panel.grid.minor = element_blank(), 
    legend.title = element_text(face = "bold")
  )



#ggplot(df, aes(x = log(`Population density (people per sq. km of land area)`), y = gdp_per_capita)) +
#  geom_point(color = "purple", size = 3, alpha = 0.6) +
#  geom_smooth(method = "lm", color = "orange") +
#  theme_minimal() +
#  labs(title = "Impact of population density on GDP per capita",
#       x = "Population density",
#       y = "GDP per capita")

df %>% filter(!is.na(Region)) %>% 
  ggplot(aes(x = reorder(Region, corruption_index, na.rm = TRUE), y = corruption_index, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() + 
  theme_minimal() +
  labs(
    title = "Distribution of Corruption Index by Region",
    x = "Region",
    y = "Corruption Index"
  )+  scale_color_viridis_d(option = "D")+
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 15, face = "bold"),
    panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )


bank_main <- bank %>%
  select(`Country Name`, Region, IncomeGroup) %>% 
  distinct()
ec <- left_join(ec, bank_main, by = c("Country" = "Country Name"))

ec %>%
  filter(!is.na(Region)) %>% 
  group_by(Year, Region) %>%
  summarise(Avg_Growth = mean(Economic.Growth...., na.rm = TRUE)) %>% 
  ggplot(aes(x = Year, y = Avg_Growth, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point() + 
  theme_minimal() +
   labs(
     title = "Average Economic Growth by Region Over Time",
     x = "Year", 
     y = "Average Economic Growth (%)",
     color = "Region"
   ) +  scale_color_viridis_d(option = "D")+
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 15, face = "bold"),
    panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
    panel.grid.minor = element_blank(), 
    legend.title = element_text(face = "bold")
  )



ec %>%
  group_by(Region) %>%
  summarise(Avg_Growth = mean(Economic.Growth...., na.rm = TRUE)) %>% 
   filter(!is.na(Region)) %>% 
   ggplot( aes(x = reorder(Region, Avg_Growth), y = Avg_Growth, fill = Region)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Average Economic Growth by Region from 2010",
       x = "Region", y = "Average Economic Growth (%)") +  
  scale_fill_viridis_d(option = "D")+
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 15, face = "bold"),
    panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )


ec %>%
  group_by(Year) %>%
  summarise(Avg_Growth = mean(Economic.Growth...., na.rm = TRUE)) %>% 
  ggplot(aes(x = Year, y = Avg_Growth)) +
  geom_line(color = "forestgreen", linewidth = 1.2) +
  geom_point(color = "forestgreen", size = 3) +
  theme_minimal() +
  labs(title = "Global Average Economic Growth",
       x = "Year", y = "Economic Growth (%)")+  
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 15, face = "bold"),
    panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
    panel.grid.minor = element_blank()
  )


df %>% 
  filter(!is.na(IncomeGroup)) %>% 
  mutate(decade = round(Year/10)*10) %>% 
  group_by(IncomeGroup, decade) %>% 
  summarise(avg_b = mean(`Birth rate, crude (per 1,000 people)`, na.rm = TRUE)) %>% 
  ggplot(aes(x = decade, y = avg_b, fill = IncomeGroup)) + 
  geom_col(position = "dodge", alpha = 0.8)+
  theme_minimal(base_size = 14) +
  labs(
    title = "Average Birth Rate Over Time",
    subtitle = "Grouped by Income Level",
    x = "Year", 
    y = "Average Birth Rate (per 1,000)",
    fill = "Income Group" 
  ) + scale_fill_viridis_d(option = "D")+
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 15, face = "bold"),
    panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
    panel.grid.minor = element_blank(), 
    legend.title = element_text(face = "bold")
  )

  df %>% 
    filter(!is.na(IncomeGroup)) %>% 
    mutate(decade = round(Year/10)*10) %>% 
    group_by(IncomeGroup, decade) %>% 
    summarise(avg_d = mean(`Death rate, crude (per 1,000 people)`, na.rm = TRUE)) %>% 
    ggplot(aes(x = decade, y = avg_d, fill = IncomeGroup)) + 
    geom_col(position = "dodge", alpha = 0.8)+
    theme_minimal(base_size = 14) +
    labs(
      title = "Average Death Rate Over Time",
      subtitle = "Grouped by Income Level",
      x = "Year", 
      y = "Average Death Rate (per 1,000)",
      fill = "Income Group" 
    ) + scale_fill_viridis_d(option = "D")+
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 15, face = "bold"),
      panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
      panel.grid.minor = element_blank(), 
      legend.title = element_text(face = "bold")
    )
  
  bank %>% 
    group_by(IncomeGroup, Year) %>% 
    summarise(
      avg_density = mean(`Population density (people per sq. km of land area)`, na.rm = TRUE)) %>% 
    ggplot(aes(x = Year, y = avg_density, color = IncomeGroup)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    theme_minimal() +
    labs(
      title = "Average Population Density Over Time",
      x = "Year", 
      y = "Average Density (people per sq. km)",
      color = "Income Group"
    ) + scale_color_viridis_d(option = "D")+
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 15, face = "bold"),
      panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
      panel.grid.minor = element_blank(), 
      legend.title = element_text(face = "bold")
    )
    
  bank %>% 
    group_by(Region, Year) %>% 
    summarise(
      avg_density = mean(`Population density (people per sq. km of land area)`, na.rm = TRUE)) %>% 
    ggplot(aes(x = Year, y = avg_density, color = Region)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    theme_minimal() +
    labs(
      title = "Average Population Density Over Time",
      x = "Year", 
      y = "Average Density (people per sq. km)",
      color = "Region"
    ) + scale_color_viridis_d(option = "D")+
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 15, face = "bold"),
      panel.grid= element_line(colour = "grey85", linewidth = 0.4 ),
      panel.grid.minor = element_blank(), 
      legend.title = element_text(face = "bold")
    )
    
    
    
    
    
    
    
    
    
    
    
