
# Import packages

library(dplyr)
library(ggplot2)
library(forcats)


# Import dataset stored as csv file format

getwd() 
setwd("D:\\r learning\\self learning")
crimes_on_women <-read.csv("crimes_on_women.csv")



# Data explore and check if any value is missing

View(crimes_on_women)
head(crimes_on_women)
tail(crimes_on_women)
str(crimes_on_women)
names(crimes_on_women)
sum(is.na(crimes_on_women)) #check missing values


# Convert all entries of the 'State' column to uppercase

crimes_on_women <- crimes_on_women %>%
  mutate(State = toupper(State))





# Data Analysis begins from here

## total number of crimes by year

total_crime_yr <- crimes_on_women %>%
  group_by(Year) %>%
  summarize(total_crime = sum(Rape + K.A + DD + AoW + AoM + DV +WT)) %>%
  arrange(desc(total_crime))

print(total_crime_yr)


# Display total number of crimes by year in bar plot

total_crime_yr %>%
  ggplot(aes(Year, total_crime, fill = total_crime)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Total Number of Crimes",
       title = "Total Number of Crimes against Women from (2001 - 2021) in India") +
  theme_bw()
  



# Find and print sum total of various crimes

total_rape <- sum(crimes_on_women$Rape)
total_K.A <- sum(crimes_on_women$K.A)
total_DD <-  sum(crimes_on_women$DD)
total_Aow <- sum(crimes_on_women$AoW)
total_AoM <- sum(crimes_on_women$AoM)
total_DV <- sum(crimes_on_women$DV)
total_WT <- sum(crimes_on_women$WT)

 total_rape
 total_K.A
 total_DD
 total_Aow
 total_Aow
 total_DV
 total_WT
 

 
## Create a summary data frame with total crimes
 
 crime_totals <- data.frame(
   Crime = c("Rape", "Kidnap and Assault", "Dowry Deaths", "Assault agaist Women", "Assault against Modesty of Women", "Domestic Violence", "Women Trafficking"),
   Total = c(total_rape, total_K.A, total_DD, total_Aow, total_AoM, total_DV, total_WT))
 
 
 ## Display total number of various crimes in bar chart
 crime_totals %>%
   ggplot(aes(fct_reorder(Crime, Total), Total, fill = Crime)) +
   geom_bar(stat = "identity") +  
   theme_bw() +
   coord_flip()+
   labs(x = "Types of Crime", y = "Number of Crimes",
        title = "Total Number of Different Crimes in 20 years (2001 - 2021) in India")
 

 
 # Incident of Various crimes in years
 
  ## Incident of Domestic Violence in years
  
  DV_by_year <- crimes_on_women %>%
    group_by(Year) %>%
    summarize(total_DV = sum(DV, na.rm = T)) %>%
    arrange(desc(total_DV))
  print(DV_by_year)
  
  
  ### Display incident of Domestic Violence in years
  
  DV_by_year %>% 
    ggplot(aes(Year, total_DV))+
    geom_smooth(stat = "identity", color = "#3fe10b") +
    labs(x = "Year", y = "Count",
         title = "Incident of Domestic Violence in 20 years (2001 -2021) in India")+
    theme_bw()
  
  
  ## Incident of Assault Against Women in years
  
  AoW_by_year <- crimes_on_women %>%
    group_by(Year) %>%
    summarize(total_AoW = sum(AoW, na.rm = T)) %>%
    arrange(desc(total_AoW))
  print(AoW_by_year)
  
  ### Display average Assault Against Women in years
  
  AoW_by_year %>%
    ggplot(aes(Year, total_AoW)) +
    geom_smooth(stat = "identity", color = "#a6ba26") +
    labs(x = "Year", y = "Count",
         title = "Incident of Assault Against Women in 20 years (2001 -2021) in India") +
    theme_bw() 
  
  ## Incident of Kidnap and Assault
  
  K.A_by_year <- crimes_on_women %>%
    group_by(Year) %>%
    summarize(total_K.A = sum(K.A, na.rm = T)) %>%
    arrange(desc(total_K.A))
  print(K.A_by_year)
  
  ### Display incident of  kidnap and assault
  
  K.A_by_year %>%
    ggplot(aes(Year, total_K.A)) +
    geom_smooth(stat = "identity", color = "#149eda") +
    labs(x = "Year", y = "Count",
         title = "Incident of Kidnap and Assault in 20 years (2001 - 2021) in India") +
    theme_bw()
  
  
  
  ## Incident of Rape in years
  
  rapes_by_year <- crimes_on_women %>%
    group_by(Year) %>%
    summarize(total_rape = sum(Rape, na.rm = T)) %>%
    arrange(desc(total_rape))
  print(rapes_by_year)
  
  
  ### Display incident of  Rape in years
  rapes_by_year %>%
  ggplot(aes(Year, total_rape)) +
    geom_smooth(stat = "identity", color = "#9214da") +
    labs(x = "Year", y = "Count",
         title = "Incident of Rape in 20 years (2001 - 2021) in India") +
    theme_bw()
  
  
  
  ## Incident of Assault against modesty of women by year
  
  AoM_by_year <- crimes_on_women %>%
    group_by(Year) %>%
    summarize(total_AoM = sum(AoM, na.rm = T)) %>%
    arrange(desc(total_AoM))
  print(AoM_by_year)

  
  ### Display incident of Assault against modesty of Women by year
  AoM_by_year %>%
    ggplot(aes(Year, total_AoM)) +
    geom_smooth(stat = "identity", color = '#d34c0c') +
    labs(x = "Year", y = "Count",
         title = "Incident of Assault against Modesty of Women in 20 years (2001 - 2021) in India") +
    theme_bw()

 
 
 ## Incident of Dowry Death by year
  
  DD_by_year <- crimes_on_women %>%
    group_by(Year) %>%
    summarize(total_DD = sum(DD, na.rm = T)) %>%
    arrange(desc(total_DD))
  print(DD_by_year)
  
  
  
### Display incident of Dowry Death by year
  
DD_by_year %>%
  group_by(Year) %>%
  ggplot(aes(Year, total_DD)) +
  geom_smooth(stat = "identity", color = "#12d30c") +
  labs(x = "Year", y = "Count",
       title = "Incident of Dowry Death of Women in 20 years (2001 - 2021) in India") +
  theme_bw()
 

## Incident of Women trafficking by year
  WT_by_year <-crimes_on_women %>%
    group_by(Year) %>%
    summarize(total_WT = sum(WT, na.rm = T)) %>%
    arrange(desc(total_WT))
  print(WT_by_year)
  
  
  ### Display incident of  Women Trafficking by year
  
  WT_by_year %>%
    group_by(Year) %>%
    ggplot(aes(Year, total_WT)) +
    geom_smooth(stat = "identity", color = "#ee3ec6") +
    labs(x = "Year", y = "Count",
         title = "Incident of Women Trafficking in 20 years (2001 - 2021) in India") +
    theme_bw()
  
  
  # Average number of crimes in different states
  
  
  avg_crime_state <- crimes_on_women %>%
    group_by(State) %>%
    summarize(avg_crime = sum((Rape + K.A + DD + AoW + AoM + DV +WT)/20)) %>%
    arrange(desc(avg_crime))
  
  print(avg_crime_state)
  
  ## Display total number of crimes by States in Dot plot
  avg_crime_state %>%
    ggplot(aes(avg_crime, State))+
    geom_point(aes(colour = State), size = 3)+
    labs(x = "Number of Crimes", y = "States",
         title = "Average Number of Crimes in Different States of 20 years (2001-2021) in India")+
    theme_bw()
    

  

  
  # Top 10 states with highest number of various crimes
  
  ## Top 10 states with highest number of domestic violence
  
  DV_by_state <- crimes_on_women %>%
    group_by(State) %>%
    summarize(total_DV = sum(DV, na.rm = T)) %>%
    arrange(desc(total_DV))
  
  print(DV_by_state[1:10, ])
  
  
  ### Display top 10 states with highest number of domestic violence
  DV_by_state[1:10, ] %>%
    ggplot(aes(reorder(State, -total_DV), total_DV, fill = total_DV)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#88e18b", high = "#19931c") +
    theme_bw()+
    labs( x = "State", y = "Total Domestic Violance",
          title = "Top 10 states by Number of Domestic Violence of 20 years (2001 - 2021) in India") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ## Top 10 states with highest number of Assault Against Women
  
  AoW_by_state <- crimes_on_women %>%
    group_by(State) %>%
    summarize(total_Aow = sum(AoW, na.rm = T)) %>%
    arrange(desc(total_Aow))
  
  print(AoW_by_state[1:10, ])
  
  
  ### Display top 10 states with highest number of Assault Against Women
  
  AoW_by_state[1:10, ] %>%
    ggplot(aes(reorder(State, -total_Aow), total_Aow, fill = total_Aow)) +
    geom_bar(stat = "identity") +
    theme_bw()+
    scale_fill_gradient(low = "#d4d3a0", high = "#8c8b3c") +
    labs( x = "State", y = "Total Assault Against Women",
          title = "Top 10 states by Number of Assault Against Women of 20 years (2001 - 2021) in India") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ## Top 10 states with highest number of Kidnap and Assault
  
  K.A_by_state <- crimes_on_women %>%
    group_by(State) %>%
    summarize(total_K.A = sum(K.A, na.rm = T)) %>%
    arrange(desc(total_K.A))
  
  print(K.A_by_state[1:10, ])
  
  
  ## Display top 10 states with highest number of Kidnap and Assault
  
  K.A_by_state[1:10, ] %>%
    ggplot(aes(reorder(State, -total_K.A), total_K.A, fill = total_K.A)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#8c88dd", high = "#1009a2") +
    theme_bw()+
    labs( x = "State", y = "Total Kidnap and Assault",
          title = "Top 10 states by Number of Kidnap and Assault of 20 years (2001 - 2021) in India") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  ## Top 10 states with highest number of rapes
  
  rape_by_state <- crimes_on_women %>%
    group_by(State) %>%
    summarize(total_rape = sum(Rape, na.rm = T)) %>%
    arrange(desc(total_rape))
  
  print(rape_by_state[1:10, ])
  
  
  ### Display top 10 states with highest number of rape
  
  rape_by_state[1:10, ] %>%
    ggplot(aes(x = reorder(State, -total_rape), y = total_rape, fill = total_rape)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#cc96ec", high = "#4b0775") +
    theme_bw() +
    labs(x = "State", y = "Total Rapes", title = "Top 10 States by Number of Rapes of 20 years (2001 - 2021) in India") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ## Top 10 states with highest number of Assault against Modesty of Women
  
  AoM_by_state <- crimes_on_women %>%
    group_by(State) %>%
    summarize(total_AoM = sum(AoM, na.rm = T)) %>%
    arrange(desc(total_AoM))
  
  print(AoM_by_state[1:10, ])
  
  
  ### Display top 10 states with highest number Assault against Modesty of Women
  
  AoM_by_state[1:10, ] %>%
    ggplot(aes(reorder(State, -total_AoM), total_AoM, fill = total_AoM)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#d6a3b4", high = "#84072f") +
    theme_bw()+
    labs( x = "State", y = "Total Assault against Modesty of Women",
          title = "Top 10 states by Number of Assault against Modesty of Women of 20 years (2001 - 2021) in India") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  ## Top 10 states with highest number of Dowry Death
  
  DD_by_state <- crimes_on_women %>%
    group_by(State) %>%
    summarize(total_DD = sum(DD, na.rm = T)) %>%
    arrange(desc(total_DD))
  
  print(DD_by_state[1:10, ])
  
  
 ### Display top 10 states with highest number of Dowry Deaths
  
  DD_by_state[1:10, ] %>%
    ggplot(aes(x = reorder(State, -total_DD), y = total_DD, fill = total_DD)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#9ee8db", high = "#0b6c5b") +  # Custom color gradient
    theme_bw() +
    labs(x = "State", y = "Total Dowry Deaths",
         title = "Top 10 States by Number of Dowry Deaths in 20 Years (2001 - 2021) in India") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  ## Top 10 states with highest number of Women Trafficking
  
  WT_by_state <- crimes_on_women %>%
    group_by(State) %>%
    summarize(total_WT = sum(WT, na.rm = T)) %>%
    arrange(desc(total_WT))
  
  print(WT_by_state[1:10, ])
  
  
  
  ### Display top 10 states with highest number of Women Trafficking
  
  WT_by_state[1:10, ] %>%
    ggplot(aes(reorder(State, -total_WT),total_WT, fill = total_WT)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#e3a2de", high = "#780870") +  
    theme_bw() +
    labs(x = "State", y = "Number of Women Trafficking",
         title = "Top 10 States by Number of Women Trafficking in 20 Years (2001 - 2021) in India") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  

    
  
  
  
  

  

  
  
  

  

  
  
  
  


