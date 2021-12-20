#Import libraries
library(ggplot2)
library(magrittr)
library(tidyr)
library(reshape2)
#Disable scentific notation
options(scipen=999)
#-------------------------------------------------------------------------------
#Data taken from: https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv
#Definitions
strDIR = "C:/Users/kwasn/Documents/"
strSourceName = "owid-covid-data.csv"

#-------------------------------------------------------------------------------
#Importing data
df_Cov = read.csv(paste(strDIR,strSourceName, sep = ""))

colnames(df_Cov)
df_Cov = cbind(seq(from = 1, to = nrow(df_Cov), by = 1), df_Cov)

#Changing name of column that I doesn't like.
names(df_Cov)[names(df_Cov) == "seq(from = 1, to = nrow(df_Cov), by = 1)"] = "ID"
colnames(df_Cov)

#-------------------------------------------------------------------------------
#Data cleaning
unique(df_Cov$location)
unique(df_Cov$continent)
df_Cov = df_Cov[df_Cov$continent != "",]
df_Cov$date = as.Date(df_Cov$date)
unique(df_Cov$tests_units)
summary(df_Cov)

#Replacing NA's with previous values for each country separately
for (country in unique(df_Cov$location)) {
  df_Cov[df_Cov$location == country,] = fill(df_Cov[df_Cov$location == country,], c("total_cases",
                          "total_deaths",
                          "total_cases_per_million",
                          "total_deaths_per_million",
                          "reproduction_rate",
                          "icu_patients",
                          "icu_patients_per_million",
                          "hosp_patients",
                          "hosp_patients_per_million",
                          "total_tests",
                          "total_tests_per_thousand",
                          "total_vaccinations",
                          "people_vaccinated",
                          "people_fully_vaccinated"), 
                .direction = "down")
}

#Dropping unnecessary columns
drops = c("tests_units", "iso_code")
df_Cov = df_Cov[, !(colnames(df_Cov)) %in% drops]

#Replacing remaining NA's with 0's
df_Cov = df_Cov %>% replace(is.na(.), 0)

summary(df_Cov)

#-------------------------------------------------------------------------------
#Analysis for chosen country (year to year)

sCountry = 'Poland'

df_Country = df_Cov[df_Cov$location==sCountry,]
rownames(df_Country) = 1:NROW(df_Country)
head(df_Country)

y1 = 366 - (df_Country[year(df_Country$date) == 2020,] %>% NROW())
y2 = 366 - df_Country[year(df_Country$date) == 2021,] %>% NROW()

nc2020 = c(rep(x = 0, y1), df_Country[year(df_Country$date) == 2020,'new_cases_per_million'])
nc2021 = c(df_Country[year(df_Country$date) == 2021,'new_cases_per_million'], rep(x = NA, y2))
ncs2020 = c(rep(x = 0, y1), df_Country[year(df_Country$date) == 2020,'new_cases_smoothed_per_million'])
ncs2021 = c(df_Country[year(df_Country$date) == 2021,'new_cases_smoothed_per_million'], rep(x = NA, y2))
nd2020 = c(rep(x = 0, y1), df_Country[year(df_Country$date) == 2020,'new_deaths_smoothed_per_million'])
nd2021 = c(df_Country[year(df_Country$date) == 2021,'new_deaths_smoothed_per_million'], rep(x = NA, y2))
nh2020 = c(rep(x = 0, y1), df_Country[year(df_Country$date) == 2020,'hosp_patients_per_million'])
nh2021 = c(df_Country[year(df_Country$date) == 2021,'hosp_patients_per_million'], rep(x = NA, y2))

days = seq(from = ISOdate(2020, 01, 01),
           to = ISOdate(2020, 12, 31),
           by = ISOdate(2020, 01, 02)-ISOdate(2020, 01, 01))
days = as.Date(days)

df_Country_1Y = data.frame(day_of_year = days,
                       new_cases_2020 = nc2020,
                       new_cases_2021 = nc2021,
                       new_cases_smthd_2020 = ncs2020,
                       new_cases_smthd_2021 = ncs2021,
                       new_deaths_2020 = nd2020,
                       new_deaths_2021 = nd2021,
                       patients_hosp_20 = nh2020,
                       patients_hosp_21 = nh2021)

rm(list = c('days', 'nc2020', 'nc2021', 'ncs2020', 'ncs2021', 'nd2020', 'nd2021', 'nh2020', 'nh2021'))

ggplot(df_Country_1Y) +
  geom_line(aes(x=day_of_year, y=new_cases_2020, color = '2020')) +
  geom_line(aes(x=day_of_year, y=new_cases_2021, color = '2021')) +
  theme(legend.position = c(0, 1),legend.justification = c(0, 1)) +
  labs(colour = 'Years', y = 'New cases', x = 'Days of year') +
  ggtitle('New cases per million 2020 vs 2021', subtitle = sCountry) + 
  scale_color_manual(values = c('orange', 'blue')) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")

ggplot(df_Country_1Y) +
  geom_line(aes(x=day_of_year, y=new_cases_smthd_2020, color = '2020')) +
  geom_line(aes(x=day_of_year, y=new_cases_smthd_2021, color = '2021')) +
  theme(legend.position = c(0, 1),legend.justification = c(0, 1)) +
  labs(colour = 'Years', y = 'New cases', x = 'Days of year') +
  ggtitle('New cases per million 2020 vs 2021', subtitle = sCountry) + 
  scale_color_manual(values = c('orange', 'blue')) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") 

ggplot(df_Country_1Y) +
  geom_line(aes(x=day_of_year, y=new_deaths_2020, color = '2020')) +
  geom_line(aes(x=day_of_year, y=new_deaths_2021, color = '2021')) +
  theme(legend.position = c(0, 1),legend.justification = c(0, 1)) +
  labs(colour = 'Years', y = 'New deaths', x = 'Days of year') +
  ggtitle('New deaths per million 2020 vs 2021', subtitle = sCountry) + 
  scale_color_manual(values = c('orange', 'blue')) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") 

ggplot(df_Country_1Y) +
  geom_line(aes(x=day_of_year, y=patients_hosp_20, color = '2020')) +
  geom_line(aes(x=day_of_year, y=patients_hosp_21, color = '2021')) +
  theme(legend.position = c(0, 1),legend.justification = c(0, 1)) +
  labs(colour = 'Years', y = 'Hosp_patients', x = 'Days of year') +
  ggtitle('Hosp patients per million 2020 vs 2021', subtitle = sCountry) + 
  scale_color_manual(values = c('orange', 'blue')) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")

ggplot(df_Country) + 
  geom_point(aes(y = new_cases_per_million, x = people_fully_vaccinated_per_hundred),
             shape = 4, colour = "red") +
  labs(x = 'People fully vaccinated per hundred', y = 'New cases per million' ) +
  ggtitle('Vaccination vs cases', subtitle = sCountry)
  scale_x_continuous(n.breaks = 6)

ggplot(df_Country) +
  geom_line(aes(y = people_fully_vaccinated, x = as.Date(date), colour = "Vaccinations")) +
  geom_line(aes(y = total_cases, x = as.Date(date), colour = "Cases")) + 
  labs(x = 'Date', y = 'Number') + 
  ggtitle('Total cases vs people fully vaccinated', subtitle = sCountry) + 
  scale_color_manual(values = c('orange', 'blue')) +
  scale_x_date(date_breaks = '2 months', date_labels = "%b %y")

#Checking death percentage through the pandemic
df_Country$death_percentage = (df_Country$total_deaths / df_Country$total_cases * 100) %>% 
  round(digits = 2)
df_Country$death_percentage

#Coefficient for secondary Y-axis
coeff = 100
ggplot(df_Country) +
  geom_line(aes(x=date, y=death_percentage, colour = 'Fatality, %')) +
  geom_line(aes(x=date, y=new_cases_smoothed_per_million/coeff, colour = 'New cases per 1M')) +
  scale_y_continuous(
    name = "Fatality percentage, %",
    sec.axis = sec_axis(~.*coeff, name="New cases per million")
  ) +
  labs(colour = '') +
  ggtitle(label = paste('Covid in', sCountry), subtitle = 'New cases & fatality')

#-------------------------------------------------------------------------------
#Analyses for World - timeseries

ggplot() +
  geom_line(data = df_Cov[df_Cov$location == 'Poland',], 
            aes(x=date, y=new_cases_smoothed_per_million, colour = 'Poland'), size = 1) +
  geom_line(data = df_Cov[df_Cov$location == 'Germany',], 
            aes(x=date, y=new_cases_smoothed_per_million, colour = 'Germany')) +
  geom_line(data = df_Cov[df_Cov$location == 'France',], 
            aes(x=date, y=new_cases_smoothed_per_million, colour = 'France')) +
  geom_line(data = df_Cov[df_Cov$location == 'United Kingdom',], 
            aes(x=date, y=new_cases_smoothed_per_million, colour = 'UK')) +
  scale_x_date(date_breaks = '2 months', date_labels = "%b %y") +
  labs(colour = '') +
  ggtitle(label = 'Covid in countries', subtitle = 'New cases')


ggplot() +
  geom_line(data = df_Cov[df_Cov$location == 'Poland',], 
            aes(x=date, y=new_deaths_smoothed_per_million, colour = 'Poland'), size = 1) +
  geom_line(data = df_Cov[df_Cov$location == 'Germany',], 
            aes(x=date, y=new_deaths_smoothed_per_million, colour = 'Germany')) +
  geom_line(data = df_Cov[df_Cov$location == 'France',], 
            aes(x=date, y=new_deaths_smoothed_per_million, colour = 'France')) +
  geom_line(data = df_Cov[df_Cov$location == 'United Kingdom',], 
            aes(x=date, y=new_deaths_smoothed_per_million, colour = 'UK')) +
  scale_x_date(date_breaks = '2 months', date_labels = "%b %y") +
  labs(colour = '') +
  ggtitle(label = 'Covid in countries', subtitle = 'New deaths')

ggplot() +
  geom_line(data = df_Cov[df_Cov$location == 'Poland',], 
            aes(x=date, y=hosp_patients_per_million, colour = 'Poland'), size = 1) +
  geom_line(data = df_Cov[df_Cov$location == 'Germany',], 
            aes(x=date, y=hosp_patients_per_million, colour = 'Germany')) +
  geom_line(data = df_Cov[df_Cov$location == 'France',], 
            aes(x=date, y=hosp_patients_per_million, colour = 'France')) +
  geom_line(data = df_Cov[df_Cov$location == 'United Kingdom',], 
            aes(x=date, y=hosp_patients_per_million, colour = 'UK')) +
  scale_x_date(date_breaks = '2 months', date_labels = "%b %y") +
  labs(colour = '') +
  ggtitle(label = 'Covid in countries', subtitle = 'Hosp patients')

#-------------------------------------------------------------------------------
#Analyses for World - looking for correlations
dfSummary = df_Cov[df_Cov$date == max(df_Cov$date),]
dfSummary$death_percentage = (dfSummary$total_deaths / dfSummary$total_cases * 100) %>% 
  round(digits = 2)
dfSummary %>% summary()

picks = c(
  #'continent',
  #'location',
  'total_tests_per_thousand',
  'total_vaccinations_per_hundred',
  'people_fully_vaccinated_per_hundred',
  'population',
  'median_age',
  'aged_70_older',
  'extreme_poverty',
  'diabetes_prevelance',
  'male_smokers',
  'hospital_beds_per_thousand',
  'human_development_index',
  #'excess_mortality_cumulative_per_million',
  'total_deaths_per_million',
  'total_cases_per_million',
  'icu_patients_per_million',
  'hosp_patients_per_million',
  'people_vaccinated_per_hundred',
  'total_boosters_per_hundred',
  'population_density',
  'aged_65_older',
  'gdp_per_capita',
  'cardiovasc_death_rate',
  'female_smokers',
  'handwashing_facilities',
  'life_expectancy',
  #'excess_mortality_cumulative_absolute',
  'death_percentage'
)

dfSummary = dfSummary[, (colnames(dfSummary)) %in% picks]
dfSummary %>% summary()

#Correlation matrix (Pearson)
corr_matrix = dfSummary %>% cor() %>% round(digits = 2)

#Correlation heatmap using built-in function
heatmap(corr_matrix, scale = 'none', col = terrain.colors(24), Colv = NA, Rowv = NA, 
        RowSideColors = terrain.colors(24))

#Reordering correlation matrix
dd = as.dist((1-corr_matrix)/2)
hc = hclust(dd)
corr_matrix = corr_matrix[hc$order, hc$order]

#Removing redundant information - leaving lower part of the matrix
lower_tri = corr_matrix
lower_tri[upper.tri(corr_matrix)] = NA
lower_tri
melted_corr_matrix = lower_tri %>% melt(na.rm = T)
melted_corr_matrix

#Correlation heatmap - using ggplot2
ggheatmap = ggplot(melted_corr_matrix) + 
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  labs(caption = 'TomaszDataResearcher') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
 
#-------------------------------------------------------------------------------
#Analyses for World - correlation: cases to other params
corr_df = data.frame(
  params = names(corr_matrix['total_cases_per_million', -1]),
  correlation = unname(corr_matrix['total_cases_per_million', -1]))

corr_df = corr_df[order(corr_df$correlation, decreasing = T),]
rownames(corr_df) = 1:(NROW(corr_df))

ggplot(corr_df) +
  geom_col(aes(x = reorder(params, -correlation), y = correlation, fill = correlation)) +
  labs(title = 'Correlation', subtitle = 'Total cases to other parameters',
      y = 'correlation',caption = 'TomaszDataResearcher') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1),
        axis.title.x = element_blank(), 
        legend.direction = 'horizontal',
        legend.position = c(.8, .7),
        legend.background = element_blank()) +
  scale_fill_gradient2(low='blue', mid='white', high='red') +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

#-------------------------------------------------------------------------------
#Analyses for World - correlation: deaths to other params
corr_df = data.frame(
  params = names(corr_matrix['total_deaths_per_million', -2]),
  correlation = unname(corr_matrix['total_deaths_per_million', -2]))

corr_df = corr_df[order(corr_df$correlation, decreasing = T),]
rownames(corr_df) = 1:(NROW(corr_df))

ggplot(corr_df) +
  geom_col(aes(x = reorder(params, -correlation), y = correlation, fill = correlation)) +
  labs(title = 'Correlation', subtitle = 'Total deaths to other parameters',
       y = 'correlation', caption = 'TomaszDataResearcher') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1),
        axis.title.x = element_blank(), 
        legend.direction = 'horizontal',
        legend.position = c(.8, .7),
        legend.background = element_blank()) +
  scale_fill_gradient2(low='blue', mid='white', high='red') +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))
