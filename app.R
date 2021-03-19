library(shiny)
library(ggplot2)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(rsconnect)
library(extrafont)
library(ggrepel)
library(shinyWidgets)
library(shinycssloaders)

###Pyramid chart data wrangling=====================================
#Male
#Loading the male excel
pyramid_male <- read_excel("WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx", sheet = "ESTIMATES", na = "...")

#row_to_names() from janitor package removes the rows we want and make the row we want as the header of the data frame. 
#very useful function
pyrmd_male_cln <- row_to_names(pyramid_male, row_number = 12, remove_rows_above = 1:11) 

#cleaning the data frame
pyrmd_male_cln <- pyrmd_male_cln %>% 
  select(`Region, subregion, country or area *`, `Reference date (as of 1 July)`, 9: 29) %>% 
  rename(Region_country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`) 

#dropping NAs
pyrmd_male_cln <- drop_na(pyrmd_male_cln) 

#converting from wide to long the age column
pyrmd_sexes <- pyrmd_male_cln %>% 
  pivot_longer(cols = 3: 23, names_to="age", values_to="pop") 

#dropping year 2020 as we will take it from the Medium Variant sheet
pyrmd_sexes <- pyrmd_sexes %>% 
  filter(year!=c(2020)) 

#loading the median variant sheet of the excel********
pyramid_mal_medium <- read_excel("WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx", sheet = "MEDIUM VARIANT", na = "...")
pyrmd_med_clnM <- row_to_names(pyramid_mal_medium , row_number = 12, remove_rows_above = 1:11) 

#cleaning the data frame
pyrmd_med_clnM <- pyrmd_med_clnM %>% 
  select(`Region, subregion, country or area *`, `Reference date (as of 1 July)`, 9: 29) %>% 
  rename(Region_country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`) 

#dropping all the NAs
pyrmd_med_clnM  <- drop_na(pyrmd_med_clnM) 

#converting from wide to long the age column
pyrmd_sexes_med <- pyrmd_med_clnM %>% 
  pivot_longer(cols = 3: 23, names_to="age", values_to="pop") 

#combining the two dataframes 
pyrmd_sexes_tot <- rbind(pyrmd_sexes, pyrmd_sexes_med) 

#creating a gender variable of the length of pyrmd_sexes_tot
gender <- rep("male", length(pyrmd_sexes_tot$year))

#adding the gender column to the data frame
pyrmd_sexes_tot <- cbind(pyrmd_sexes_tot, gender) 

#Female
#loading the female excel============
pyramid_female <- read_excel("WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx", sheet = "ESTIMATES", na = "...")

#row_to_names() from janitor package removes the rows we want and make the row we want as the header of the data frame. 
#very useful function
pyrmd_female_cln <- row_to_names(pyramid_female, row_number = 12, remove_rows_above = 1:11) 

#cleaning the data frame
pyrmd_female_cln<- pyrmd_female_cln %>% 
  select(`Region, subregion, country or area *`, `Reference date (as of 1 July)`, 9: 29) %>% 
  rename(Region_country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`) 

#dropping NAs
pyrmd_female_cln <- drop_na(pyrmd_female_cln) 

#converting from wide to long the age column
pyrmd_sexes_fm <- pyrmd_female_cln %>% 
  pivot_longer(cols = 3: 23, names_to="age", values_to="pop") 

#dropping year 2020 as we will take it from the Medium Variant sheet
pyrmd_sexes_fm  <- pyrmd_sexes_fm %>% 
  filter(year!=c(2020)) 

#loading the median variant sheet of the excel********
pyramid_femal_medium <- read_excel("WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx", sheet = "MEDIUM VARIANT", na = "...")
pyrmd_med_clnF <- row_to_names(pyramid_femal_medium  , row_number = 12, remove_rows_above = 1:11) 

#cleaning the data frame
pyrmd_med_clnF <- pyrmd_med_clnF %>% 
  select(`Region, subregion, country or area *`, `Reference date (as of 1 July)`, 9: 29) %>% 
  rename(Region_country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`) 

#dropping all the NAs
pyrmd_med_clnF <- drop_na(pyrmd_med_clnF) 

#converting from wide to long the age column
pyrmd_sexes_med_Fm <- pyrmd_med_clnF %>% 
  pivot_longer(cols = 3: 23, names_to="age", values_to="pop") 

#combining the two long dataframes 
pyrmd_sexes_tot_fm <- rbind(pyrmd_sexes_fm, pyrmd_sexes_med_Fm) 

#creating a gender variable of the length of pyrmd_sexes_tot
gender <- rep("female", length(pyrmd_sexes_tot_fm$year))

#adding the gender column to the data frame
pyrmd_sexes_tot_fm <- cbind(pyrmd_sexes_tot_fm, gender) 

#Combining the male and Female data frames to create final data frame
pyrmd_sexes_final <- rbind(pyrmd_sexes_tot_fm , pyrmd_sexes_tot) 


#Working on creating the pyramid chart
pyrmd_sexes_final$age <- factor(pyrmd_sexes_final$age, levels=c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                                                    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                                                                    "75-79", "80-84", "85-89", "90-94", "95-99", "100+"), ordered=TRUE)

  
pyrmd_sexes_shiny <- function(country_name, first_year, second_year, third_year, type){
  
  pyrmd_sexes_country <- pyrmd_sexes_final %>%
    filter(Region_country==country_name)
  
  pyrmd_sexes_country$pop <- as.numeric(pyrmd_sexes_country$pop)
  
  #creating new variable which is .5 step
  pyrmd_sexes_country <- pyrmd_sexes_country %>% 
    mutate(ageno = as.numeric(age) - 0.5)
  
  if(type=="Bar"){
    
    ###ONLY RUN THIS IF MAKING THE BAR PYRAMID. DO NOT RUN IF CREATING STEP PYRAMID###
    pyrmd_sexes_country <- pyrmd_sexes_country %>%
      bind_rows(pyrmd_sexes_country %>% filter(year==third_year, age=="100+") %>% mutate(ageno = ageno + 1)) 
    
    #bar pyramid chart
    g <- ggplot(data = pyrmd_sexes_country, aes(x = age, y = pop, fill = as.factor(year))) +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "female", year ==third_year),
               stat = "identity",
               position = "identity") +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "male", year == third_year) ,
               stat = "identity",
               position = "identity",
               mapping = aes(y = -pop)) +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "female", year ==second_year),
               stat = "identity",
               position = "identity") +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "male", year == second_year) ,
               stat = "identity",
               position = "identity",
               mapping = aes(y = -pop))+
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "female", year ==first_year),
               stat = "identity",
               position = "identity") +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "male", year == first_year) ,
               stat = "identity",
               position = "identity",
               mapping = aes(y = -pop)) +
      coord_flip() +
      theme_minimal()+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = abs)+
      # scale_y_continuous(labels = abs)+
      theme(legend.title = element_blank(), 
            legend.position = "top",
            legend.text = element_text(size = 10,  family = "Corbel"),
            plot.title = element_text(face = "bold", size = 15),
            axis.title = element_text(size = 10, face = "bold",  family = "Corbel"),
            axis.text = element_text(size = 9, face = "bold",  family = "Corbel"))+
      annotate(geom="text", y=max(pyrmd_sexes_country$pop*.75), x=21, label="Female", size=4, fontface="bold",  family = "Corbel")+
      annotate(geom="text", y=-max(pyrmd_sexes_country$pop*.75), x=21, label="Male", size=4, fontface="bold",  family = "Corbel") +
      xlab("Age")+
      ylab ("Population (thousands)")+
      labs(title = paste(country_name, ": Population Pyramid (bars)"))
    
    g
    
  }else if(type=="Line"){
    
    pyrmd_sexes_country <- pyrmd_sexes_country %>%
      bind_rows(pyrmd_sexes_country %>% filter(year==third_year, age=="100+") %>% mutate(ageno = ageno + 1)) 
    
    ggplot(data = pyrmd_sexes_country, aes(x = unique(pyrmd_sexes_country$age), y = pop)) +
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "female", year == first_year), 
                aes(x = ageno,  color=first_year),size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "male", year == first_year), 
                aes(x = ageno, y=-pop, color=first_year),size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "female", year == second_year), 
                aes(x = ageno,  color=second_year),size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "male", year == second_year), 
                aes(x = ageno, y=-pop, color=second_year),size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "female", year == third_year), 
                aes(x = ageno, color=third_year), size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "male", year == third_year), 
                aes(x = ageno, y=-pop,  color=third_year),size=.5)+

      scale_x_discrete(limits=factor(unique(pyrmd_sexes_country$ageno)), labels=c("0-4", "5-9", "10-14", "15-19", "20-24",
                                                                                  "25-29", "30-34", "35-39", "40-44",
                                                                                  "45-49", "50-54", "55-59", "60-64", "65-69",
                                                                                  "70-74", "75-79", "80-84", "85-89", "90-94",
                                                                                  "95-99", "100+", ""))+
      coord_flip()+ 
      theme_minimal()+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = abs)+
      theme(legend.title = element_blank(), 
            legend.position = "top",
            legend.text = element_text(size = 10),
            plot.title = element_text(face = "bold", size = 15),
            axis.title = element_text(size = 10, face = "bold"),
            axis.text = element_text(size = 9, face = "bold"))+
      annotate(geom="text", y=max(pyrmd_sexes_country$pop*.75), x=21, label="Female", size=4, fontface="bold",  family = "Corbel")+
      annotate(geom="text", y=-max(pyrmd_sexes_country$pop*.75), x=21, label="Male", size=4, fontface="bold",  family = "Corbel") +
      xlab("Age")+
      ylab ("Population (thousands)")+
      labs(title = paste(country_name, ": Population Pyramid (line)"))
  }
} 

# pyramid download function--
pyrmd_sexes_download <- function(country_name, first_year, second_year, third_year, type){
  
  pyrmd_sexes_country <- pyrmd_sexes_final %>%
    filter(Region_country==country_name)
  
  pyrmd_sexes_country$pop <- as.numeric(pyrmd_sexes_country$pop)
  
  #creating new variable which is .5 step
  pyrmd_sexes_country <- pyrmd_sexes_country %>% 
    mutate(ageno = as.numeric(age) - 0.5)
  
  if(type=="Bar"){
    
    ###ONLY RUN THIS IF MAKING THE BAR PYRAMID. DO NOT RUN IF CREATING STEP PYRAMID###
    pyrmd_sexes_country <- pyrmd_sexes_country %>%
      bind_rows(pyrmd_sexes_country %>% filter(year==third_year, age=="100+") %>% mutate(ageno = ageno + 1)) 
    
    #bar pyramid chart
    g <- ggplot(data = pyrmd_sexes_country, aes(x = age, y = pop, fill = as.factor(year))) +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "female", year ==third_year),
               stat = "identity",
               position = "identity") +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "male", year == third_year) ,
               stat = "identity",
               position = "identity",
               mapping = aes(y = -pop)) +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "female", year ==second_year),
               stat = "identity",
               position = "identity") +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "male", year == second_year) ,
               stat = "identity",
               position = "identity",
               mapping = aes(y = -pop))+
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "female", year ==first_year),
               stat = "identity",
               position = "identity") +
      geom_bar(data = pyrmd_sexes_country %>% filter(gender == "male", year == first_year) ,
               stat = "identity",
               position = "identity",
               mapping = aes(y = -pop)) +
      coord_flip() +
      theme_minimal()+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = abs)+
      # scale_y_continuous(labels = abs)+
      theme(legend.title = element_blank(), 
            legend.position = "top",
            legend.text = element_text(size = 5,  family = "Corbel"),
            axis.title = element_text(size = 5, face = "bold",  family = "Corbel"),
            axis.text = element_text(size = 5, face = "bold",  family = "Corbel"))+
      annotate(geom="text", y=max(pyrmd_sexes_country$pop*.75), x=21, label="Female", size=2.5, fontface="bold",  family = "Corbel")+
      annotate(geom="text", y=-max(pyrmd_sexes_country$pop*.75), x=21, label="Male", size=2.5, fontface="bold",  family = "Corbel") +
      xlab("Age")+
      ylab ("Population (thousands)")+
      labs(title = paste(country_name, ":\n Population Pyramid (bars)"))
    
    g
    
  }else if(type=="Line"){
    
    pyrmd_sexes_country <- pyrmd_sexes_country %>%
      bind_rows(pyrmd_sexes_country %>% filter(year==third_year, age=="100+") %>% mutate(ageno = ageno + 1)) 
    
    ggplot(data = pyrmd_sexes_country, aes(x = unique(pyrmd_sexes_country$age), y = pop)) +
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "female", year == first_year), 
                aes(x = ageno,  color=first_year),size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "male", year == first_year), 
                aes(x = ageno, y=-pop, color=first_year),size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "female", year == second_year), 
                aes(x = ageno,  color=second_year),size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "male", year == second_year), 
                aes(x = ageno, y=-pop, color=second_year),size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "female", year == third_year), 
                aes(x = ageno, color=third_year), size=.5)+
      geom_step(data =  pyrmd_sexes_country %>% filter(gender == "male", year == third_year), 
                aes(x = ageno, y=-pop,  color=third_year),size=.5)+
      
      scale_x_discrete(limits=factor(unique(pyrmd_sexes_country$ageno)), labels=c("0-4", "5-9", "10-14", "15-19", "20-24",
                                                                                  "25-29", "30-34", "35-39", "40-44",
                                                                                  "45-49", "50-54", "55-59", "60-64", "65-69",
                                                                                  "70-74", "75-79", "80-84", "85-89", "90-94",
                                                                                  "95-99", "100+", ""))+
      coord_flip()+ 
      theme_minimal()+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = abs)+
      theme(legend.title = element_blank(), 
            legend.position = "top",
            legend.text = element_text(size = 7),
            axis.title = element_text(size = 7, face = "bold"),
            axis.text = element_text(size = 6, face = "bold"))+
      annotate(geom="text", y=max(pyrmd_sexes_country$pop*.75), x=21, label="Female", size=2.5, fontface="bold",  family = "Corbel")+
      annotate(geom="text", y=-max(pyrmd_sexes_country$pop*.75), x=21, label="Male", size=2.5, fontface="bold",  family = "Corbel") +
      xlab("Age")+
      ylab ("Population (thousands)")+
      labs(title = paste(country_name, ":\n Population Pyramid (line)"))
  }
}  

###Population broad age chart data wrangling=====================================
broad_est <- read_excel("WPP2019_POP_F08_1_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_BOTH_SEXES.xlsx", sheet = "ESTIMATES", na = "...")
broad_med <- read_excel("WPP2019_POP_F08_1_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_BOTH_SEXES.xlsx", sheet = "MEDIUM VARIANT", na = "...")
broad_high <- read_excel("WPP2019_POP_F08_1_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_BOTH_SEXES.xlsx", sheet = "HIGH VARIANT", na = "...")
broad_low <- read_excel("WPP2019_POP_F08_1_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_BOTH_SEXES.xlsx", sheet = "LOW VARIANT", na = "...")

#Normal Estimate
#row_to_names() from janitor package removes the rows we want and make the row we want as the header of the data frame. 
#very useful function
broad_est_cln <- row_to_names(broad_est, row_number = 12, remove_rows_above = 1:11) 

#cleaning the data frame
broad_est_cln <- broad_est_cln %>% 
  select(`Region, subregion, country or area *`, `Reference date (as of 1 July)`, `Total`, `0-14`, `15-24`, `25-64`, `65+`) %>% 
  rename(Region_country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`) 

#dropping NAs
broad_est_cln <- drop_na(broad_est_cln) 

#dropping 2020 from this data frame
broad_est_cln <- broad_est_cln %>% 
  filter(year!=2020) 

#Medium variant--
#row_to_names() from janitor package removes the rows we want and make the row we want as the header of the data frame. 
#very useful function
broad_med_cln<- row_to_names(broad_med, row_number = 12, remove_rows_above = 1:11) 

#cleaning the data frame
broad_med_cln <- broad_med_cln %>% 
  select(`Region, subregion, country or area *`, `Reference date (as of 1 July)`, `Total`, `0-14`, `15-24`, `25-64`, `65+`) %>% 
  rename(Region_country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`) 

#dropping NAs
broad_med_cln  <- drop_na(broad_med_cln)


#High variant--
#row_to_names() from janitor package removes the rows we want and make the row we want as the header of the data frame. 
#very useful function
broad_high_cln<- row_to_names(broad_high, row_number = 12, remove_rows_above = 1:11) 

#cleaning the data frame
broad_high_cln <- broad_high_cln %>% 
  select(`Region, subregion, country or area *`, `Reference date (as of 1 July)`, `Total`, `0-14`, `15-24`, `25-64`, `65+`) %>% 
  rename(Region_country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`) 

#dropping NAs
broad_high_cln  <- drop_na(broad_high_cln)


#Low variant--
broad_low_cln<- row_to_names(broad_low, row_number = 12, remove_rows_above = 1:11) 

#cleaning the data frame
broad_low_cln <- broad_low_cln %>% 
  select(`Region, subregion, country or area *`, `Reference date (as of 1 July)`, `Total`, `0-14`, `15-24`, `25-64`, `65+`) %>% 
  rename(Region_country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`) 

#dropping NAs
broad_low_cln  <- drop_na(broad_low_cln)

#combining the data frames
broad_med_tot <- rbind(broad_med_cln, broad_est_cln) 
broad_high_tot <- rbind(broad_high_cln, broad_est_cln)
broad_low_tot <- rbind(broad_low_cln, broad_est_cln)

#creating the total population dataframe---
broad_totpop <- broad_med_tot %>% 
  select(Region_country, year, Total) %>% 
  rename(`Medium variant`=`Total`, `Year`=`year`) 

broad_lowpop <- broad_low_tot %>% 
  select(Total) %>% 
  rename(`Low variant`=`Total`) 

broad_highpop <- broad_high_tot %>% 
  select(Total) %>% 
  rename(`High variant`=`Total`)

#create a new column
age <- rep("Total population", length(broad_totpop$Year))

#combining the three dataframes
df_totpop <- cbind(broad_totpop,broad_lowpop, broad_highpop, age) 

#changing class to numeric and dividing by 1000
cols <- c(3:5)
df_totpop[cols] <- lapply(df_totpop[cols], as.numeric) 
df_totpop[cols] <- lapply(df_totpop[cols], function(x){x/1000})  

#creating the 0-14 dataframe---
#creating the total population dataframe---
broad_014pop <- broad_med_tot %>% 
  select(Region_country, year, `0-14`) %>% 
  rename(`Medium variant`=`0-14`, `Year`=`year`) 

broad_low014 <- broad_low_tot %>% 
  select(`0-14`) %>% 
  rename(`Low variant`=`0-14`) 

broad_high014 <- broad_high_tot %>% 
  select(`0-14`) %>% 
  rename(`High variant`=`0-14`)

#create a new columns
age <- rep("0-14", length(broad_014pop$Year))

#combining the three dataframes
df_tot014 <- cbind(broad_014pop,broad_low014, broad_high014, age) 

#changing class to numeric and dividing by 1000
cols <- c(3:5)
df_tot014[cols] <- lapply(df_tot014[cols], as.numeric) 
df_tot014[cols] <- lapply(df_tot014[cols], function(x){x/1000})  

#creating the 15-24 dataframe---
#creating the total population dataframe---
broad_1524pop <- broad_med_tot %>% 
  select(Region_country, year, `15-24`) %>% 
  rename(`Medium variant`=`15-24`, `Year`=`year`) 

broad_low1524 <- broad_low_tot %>% 
  select(`15-24`) %>% 
  rename(`Low variant`=`15-24`) 

broad_high1524 <- broad_high_tot %>% 
  select(`15-24`) %>% 
  rename(`High variant`=`15-24`)

#create a new columns
age <- rep("15-24", length(broad_1524pop$Year))

#combining the three dataframes
df_tot1524 <- cbind(broad_1524pop,broad_low1524, broad_high1524, age)

#changing class to numeric and dividing by 1000
cols <- c(3:5)
df_tot1524[cols] <- lapply(df_tot1524[cols], as.numeric) 
df_tot1524[cols] <- lapply(df_tot1524[cols], function(x){x/1000})  

#creating the 25-64 dataframe---
#creating the total population dataframe---
broad_2564pop <- broad_med_tot %>% 
  select(Region_country, year, `25-64`) %>% 
  rename(`Medium variant`=`25-64`, `Year`=`year`) 

broad_low2564 <- broad_low_tot %>% 
  select(`25-64`) %>% 
  rename(`Low variant`=`25-64`) 

broad_high2564 <- broad_high_tot %>% 
  select(`25-64`) %>% 
  rename(`High variant`=`25-64`)

#create a new columns
age <- rep("25-64", length(broad_2564pop$Year))

#combining the three dataframes
df_tot2564 <- cbind(broad_2564pop,broad_low2564, broad_high2564, age)

#changing class to numeric and dividing by 1000
cols <- c(3:5)
df_tot2564[cols] <- lapply(df_tot2564[cols], as.numeric) 
df_tot2564[cols] <- lapply(df_tot2564[cols], function(x){x/1000}) 

#creating the 65+ dataframe---
#creating the total population dataframe---
broad_65pop <- broad_med_tot %>% 
  select(Region_country, year, `65+`) %>% 
  rename(`Medium variant`=`65+`, `Year`=`year`) 

broad_low65 <- broad_low_tot %>% 
  select(`65+`) %>% 
  rename(`Low variant`=`65+`) 

broad_high65 <- broad_high_tot %>% 
  select(`65+`) %>% 
  rename(`High variant`=`65+`)

#create a new columns
age <- rep("65+", length(broad_65pop$Year))

#combining the three dataframes
df_tot65 <- cbind(broad_65pop,broad_low65, broad_high65, age)

#changing class to numeric and dividing by 1000
cols <- c(3:5)
df_tot65[cols] <- lapply(df_tot65[cols], as.numeric) 
df_tot65[cols] <- lapply(df_tot65[cols], function(x){x/1000})

pop_broad_chart <- function(country_name, starting_year, ending_year){
  
  #subsetting data frames for selected country
  df_totpop_sub <- df_totpop %>% 
    filter(`Region_country`==country_name & Year>=starting_year & Year<=ending_year) 
  
  df_tot014_sub <- df_tot014 %>% 
    filter(`Region_country`==country_name & Year>=starting_year & Year<=ending_year) 
  
  df_tot1524_sub <- df_tot1524 %>% 
    filter(`Region_country`==country_name & Year>=starting_year & Year<=ending_year) 
  
  df_tot2564_sub <- df_tot2564 %>% 
    filter(`Region_country`==country_name & Year>=starting_year & Year<=ending_year)
  
  df_tot65_sub <- df_tot65 %>% 
    filter(`Region_country`==country_name & Year>=starting_year& Year<=ending_year)
  
  #creating the chart
  ggplot()+
    geom_line(data = df_totpop_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="#012169", size=.8)+
    geom_ribbon(data = df_totpop_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="#012169")+
    geom_text_repel(data = df_totpop_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),  hjust = 1, vjust=1.4, size=3.5, family = "Corbel")+
    
    geom_line(data = df_tot014_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="red", size=.8)+
    geom_ribbon(data = df_tot014_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="red")+
    geom_text_repel(data = df_tot014_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),  hjust = -1, vjust=1.4, size=3.5, family = "Corbel")+
    
    geom_line(data = df_tot1524_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="#2FF22F", size=.8)+
    geom_ribbon(data = df_tot1524_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="#2FF22F")+
    geom_text_repel(data = df_tot1524_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),hjust = -1, vjust=1.7, size=3.5, family = "Corbel")+
    
    
    geom_line(data = df_tot2564_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="#2FF2F2", size=.8)+
    geom_ribbon(data = df_tot2564_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="#2FF2F2")+
    geom_text_repel(data = df_tot2564_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),hjust = -1, vjust=-.3, size=3.5, family = "Corbel")+
    
    geom_line(data = df_tot65_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="#2F91F2", size=.8)+
    geom_ribbon(data = df_tot65_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="#2F91F2")+
    geom_text_repel(data = df_tot65_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),hjust = -1, vjust=-.3, size=3.5, family = "Corbel")+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
    
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 12, family = "Corbel"),
          axis.text.y = element_text(size = 12, family = "Corbel"),
          plot.title = element_text(face = "bold", size = 15),
          axis.title.y = element_text(face = "bold", size = 13, family = "Corbel"),
          axis.title.x = element_text(face = "bold", size = 13, family = "Corbel"))+
    ylab("Population (millions)")+
    labs(title = paste(country_name, ": Population by broad age groups"))
}

#function for plotting the downloading the chart--
pop_broad_download <- function(country_name, starting_year, ending_year){
  
  #subsetting data frames for selected country
  df_totpop_sub <- df_totpop %>% 
    filter(`Region_country`==country_name & Year>=starting_year & Year<=ending_year) 
  
  df_tot014_sub <- df_tot014 %>% 
    filter(`Region_country`==country_name & Year>=starting_year & Year<=ending_year) 
  
  df_tot1524_sub <- df_tot1524 %>% 
    filter(`Region_country`==country_name & Year>=starting_year & Year<=ending_year) 
  
  df_tot2564_sub <- df_tot2564 %>% 
    filter(`Region_country`==country_name & Year>=starting_year & Year<=ending_year)
  
  df_tot65_sub <- df_tot65 %>% 
    filter(`Region_country`==country_name & Year>=starting_year& Year<=ending_year)
  
  #creating the chart
  ggplot()+
    geom_line(data = df_totpop_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="#012169", size=.3)+
    geom_ribbon(data = df_totpop_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="#012169")+
    geom_text_repel(data = df_totpop_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),  hjust = 1, vjust=1.4, size=2.5, family = "Corbel")+
    
    geom_line(data = df_tot014_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="red", size=.3)+
    geom_ribbon(data = df_tot014_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="red")+
    geom_text_repel(data = df_tot014_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),  hjust = -1, vjust=1.4, size=2.5, family = "Corbel")+
    
    geom_line(data = df_tot1524_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="#2FF22F", size=.3)+
    geom_ribbon(data = df_tot1524_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="#2FF22F")+
    geom_text_repel(data = df_tot1524_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),hjust = -1, vjust=1.7, size=2.5, family = "Corbel")+
    
    
    geom_line(data = df_tot2564_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="#2FF2F2", size=.3)+
    geom_ribbon(data = df_tot2564_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="#2FF2F2")+
    geom_text_repel(data = df_tot2564_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),hjust = -1, vjust=-.3, size=2.5, family = "Corbel")+
    
    geom_line(data = df_tot65_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2), color="#2F91F2", size=.3)+
    geom_ribbon(data = df_tot65_sub, mapping = aes(x= Year, y=`Medium variant`, ymin=`Low variant`, ymax= `High variant`, group=2),alpha=0.2, fill="#2F91F2")+
    geom_text_repel(data = df_tot65_sub %>% 
                      filter(Year==ending_year), aes(x= Year, y=(`Medium variant`),label=`age`),hjust = -1, vjust=-.3, size=2.5, family = "Corbel")+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
    
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 7, family = "Corbel"),
          axis.text.y = element_text(size = 7, family = "Corbel"),
          axis.title.y = element_text(face = "bold", size = 10, family = "Corbel"),
          axis.title.x = element_text(face = "bold", size = 10, family = "Corbel"))+
    ylab("Population (millions)")+
    labs(title = paste(country_name, ":\n Population by broad age groups"))
}

##Shiny====
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      #ui for the pyramid charts----
      tags$h4((tags$b(("Population Pyramid Chart")))), 
      selectInput(inputId = "country_pyrmd", label = "Select a country", choices = NULL),
      
      tags$h6((tags$b(("Select the years:")))),
      
      div(style = "margin-top:-20px"),
      
      div(style="display:inline-block",
          selectInput("year1", "", choices = NULL, multiple = FALSE,  width = "80px")),
      
      div(style="display:inline-block",
          selectInput("year2", "", choices = NULL, multiple = FALSE,  width = "80px")),
      
      div(style="display:inline-block",
          selectInput("year3", "", choices = NULL, multiple = FALSE,  width = "80px")),
      
      div(style = "margin-top:-10px"),

      tags$h6(tags$i(tags$b("Note:")), tags$i("Dowloading the size of chart that fits your requirement for the 2-4 pager country profile requires a little adjustment. 
                       Please try different height and width options, and download the chart by clicking"), tags$i(tags$b("`Export pyramid as png`")),
              tags$i("and see what works for you. First download the chart with the pre-populated default size below, if it does not work adjust the 
                                              size.")),
      
      tags$h5((tags$i(("Select the size of the chart, in inches, to download or use the default values below:")))),
      
      div(style = "margin-top:-8px"),
      
      div(style="display:inline-block" ,numericInput("height_1", "Height:", min = 1, max = 15, value = 4,step = .5,
                   width = "70px")),
      
      div(style="display:inline-block", numericInput("width_1", "Width:", min = 1, max = 15, value = 4.5,step = .5,
                   width = "70px")),
  
      #ui for the population broad age chart----    
      br(),
      tags$h4((tags$b(("Population by broad age group Chart")))), 
      
      selectInput(inputId = "country_pop", label = "Select a country", choices = NULL),
      
       tags$h5((tags$i(("Note: `Start year` should be less that `End year` below.")))),
      
      div(style = "margin-top:-5px"),
      
      div(style="display:inline-block",
          selectInput("year_start", "Start year:", choices = NULL, multiple = FALSE,  width = "80px")),
      
      div(style="display:inline-block",
          selectInput("year_end", "End year:", choices = NULL, multiple = FALSE,  width = "80px")),
      
      div(style = "margin-top:-10px"),
      
      tags$h6(tags$i(tags$b("Note:")), tags$i("Dowloading the size of chart that fits your requirement for the 2-4 pager country profile requires a little adjustment. 
                       Please try different height and width options, and download the chart by clicking"), tags$i(tags$b("`Export pop. as png`")),
                       tags$i("and see what works for you. First download the chart with the pre-populated default size below, if it does not work adjust the 
                                              size.")),
      
      tags$h5((tags$i(("Select the size of the chart, in inches, to download or use the default values below:")))),
      
      div(style = "margin-top:-8px"),
      
      div(style="display:inline-block", numericInput("height", "Height:", min = 1, max = 15, value = 5,step = .5,
                   width = "70px")),
     
      div(style="display:inline-block", numericInput("width", "Width:", min = 1, max = 15, value = 4,step = .5,
                   width = "70px")),
      br(),
      "Code and input data used to generate charts are available on", a(href="https://github.com/pyarasid/UN_Population_interactive_APP", target="_blank" ,  "Github."),
    ),
    
    mainPanel(
      div(style="display:inline-block",
          radioGroupButtons("chart_type", label = "", choices = c("Bar", "Line"), 
                            selected = "Bar", size="sm")),
      div(style="display:inline-block",
          downloadButton('ExportPlot_1', 'Export pyramid as png', style='padding:2px; font-size:100%')),
      
      plotOutput("pyrmd_sexes_shiny")%>% withSpinner(),
      br(),
      
      div(style="display:inline-block",
          downloadButton('ExportPlot_2', 'Export pop. as png', style='padding:2px; font-size:100%')),
      
      plotOutput("pop_broad_chart")%>% withSpinner(),
      
    )
  )
  
)

server <- function(input, output, session){
  
#population pyramid update  
  updateSelectInput(session, "country_pyrmd", choices = unique(pyrmd_sexes_final$Region_country), selected = "Nigeria")
  
  updateSelectInput(session, "year1", choices = unique(pyrmd_sexes_final$year), selected = 2000)
  
  updateSelectInput(session, "year2", choices = unique(pyrmd_sexes_final$year), selected = 2020)
  
  updateSelectInput(session, "year3", choices = unique(pyrmd_sexes_final$year), selected = 2050)
  
#plot chart pyramid  
  plotInput <- function(){

    pyrmd_sexes_shiny(input$country_pyrmd, input$year1, input$year2, input$year3, input$chart_type) 
  }
  
  output$pyrmd_sexes_shiny <- renderPlot({
    plotInput()
  })

  #plot download pyramid  
  plotInput_downpyrmd <- function(){
    pyrmd_sexes_download(input$country_pyrmd, input$year1, input$year2, input$year3, input$chart_type) 
  }
  
  output$ExportPlot_1  <- downloadHandler(
    filename = function(){
      paste("Pyramid",input$country_pyrmd, input$chart_type, "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, plotInput_downpyrmd(), width = input$width_1, height = input$height_1, dpi = 600, units="in")
    })  
  
#population broad age update----
  updateSelectInput(session, "country_pop", choices= unique(df_totpop$Region_country), selected = "Nigeria")
  
  
  updateSelectInput(session, "year_start", choices= unique(sort(df_totpop$Year)), selected= 1990)

  observe({
    xx <-  df_totpop %>% 
      filter(Year>input$year_start) %>% 
      arrange(-desc(Year)) %>% 
      distinct(Year)
    
    updateSelectInput(session, "year_end", choices= xx, selected= 2050)
  })
  
#plot the broad population chart  
  plotInput_pop <- function(){
    
    pop_broad_chart(input$country_pop, input$year_start, input$year_end) 
  }
  
  output$pop_broad_chart <- renderPlot({
    plotInput_pop()
  })
  
#download the broad population chart  
  plot_download <- function(){
    pop_broad_download(input$country_pop, input$year_start, input$year_end)
  }
  
  
  output$ExportPlot_2  <- downloadHandler(
    filename = function(){
      paste("Population",input$country_pop, "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, plot_download(), width = input$width, height = input$height, dpi = 600, units="in")
    })  
  
}

shinyApp(ui=ui, server=server)