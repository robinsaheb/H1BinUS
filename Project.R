### Group 9 Final Project
### IST 719 - Information Visualization
### Saheb, Piyush, Kritarth

dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!
library(ggplot2)
library(dplyr)
library(ggmap)


# Loading the dataset
fname <-file.choose()
fname
df <- read.csv(fname,header = T, stringsAsFactors = F)
#View(df)

# Getting State name from the last word in WORKSITE
df$state <- trimws(gsub("^.*,", "", df$WORKSITE))


# Making Barplot for Case Status 

# Number of Case Studies.
case.status <- df %>% filter(!is.na(CASE_STATUS)) %>% group_by(CASE_STATUS) %>%  
  summarise(number = length(CASE_STATUS)) %>% top_n(n=4) %>% ungroup()

# Barplot for Case Status
gbar <- ggplot(case.status, aes(x=reorder(CASE_STATUS, number), y=number/1000))
gbar <- gbar + geom_bar(stat = "identity", fill="red3", color="black")
gbar <- gbar + coord_flip() + theme_bw(base_size = 10) 
gbar <- gbar + labs(title = "", x="Case Status", y="Number of Applicants")
gbar


# Line Chart Showing Case Status Per Year
# Cases per year
cases.per.year <- df %>% filter(!is.na(CASE_STATUS)) %>% filter(!is.na(YEAR)) %>% 
  filter(CASE_STATUS !='PENDING QUALITY AND COMPLIANCE REVIEW - UNASSIGNED')%>% 
  filter(CASE_STATUS !='REJECTED')%>% filter(CASE_STATUS !='INVALIDATED')%>%  
  group_by(CASE_STATUS,YEAR) %>% summarise(number = length(CASE_STATUS)) %>% ungroup()

cases.per.year$col[cases.per.year$CASE_STATUS=='CERTIFIED'] <- '70BF4B'
cases.per.year$col[cases.per.year$CASE_STATUS=='CERTIFIED-WITHDRAWN'] <- 'F2CB05'
cases.per.year$col[cases.per.year$CASE_STATUS=='DENIED'] <- 'F20505'
cases.per.year$col[cases.per.year$CASE_STATUS=='WITHDRAWN'] <- '5A58A6'
ggplot(cases.per.year, aes(x=YEAR, y=number/1000, color = CASE_STATUS)) + geom_line(lwd=1) +
  geom_point() +
  labs(x="Year", y="Application Number in Thousands")



# Pie Chart Showing Employment Type
emptype <- df %>% filter(!is.na(FULL_TIME_POSITION)) %>% group_by(FULL_TIME_POSITION) %>% 
  summarise(number = length(FULL_TIME_POSITION)) %>% ungroup()
emptype
lbls <- c("Part Time", "Full Time")
pie(emptype$number,labels = lbls, col=c('blue4', 'orange2'),
    main="Pie Chart of Countries")


# Showing top 10 companies for H1B sponsor
top10 <- df %>% filter(!is.na(EMPLOYER_NAME)) %>% group_by(EMPLOYER_NAME) %>% summarise(number = length(CASE_STATUS)) %>% 
  top_n(n = 10) %>% ungroup()

#top10
ggplot(top10, aes(x=reorder(EMPLOYER_NAME, number), y=number/1000))+
  geom_bar(stat = "identity", fill="green3", color="black") + coord_flip() + theme_bw(base_size = 10) + 
  geom_label(label=top10$number) + labs(title = "", x="Employer Name", y="Number of Applicants")



# Line chart for salaries for Common Jobs 

# Getting top 50 jobs by number 
top.50.jobs <- df %>% group_by(JOB_TITLE) %>% summarise(number = length(lat)) %>% 
  top_n(n=50) %>% arrange(-number) %>% ungroup()

# Getting top 10 jobs and getting mean salaries over the year
top.10.jobs <- df %>% filter(!is.na(PREVAILING_WAGE)) %>% filter(PREVAILING_WAGE>0) %>% filter(!is.na(YEAR)) %>% 
  filter(CASE_STATUS == "CERTIFIED") %>% filter(JOB_TITLE %in% top.50.jobs$JOB_TITLE[1:5]) %>%
  group_by(JOB_TITLE,YEAR) %>% summarise(Avg = mean(PREVAILING_WAGE)) %>% ungroup()

ggplot(top.10.jobs, aes(x=YEAR, y=Avg/1000, color=JOB_TITLE)) + 
  geom_line(lwd=2) +
  labs(title = "Prevailing Salaries Per Year and Job Title", x="Year", y="Average Salary Per Thousands USD", color = "Job Title")



# Making map of the united states

distribution1 <- df %>% filter(!is.na(lat)) %>% filter(!is.na(lon)) %>% group_by(state) %>% 
  filter(CASE_STATUS =="CERTIFIED") %>% summarise(number = length(lat)/1000) %>% ungroup()
#install.packages("usmap")
library(usmap)
plot_usmap(data = distribution1, values = "number", color = "black", labels = TRUE) + 
  scale_fill_continuous(
    low = "white", high = "green3", name = "Number of Applications \n(Thousands)", label = scales::comma
  ) + theme(legend.position = "bottom")  
  

# Map of the western states

plot_usmap(data = distribution1, values = "number", include = c("CA", "ID", "NV", "OR", "WA"), color = "black", labels = TRUE) + 
  scale_fill_continuous(
    low = "white", high = "green3", name = "Number of Applications"
  ) + 
  labs(title = "Western US States", subtitle = "These are the states in the Pacific Timezone.") +
  theme(legend.position = "right")


# Map of the mid atlantic states

plot_usmap(data = distribution1, values = "number", include = c("NJ", "NY", "PA"), color = "black", labels = TRUE) + 
  scale_fill_continuous(
    low = "white", high = "#AAD58E", name = "Number of Applications"
  ) + 
  labs(title = "Mid Atlantic US States", subtitle = "These are the states in the Standard Eastern Timezone.") +
  theme(legend.position = "right")



# Line Chart showing number of applications per year
distribution2 <- df %>% filter(!is.na(lat)) %>% filter(!is.na(lon)) %>% 
  filter(!is.na(CASE_STATUS)) %>% filter(!is.na(YEAR))%>% 
  group_by(state, YEAR) %>% summarise(Avg = mean(length(CASE_STATUS))) %>% ungroup()
distribution2


ds <- distribution2 %>% filter(!is.na(YEAR))%>%filter(state == "NEW JERSEY" | state== "NEW YORK" | state == "CALIFORNIA" | state == "TEXAS")
ds$col[ds$state=='NEW JERSEY'] <- '33FF00'
ds$col[ds$state=='NEW YORK'] <- 'F2CB05'
ds$col[ds$state=='CALIFORNIA'] <- 'F20505'
ds$col[ds$state=='TEXAS'] <- '5A58A6'
ggplot(ds, aes(x=YEAR, y=Avg/1000, color=state)) + 
  geom_line(lwd=2) + 
  labs(title = "Number of Certified Applications Per Year", x="Year", y="Average Number of Applications", color = "State")















































