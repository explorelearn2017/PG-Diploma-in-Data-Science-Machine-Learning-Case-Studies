-------------------------------------------------------------------------------
  # Title: "Spark Investments Case Study - Investments Analysis of all funds"
  -------------------------------------------------------------------------------
  # Objective of Analysis
  # Identify
  #     f(Best Sectors + Countries + Investment Fund type)
  # based on the investments done by most of the investors are investing

  # Key Constraints for Analysis
  # Can Invest ONLY between $5 - $15 million
  # Invest ONLY in ENGLISH speaking countries - Official Language as English

  # Set your working directory as a first step using GUI or setwd()

# Loading Libraries
# install.packages("dplyr","tidyr","stringr","stringi")
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)

# Import Investments Dataset Information
rounds2 <- read.csv(file = "rounds2.csv", stringsAsFactors = F, fill = T)
No_of_Observations_rounds2 <- length(rounds2$company_permalink)

# Convert to lower case
rounds2$company_permalink <- tolower(rounds2$company_permalink)
str(rounds2)

# Import Companies Dataset Information
companies <- read.delim("companies.txt", stringsAsFactors = F, fill = T)
No_of_Observations_companies <- length(companies$permalink)

# Convert to lower case
companies$permalink <- tolower(companies$permalink)
companies$category_list <- tolower(companies$category_list)
str(companies)

# Import Category and Sub-Category files Information into mapping dataframe
mapping1 <- read.csv(file = "mapping.csv", stringsAsFactors = F, fill = T)
No_of_Observations_mapping <- length(mapping1$category_list)
str(mapping1)

# Convert Wider format of Sub-Category Info into Loner Format for mapping with master_frame file
mapping2 = gather(mapping1,
                  category,
                  category_value,
                  Automotive...Sports:Social..Finance..Analytics..Advertising)

# Filter Unrelated data
mapping   = mapping2[!(mapping2$category_value == 0),]

# Convert to lowercase
mapping$category_list = tolower(mapping$category_list)
mapping$category  = tolower(mapping$category)
mapping = mapping[,-3]
str(mapping)

# Replace "0" with "na" which has an data issue in mapping file
mapping$category_list = gsub("[0]", "na", mapping$category_list)
mapping$category_list = gsub("\\.na", ".0", mapping$category_list)
str(mapping)
View(mapping)

# Append missing sub-categories information into mapping file with category as "Others"
#mapping = rbind(c("toys", ))

############################Check Point 1########################################################

############################Table 1.1########################################################
# --------------------------------------------------------------------------------------------------
# Checkpoint 1: Understanding the datasets

# No. of Unique Companies in round2 dataset
unique_companies_round2   <- length(unique(rounds2$company_permalink))

# No. of Unique Companies in companines dataset
unique_companies_comp   <- length(unique(companies$permalink))

# Unique Key in Companies Dataset
# permalink column is unique key

# Merging Dataframes - Companies & Rounds2 and copy into master_frame
master_frame = merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")
View(master_frame)

# No. of Observations in master frame --> 114949
No_of_Observations_master_frame <- length(master_frame$company_permalink)

############################Checkpoint 2: Funding Type Analysis############################

############################Table 2.1########################################################

#Calcualte Average funding amount for "Venture","angel",private equity","Seed"

investment_types<-filter(select(master_frame,funding_round_type,raised_amount_usd),funding_round_type=='angel'
                         | funding_round_type=='seed'
                         |funding_round_type=='venture'
                         | funding_round_type=='private_equity')

average_funding_amount<-aggregate(raised_amount_usd~funding_round_type, data=investment_types,mean)

View(average_funding_amount)

#Based on the average investment amount calculated above, which investment type do you think is the most suitable for Spark Funds?

suitable_investment_type<-filter(average_funding_amount,raised_amount_usd>=5000000 & raised_amount_usd <=15000000)
View(suitable_investment_type)

############################# Table 3.1: Country Analysis -- Checkpoint # 3############################
# --------------------------------------------------------------------------------------------------
# Filter the master_frame to have ONLY Venture Investments Information
master_frame_venture = filter(master_frame, funding_round_type == 'venture')
#View(master_frame_venture)

# Group By Countries
master_frame_venture_countries = group_by(master_frame_venture, country_code)

# Summarise the average investment by Country wise
summary_country_investment = summarise(master_frame_venture_countries, sum(raised_amount_usd,
                                                                           na.rm = T))
colnames(summary_country_investment) = c('country', 'total_country_investment_by_venture_cap')

#View(summary_country_investment)
# Top 9 Countries where Venture Captalists Invested
top9_new = arrange(summary_country_investment,desc(total_country_investment_by_venture_cap))

# View(top9_new)
top9 = top9_new[1:9,]

View(top9)
# Top 3 English Speaking Countries are 'USA', 'United Kingdom', 'India'

############################# Checkpoint Analysis - 4: Sector Analysis 1############################
# --------------------------------------------------------------------------------------------------
# Business Rule - First Sub-Category as the main sub-category in master_frame$category_list
# Applies above business rule
master_frame = separate(master_frame, category_list,
                        into = c("main_sub_category"),
                        sep = '\\|', remove = T)

# Append Categories Info into master_frame
master_frame_test = master_frame
str(master_frame_test)
master_frame = merge(master_frame, mapping, by.x = "main_sub_category", by.y = "category_list",
                     all.x = T)

############################# Table 5.1: Sector Analysis 2 -- Checkpoint # 5############################
# --------------------------------------------------------------------------------------------------
# Country = USA & Investment Type = Venture & 5-15 million USD :: Create a D1 data frame
D1_inter = filter(master_frame, country_code == 'USA' &
                    funding_round_type == 'venture' &
                    raised_amount_usd <= 15000000 &
                    raised_amount_usd >= 5000000)
D1_totals = D1_inter %>%
  group_by(category) %>%
  summarise(Total_investment = sum(raised_amount_usd, na.rm=T),
            Num_of_investments = length(raised_amount_usd))

D1 = merge(D1_inter, D1_totals, by = "category")

# Total Investment in Country - USA & No. of Investments
sum(D1_totals$Total_investment)
sum(D1_totals$Num_of_investments)

# Arrange the D3 dataframe in Descending order of No. of Investments received
D1_totals = arrange(D1_totals, desc(Num_of_investments))

# which company received the highest investment? for top sector count-wise
D1_Top_sector = filter(D1, category == D3_totals$category[1])
D1_Top_sector = arrange(D1_Top_sector, desc(raised_amount_usd))
D1_Top_sector$company_permalink[1]

# which company received the highest investment? for 2nd top sector count-wise
D1_2nd_sector = filter(D1, category == D3_totals$category[2])
D1_2nd_sector = arrange(D1_2nd_sector, desc(raised_amount_usd))
D1_2nd_sector$company_permalink[1]

# Country = GBR & Investment Type = Venture & 5-15 million USD :: Create a D2 data frame
D2_inter = filter(master_frame, country_code == 'GBR' &
                    funding_round_type == 'venture' &
                    raised_amount_usd <= 15000000 &
                    raised_amount_usd >= 5000000)
D2_totals = D2_inter %>%
  group_by(category) %>%
  summarise(Total_investment = sum(raised_amount_usd, na.rm=T),
            Num_of_investments = length(raised_amount_usd))

D2 = merge(D2_inter, D2_totals, by = "category")

# Total Investment in Country - GBR
sum(D2_totals$Total_investment)
sum(D2_totals$Num_of_investments)

# Arrange the D3 dataframe in Descending order of No. of Investments received
D2_totals = arrange(D2_totals, desc(Num_of_investments))

# which company received the highest investment? for top sector count-wise
D2_Top_sector = filter(D2, category == D2_totals$category[1])
D2_Top_sector = arrange(D2_Top_sector, desc(raised_amount_usd))
D2_Top_sector$company_permalink[1]

# which company received the highest investment? for 2nd top sector count-wise
D2_2nd_sector = filter(D2, category == D2_totals$category[2])
D2_2nd_sector = arrange(D2_2nd_sector, desc(raised_amount_usd))
D2_2nd_sector$company_permalink[1]

# Country = IND & Investment Type = Venture & 5-15 million USD :: Create a D3 data frame
D3_inter = filter(master_frame, country_code == 'IND' &
                    funding_round_type == 'venture' &
                    raised_amount_usd <= 15000000 &
                    raised_amount_usd >= 5000000)
D3_totals = D3_inter %>%
  group_by(category) %>%
  summarise(Total_investment = sum(raised_amount_usd, na.rm=T),
            Num_of_investments = length(raised_amount_usd))

D3 = merge(D3_inter, D3_totals, by = "category")

# Total Investment in Country - IND
sum(D3_totals$Total_investment)
sum(D3_totals$Num_of_investments)

# Arrange the D3 dataframe in Descending order of No. of Investments received
D3_totals = arrange(D3_totals, desc(Num_of_investments))

# which company received the highest investment? for top sector count-wise
D3_Top_sector = filter(D3, category == D3_totals$category[1])
D3_Top_sector = arrange(D3_Top_sector, desc(raised_amount_usd))
D3_Top_sector$company_permalink[1]

# which company received the highest investment? for 2nd top sector count-wise
D3_2nd_sector = filter(D3, category == D3_totals$category[2])
D3_2nd_sector = arrange(D3_2nd_sector, desc(raised_amount_usd))
D3_2nd_sector$company_permalink[1]
