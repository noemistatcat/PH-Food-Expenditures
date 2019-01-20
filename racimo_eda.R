library(survey)
library(dplyr)
library(tidyr)

setwd("D:/My Files/A-FreelanceWork/Racimo")

#IMPORT ALL NEEDED DATA
df_food <- read.csv("raw_data/FAMILY INCOME AND EXPENDITURE SURVEY (2015) VOLUME 2 - FOOD EXPENDITURE - raw data.csv", header = T)
df_hh <- read.csv("raw_data/FAMILY INCOME AND EXPENDITURE SURVEY (2015) VOLUME 2 - HOUSEHOLD DETAILS AND HOUSING CHARACTERISTICS - raw data.csv", header = T)
df_income_r <- read.csv("raw_data/FAMILY INCOME AND EXPENDITURE SURVEY (2015) VOLUME 2 - INCOME AND OTHER RECEIPTS - raw data.csv", header = T)
df_nonfood <- read.csv("raw_data/FAMILY INCOME AND EXPENDITURE SURVEY (2015) VOLUME 2 - NONFOOD EXPENDITURE - raw data.csv", header = T)
df_totals <- read.csv("raw_data/FAMILY INCOME AND EXPENDITURE SURVEY (2015) VOLUME 2 - TOTALS OF INCOME AND EXPENDITURE - raw data.csv", header = T)
df_urban_rural <- read.csv("raw_data/FIES2015_vol1.csv")
food_map <- read.csv("racimo/mapping_food_expend.csv", header=T) 

#JOIN WEIGHTS TO ALL DATA FRAMES
df_totals$concat <- paste(df_totals$w_id, df_totals$w_shsn, df_totals$w_hcn, sep=" ")
df_food$concat <- paste(df_food$w_id, df_food$w_shsn, df_food$w_hcn, sep=" ")
df_hh$concat <- paste(df_hh$w_id, df_hh$w_shsn, df_hh$w_hcn, sep=" ")
df_income_r$concat <- paste(df_income_r$w_id, df_income_r$w_shsn, df_income_r$w_hcn, sep=" ")
df_nonfood$concat <- paste(df_nonfood$w_id, df_nonfood$w_shsn, df_nonfood$w_hcn, sep=" ")
df_urban_rural$concat <- paste(df_urban_rural$W_ID, df_urban_rural$W_SHSN, df_urban_rural$W_HCN, sep=" ")


df_food$rfact <- df_totals$rfact[match(df_food$concat, df_totals$concat)]
df_hh$rfact <- df_totals$rfact[match(df_hh$concat, df_totals$concat)]
df_income_r$rfact <- df_totals$rfact[match(df_income_r$concat, df_totals$concat)]
df_nonfood$rfact <- df_totals$rfact[match(df_nonfood$concat, df_totals$concat)]

#CONVERT DATA TO NUMERIC FORMAT
df_food_backup <- df_food
df_food[,5:278] <- apply(df_food[,-c(1:4)], 2, function(x){
  x=as.numeric(x)
  x=ifelse(is.na(x), 0, x)
})

#SUBSET ALL NEEDED FOOD VARIABLES
my_vars <- c("concat", "rfact", as.character(food_map$v_food))
df_food <- df_food[,my_vars]

food_vars <- as.character(food_map$v_food)

df_food[,29:54] <- 0

df_food[,29:54] <- df_food[,"rfact"]*df_food[,3:28]

names(df_food)[c(29:54)] <- as.vector(sapply(food_vars,function(x){paste("w", x, sep="")}))

#CHECK SUM OF FOOD EXPENDITURES
#View(colSums(select(df_food, wtrice:wtfothernonalcohol)))

#JOIN FOOD OUT OF HOME, TOTAL EXPENDITURE, AND INCOME
df_food <- left_join(df_food, select(df_totals, 
                                     toinc, 
                                     tfoodhome,
                                     tfood,
                                     ttotex, concat), by="concat")

df_food[,c("wtoinc", "wtfoodhome", "wtfood", "wttotex")] <- 0
#df_food$tfoodoutside <- as.numeric(df_food$tfoodoutside)
df_food[,c("toinc", "tfoodhome", "tfood", "ttotex")] <- apply(df_food[,c("toinc", "tfoodhome", "tfood", "ttotex")], 2, function(x){
  x=as.numeric(x)
  x=ifelse(is.na(x), 0, x)
})

df_food[,c("wtoinc", "wtfoodhome", "wtfood", "wttotex")] <- df_food[,"rfact"]*df_food[,c("toinc", "tfoodhome", "tfood", "ttotex")]

df_food$wtfoodoutside <- df_food$wtfood - df_food$wtfoodhome
df_food$wnonfood <- df_food$wttotex - df_food$wtfood 

#RECODING INCOME CLASSES
df_food <- df_food %>% 
            mutate(income_class = case_when(toinc < 40000 ~ "Under 40,000", 
                                                    toinc < 60000 & toinc >= 40000 ~ "40,000 - 59,999",
                                                    toinc < 100000 & toinc >= 60000 ~ "60,000 - 99,999",
                                                    toinc < 250000 & toinc >= 100000 ~ "100,000 - 249,999",
                                                    toinc >= 250000 ~ "250,000 and over"))

#JOIN URBAN-RURAL INFO
df_food$Urban_Rural <- df_urban_rural$URB[match(df_food$concat, df_urban_rural$concat)]
df_food$Urban_Rural <- recode(df_food$Urban_Rural, 
                              `1`="Urban",
                              `2`="Rural")

#JOINING OTHER DEMOGRAPHIC VARIABLES
df_food <- left_join(df_food, select(df_hh, w_regn, members, ageless5, age5_17, concat), by="concat")
df_food$broad_regions <- recode(df_food$w_regn, 
                             "Region I - Ilocos Region"="North / Central Luzon",
                             "Region II - Cagayan Valley"="North / Central Luzon",
                             "Region III - Central Luzon"="North / Central Luzon",
                             "Region IVa - Calabarzon"="South Luzon / Bicol",
                             "Region IVb - Mimaropa"="South Luzon / Bicol",
                             "Region IX - Western Mindanao"="Mindanao",
                             "Region V - Bicol Region"="South Luzon / Bicol",
                             "Region VI - Western Visayas"="Visayas",
                             "Region VII - Central Visayas"="Visayas",
                             "Region VIII - Eastern Visayas"="Visayas",
                             "Region X - Northern Mindanao"="Mindanao",
                             "Region XI - Southern Mindanao"="Mindanao",
                             "Region XII - Central Mindanao"="Mindanao",
                             "Region XIII - NCR"="NCR",
                             "Region XIV - CAR"="North / Central Luzon",
                             "Region XV - ARMM"="Mindanao",
                             "Region XVI - CARAGA"="Mindanao") 

df_food <- df_food %>% 
            mutate(members_class = case_when(
                  members <= 2 ~ "1-2",
                  members %in% 3:4 ~ "3-4",
                  members %in% 5:6 ~ "5-6",
                  members > 6 ~ "more than 6"
            ))

#FOOD EXPENDITURE ALLOCATION

#df_food$wtinhome <- rowSums(select(df_food, wtrice:wtfothernonalcohol))
#df_food$wtfood <- df_food$wtinhome + df_food$wtfoodoutside

df_fexpend <- df_food %>%
                select(wtrice:wtfothernonalcohol, wtfoodoutside, broad_regions, income_class, members_class, Urban_Rural) %>%
                gather(key=food_expend, value=texpend, wtrice:wtfoodoutside) 

#AGGREGATION 1 - FOOD EXPENDITURE ALLOCATION - IN-HOME              
df_finhome <- df_fexpend %>% 
              filter(food_expend != "wtfoodoutside") %>% 
              group_by(food_expend, broad_regions, income_class, members_class, Urban_Rural) %>% 
              summarize(t_expend = sum(texpend))

df_phinhome <- df_fexpend %>% 
              filter(food_expend != "wtfoodoutside") %>% 
              group_by(food_expend) %>% 
              summarize(ph_expend = sum(texpend))

df_finhome$ph_expend <- df_phinhome$ph_expend[match(df_finhome$food_expend, df_phinhome$food_expend)]            
              
write.csv(df_finhome, "racimo/food_expend.csv")

#AGGREGATION 2 - AVERAGE SPEND ON FOOD - ANNUAL              
df_hh_ph <- df_food %>% 
  group_by(broad_regions, income_class, members_class, Urban_Rural) %>% 
  summarize(hh_count = sum(rfact)) %>% 
  mutate(concat = paste(broad_regions, income_class, members_class, Urban_Rural))

df_fspend <- df_fexpend %>% 
              group_by(broad_regions, income_class, members_class, Urban_Rural) %>% 
              summarise(tfspend = sum(texpend)) %>% 
              mutate(concat = paste(broad_regions, income_class, members_class, Urban_Rural))

df_fspend$hh_count <- df_hh_ph$hh_count[match(df_fspend$concat, df_hh_ph$concat)] 

df_fspend$spend_hh <- df_fspend$tfspend / df_fspend$hh_count

write.csv(df_fspend, "racimo/food_expend_ave.csv")

#AGGREGATION 3 - PIE CHART - ALLOCATION OF FOOD (IN-HOME, OUT OF HOME, TOTAL FOOD) OUT OF TOTAL EXPENDITURE              
df_totfexpend <- df_food %>%
  select(wtfoodhome, wtfoodoutside, wnonfood, broad_regions, income_class, members_class, Urban_Rural) %>%
  gather(key=inouthome, value=totfexpend, wtfoodoutside, wtfoodhome, wnonfood) 

df_fhome <- df_totfexpend %>% 
  group_by(inouthome, broad_regions, income_class, members_class, Urban_Rural) %>% 
  summarize(totf_expend = sum(totfexpend))


write.csv(df_fhome, "racimo/food_expend_inouthome.csv")

