library(dplyr)
library(tidyr)
library(ggplot2)

### load data
setwd("__") #your wd here
economic <- read.csv("economic.csv", stringsAsFactors = F) #economic NTA variables
housing <- read.csv("housing.csv", stringsAsFactors = F) #housing NTA variables

### filter to washington heights (mn35/36) and inwood (MN01)
economic <- economic %>% filter(NTACode=="MN01"|NTACode=="MN35"|NTACode=="MN36")
housing <- housing %>% filter(NTACode=="MN01"|NTACode=="MN35"|NTACode=="MN36")

manhattan <- economic %>%
  left_join(housing,
            by = c("OBJECTID","BoroCode","BoroName","CountyFIPS","NTACode","NTAName","geotype","geogname","geoid"))
rm(economic, housing)

### culling
manhattan <- manhattan %>%
  select(BoroName, NTAName, hh2E,
         hhiu10E, hhi10t14E, hhi15t24E, hhi25t34E, hhi35t49E, hhi50t74E,
         hhi75t99E, hi100t149E, hi150t199E, hhi200plE, mdhhincE, mnhhincE,
         fampvuE, fambwpvE)

### write
write.csv(manhattan, "manhattan_demographics.csv")

### calculate totals
manhattan <- manhattan %>%
  mutate(hhpop_share=hh2E/sum(hh2E)) %>%
  relocate(fampvuE:hhpop_share, .after = NTAName)
## for all
manhattan <- manhattan %>%
  rbind(c("Manhattan",
          "Washington Heights-Inwood",
          colSums(manhattan %>% select(fampvuE:hhi200plE)),
          NA,
          sum(.$hhpop_share*.$mnhhincE)
  )) %>% mutate(across(c(fampvuE:mnhhincE), as.numeric))
## for Washington Heights
manhattan <- manhattan %>%
  rbind(manhattan %>% slice(2:3) %>%
          rbind(c("Manhattan",
                  "Washington Heights",
                  colSums(manhattan %>% slice(2,3) %>% select(fampvuE:hhi200plE)),
                  NA,
                  sum((.$hhpop_share)/sum(.$hhpop_share)*.$mnhhincE))) %>%
          slice(3)
        ) %>% mutate(across(c(fampvuE:mnhhincE), as.numeric))

### calculate percentages
manhattan <- manhattan %>%
  mutate(hhiu10P=hhiu10E/hh2E, hhi10t14P=hhi10t14E/hh2E, hhi15t24P=hhi15t24E/hh2E,
         hhi25t34P=hhi25t34E/hh2E, hhi35t49P=hhi35t49E/hh2E, hhi50t74P=hhi50t74E/hh2E,
         hhi75t99P=hhi75t99E/hh2E, hi100t149P=hi100t149E/hh2E, hi150t199P=hi150t199E/hh2E,
         hhi200plP=hhi200plE/hh2E, fambwpvP=fambwpvE/fampvuE)


####------------ isolate Inwood income data ------------
inwood <- manhattan %>% slice(1) %>%
  select(!BoroName & !NTAName) %>%
  pivot_longer(hhiu10E:hhi200plE, names_to = "hhi_range", values_to = "hhi_count") %>%
  pivot_longer(hhiu10P:hhi200plP, names_to = "hhi_rangeP", values_to = "hhi_perc") %>%
  mutate(hhi_range = gsub("E", "", hhi_range),
         hhi_rangeP = gsub("P", "", hhi_rangeP)) %>%
  filter(hhi_range == hhi_rangeP)
## rename income ranges
inwood <- inwood %>%
  select(-hhi_rangeP) %>%
  mutate(hhi_range = case_when(
    hhi_range == "hhiu10" ~ "Under 10",
    hhi_range == "hhi10t14" ~ "10-14",
    hhi_range == "hhi15t24" ~ "15-24",
    hhi_range == "hhi25t34" ~ "25-34",
    hhi_range == "hhi35t49" ~ "35-49",
    hhi_range == "hhi50t74" ~ "50-74",
    hhi_range == "hhi75t99" ~ "75-99",
    hhi_range == "hi100t149" ~ "100-149",
    hhi_range == "hi150t199" ~ "150-199",
    hhi_range == "hhi200pl" ~ "200+"
  )) %>%
  mutate(n=as.numeric(row.names(.)))

## plot income distribution
ggplot(inwood, aes(x = reorder(hhi_range, n), y = hhi_count, group = 1)) +
  geom_bar(stat = "identity", color = "blue", fill = "blue") +
  ggtitle("Inwood household income distribution") +
  xlab("Dollars (in thousands)") + ylab("Number of households") +
  theme(plot.title = element_text(hjust = .5) ) +
  geom_line(color = "red", size = 1.5)


####------------ get Inwood rents ------------
inwoodrents <- rbind(
  read.csv("C:\\Users\\kylej\\Google Drive\\Work\\PIT-DSC\\zillow_scrape\\20210615\\zillow_rents_week1.csv", stringsAsFactors = F),
  read.csv("C:\\Users\\kylej\\Google Drive\\Work\\PIT-DSC\\zillow_scrape\\20210621\\zillow_rents_week2.csv", stringsAsFactors = F),
  read.csv("C:\\Users\\kylej\\Google Drive\\Work\\PIT-DSC\\zillow_scrape\\20210628\\zillow_rents_week3.csv", stringsAsFactors = F),
  read.csv("C:\\Users\\kylej\\Google Drive\\Work\\PIT-DSC\\zillow_scrape\\20210705\\zillow_rents_week4.csv", stringsAsFactors = F),
  read.csv("C:\\Users\\kylej\\Google Drive\\Work\\PIT-DSC\\zillow_scrape\\20210712\\zillow_rents_week5.csv", stringsAsFactors = F),
  read.csv("C:\\Users\\kylej\\Google Drive\\Work\\PIT-DSC\\zillow_scrape\\20210719\\zillow_rents_week6.csv", stringsAsFactors = F))
inwoodrents <- inwoodrents %>%
  filter(Neighborhood == "Inwood" & Beds >=2) %>%
  group_by(Address) %>%
  mutate(mn_rent = mean(Price)) %>% slice_head
### hypothetical income range for rent to be 30%, 45%, and 60%
inwoodrents <- inwoodrents %>% mutate(burden30inc = mn_rent*12/1000/.3,
                                      burden45inc = mn_rent*12/1000/.45,
                                      burden60inc = mn_rent*12/1000/.6,
                                      burden30range = case_when(
                                        burden30inc < 10 ~ "Under 10",
                                        burden30inc >=10 & burden30inc < 15 ~ "10-14",
                                        burden30inc >=15 & burden30inc < 25 ~ "15-24",
                                        burden30inc >=25 & burden30inc < 35 ~ "25-34",
                                        burden30inc >=35 & burden30inc < 50 ~ "35-49",
                                        burden30inc >=50 & burden30inc < 75 ~ "50-74",
                                        burden30inc >=75 & burden30inc < 100 ~ "75-99",
                                        burden30inc >=100 & burden30inc < 150 ~ "100-149",
                                        burden30inc >=150 & burden30inc < 200 ~ "150-199",
                                        burden30inc >= 200 ~ "200+"),
                                      burden45range = case_when(
                                        burden45inc < 10 ~ "Under 10",
                                        burden45inc >=10 & burden45inc < 15 ~ "10-14",
                                        burden45inc >=15 & burden45inc < 25 ~ "15-24",
                                        burden45inc >=25 & burden45inc < 35 ~ "25-34",
                                        burden45inc >=35 & burden45inc < 50 ~ "35-49",
                                        burden45inc >=50 & burden45inc < 75 ~ "50-74",
                                        burden45inc >=75 & burden45inc < 100 ~ "75-99",
                                        burden45inc >=100 & burden45inc < 150 ~ "100-149",
                                        burden45inc >=150 & burden45inc < 200 ~ "150-199",
                                        burden45inc >= 200 ~ "200+"),
                                      burden60range = case_when(
                                        burden60inc < 10 ~ "Under 10",
                                        burden60inc >=10 & burden60inc < 15 ~ "10-14",
                                        burden60inc >=15 & burden60inc < 25 ~ "15-24",
                                        burden60inc >=25 & burden60inc < 35 ~ "25-34",
                                        burden60inc >=35 & burden60inc < 50 ~ "35-49",
                                        burden60inc >=50 & burden60inc < 75 ~ "50-74",
                                        burden60inc >=75 & burden60inc < 100 ~ "75-99",
                                        burden60inc >=100 & burden60inc < 150 ~ "100-149",
                                        burden60inc >=150 & burden60inc < 200 ~ "150-199",
                                        burden60inc >= 200 ~ "200+"),
                                      ) %>% select(-c(burden30inc, burden45inc, burden60inc))

### joining to Inwood tibble
inwood_apts_mapping30 <- inwoodrents %>%
  group_by(burden30range) %>% 
  summarise(n_apts = n()) %>% 
  mutate(perc_apts = n_apts / sum(n_apts))
inwood_apts_mapping45 <- inwoodrents %>%
  group_by(burden45range) %>% 
  summarise(n_apts = n()) %>% 
  mutate(perc_apts = n_apts / sum(n_apts))
inwood_apts_mapping60 <- inwoodrents %>%
  group_by(burden60range) %>% 
  summarise(n_apts = n()) %>% 
  mutate(perc_apts = n_apts / sum(n_apts))

inwood30 <- left_join(inwood, inwood_apts_mapping30, by = c("hhi_range" = "burden30range")) %>%
  mutate(across(c(n_apts, perc_apts), ~replace(., is.na(.), 0)))
inwood45 <- left_join(inwood, inwood_apts_mapping45, by = c("hhi_range" = "burden45range")) %>%
  mutate(across(c(n_apts, perc_apts), ~replace(., is.na(.), 0)))
inwood60 <- left_join(inwood, inwood_apts_mapping60, by = c("hhi_range" = "burden60range")) %>%
  mutate(across(c(n_apts, perc_apts), ~replace(., is.na(.), 0)))


### apt distributions
# 30% benchmark
ggplot(inwood30, aes(x = reorder(hhi_range, n), y = perc_apts, group = 1)) +
  geom_bar(stat = "identity", color = "blue", fill = "blue") +
  ggtitle("Inwood rent distribution by income range, \n by 30% affordability metric") +
  xlab("Income (in thousands)") + ylab("Percent of apartments") +
  theme(plot.title = element_text(hjust = .5) ) +
  geom_line(color = "red", size = 1.5) +
  theme(panel.border = element_rect(color = "black", fill=NA, size=1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# 45% benchmark
ggplot(inwood45, aes(x = reorder(hhi_range, n), y = perc_apts, group = 1)) +
  geom_bar(stat = "identity", color = "blue", fill = "blue") +
  ggtitle("Inwood rent distribution by income range, \n by 45% affordability metric") +
  xlab("Income (in thousands)") + ylab("Percent of apartments") +
  theme(plot.title = element_text(hjust = .5) ) +
  geom_line(color = "red", size = 1.5) +
  theme(panel.border = element_rect(color = "black", fill=NA, size=1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# 60% benchmark
ggplot(inwood60, aes(x = reorder(hhi_range, n), y = perc_apts, group = 1)) +
  geom_bar(stat = "identity", color = "blue", fill = "blue") +
  ggtitle("Inwood rent distribution by income range, \n by 60% affordability metric") +
  xlab("Income (in thousands)") + ylab("Percent of apartments") +
  theme(plot.title = element_text(hjust = .5) ) +
  geom_line(color = "red", size = 1.5) +
  theme(panel.border = element_rect(color = "black", fill=NA, size=1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# ### comparative barchart
# inwood_melt <- inwood %>%
#   select(hhi_range, hhi_perc, perc_apts, n) %>%
#   rename(Households = hhi_perc, Apartments = perc_apts) %>%
#   pivot_longer(Households | Apartments, names_to="key", values_to="value")
# ggplot(inwood_melt, aes(x = reorder(hhi_range, n), y = value)) + 
#   geom_bar(aes(fill = key), stat = "identity", position = "dodge") +
#   ggtitle("Distribution of Inwood household income vs. \n affordable apartments (30% metric)") +
#   xlab("Dollars (in thousands)") + ylab("Percent of households/apartments") +
#   theme(plot.title = element_text(hjust = .5) )

rm(inwood30, inwood45, inwood60, inwood_apts_mapping30, inwood_apts_mapping45, inwood_apts_mapping60)

####----- track changes in Inwood rents ----
inwoodrents %>% group_by(Week) %>%
  mutate(md_rent = median(Price, na.rm = T)) %>% slice_head %>%
  ggplot(aes(x = Week, y = md_rent)) +
    geom_point() + geom_line() +
    ggtitle("Inwood median rents over time") +
    xlab("Week") + ylab("Rent (dollars)") +
    theme(plot.title = element_text(hjust = .5)) +
    geom_hline(yintercept = 1265.7, size=1.5, color = 'green') +
    geom_hline(yintercept = 1898.55, size = 1.5, color = 'yellow') +
    geom_hline(yintercept = 2531.4, size = 1.5, color = 'red') +
    annotate("text", 1.5, 1265.7, vjust = -1, label = "30% of median income") +
    annotate("text", 1.5, 1898.55, vjust = -1, label = "45% of median income") +
    annotate("text", 1.5, 2531.4, vjust = -1, label = "60% of median income") +
    theme(panel.border = element_rect(color = "black", fill=NA, size=1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())


####------------ isolate wahi income data ------------
wahi <- manhattan %>% slice(5) %>%
  select(!BoroName & !NTAName) %>%
  pivot_longer(hhiu10E:hhi200plE, names_to = "hhi_range", values_to = "hhi_count") %>%
  pivot_longer(hhiu10P:hhi200plP, names_to = "hhi_rangeP", values_to = "hhi_perc") %>%
  mutate(hhi_range = gsub("E", "", hhi_range),
         hhi_rangeP = gsub("P", "", hhi_rangeP)) %>%
  filter(hhi_range == hhi_rangeP)
## rename income ranges
wahi <- wahi %>%
  select(-hhi_rangeP) %>%
  mutate(hhi_range = case_when(
    hhi_range == "hhiu10" ~ "Under 10",
    hhi_range == "hhi10t14" ~ "10-14",
    hhi_range == "hhi15t24" ~ "15-24",
    hhi_range == "hhi25t34" ~ "25-34",
    hhi_range == "hhi35t49" ~ "35-49",
    hhi_range == "hhi50t74" ~ "50-74",
    hhi_range == "hhi75t99" ~ "75-99",
    hhi_range == "hi100t149" ~ "100-149",
    hhi_range == "hi150t199" ~ "150-199",
    hhi_range == "hhi200pl" ~ "200+"
  )) %>%
  mutate(n=as.numeric(row.names(.)))

## plot income distribution
ggplot(wahi, aes(x = reorder(hhi_range, n), y = hhi_count)) +
  geom_bar(stat = "identity", color = "blue", fill = "blue") +
  ggtitle("Washington Heights household income distribution") +
  xlab("Dollars (in thousands)") + ylab("Number of households") +
  theme(plot.title = element_text(hjust = .5) ) +
  geom_line(color = "red", size = 1.5)


####------------ get wahi rents ------------
# wahirents <- rbind(
#   read.csv("C:\\Users\\kylej\\Google Drive\\Work\\PIT-DSC\\zillow_scrape\\20210615\\zillow_rents_week1.csv", stringsAsFactors = F),
#   read.csv("C:\\Users\\kylej\\Google Drive\\Work\\PIT-DSC\\zillow_scrape\\20210621\\zillow_rents_week2.csv", stringsAsFactors = F))
# wahirents <- wahirents %>%
#   filter(Neighborhood == "Washington Heights" & Beds >=2) %>%
#   group_by(Address) %>%
#   mutate(mn_rent = mean(Price)) %>% slice_head
# ### hypothetical income range for rent to be 30%
# wahirents <- wahirents %>% mutate(burden30inc = mn_rent*12/1000/.3,
#                                   burden30range = case_when(
#                                     burden30inc < 10 ~ "Under 10",
#                                     burden30inc >=10 & burden30inc < 15 ~ "10-14",
#                                     burden30inc >=15 & burden30inc < 25 ~ "15-24",
#                                     burden30inc >=25 & burden30inc < 35 ~ "25-34",
#                                     burden30inc >=35 & burden30inc < 50 ~ "35-49",
#                                     burden30inc >=50 & burden30inc < 75 ~ "50-74",
#                                     burden30inc >=75 & burden30inc < 100 ~ "75-99",
#                                     burden30inc >=100 & burden30inc < 150 ~ "100-149",
#                                     burden30inc >=150 & burden30inc < 200 ~ "150-199",
#                                     burden30inc >= 200 ~ "200+" 
#                                   )) %>% select(-burden30inc)
# 
# ### joining to wahi tibble
# wahi_apts_mapping <- wahirents %>%
#   group_by(burden30range) %>% 
#   summarise(n_apts = n()) %>% 
#   mutate(perc_apts = n_apts / sum(n_apts))
# wahi <- left_join(wahi, wahi_apts_mapping, by = c("hhi_range" = "burden30range")) %>%
#   mutate(across(c(n_apts, perc_apts), ~replace(., is.na(.), 0)))

# ### comparative barchart
# wahi_melt <- wahi %>%
#   select(hhi_range, hhi_perc, perc_apts, n) %>%
#   rename(Households = hhi_perc, Apartments = perc_apts) %>%
#   pivot_longer(Households | Apartments, names_to="key", values_to="value")
# ggplot(wahi_melt, aes(x = reorder(hhi_range, n), y = value)) + 
#   geom_bar(aes(fill = key), stat = "identity", position = "dodge") +
#   ggtitle("Distribution of Washington Heights household income vs. \n affordable apartments (30% metric)") +
#   xlab("Dollars (in thousands)") + ylab("Percent of households/apartments") +
#   theme(plot.title = element_text(hjust = .5) )
