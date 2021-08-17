####---- packages ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(rlang)

####---- load data ----
setwd("C:\\Users\\kylej\\Desktop\\PIT-DSC\\living_costs")
expenses <- read.csv("expenses.csv", stringsAsFactors = F)

####---- restructure ----
expenses <- rbind(colnames(expenses), expenses)
expenses <- t(expenses)
row.names(expenses) <- NULL

colnames(expenses) <- expenses[1,]
expenses <- expenses %>% as_tibble
expenses <- expenses[-1,]

expenses <- expenses %>%
  rename(Range = ï..) %>%
  mutate(Range = gsub("X.63.998..NYC.median.", "$63,998 (NYC median)", Range),
         Range = gsub("X.", "$", Range),
         Range = gsub("\\.\\.", " $", Range),
         Range = gsub("\\.to", " to", Range),
         Range = gsub("\\.", "\\,", Range))

expenses <- expenses %>%
  mutate(across(-Range, ~as.numeric(.)))

expenses <- expenses %>%
  mutate(SNAP=-1*SNAP,
         EITC=-1*EITC,
         Child=-1*Child) %>%
  rename(Child_tax_credit = Child)

####---- plot ----
expenses <- expenses %>%
  mutate(n=as.numeric(row.names(.)))
## all categories
expenses %>% 
  mutate(Taxes = Tax_city+Tax_federal+Tax_state,
         Food = Groceries+Restaurants,
         Savings = Emergency_savings+Retirement_savings,
         Tax_credits = EITC+Child_tax_credit) %>%
  select(-c(Tax_city, Tax_federal, Tax_state, Groceries, Restaurants, Emergency_savings, Retirement_savings, EITC, Child_tax_credit,
            Total, Midpoint, leftover, p_leftover, p_leftover_nn)) %>% #unneeded
  gather(key, value, -c(Range, n)) %>% #need but not on y-axis
  ggplot(aes(reorder(Range, n), value)) +
  geom_col(aes(fill = key)) +
  labs(x = "Income", y = "Expenditure ($)") +
  ggtitle("Estimated Expenses for Family of Four\n in New York City") +
  theme(plot.title = element_text(hjust = .5))

## food
tmp <- expenses %>% 
  mutate(Groceries = Groceries/Midpoint*100,
         Restaurants = Restaurants/Midpoint*100,
         SNAP = SNAP/Midpoint*100,
         Net_food_expenses = Groceries+Restaurants+SNAP)
tmp %>% 
  gather(Categories, value, c(Groceries, Restaurants, SNAP)) %>% #need but not on y-axis
  ggplot(aes(reorder(Range, n), value, group = 1)) +
  geom_col(aes(fill = Categories)) +
  labs(x = "Income", y = "Expenditure (% of Income)") +
  ggtitle("Estimated Food Expenses for Family of Four\n in New York City") +
  theme(plot.title = element_text(hjust = .5)) +
  geom_line(data = tmp,
            aes(reorder(Range, n), Net_food_expenses, group = 1, color = "Net food expenses"),
            size=2) +
  scale_color_manual(name = "Total",
                     values = c('Net food expenses'='red'))

## taxes
tmp <- expenses %>% 
  mutate(Tax_federal = Tax_federal/Midpoint*100,
         Tax_state = Tax_state/Midpoint*100,
         Tax_city = Tax_city/Midpoint*100,
         EITC = EITC/Midpoint*100,
         Child_tax_credit = Child_tax_credit/Midpoint*100,
         Net_taxes = Tax_city+Tax_federal+Tax_state+EITC+Child_tax_credit)
tmp %>% 
  gather(Categories, value, c(Tax_city, Tax_federal, Tax_state, EITC, Child_tax_credit)) %>% #need but not on y-axis
  ggplot(aes(reorder(Range, n), value, group = 1)) +
  geom_col(aes(fill = Categories)) +
  labs(x = "Income", y = "Expenditure (% of Income)") +
  ggtitle("Estimated Taxes for Family of Four\n in New York City") +
  theme(plot.title = element_text(hjust = .5)) +
  geom_line(data = tmp,
            aes(reorder(Range, n), Net_taxes, group = 1, color = "Net taxes"),
            size=2) +
  scale_color_manual(name = "Total",
                     values = c('Net taxes'='red'))

## savings
tmp <- expenses %>% 
  mutate(Retirement_savings = Retirement_savings/Midpoint*100,
         Emergency_savings = Emergency_savings/Midpoint*100,
         Total_savings = Retirement_savings+Emergency_savings)
tmp %>% 
  gather(Categories, value, c(Retirement_savings, Emergency_savings)) %>% #need but not on y-axis
  ggplot(aes(reorder(Range, n), value, group = 1)) +
  geom_col(aes(fill = Categories)) +
  labs(x = "Income", y = "Expenditure (% of Income)") +
  ggtitle("Estimated Savings for Family of Four\n in New York City") +
  theme(plot.title = element_text(hjust = .5)) +
  geom_line(data = tmp,
            aes(reorder(Range, n), Total_savings, group = 1, color = "Total savings"),
            size=2) +
  scale_color_manual(name = "Total",
                     values = c('Total savings'='red'))

## others
tmp <- expenses %>%
  mutate(across(Transportation:Child_tax_credit, ~./Midpoint*100))
expense_plot <- function(cat) {
  tmp %>%
    ggplot(aes(reorder(Range, n), {{cat}}, group = 1)) +
    geom_bar(stat = "identity", color = "blue", fill = "blue") +
    labs(x = "Income", y = "Expenditure (% of Income)") +
    ggtitle(paste("Estimated ", deparse(substitute(cat)) ," Expenses for Family of Four\n in New York City")) +
    theme(plot.title = element_text(hjust = .5)) +
    geom_line(color = "red", size = 2)
}
expense_plot(Entertainment)
expense_plot(Insurance)
expense_plot(Transportation)
expense_plot(Utilities)
