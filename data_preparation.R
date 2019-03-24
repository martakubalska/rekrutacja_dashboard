library(dplyr)

setwd("C:\\Users\\mkuba\\Desktop\\Payback")

#Prepare dataset
history <- read.csv2("history.csv", header = TRUE, stringsAsFactors = TRUE)
transactions <- read.csv2("transactions.csv", header = TRUE, stringsAsFactors = FALSE)

transactions[,2] <- as.Date(transactions[,2],"%d.%m.%Y")
transactions <- transactions %>% filter(transactions$data_zakupu <= "2018-02-14" & transactions$data_zakupu >= "2018-02-01")

full <- history %>% left_join(transactions)
full$kwota_zakupu <- full$kwota_zakupu %>% coalesce(full$kwota_zakupu, 0)

full <- full %>% mutate(zakup = ifelse(kwota_zakupu >0, 'Yes', "No"))
full <- full %>% mutate_if(is.character, factor)

transactions <- read.csv2("transactions.csv", header = TRUE, stringsAsFactors = FALSE)

transactions[,2] <- as.Date(transactions[,2],"%d.%m.%Y")
transactions <- transactions %>% filter(transactions$data_zakupu <= "2018-01-14" & transactions$data_zakupu >= "2018-01-01")
transactions <- rename(transactions, kwota_zakupu_sty = kwota_zakupu)
transactions <- transactions %>% select(id_klienta, kwota_zakupu_sty)

full <- full %>% left_join(transactions)

full$kwota_zakupu_sty <- full$kwota_zakupu_sty %>% coalesce(full$kwota_zakupu_sty, 0)

full <- full %>% mutate(zakup_sty = ifelse(kwota_zakupu_sty >0, 'Yes', "No"))
full <- full %>% mutate_if(is.character, factor)

#Additional check
full %>% 
  group_by(id_klienta) %>% 
  filter(n()>1) 
#3 clients made multiple (2) purchases - in sent group


full %>%
  group_by(id_klienta, grupa, zakup, zakup_sty) %>%
  summarise(kwota_zakupu = sum(kwota_zakupu), kwota_zakupu_sty = sum(kwota_zakupu_sty)) %>%
  ungroup() -> full


write.csv(full, "full.csv")
remove(list=ls())
full <- read.csv("full.csv", header = TRUE)[,-1]




#Basic statistics
stats <- full %>% group_by(grupa) %>% summarize(all = n_distinct(id_klienta))
stats_purchase <- full %>% filter(kwota_zakupu > 0) %>% group_by(grupa) %>% summarize(made_purchase = n_distinct(id_klienta))
stats <- stats %>% inner_join(stats_purchase, by = "grupa") %>% mutate(proportion = made_purchase / all)

stats <- stats %>% bind_cols(full %>% group_by(grupa) %>% summarize(avg_total = mean(kwota_zakupu), med_total = median(kwota_zakupu)) %>% 
            select(avg_total,med_total)) %>% bind_cols(full %>% filter(kwota_zakupu > 0) %>% group_by(grupa) %>% 
                  summarize(avg_purch = mean(kwota_zakupu), med_purch = median(kwota_zakupu)) %>% select(avg_purch,med_purch))

stats_purchase <- full %>% filter(kwota_zakupu_sty > 0) %>% group_by(grupa) %>% summarize(made_purchase_jan = n_distinct(id_klienta))
stats <- stats %>% inner_join(stats_purchase, by = "grupa") %>% mutate(proportion_jan = made_purchase_jan / all)

stats <- stats %>% bind_cols(full %>% group_by(grupa) %>% summarize(avg_total_jan = mean(kwota_zakupu_sty), med_total_jan = median(kwota_zakupu_sty)) %>% 
                               select(avg_total_jan,med_total_jan)) %>% bind_cols(full %>% filter(kwota_zakupu_sty > 0) %>% group_by(grupa) %>% 
                                summarize(avg_purch_jan = mean(kwota_zakupu_sty), med_purch_jan = median(kwota_zakupu_sty)) %>% select(avg_purch_jan,med_purch_jan))


statistics <- stats %>% select(grupa, proportion_jan, avg_total_jan, med_total_jan, avg_purch_jan, med_purch_jan)
statistics <- statistics %>% rename(Group = grupa, "Response_rate" = proportion_jan, "Mean_all" = avg_total_jan, 
                "Median_all" = med_total_jan, "Mean_purchase" = avg_purch_jan, "Median_purchase" = med_purch_jan)


statistics <- statistics %>% mutate_at(c(2:6), round, 2)

write.csv(statistics, "stats.csv")
#remove(list=ls())
statistics <- read.csv("stats.csv", header = TRUE)[,-1]

write.csv(stats, "stats2.csv")
#remove(list=ls())
stats <- read.csv("stats2.csv", header = TRUE)[,-1]

#Tests

test_total_avg <- round(wilcox.test(kwota_zakupu ~ grupa, data = full, paired = FALSE)$p.value,3)

only_purchases <- full %>% filter(kwota_zakupu > 0)
test_purch_avg <- round(wilcox.test(kwota_zakupu~grupa, data = only_purchases, paired = FALSE)$p.value,3)

test_prop <- round(chisq.test(full$grupa,full$zakup)$p.value, 3)
test_prop_sty <- round(chisq.test(full$grupa,full$zakup_sty)$p.value, 3)

test_total_avg_sty <- round(wilcox.test(kwota_zakupu_sty ~ grupa, data = full, paired = FALSE)$p.value,3)

only_purchases <- full %>% filter(kwota_zakupu_sty > 0)
test_purch_avg_sty <- round(wilcox.test(kwota_zakupu_sty~grupa, data = only_purchases, paired = FALSE)$p.value,3)

tests <- cbind(test_total_avg, test_purch_avg, test_prop, test_prop_sty, test_total_avg_sty, test_purch_avg_sty)
