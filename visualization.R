library(tidyverse)
cars <- read.csv("cleaned_cars.csv",stringsAsFactors = FALSE)
cargurus <- read.csv('cleaned_cargurus.csv', stringsAsFactors = FALSE)
options("scipen"=100, "digits"=4)

#full na
# cars
#fill price na with mean value
cars$price = as.numeric(cars$price)
c_model_s <- subset(cars, cars$model == 'Model S')
c_model_s$price[is.na(c_model_s$price)] = 49496
c_model_s %>% filter(model == 'Model S', is.na(price))

c_model_x <- subset(cars, cars$model == "Model X")
c_model_x$price[is.na(c_model_x$price)] = 81968
c_model_x %>% filter(model == 'Model X', is.na(price))

c_model_3 <- subset(cars, cars$model == "Model 3")
c_model_3$price[is.na(c_model_3$price)] = 50532
c_model_3 %>% filter(model == 'Model 3', is.na(price))

cars %>% filter(!is.na(price)) %>% group_by(model) %>% summarise(mean(price))

c_roadster = subset(cars, cars$model == 'Roadste')

cars1 = rbind(c_model_s,c_model_x,c_model_3,c_roadster)

#cargurus
#fill price na with mean value

cargurus$price[cargurus$price == -1] = mean(cargurus$price[cargurus$price != -1])
cargurus$mileage[is.na(cargurus$mileage)] = mean(cargurus$mileage[!is.na(cargurus$mileage)])
#merge data
colnames(cargurus)[22] <- 'vin'

merged_df = merge(cars1,cargurus,by='vin')

#----data visulazation:----
means <- aggregate(price.x ~  year, merged_df, mean)
merged_df %>%
  ggplot(., aes(x=year, y=price.x, group = year)) +
  geom_boxplot(position = 'dodge') +
  labs(title='Year price comparison', x='Year',y='price') +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = round(price.x,0), y = price.x + 3000))+
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

cars1 %>% ggplot(., aes(x=model)) + 
  geom_bar(aes(fill=model)) + 
  labs(title='Cars.com Tesla model count', x='Model',y='Count') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

labs = c(2012:2018)
cars1 %>% filter(model=='Model S') %>% 
  ggplot(., aes(x=year,fill=year)) + 
  geom_bar()+
  labs(title='Cars.com Model S Year Count', x='Year',y='Count') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

cargurus %>% 
  ggplot(., aes(x=carYear, fill = carYear)) +
  geom_bar()+
  labs(title='Cargurus.com Model S Year Count', x='Year',y='Count') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% 
  ggplot(., aes(x=year,fill=year)) + 
  geom_bar()+
  labs(title='Model S Year Count', x='Year',y='Count') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df.long = melt(merged_df, id='year', measure=c('price.x','price.y'))
merged_df.long %>% group_by(year,variable) %>% summarise(avg = mean(value)) %>% 
  ggplot(., aes(x=year, y =avg, colour = variable)) +
  geom_line()+
  labs(title='Cars Vs Cargurus Model S avg price', x='Year',y='price') +
  scale_color_manual(labels = c("Cars.com", "Cargurus.com"), values = c("red", "blue")) +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df.long1 = melt(merged_df, id = 'hotornot', measure=c('price.x','price.y'))
merged_df.long1 %>% group_by(hotornot,variable) %>% summarise(avg = mean(value)) %>% 
  ggplot(., aes(x=hotornot, y = avg, fill = variable)) +
  geom_col(position = 'dodge')+
  labs(title='Cars Vs Cargurus hot or not', x='Hot or Not',y='price') +
  scale_color_manual(labels = c("Cars.com", "Cargurus.com"), values = c("red", "blue")) +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

means <- aggregate(price.x ~  hotornot, merged_df, mean)
merged_df %>%
  ggplot(., aes(x=hotornot, y=price.x)) +
  geom_boxplot(position = 'dodge') +
  labs(title='Hot or not Hot price comparison', x='Hot or Not',y='price') +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = round(price.x,0), y = price.x + 3000))+
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df.long2 = melt(merged_df, id = 'hasAccidents', measure=c('price.x','price.y'))
merged_df.long2 %>% group_by(hasAccidents,variable) %>% summarise(avg = mean(value)) %>% 
  ggplot(., aes(x=hasAccidents, y = avg, fill = variable)) +
  geom_col(position = 'dodge') +
  labs(title='Cars Vs Cargurus Accident comparison', x='Accident',y='price') +
  scale_color_manual(labels = c("Cars.com", "Cargurus.com"), values = c("red", "blue")) +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

means <- aggregate(price.x ~  hasAccidents, merged_df, mean)
merged_df %>% group_by(hasAccidents) %>% 
  ggplot(., aes(x=hasAccidents,y=price.x)) +
  geom_boxplot(position = 'dodge') +
  labs(title='Accident comparison', x='Accident',y='price') +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = round(price.x,0), y = price.x + 3000))+
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df.long3 = melt(merged_df, id = 'fleet', measure=c('price.x','price.y'))
merged_df.long3 %>% group_by(fleet,variable) %>% summarise(avg = mean(value)) %>% 
  ggplot(., aes(x=fleet, y = avg, fill = variable)) +
  geom_col(position = 'dodge') +
  labs(title='Cars Vs Cargurus Fleet condition comparison', x='Accident',y='price') +
  scale_color_manual(labels = c("Cars.com", "Cargurus.com"), values = c("red", "blue")) +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

means <- aggregate(price.x ~  fleet, merged_df, mean)
merged_df %>% 
  ggplot(., aes(x=fleet,y=price.x)) +
  geom_boxplot(position = 'dodge') +
  labs(title='Fleet comparison', x='Fleet',y='price') +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = round(price.x,0), y = price.x + 3000))+
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

options("scipen"=100, "digits"=4)
merged_df %>% group_by(mileage_range=cut(mileage.x, breaks = seq(0,160000, by = 10000))) %>% 
  summarise(avg = mean(price.x)) %>% 
  ggplot(aes(x=mileage_range, y = avg)) +
  geom_bar(stat = 'identity', fill = 'deeppink3') +
  geom_text(aes(label=round(avg,0)),vjust=0) +
  scale_x_discrete("Mileage", labels = seq(10000,160000,10000))+
  labs(title='Mileage vs Price', x='Mileage',y='price') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank()) +
  coord_flip()


merged_df %>% group_by(mileage_range=cut(mileage.x, breaks = seq(0,160000, by = 10000))) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x=mileage_range, y = count)) +
  geom_bar(stat = 'identity', fill = 'chartreuse3') +
  geom_text(aes(label=round(count,0)),vjust=0) +
  scale_x_discrete("Mileage", labels = seq(10000,160000,10000))+
  labs(title='Mileage vs Number of cars', x='Mileage',y='Count') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank()) +
  coord_flip()

merged_df %>% group_by(hasAccidents,year) %>% summarise(avg = mean(price.x)) %>% 
  ggplot(aes(x=hasAccidents, y = avg, group = year,fill=year)) + 
  geom_col(position = 'dodge') +
  labs(title='Price overview based on year and accident', x='Accident',y='price') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(ext_color) %>% summarise(avg = mean(price.x)) %>% 
  ggplot(aes(x = reorder(ext_color, -avg), y = avg)) + 
  geom_col(position = 'dodge') +
  geom_text(aes(label=round(avg,0)),vjust=0) +
  labs(title='Price based on color', x='color',y='price') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(trimName) %>% 
  ggplot(aes(x=trimName))+
  geom_bar()+
  labs(title='HOT CAR trim hist') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

merged_df %>% group_by(trimName,hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=trimName,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR trim ratio', x='Trim',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

merged_df %>% group_by(price_range=cut(mileage.x, breaks = seq(0,160000, by = 20000)),hotornot) %>% 
  summarise(total = n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=price_range, y = ratio)) +
  geom_col() +
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  scale_x_discrete("Price", labels = seq(10000,160000,20000))+
  labs(title='HOT CAR Price ratio') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank()) +
  coord_flip()

merged_df %>%
  ggplot(aes(x=price.x, color = hotornot)) +
  geom_density() + 
  labs(title='Price range') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% 
  ggplot(aes(x=year,color=hotornot)) +
  geom_density() + 
  labs(title='year') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(year,hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=year,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR year ratio', x='year',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()
#----Days on market

merged_df %>% 
  ggplot(aes(x=daysOnMarket, color=hotornot)) +
  geom_density(aes(y=..density..)) + 
  labs(title='Days on Market') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(days_range=cut(daysOnMarket, breaks = seq(0,600, by = 10)),hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=days_range,y=ratio))+
  geom_col()+
  scale_x_discrete("Days on market", labels = seq(10,600,10))+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR Days on Market ratio', x='color',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

#-----trim
merged_df$ext_color = factor(merged_df$ext_color)
merged_df %>%
  ggplot(aes(x=trimName,color=hotornot)) +
  stat_density(aes(group = hotornot, color = hotornot),position="identity",geom="line") + 
  labs(title='Trim') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

merged_df %>% group_by(ext_color,hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=ext_color,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR color ratio', x='color',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()
#----mileage
merged_df %>% 
  ggplot(aes(x=mileage.x, color=hotornot)) +
  geom_density(aes(y=..density..)) + 
  labs(title='Mileage') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(mileage_range=cut(mileage.x, breaks = seq(0,160000, by = 20000)),hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=mileage_range,y=ratio))+
  geom_col()+
  scale_x_discrete("Mileage", labels = seq(25,600,25))+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR Mileage ratio', x='Mileage',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()
#----Owner count
merged_df %>% 
  ggplot(aes(x=ownerCount, color=hotornot)) +
  geom_density(aes(y=..density..)) + 
  labs(title='Number of previous owner') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(ownerCount,hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=ownerCount,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR Owner count ratio', x='Owner Count',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()
#----accidents
merged_df %>% 
  ggplot(aes(x=hasAccidents, color=hotornot)) +
  stat_density(aes(group = hotornot, color = hotornot),position="identity",geom="line") + 
  labs(title='Has Accidents') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(hasAccidents,hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=hasAccidents,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR Accident ratio', x='Accident condition',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()
#----deal
merged_df %>%
  ggplot(aes(x=deal))+
  stat_density(aes(group = hotornot, color = hotornot),position="identity",geom="line") + 
  labs(title='HOT CAR deal by cars.com') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(deal) %>% 
  ggplot(aes(x=deal))+
  geom_bar()+
  labs(title='HOT CAR deal hist') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

merged_df %>% group_by(deal,hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=deal,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR deal ratio', x='Deal',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

#----color
merged_df %>% group_by(ext_color) %>% 
  ggplot(aes(x=ext_color))+
  stat_density(aes(group = hotornot, color = hotornot),position="identity",geom="line") + 
  labs(title='color') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

merged_df %>% group_by(ext_color,hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=ext_color,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR color ratio', x='color',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

#----fleet
merged_df %>%
  ggplot(aes(x=fleet))+
  stat_density(aes(group = hotornot, color = hotornot),position="identity",geom="line") + 
  labs(title='fleet') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>%
  ggplot(aes(x=fleet))+
  geom_histogram(stat="count")+
  labs(title='fleet') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

merged_df %>% group_by(fleet,hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=fleet,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR fleet ratio', x='fleet',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

#----deal carguru
merged_df %>% 
  ggplot(aes(x=savingsRecommendation))+
  stat_density(aes(group = hotornot, color = hotornot),position="identity",geom="line") + 
  labs(title='HOT CAR deal by Cargurus') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(savingsRecommendation) %>% 
  ggplot(aes(x=savingsRecommendation))+
  geom_bar()+
  labs(title='HOT CAR deal hist by Cargurus') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

merged_df %>% group_by(savingsRecommendation,hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=savingsRecommendation,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR deal ratio by Cargurus', x='Deal',y='Ratio') +
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()
#----dealer rating
merged_df %>% 
  ggplot(aes(x=dealer_rating))+
  stat_density(aes(group = hotornot, color = hotornot),position="identity",geom="line") + 
  labs(title='dealer rating by cars.com') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% 
  ggplot(aes(x=dealer_rating))+
  geom_histogram()+
  labs(title='dealer_rating hist by cars.com') +
  theme_bw() +
  theme(legend.key=element_blank())
merged_df %>% 
  ggplot(aes(x=sellerRating))+
  geom_histogram()+
  labs(title='dealer rating hist by Cargurus.com') +
  theme_bw() +
  theme(legend.key=element_blank())
merged_df %>% 
  ggplot(aes(x=sellerRating))+
  stat_density(aes(group = hotornot, color = hotornot),position="identity",geom="line") + 
  labs(title='dealer rating  by Cargurus.com') +
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(srating_range=cut(dealer_rating, breaks = seq(0,5, by = 1)),hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=srating_range,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR deal rating ratio by Cars', x='Deal',y='Ratio') +
  scale_x_discrete("Rating", labels = seq(3.5,5.5,0.5))+
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()

merged_df %>% group_by(srating_range=cut(sellerRating, breaks = seq(0,6, by = 0.5)),hotornot) %>% 
  summarise(total=n()) %>% 
  mutate(total_total = sum(total), ratio = total/total_total) %>% 
  filter(hotornot=='HOT CAR') %>% 
  ggplot(aes(x=srating_range,y=ratio))+
  geom_col()+
  geom_text(aes(label=round(ratio,2)),vjust=0) +
  labs(title='HOT CAR deal rating ratio by Cargurus', x='Deal',y='Ratio') +
  scale_x_discrete("Rating", labels = seq(3.5,5.5,0.5))+
  theme_bw() +
  theme(legend.key=element_blank())+
  coord_flip()


means <- aggregate(price.x ~  hasAccidents, merged_df, mean)
merged_df %>% group_by(hasAccidents) %>% 
  ggplot(., aes(x=hasAccidents,y=price.x)) +
  geom_boxplot(position = 'dodge') +
  labs(title='Accident comparison', x='Accident',y='price') +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = round(price.x,0), y = price.x + 3000))+
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

means <- aggregate(price.x ~  ownerCount, merged_df, mean)
merged_df %>% 
  ggplot(aes(x=ownerCount, y=price.x, group=ownerCount)) +
  geom_boxplot(position='dodge')+
  labs(title='Owners Count vs Avg price') +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = round(price.x,0), y = price.x + 3000))+
  theme_bw() +
  theme(legend.key=element_blank())

merged_df %>% group_by(price_range=cut(daysOnMarket, breaks = seq(0,600, by = 25))) %>% 
  ggplot(aes(x=price_range, y=price.x, group = price_range)) +
  geom_boxplot() + 
  scale_x_discrete("Days on market", labels = seq(25,600,25))+
  labs(title='Days on Market vs price range') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

#----new plot:
library(VIM)
sapply(merged_df, sd, na.rm = T)
colnames(merged_df)

m_df = merged_df %>% dplyr::select(hotornot,year,trimName,drivetrain,mileage.x,ext_color,int_color,
         price.x,good_deal_margin,deal,dealer,dealer_rating,
         dealer_review_number,consumer_rating,consumer_review_number,
         fleet,hasAccidents,daysOnMarket,ownerCount)
colSums(is.na(m_df))
summary(m_df)
floor(sqrt(nrow(m_df)))
str(m_df)

#price, mil, year
m_df %>% ggplot(aes(x=mileage.x,y=price.x,color=year))+
  geom_point() +
  labs(title='Price, Mileage, and Year') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

#price, mil, hot or not
m_df %>% ggplot(aes(x=mileage.x,y=price.x,color=hotornot))+
  geom_point()+
  labs(title='Price, Mileage, and HOT CAR') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

m_df %>% ggplot(aes(x=mileage.x,y=price.x,color=deal))+
  geom_point()

m_df %>% ggplot(aes(x=mileage.x,y=price.x,color=hasAccidents))+
  geom_point() +
  labs(title='Price, Mileage, and Accident') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())


m_df %>% ggplot(aes(x=mileage.x,y=price.x,color=ownerCount))+
  geom_point() +
  labs(title='Price, Mileage, and Previous owner number') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

m_df %>% ggplot(aes(x=mileage.x,y=price.x,color=ext_color))+
  geom_point() +
  labs(title='Price, Mileage, and Color') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

m_df %>% ggplot(aes(x=mileage.x,y=price.x,color=trimName))+
  geom_point()+
  labs(title='Price, Mileage, and Trim') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())




#----linear model----
#fill missing value:
cars1$good_deal_margin[is.na(cars1$good_deal_margin)] <- cars1$good_deal_margin[!is.na(cars1$good_deal_margin)] %>% mean()
cars1$dealer_rating[is.na(cars1$dealer_rating)] <- cars1$dealer_rating[!is.na(cars1$dealer_rating)] %>% mean()
cars1$dealer_review_number[is.na(cars1$dealer_review_number)] <- cars1$dealer_review_number[!is.na(cars1$dealer_review_number)] %>% mean()


c_model_s <- subset(cars1, cars1$model == 'Model S')
boxplot(c_model_s$year)
boxplot(c_model_s$price)
boxplot(c_model_s$mileage)

str(c_model_s)
test_model_s <- cbind(c_model_s$year,c_model_s$config,c_model_s$mileage,
                      c_model_s$drivetrain,c_model_s$ext_color,c_model_s$int_color,
                      c_model_s$price,c_model_s$owners,c_model_s$good_deal_margin,
                      c_model_s$deal,c_model_s$hotornot,c_model_s$dealer,c_model_s$dealer_rating,
                      c_model_s$dealer_review_number,c_model_s$consumer_rating,c_model_s$consumer_review_number)

test_model_s = data.frame(test_model_s)

colnames(test_model_s) = c('year','config','mileage','drivetrain','ext_color','int_color','price','owners','good_deal_margin',
                           'deal','hotornot','dealer','dealer_rating','dealer_review_number','consumer_rating','consumer_review_number')

test_model_s$mileage = as.numeric(test_model_s$mileage)
test_model_s$year = as.numeric(test_model_s$year)
test_model_s$config = factor(test_model_s$config)
test_model_s$drivetrain = factor(test_model_s$drivetrain)
test_model_s$ext_color = factor(test_model_s$ext_color)
test_model_s$int_color = factor(test_model_s$int_color)
test_model_s$good_deal_margin = as.numeric(test_model_s$good_deal_margin)
test_model_s$deal = factor(test_model_s$deal)
test_model_s$hotornot = factor(test_model_s$hotornot)
test_model_s$dealer = factor(test_model_s$dealer)
test_model_s$dealer_rating = as.numeric(test_model_s$dealer_rating)
test_model_s$dealer_review_number = as.numeric(test_model_s$dealer_review_number)
test_model_s$consumer_rating = as.numeric(test_model_s$consumer_rating)
test_model_s$consumer_review_number = as.numeric(test_model_s$consumer_review_number)
test_model_s$price = as.numeric(test_model_s$price)

smp_size <- floor(0.85*nrow(test_model_s))

set.seed(3)
train_ind <- sample(seq_len(nrow(test_model_s)), size = smp_size)

train <- test_model_s[train_ind, ]
test <- test_model_s[-train_ind, ]

head(test_model_s)
str(test_model_s)

model1 = lm(price ~ .,data=train)
summary(model1)
plot(c_model_s$year,c_model_s$price,xlab="Car Year",ylab="USD",main="Model S Price by Year")
plot(c_model_s$mileage,c_model_s$price,xlab="Milage",ylab="USD",main="Model S Price by Milage")

plot(model1)

model2 <- lm(sqrt(price) ~ ., data =train)
summary(model2)
plot(model2)


