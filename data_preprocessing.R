data<-read.csv("hotels.csv",header = TRUE,sep = ",",
               stringsAsFactors = FALSE,na.strings = c("", "NULL","NA"))

# 1.Find and deal with in missing values
missing_counts<-colSums(is.na(data))
missing_counts <- missing_counts[missing_counts > 0]
print(missing_counts)
#Delete column agent and company
#(information is provided in column distribution channel)
data <- subset(data, select = -c(agent, company))
#fill the NA value of children with 0
data$children[is.na(data$children)] <- 0
print(colnames(data))
#degrade country variable to the first ten countries+other
top_countries <- names(sort(table(data$country), decreasing = TRUE)[1:10])
data$country <- ifelse(data$country %in% top_countries, data$country, "Other")

missing_counts<-colSums(is.na(data))
missing_counts <- missing_counts[missing_counts > 0]
print(missing_counts)

#2. Delete columns that have few impact on classification tasks
data<-subset(data,select = -c(reservation_status_date,reservation_status,market_segment))

#3. deal with outliers
#adr
boxplot(data$adr, main = "ADR Distribution")
summary(data$adr)
quantiles <- quantile(data$adr, probs = c(0.01, 0.99), na.rm = TRUE)
#lead_time
boxplot(data$lead_time, main = "Lead Time Distribution")
summary(data$lead_time)
# install.packages("dplyr")
library(dplyr)

data <- data %>% filter(adr >= quantiles[1] & adr <= quantiles[2])

Q1 <- quantile(data$lead_time, 0.25, na.rm = TRUE)
Q3 <- quantile(data$lead_time, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data %>% filter(lead_time >= lower_bound & lead_time <= upper_bound)


str(data)

#4. feature engineering

#total nights of stay
data$total_nights <- data$stays_in_weekend_nights + data$stays_in_week_nights
data <- subset(data, select = -c(stays_in_weekend_nights, stays_in_week_nights))

#arrival date
Sys.setlocale("LC_TIME", "English")
data$arrival_date <- as.Date(
  paste(data$arrival_date_year, data$arrival_date_month, data$arrival_date_day_of_month, sep = "-"),
  format = "%Y-%B-%d"
)
data$weekday <- weekdays(data$arrival_date)
data$is_weekend <- ifelse(data$weekday %in% c("Saturday", "Sunday"), 1, 0)
data$is_weekend <- as.factor(data$is_weekend)
head(data)
#roomtype consistency
data$consistency_roomtype<-ifelse(data$reserved_room_type==data$assigned_room_type,1,0)

data<-data%>%select(-c(arrival_date_year,arrival_date_week_number,arrival_date,
                       weekday,reserved_room_type,assigned_room_type))


#calculate information gain
library(FSelector)
information.gain(is_canceled ~ ., data = data)
data<-data%>%select(-c(is_weekend,arrival_date_day_of_month))

