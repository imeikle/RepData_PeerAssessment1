activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
library(dplyr)

# Which days have how many NAs?
act_days <- activity %>%
    group_by(date) %>%
    summarise(sum(is.na(steps)))

act_days <- activity %>%
    group_by(date) %>%
    summarise(steps = sum(is.na(steps))) %>%
    mutate(day = weekdays(date))



# Hmm. It seems if a day has NAs then it is for every interval
# But thare is no obvious pattern for the days with no data.
# US public holidays?

# Need to add a factor to produce plot panels
wkday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
# Add day_type to act_days for NA type id.
act_panels <- mutate(act_days, day_type = ifelse(weekdays(date) %in% wkday, "Weekday", "Weekend"))
act_panels$day_type <- as.factor(act_panels$day_type)

# Add day_type to activity for panel plots
act_panels <- mutate(activity, day_type = ifelse(weekdays(date) %in% wkday, "Weekday", "Weekend"))
act_panels$day_type <- as.factor(act_panels$day_type)
