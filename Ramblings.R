activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

# Generates mean, problems when steps = 0

activity %>%
group_by(date) %>%
summarise(mean(steps, na.rm =T))


# Generates logical values and NAs
activity %>%
group_by(date) %>%
summarise(ifelse(sum(steps) > 0, mean(steps, na.rm =T), sum(steps)))

# doesn't work
activity %>%
group_by(date) %>%
summarise(ifelse(steps > 0, mean(steps, na.rm =T), steps))

# Works, but need to tweak format to reduce number of decimal places
activity %>%
group_by(date) %>%
summarise(ifelse(is.finite(mean(steps, na.rm =T)), mean(steps, na.rm =T), 0))

# BUT, from Course Discussion:
#
# I take that to say we need to report One Mean value, and One Median value.
#
# First, calculate the total number of steps for each day.
# Then calculate the mean of those, and then the median of those.


# barplot
act_summ <- activity%>%
group_by(date) %>%
summarise(sum(steps, na.rm = T))
act_gg <- ggplot(act_summ, aes(x = date, y = steps))
act_gg + geom_bar(stat = "identity")

# BUT, this is not a histogram, so:
hist(act_summ$steps, breaks = "FD")

# Or with ggplot,

bw <- with(act_summ, diff(range(steps)) / (2 * IQR(steps) / length(steps)^1/3))
ggplot() + geom_histogram(aes(act_summ$steps), binwidth = bw)

# AVE DAILY PATTERN

act_int <- activity %>%
group_by(interval) %>%
summarise(mean(steps, na.rm = T))
setnames(act_int, "mean(steps, na.rm = T)", "ave_steps")

# Need to adjust interval for labelling mistake
# (Or just re-label?)
# then convert to a time interval

with(act_int, plot(ave_steps ~ interval, type ="l"))

act_int[act_int$ave_steps == max(act_int$ave_steps),]

# Imputing missing values

sum(is.na(activity$steps))

# See:
# http://www.stat.columbia.edu/~gelman/arm/missing.pdf
# for categorisation of missing data
# assume that a day with all NAs corresponds to a day with no steps?
# and days with a few missing variables can be calculated using mean for specific interval?
# or calculate mean between intervals?
