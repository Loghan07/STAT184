# problem 2 Importing, Cleaning, and Summarizing data

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

measurements <- read.csv(text = csv_text, na.strings = c("", "NA"))

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements)) 

measurements_complete <- measurements[complete.cases(measurements), ]

removed_samples <- measurements[!complete.cases(measurements), "sample_id"]
removed_samples
 
#Part B: Transform, filter, and summarize
measurements$site <- as.factor(measurements$site)
measurements$status <- as.factor(measurements$status)
levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements_below_7 <- measurements$ph < 7

subset_obs <- measurements[
  (measurements$site == "North" | measurements$site == "South") & 
    measurements$status == "ok" & complete.cases(measurements),
]

subset_obs[, c("sample_id", "site", "temp_c", "temp_f", "ph")]

mean_overall_temp <- mean(measurements$temp_c, na.rm = TRUE)
mean_south_temp <- mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

mean_overall_temp
mean_south_temp

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

elementwise_result <- A * B
matrix_result <- A %*% B

elementwise_result
matrix_result
