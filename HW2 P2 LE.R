reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)
  # PART A 
  # A1
reports_labeled <- reports_raw |> 
  arrange(form, line) |>
  group_by(form) |> 
  fill(customer_id, .direction = "down") |>
  ungroup()
print(reports_labeled)

  # A2
reports_clean <- reports_labeled |> 
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )
print(reports_clean|>
  select(form, line, customer_id, round, primary_amount, backup_amount, amount)
)

  #A recorded primary value is kept instead of the backup because it is a real measurement, 
  #calesce only is used if the primary value is NA. 0 is kept because 0 is a real value, 
  # and therefore its kept. 

  # A3

reports_complete <- reports_clean |> 
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |> 
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))
print(reports_complete)

n_original <- sum(reports_complete$recorded_row)
n_added <- sum(!reports_complete$recorded_row)
missing_before <- sum(is.na(reports_clean$amount))
missing_after <- sum(is.na(reports_complete$amount))

n_original
n_added
missing_before
missing_after

  #explicit missing was C3, round 1 exists in reports_clean but has an amount of NA
  #implicit missing was c1, round 2 had no row at all before complete

  # PART B
  # B1
customers |> count(customer_id) |> filter(n>1)
customers |> filter(is.na(customer_id))
reports_clean |> count(customer_id, round) |> filter(n>1) 

  #B2
customers_marked <- customers |> mutate(customer_found = TRUE)

reports_joined <- reports_clean |> 
  left_join(customers_marked,
            by = join_by(customer_id),
            relationship = "many-to-one",
            na_matches = "never") |>
  mutate(customer_found = coalesce(customer_found, FALSE))

print(reports_joined |>
        select(customer_id, round, amount, region, customer_found))

  #B3
only_in_reports <- reports_clean |>
  anti_join(customers, by = join_by(customer_id),na_matches = "never") |>
  distinct(customer_id)

only_in_customers <- customers |>
  anti_join(reports_clean,by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

print(only_in_reports)
print(only_in_customers)

  #C3 and C4 both have NA for region after the join, so the region alone cannot 
  #distinguish them. customer_found does. C3 is true and has a record while C4 is false, having no record at all. 
  # customer_found helps separate an unknwon feild from a missing match. 

# PART C Check and Reshape Reports

# C1
# Defines Customers_bad and report_duplicated 
customers_bad <- bind_rows(customers, tibble(customer_id = "C2", region = "East"))

reports_duplicated <- reports_clean |>
  left_join(customers_bad,
            by = join_by(customer_id), 
            relationship = "many-to-many")

nrow(reports_clean)
nrow(reports_duplicated)
sum(reports_clean$amount,na.rm = TRUE)
sum(reports_duplicated$amount,na.rm = TRUE)
reports_duplicated |> count(customer_id, round) |> filter(n>1)

#Rows go from 6 to 7 and the total goes from 75 to 95.the repeated report is customer
# c2, round 1. C2 has two records in customers_bad in south and east, so the report counts
# twice, adding an extra 20 to the 75 making it 95

# C2
  # with relationship = "many-to-one" it would read an error when trying to join, 
  # because C2 matches more than one room, which doesnt follow "many-to-one"

  # It doesnt fix the problem, it js switches off the check. The report is still 
  # duplicated and the table would have to be corrected

# C3 Reports Wide 
reports_wide <- reports_complete |> 
  select(customer_id, round, amount) |>
  pivot_wider(names_from = round, names_prefix = "round_", values_from = amount)
print(reports_wide)

  #Dropping missing values will only remove the rows. It wont recover unknown values
  #My prediction would be that there would be 5 rows since there are 5 entries with data
  #and the other 7 would be deleted because they dont have data
