library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
  
)
# Part A 

sales_clean <- sales_raw |> 
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw)/100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small" 
    )
  )
    
  # Display new columns
print(sales_clean |> select(sale_id, item_clean, amount, discount_rate, net_amount, size))

  


 # Part B: Validating codes and split strings 

 # 1. Creating sales_codes and code_clean
sales_codes <- sales_clean |> 
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

 # 2. 
invalid_codes <- sales_codes |> 
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)
print(invalid_codes)


valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  select(sale_id,code_raw, code_clean) |>
  separate_wider_delim(code_clean, delim = "-", names = c("channel", "number"))
print(valid_code_parts)

# 3. 
sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")
print(sale_tags)

# The difference between Wider and Longer is that separate_wider_delim splits 
# one etxt column into several new columns, keeping the number of rows the same, 
# while separate_longer_delim splits into several rows , keeping same columns

  
# PART C 

customer_summary <- sales_clean |> 
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(known_n == 0, NA_real_, large_n / known_n),
    .by = customer_id
  ) |>
  arrange(customer_id)
print(customer_summary) 
  
# The numerator is 0 because the highest C1 Net amount, 10, is less than 20, therefore 0.
# the denominator is 1, because its the only known net amount not missing

sales_factors <- sales_clean |> 
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean),n = 2, other_level = "Other")
  )
  
print(levels(sales_factors$item_factor))
print(sales_factors |> count(item_factor))
print(levels(sales_factors$item_group))
print(sales_factors |> count(item_group))

#fct_infreq changes the order of the levels, items remain seperate, 
#while fct_lump_n combines categories, so there are fewer levels and counts change


