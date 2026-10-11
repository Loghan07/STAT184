# Problem 3 Two sample T test

# 3. A1
two_sample_t <- function(x, y, equal_var = FALSE, alpha = .05){
  nx <- length(x)
  ny <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  
  if (equal_var) {
    method <- "Pooled"
    sp2 <- ((nx-1) * var_x + (ny - 1) * var_y) / (nx + ny - 2 )
    se <- sqrt(sp2 * (1/ nx + 1 / ny)) 
    df <- nx +ny - 2 
  } else{
    method <- "Welch"
    se <- sqrt(var_x / nx + var_y / ny) 
    df <- (var_x / nx + var_y / ny)^2 / 
      ((var_x/nx)^2 /(nx - 1)+ (var_y/ ny)^2 /(ny-1))
        
  }
  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
                  
return(list(
  method = method,
  t_stat = t_stat,
  df = df,  
  p_value = p_value, 
  reject = p_value < alpha
) )
}

x_check <- c(12,15,14,10,13)
y_check <- c(9,11,8,10,7,12,9,8)

two_sample_t(x_check,y_check)
two_sample_t(x_check,y_check, equal_var = TRUE)
             
ref_welch <- t.test(x_check, y_check, var.equal = FALSE)
ref_pooled <- t.test(x_check, y_check, var.equal = TRUE)

mine_welch <- two_sample_t(x_check, y_check)
mine_pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)

abs(mine_welch$t_stat - ref_welch$statistic) < 1e-8
abs(mine_welch$df - ref_welch$parameter) < 1e-8
abs(mine_welch$p_value - ref_welch$p.value) < 1e-8

abs(mine_pooled$t_stat - ref_pooled$statistic) < 1e-8
abs(mine_pooled$df - ref_pooled$parameter) < 1e-8
abs(mine_pooled$p_value - ref_pooled$p.value) < 1e-8

#all six differences are smaller than 1e-8, so my function t_stat, df and 
#p_value match t.test() for bothe the welch and pooled versions. 



 # B
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null", "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)
set.seed(2026)

n_rows <- nrow(scenarios) * B * 2
res_scenario <- character(n_rows)
res_replicate <- integer(n_rows) 
res_method <- character(n_rows)
res_t <- numeric(n_rows)
res_df <- numeric(n_rows)
res_p <- numeric(n_rows) 
res_reject <- logical(n_rows)

row_i <- 0
for(s in seq_len(nrow(scenarios))) {
  for(b in seq_len(B)) {
    x <- rnorm(n_x,mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])
 
    out_welch <- two_sample_t(x, y, equal_var = FALSE, alpha = alpha)
    row_i <- row_i + 1
    res_scenario[row_i] <- scenarios$scenario[s]
    res_replicate[row_i] <- b
    res_method[row_i] <- out_welch$method
    res_t[row_i] <- out_welch$t_stat
    res_df[row_i] <- out_welch$df
    res_p[row_i] <- out_welch$p_value
    res_reject[row_i] <- out_welch$reject

    out_pooled <- two_sample_t(x, y, equal_var = TRUE, alpha = alpha)
    row_i <- row_i + 1
    res_scenario[row_i] <- scenarios$scenario[s]
    res_replicate[row_i] <- b
    res_method[row_i] <- out_pooled$method
    res_t[row_i] <- out_pooled$t_stat
    res_df[row_i] <- out_pooled$df
    res_p[row_i] <- out_pooled$p_value
    res_reject[row_i] <- out_pooled$reject
 }
}

simulation_results <- data.frame(
  scenario = res_scenario,
  replicate = res_replicate,
  method = res_method,
  t_stat = res_t, 
  df = res_df,
  p_value = res_p, 
  reject = res_reject
)
  print(head(simulation_results, 6))
  nrow(simulation_results)

# B-2 
  
simulation_summary <- simulation_results |> 
  summarise(
    mean_t = round(mean(t_stat),3),
    sd_t = round(sd(t_stat),3),
    rejection_rate = round(mean(reject),3),
    .by = c(scenario, method)
  )

print(simulation_summary)
  
# B-3

# When the means are equal, both tests reject around 5% of the time, if the
# variances are equal, however, when they are unequal, the pooled test rejects 13.9%
# while Welch stays at 6.1% so the pooled test makes more Type 1 errors.
# When the means differ, both tests have high power(.96) with equal variances, but power
# drops with unequal variances(.282 Welsh, .497 Pooled).The pooled test looks better, but thats because it rejects
# too easily, so a higher power doesn't automcattically make it a better test, the numbers
# could change because of random variation. 
# 