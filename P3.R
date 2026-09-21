# Problem 3
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

# Part A; Write a Grading Function 
grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
      return(NA_character_)
  } 
  else if (score >= a_min) {
    return("A")
  }  else if (score >= b_min) {
    return("B")
  }  else if (score >= c_min) {
    return("C")
  }   else if (score >= d_min) {
    return("D")
  }   else { return("F")}  
}  
  
grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

  
# Part B: Applying Functions with loop  

  
grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  # Store one grade here
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id
r_int <- as.integer(scores)
summary(r_int)

# Part C-1: Function Behavior


summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
    # Summarizing Statistics
    total_count <- length(x)
    missing_count <- sum(is.na(x))
    mean_val <- mean(x, na.rm = na.rm)
    sd_val <- sd(x, na.rm = na.rm)
    min_val <- min(x, na.rm = na.rm)
    max_val <- max(x, na.rm = na.rm)
    
    # Returning Rounding variables
    result <- c(
      total = total_count,
      missing = missing_count,
      mean = round(mean_val, digits),
      sd = round(sd_val, digits),
      min = round(min_val, digits),
      max = round(max_val, digits)
    )
    
    return(result)
  }
# 3 Cases of summarize_score functions.
summarize_scores(scores)

# na.rm true removes Null to properly calculate stats 
summarize_scores(x=scores,na.rm = TRUE, digits = 2) 

# na.rm false keeps null, but results in incomplete data
summarize_scores(x=scores,na.rm = FALSE, digits = 4)

# C-2 Function Behavior 

plot_scores <- function(x, ...) {
  plot(x, ...)
}
# Plot displays students grades with 
# students 1,6,8 have a score greater to or equal to 88 and the remaining are less than 88  
# student 3 scores is not included due to NA data
plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )


  





  