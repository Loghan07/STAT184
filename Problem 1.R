student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
 
# Part A: Building Data Structures
section <- factor(section, levels = c("A","B"))
students <- data.frame(student_id,section,quiz1,quiz2,passed)
score_matrix <- matrix(c(quiz1,quiz2),nrow=6, ncol=2,dimnames=list(student_id, c("quiz1","quiz2")))
course_record <- list(
  course = "R Programming",
  scores = score_matrix,
  cutoffs = c(pass = 70, excellent = 90),
  students = students)

# Part B:Subsetting
score_matrix["S04","quiz2"]
score_matrix[1:2, , drop = FALSE]
course_record["course"]
course_record[["course"]] 
course_record$course

# Part C:Vectorized Calculations 
students$average <- (students$quiz1 + students$quiz2) / 2
students$excellent <- students$average >= 90
secA_subset <- students[students$section == "A" & students$average >= 80, ]
secA_subset[, c("student_id", "section", "average")]

student_averages <- students$average
names(student_averages) <- students$student_id
student_average
