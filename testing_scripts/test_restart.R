devtools::install()

rstudioapi::restartSession()
Sys.sleep(5)

library(convenr)
canvas_setup()
canvas_data <- compile_canvas_data(course_id = 27531, sub_id = 130992, sub_type = "assignments")
