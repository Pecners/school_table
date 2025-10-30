library(tidyverse)
library(wisconsink12)

clean |> 
  select("School Name" = school_name,
         "Sector" = accurate_agency_type,
         "Grade Band" = grade_band,
         "Enrollment" = school_enrollment,
         "ELA % Prof." = ELA,
         "Math % Prof." = Mathematics,
         "ELA # Prof." = count_ELA,
         "Math # Prof." = count_Mathematics,
         "Overall Rating" = overall_rating,
         "Overall Score" = overall_score,
         "Achievement Score" = sch_ach,
         "Growth Score" = sch_growth,
         "Economically Disadvantaged" = per_ed,
         "Students with Disabilitites" = per_swd,
         "Limited English Proficiency" = per_lep,
         "Black" = per_b_aa,
         "Latino" = per_hisp_lat) |> 
  write_csv("data/Milwaukee Schools Data Table 2025.csv")

# 
# # for Big Lake Data export
# d <- read_rds("data/milwaukee_schools_2024.rda")
# 
# d |> 
#   mutate(accurate_agency_type = str_remove(accurate_agency_type, "\\*")) |>
#   filter(!is.na(accurate_agency_type)) |> 
#   group_by(accurate_agency_type) |> 
#   left_join(geocodes) |> 
#   select(school_year,
#          dpi_true_id,
#          lat,
#          long,
#          school_name,
#          accurate_agency_type,
#          grade_band,
#          school_enrollment,
#          ELA,
#          Mathematics,
#          overall_rating,
#          overall_score,
#          sch_ach,
#          sch_growth,
#          per_ed,
#          per_swd,
#          per_lep,
#          per_b_aa,
#          per_hisp_lat) |> 
#   write_csv("data/milwaukee_schools_data_2023-24.csv")
