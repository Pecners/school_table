clean |> 
  select("School Name" = school_name,
         "Sector" = accurate_agency_type,
         "Grade Band" = grade_band,
         "Enrollment" = school_enrollment,
         ELA,
         "Math" = Mathematics,
         "Overall Rating" = overall_rating,
         "Overall Score" = overall_score,
         "Achievement Score" = sch_ach,
         "Growth Score" = sch_growth,
         "Economically Disadvantaged" = per_ed,
         "Students with Disabilitites" = per_swd,
         "Limited English Proficiency" = per_lep,
         "Black" = per_b_aa,
         "Latino" = per_hisp_lat) |> 
  write_csv("data/Milwaukee Schools Data Table 2023.csv")
