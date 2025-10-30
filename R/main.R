library(tidyverse)
library(scales)
library(glue)
library(reactable)
library(reactablefmtr)
library(colorspace)
library(cityforwardcollective)
library(crosstalk)
library(htmltools)
source("R/styles.R")


d <- read_rds("data/milwaukee_schools_2025.rda")
star <- "\u2605"
d <- d |> 
  arrange(desc(overall_score)) |> 
  mutate(stars = case_when(
    overall_rating == "Significantly Exceeds Expectations" ~ paste0(rep(star, 5), collapse = ""),
    overall_rating == "Exceeds Expectations" ~ paste0(rep(star, 4), collapse = ""),
    overall_rating == "Meets Expectations" ~ paste0(rep(star, 3), collapse = ""),
    overall_rating == "Meets Few Expectations" ~ paste0(rep(star, 2), collapse = ""),
    overall_rating == "Fails to Meet Expectations" ~ paste0(rep(star, 1), collapse = ""),
    TRUE ~ ""
  ))

clean <- d |> 
  select(-c(school_year, dpi_true_id))

source("R/write_table_for_export.R")



tbl <- clean |> 
  select(school_name,
         accurate_agency_type,
         grade_band,
         school_enrollment,
         ELA,
         Mathematics,
         overall_rating,
         overall_score,
         sch_ach,
         sch_growth,
         per_ed,
         per_swd,
         per_lep,
         per_b_aa,
         per_hisp_lat) |> 
  reactable(
    # Column defs
    columns = list(
    school_name = colDef(name = "School Name", 
                         minWidth = 125,
                         searchable = TRUE,
                         align = "left",
                         style = list(
                           backgroundColor = "white",
                           fontSize = ".75rem"
                         )),
    accurate_agency_type = colDef(name = "Sector",
                                  minWidth = 100,
                                  searchable = TRUE,
                                  style = list(
                                    fontSize = ".75rem",
                                    backgroundColor = "white"
                                  )),
    grade_band = colDef(name = "Grade Band",
                        searchable = TRUE,
                        minWidth = 80,
                        headerStyle = list(
                          fontSize = ".6rem",
                          borderBottom = col_bor
                        )),
    school_enrollment = colDef(name = "Enrollment",
                               minWidth = 85,
                               cell = label_comma(),
                               style = list(
                                 borderRight = col_bor,
                                 backgroundColor = "white",
                                 fontSize = "1rem"
                               ),
                               headerStyle = list(
                                 fontSize = ".6rem",
                                 borderBottom = col_bor
                               )),
    per_ed = perc_column(name = "ECD",
                         header = with_tooltip("ECD", "Economically Disadvantaged Students"),
                         minWidth = 70,
                         borderLeft = TRUE),
    per_swd = perc_column(name = "SwD",
                          header = with_tooltip("SwD", "Students with Disabilities"),
                          minWidth = 70),
    per_lep = perc_column(name = "LEP",
                          header = with_tooltip("LEP", "Limited English Proficiency"),
                          minWidth = 70),
    per_b_aa = perc_column(name = "Black",
                           minWidth = 75),
    per_hisp_lat = perc_column(name = "Latino",
                               minWidth = 75),
    ELA = ela_prof_column(),
    Mathematics = math_prof_column(name = "Math"),
    overall_rating = rating_column(name = "Overall Rating",
                                   class = "rating-col"),
    overall_score = colDef(name = "Overall",
                           style = big_col_style,
                           cell = function(value) {
                             label_comma(.1)(value)
                           },
                           minWidth = 70),
    sch_ach = colDef(name = "Achievement",
                     style = big_col_style,
                     cell = function(value) {
                       label_comma(.1)(value)
                     },
                     minWidth = 100),
    sch_growth = colDef(name = "Growth",
                        style = big_col_style,
                        cell = function(value) {
                          ifelse(!value == 100,
                                 label_comma(.1)(value),
                                 value)
                          },
                        minWidth = 70)),
    
    columnGroups = list(
      colGroup(name = "SCHOOL REPORT CARD SCORES",
               columns = c("overall_rating",
                           "overall_score",
                           "sch_ach",
                           "sch_growth"),
               headerStyle = group_head_style),
      colGroup(name = "PROFICIENCY %/#",
               columns = c("ELA",
                           "Mathematics"),
               headerStyle = group_head_style),
      colGroup(name = "STUDENT BODY COMPOSITION",
               columns = c("per_ed",
                           "per_swd",
                           "per_lep",
                           "per_b_aa",
                           "per_hisp_lat"),
               headerStyle = group_head_style)
      ),
    
    # Other settings
    pagination = FALSE, 
    searchable = TRUE, 
    showSortIcon = FALSE,
    # rowStyle = group_border_sort(columns = c("overall_rating",
    #                                          "overall_score",
    #                                          "sch_ach",
    #                                          "sch_growth"),
    #                              border_color = "red",
    #                              border_width = "2px"),
    defaultColDef = colDef(sortNALast = TRUE, 
                           style = list(
                             backgroundColor = "white",
                             fontSize = ".85rem"
                           ),
                           align = "center", 
                           vAlign = "center",
                           searchable = FALSE,
                           headerClass = "header",
                           headerVAlign = "bottom", 
                           headerStyle = list(
                             borderBottomStyle = "solid",
                             borderBottomColor = cfc_darkblue,
                             fontSize = ".7rem"
                           )),
    language = reactableLang(
      searchPlaceholder = "Search for a school, sector, or grade band",
      noData = "No schools found"),
    theme = reactableTheme(backgroundColor = "transparent", 
                           searchInputStyle = list(
                             paddingLeft = "1.9rem",
                             paddingTop = "0.5rem",
                             paddingBottom = "0.5rem",
                             width = "100%",
                             border = "none",
                             backgroundSize = "1rem",
                             backgroundPosition = "left 0.5rem center",
                             backgroundRepeat = "no-repeat",
                             backgroundColor = "#ecf2fa"
                           )),
    elementId = "schools-table")

tbl

# search for the <body> tag in the file to copy
save_reactable_test(tbl, "test.html")

# w <- browsable(
#   tagList(
#     tags$button("Download", 
#                 onclick = "Reactable.downloadDataCSV('schools-table', 'Milwaukee Schools 2022-23.csv')"),
#     
#     tbl
#   )
# )
# 
# htmltools::save_html(w, "test.html")
