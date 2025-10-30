col_bor <- glue("2px solid {cfc_darkblue}")

rating_color <- function(r) {
  switch(r,
         "Significantly Exceeds Expectations" = "#63BE7B",
         "Exceeds Expectations" = "#97CE80",
         "Meets Expectations" = "#CDDD82",
         "Meets Few Expectations" = "#FEEB84",
         "Fails to Meet Expectations" = "#FBBD7B",
         "white")
}

rating_column <- function(minWidth = 125, class = NULL, ...) {
  colDef(
    minWidth = minWidth,
    class = class,
    cell = function(value, index) {
      s <- clean$stars[index]
      div(
        div(s), div(value, style = "font-size: 0.65rem;line-height: .9rem")
      )
    },
    style = function(value) {
      list(background =rating_color(value),
           borderLeft = col_bor)
    },
    ...
  )
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) {
    if (!is.na(x)) {
      rgb(get_color(x), maxColorValue = 255)
    } else {
      "white"
    }
  }
}

perc_col_ratings <- make_color_pal(hcl.colors(3, "RdYlGn"))


perc_column <- function(minWidth = 65, 
                        borderRight = FALSE, 
                        borderLeft = FALSE,
                        ...) {
  colDef(
    class = "spi-rating",
    cell = label_percent(.1),
    minWidth = minWidth, 
    style = function(x) {
      l <- list(
        backgroundColor = "white",
        fontSize = "1rem"
      ) 
      if (borderRight) {
        l <- append(l,
                    list(borderRight = col_bor))
      }
      if (borderLeft) {
        l <- append(l,
                    list(borderLeft = col_bor))
      }
      return(l)
    }, 
    ...
  )
}

ela_prof_column <- function(minWidth = 65, ...) {
  colDef(
    class = "spi-rating",
    # cell = label_percent(.1),
    cell = function(value, index) {
      s <- clean$count_ELA[index]
      div(
        div(label_percent(.1)(value)), 
        div(s, style = "font-size: 0.75rem;line-height: .9rem"))
    },
    minWidth = minWidth, 
    style = function(x) {
      list(
        fontSize = "1rem",
        background = perc_col_ratings(x),
        color = ifelse(x > .1, "#000", rgb(255, 255, 255, .9 * 250, maxColorValue = 255))
      )},
    ...
  )
}

math_prof_column <- function(minWidth = 65, ...) {
  colDef(
    class = "spi-rating",
    # cell = label_percent(.1),
    cell = function(value, index) {
      s <- clean$count_Mathematics[index]
      div(
        div(label_percent(.1)(value)), 
        div(s, style = "font-size: 0.75rem;line-height: .9rem"))
    },
    minWidth = minWidth, 
    style = function(x) {
      list(
        fontSize = "1rem",
        background = perc_col_ratings(x),
        color = ifelse(x > .1, "#000", rgb(255, 255, 255, .9 * 250, maxColorValue = 255))
      )},
    ...
  )
}

big_col_style <- list(
  fontSize = "1rem",
  backgroundColor = "white"
)

group_head_style <- list(
  fontWeight = 700,
  fontSize = ".75em"
)

with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; border-style: none",
            title = tooltip, value)
}
