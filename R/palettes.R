# build ggplot2 colour palettes based on ALA standard colours
# following the workflow outlined here:
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# palette
ala_colors <- c(
  flamingo = "#E06E53", # primary
  rust = "#B8573E",
  grey = "#667073",
  concrete = "#EEECEA", # secondary (monochrome)
  silver = "#9E9E9F",
  charcoal = "#222322",
  honey = "#FFC557", # extended
  pale_moss = "#B7CD96",
  seafoam = "#6BDAD5",
  ocean = "#003A70",
  lavender = "#A191B2",
  plum = "#691C32"
)

ala_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols)) {
    return(ala_colors)
  }
  ala_colors[cols]
}

ala_palettes <- list(
  "1" = ala_cols("flamingo"),
  "2" = ala_cols("concrete", "flamingo"),
  "3" = ala_cols("flamingo", "silver", "charcoal"),
  "6" = ala_cols("flamingo", "honey", "pale_moss", "seafoam", "ocean",
                 "lavender"),
  "7" = ala_cols("flamingo", "honey", "pale_moss", "seafoam", "ocean",
                 "lavender", "plum"),
  "8" = ala_cols("flamingo", "honey", "pale_moss", "seafoam", "ocean",
                 "lavender", "plum", "silver"),
  "9" = ala_cols("flamingo", "honey", "pale_moss", "seafoam", "ocean",
                 "lavender", "plum", "silver", "charcoal"),
  "12" = ala_cols()
)

ala_pal <- function(ncol, reverse = FALSE) {
  pal <- ala_palettes[[as.character(ncol)]]
  if (is.null(pal)) {
    message("No palette available for ", ncol, "colours, defaulting to a gradient color scheme.")
    pal_fn <- colorRampPalette(c(ala_cols("flamingo"), ala_cols("silver")))
    pal <- pal_fn(ncol)
  }
  names(pal) <- NULL
  if (reverse) {
    pal <- rev(pal)
  }
  pal
}