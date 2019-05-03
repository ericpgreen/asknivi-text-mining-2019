# Corporate Colors
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2?utm_content=buffer68f3b&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer

# define colors
  nivi_colors <- c(
    `blue`       = "#005BE7",
    `purple`     = "#812990",
    `orange`     = "#F37021",
    `rose`       = "#F17DB1",
    `mint`       = "#4FBFA5",
    `yellow`     = "#FFC20E",
    `black`      = "#000000",
    `lightgrey`  = "#D3D3D3",
    `darkgrey`   = "#A9A9A9",
    `grey`       = "#808080"
  )


#' Function to extract colors as hex codes
#'
#' @param ... Character names of Nivi colors
#'
  nivi_cols <- function(...) {
    cols <- c(...)

    if (is.null(cols))
      return (nivi_colors)

    nivi_colors[cols]
  }

# define palettes
  nivi_palettes <- list(
    `all`        = nivi_cols("blue", "purple", "orange", "yellow", "mint", "rose"),
    `primary`    = nivi_cols("blue", "grey"),
    `secondary`  = nivi_cols("purple", "orange", "yellow"),
    `askNivi`    = nivi_cols("mint", "rose"),
    `purple`     = nivi_cols("purple", "blue", "mint", "orange", "yellow")
  )

  #' Return function to interpolate a Nivi color palette
  #'
  #' @param palette Character name of palette in nivi_palettes
  #' @param reverse Boolean indicating whether the palette should be reversed
  #' @param ... Additional arguments to pass to colorRampPalette()
  #'
  nivi_pal <- function(palette = "main", reverse = FALSE, ...) {
    pal <- nivi_palettes[[palette]]

    if (reverse) pal <- rev(pal)

    colorRampPalette(pal, ...)
  }

  #' Color scale constructor for Nivi colors
  #'
  #' @param palette Character name of palette in nivi_palettes
  #' @param discrete Boolean indicating whether color aesthetic is discrete or not
  #' @param reverse Boolean indicating whether the palette should be reversed
  #' @param ... Additional arguments passed to discrete_scale() or
  #'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
  #'
  scale_color_nivi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
    pal <- nivi_pal(palette = palette, reverse = reverse)

    if (discrete) {
      discrete_scale("colour", paste0("nivi_", palette), palette = pal, ...)
    } else {
      scale_color_gradientn(colours = pal(256), ...)
    }
  }

  #' Fill scale constructor for Nivi colors
  #'
  #' @param palette Character name of palette in nivi_palettes
  #' @param discrete Boolean indicating whether color aesthetic is discrete or not
  #' @param reverse Boolean indicating whether the palette should be reversed
  #' @param ... Additional arguments passed to discrete_scale() or
  #'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
  #'
  scale_fill_nivi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
    pal <- nivi_pal(palette = palette, reverse = reverse)

    if (discrete) {
      discrete_scale("fill", paste0("nivi_", palette), palette = pal, ...)
    } else {
      scale_fill_gradientn(colours = pal(256), ...)
    }
  }
