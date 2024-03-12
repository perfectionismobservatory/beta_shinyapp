box::use(
    bsl = bslib,
    gg = ggplot2,
    ggt = ggtext,
)

#' @export
plot_palette <- c("#2CACF2", "#5D985D", "#FDFD81", "#57707d", "#495e49", "#7f7f71")

#' @export
light <- bsl$bs_theme(
  version = 5,
  primary = "#7A9DAF",
  secondary = "#f9fbfb",
  warning = "#FFD885",
  danger = "#FF230F",
  success = "#00B321",
  info = "#cfdce2",
  base_font = bsl$font_collection(
    bsl$font_google("Noto Sans", local = FALSE), "Roboto", "serif"
  ),
  heading_font = bsl$font_google("Palatino"),
  code_font = bsl$font_google("Roboto Mono"),
  "btn-border-radius" = "10px"
)

dark <- bsl$bs_theme()

#' @export
ggtheme <- gg$theme(
  text = gg$element_text(family = "Noto Sans"),
  plot.title = ggt$element_textbox_simple(
    family = "Merriweather",
    hjust = 0,
    size = 18,
  ),
  plot.subtitle = ggt$element_textbox_simple(
    family = "Noto Sans",
    hjust = 0,
    lineheight = 1.2,
    margin = gg$margin(t = 8, b = 16)
  ),
  plot.caption = gg$element_text(
    family = "Noto Sans",
    hjust = 1,
    lineheight = 1,
    margin = gg$margin(t = 20)
  ),
  plot.margin = gg$margin(16, 20, 16, 16),
  strip.placement = "outside"
)