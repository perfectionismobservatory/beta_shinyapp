box::use(
    bsl = bslib,
)

#' @export
light <- bsl$bs_theme(
  version = 5,
  primary = "#7A9DAF",
  secondary = "#f7fcff",
  warning = "#FFD885",
  danger = "#FF230F",
  success = "#00B321",
  info = "#cfdce2",
  base_font = bsl$font_collection(
    bsl$font_google("Noto Sans", local = FALSE), "Roboto", "serif"
  ),
  heading_font = bsl$font_google("Palatino"),
  code_font = bsl$font_google("Roboto Mono"),
  #spacer = "1.5rem"
  "btn-border-radius" = "10px"
  #"btn-hover-bg" = "black"
)

dark <- bsl$bs_theme()
