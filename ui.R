gridlayout::grid_page(
  layout = c(
    "header header",
    "sidebar distPlot",
    "sidebar distPlot"
  ),
  row_sizes = c(
    "100px",
    "1fr",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "15px",
  gridlayout::grid_panel_text(
    area = "header",
    content = "Spirograph",
    h_align = "start",
    is_title = TRUE
  ),
  gridlayout::grid_panel_stack(
    area = "sidebar",
    item_alignment = "top",
    item_gap = "12px",
    shiny::sliderInput(
      inputId = "point",
      label = "Points Drawn",
      min = 1L,
      max = 6000L,
      value = 1L
    ),
    shiny::sliderInput(
      inputId = "ring1",
      label = "Radius Ring 1",
      min = 1L,
      max = 120L,
      value = 100L,
      width = "100%"
    ),
    shiny::sliderInput(
      inputId = "ring2",
      label = "Radius Ring2",
      min = -100L,
      max = 30L,
      value = -30L,
      width = "100%"
    ),
    shiny::checkboxInput(
      inputId = "showRings",
      label = "Show Rings?",
      value = TRUE
    ),
    shiny::radioButtons(
      inputId = "penColor",
      label = "Pen Color",
      choices = list(
        Red = "red",
        Green = "darkgreen",
        Blue = "blue"
      )
    ),
    shiny::sliderInput(
      inputId = "penLoc",
      label = "Pen length relative to radius",
      min = 0L,
      max = 50L,
      value = 20L,
      width = "100%"
    ),
    shiny::checkboxInput(
      inputId = "zoom",
      label = "Zoom? (Won't always make a difference)",
      value = FALSE
    )
  ),
  gridlayout::grid_panel_plot(area = "distPlot")
)
