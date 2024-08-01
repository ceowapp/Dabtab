## ui for radiant
navbar_proj(
  suppressWarnings(
    do.call(
      fluidPage_ui,
      c(
        "Radiant for R",
        getOption("radiant.landing_ui"),
        getOption("radiant.basic_ui"),
        getOption("radiant.shared_ui"),
        help_menu("help_ui")
      )
    )
  )
)
