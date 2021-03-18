top_bar_colour <- "#434C5E"
top_bar_text_colour <- "#fff"

light_blue <- "#434C5E"
dark_blue <- "#2E3440"

# Spinner colour, 
options(spinner.color=light_blue)

# Thematic
# thematic_shiny(bg = "#000000", fg = "#000000", accent = "#000000")

# Tab box CSS
tabbox_css <- ".nav-tabs-custom>.tab-content {background: #D8DEE9;}
.nav-tabs-custom>.nav-tabs {border-bottom-color: #2E3440;}
.nav-tabs-custom>.nav-tabs>li.active>a, .nav-tabs-custom>.nav-tabs>li.active:hover>a {background-color: #D8DEE9;}
.form-control {background-color: #D8DEE9;}"

# Notification bar colour
# n_bar_css <- '.shiny-notification {color:#ffffff;}'
n_bar_css <- paste0('.progress-bar {background-color: ', light_blue, ';}')

custom_theme <-  create_theme(
  adminlte_color(
    light_blue = light_blue
  ),
  adminlte_sidebar(
    # width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#f2f2f2",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

css <- 