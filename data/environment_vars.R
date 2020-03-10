require(here)
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else x
}

bold_col <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textbf{\\textcolor{%s}{%s}}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else x
}


slide_img_dir = "slides/slide_images"
chris_img_dir = "Images"
chris_dat_dir = "data/chris_data"

due_date_format = "%A, %B %d %Y at %I:%M PM"

# book_data = "/data/Fletcher_Fortin-2018-Supporting_Files/data/"

# Store data locally:
# dir.exists(book_data)
# book_data = "C:/Users/michaelnelso/Box\ Sync/data/Fletcher_Fortin-2018-Supporting_Files/data/"
