# from https://pkg.garrickadenbuie.com/xaringan2powerpoint/slides.html#9

#this part not working, saved by hand in chrome from presentations gh pages
#pagedown::chrome_print(here("docs/20230201_BluefishRT_ForageIndexSummary_Gaichas.html"), 
#                       output = here("IEA/BluefishRT_2022-12-07/BluefishRT_ForageIndexSummary.pdf"))

library(here)

pages <- pdftools::pdf_info(here("IEA/BluefishRT_2022-12-07/BluefishRT_ForageIndexSummary.pdf"))$pages
filenames <- sprintf(here("IEA/BluefishRT_2022-12-07/summaryppt/BluefishRT_ForageIndexSummary_%02d.png"), 1:pages)
dir.create(here("IEA/BluefishRT_2022-12-07/summaryppt"))
pdftools::pdf_convert(here("IEA/BluefishRT_2022-12-07/BluefishRT_ForageIndexSummary.pdf"), filenames = filenames)
slide_images <- glue::glue(
  "
![]({filenames})
---
")
slide_images <- paste(slide_images, collapse = "\n")
md <- glue::glue(
  "
  ---
  output: powerpoint_presentation
  ---
  {slide_images}
  "
)
cat(md, file = here("IEA/BluefishRT_2022-12-07/BluefishRT_ForageIndexSummary_ppt.Rmd"))

# this last bit doesnt work either, only keeps last slide
rmarkdown::render(here("IEA/BluefishRT_2022-12-07/BluefishRT_ForageIndexSummary_ppt.Rmd"))  ## powerpoint!
