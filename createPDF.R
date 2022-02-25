library(xaringanBuilder)


build_pdf(input = "Report.Rmd",
          output_file = "Report.pdf",
          complex_slides = TRUE,
          partial_slides = TRUE)


build_pptx(input = "Report.Rmd",
          output_file = "Report.pptx",
          complex_slides = TRUE,
          partial_slides = TRUE)


