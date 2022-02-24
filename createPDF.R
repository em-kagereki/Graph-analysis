library(xaringanBuilder)
build_pdf("C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Report.html")

browser="google-chrome-stable"
pagedown::chrome_print("C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Report.html",output="Report.pdf")
build_pdf("C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Report.html", complex_slides = TRUE)

build_pdf("C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Report.Rmd", complex_slides = TRUE)


library(chromote)

build_pdf(input = "C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Report.html",
          output_file = "C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Report.pdf",
          complex_slides = TRUE,
          partial_slides = TRUE)


build_pdf(input = "C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Report.Rmd",
          output_file = "C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Report.pdf",
          complex_slides = TRUE,
          partial_slides = TRUE)

library(xaringanBuilder)
setwd("C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/")
#build_pdf("Report.Rmd")
build_pdf("Report.html")


pagedown::chrome_print("test.html",output="test.pdf")

