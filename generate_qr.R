# generate_qr.R
# Generates a QR that opens your deployed User App URL


# install.packages("qrcode")
library(qrcode)


user_app_url <- "https://joematthews-coffeetracker.share.connect.posit.cloud/" # <-- replace


plot(qr_code(user_app_url, ecl = "M"))


message("Saved ", outfile, ". Print and stick it by the machine.")