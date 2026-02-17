# containment file for the awful data cleaning procedure for the rstar dataset

# r* and output gap
# Ran this just once and then processedadded missing values in Excel

# annoying format so we'll do this the awful and slow way
# every vintage is in its own sheet so we just iterate through every sheet and take the last observation
size=74
rt_rstar <- data_frame(
  date=as.Date(matrix(nrow=size))
)

# format completely fucking changes at [62], thank you America
for (i in 1:62) {
  buf <- read_excel(path="data/rt_rstar.xlsx", sheet=i)
  rt_rstar$date[i] <- as.Date(
    as.integer(tail(buf[1], n=1)),
    origin="1899-12-30"
  )
  rt_rstar$gap[i] <- as.numeric(tail(buf$...12, n=1))
  rt_rstar$rstar[i] <- as.numeric(tail(buf$...15, n=1))
}

for (i in 63:size) {
  buf <- read_excel(path="data/rt_rstar.xlsx", sheet=i)
  rt_rstar$date[i] <- as.Date(
    as.integer(tail(buf[1], n=1)),
    origin="1899-12-30"
  )
  rt_rstar$gap[i] <- as.numeric(tail(buf$...11, n=1))
  rt_rstar$rstar[i] <- as.numeric(tail(buf$...8, n=1))
}

# shift rt_rstar by 1 day ahead, then 1 quarter behind to line up with other data
rt_rstar$date[ rt_rstar$date < as.Date("2022-10-01") ] <- rt_rstar$date + days(1)
rt_rstar$date <- lag(rt_rstar$date)
rt_rstar$date[1] <- as.Date("2005-01-01")

# write_csv(rt_rstar, "data/rt_rs_clean.csv")