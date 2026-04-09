# Select sample sites for additional measurements or tests

#TEST - selection of field points along a wildness gradient, inside a district, with similar income ----

field <- uws_iris_clean %>%
  filter(
    Arr == "Ivry",   # keep if we have irvy
    X_revenus_m > 17000 & X_revenus_m < 22000         # keep if 17000<income<22000
  )

nrow(field)


sept <- uws_iris_clean %>%
  filter(Arr == "7e") %>%
  arrange(desc(`Total wildness`))

spt_field = sept[floor(seq(1, nrow(sept), length.out = 10)), ]
