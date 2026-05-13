pacman::p_load_gh("coatless/ucidata")
data(adult)
adult = na.omit(adult) #kill any observations with missingness
adult$y = as.numeric(adult$income == ">50K")
adult$income = NULL
adult$native_country = NULL
prob_mod = glm(y ~ . - education_num, adult, family = binomial("probit"))
prob_mod
p_hat = predict(prob_mod, adult, type = "response")

p_th = 0.5
table(y = adult$y, y_hat = as.numeric(p_hat > p_th))
1598/(21056 + 1598)
4520 / (2988 + 4520)

ucidata::heart_disease_ch

pacman::p_load(data.table, xtable)
adult = data.table(adult)
skimr::skim(adult[, .(fnlwgt, education_num, capital_gain)])
print(xtable(adult[1:5, .(A = fnlwgt, B = education_num, C = capital_gain)]), include.rownames = FALSE)
print(xtable(melt(
  adult[1:5, .(A = fnlwgt, B = education_num, C = capital_gain)],
  measure.vars = c("A", "B", "C"),
  variable.name = "metric",
  value.name = "val"
)), include.rownames = FALSE)
