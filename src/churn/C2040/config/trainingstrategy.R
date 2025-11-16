trainingstrategy <- list()

trainingstrategy$testing <- c(202106)
trainingstrategy$training <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104
)

trainingstrategy$undersampling <- 0.05
trainingstrategy$positivos <- c( "BAJA+1", "BAJA+2")
