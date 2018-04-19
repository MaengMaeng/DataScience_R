# rm(list=ls()) # 워크 스페이스 클리어

# setwd(C:\Users\tjdqo_000\DataScience_R)

rawdata = read.csv("./toyota.csv") # 워킹 디렉토리 설정 및 데이터 로드

colnames(rawdata) = c("Id", "Model", "Price", "Age_08_04", "Km", "Fuel_Type", "HP", "Color")
unique_price = unique(rawdata$Fuel_Type)
price_dummy = as.data.frame(matrix(0, nrow(rawdata), length(unique_price)))

for(i in 1:(length(unique_price))){
  tmp = unique_price[i]
  tmp_idx = which(rawdata$Fuel_Type == tmp)
  price_dummy[tmp_idx, i] = 1
  colnames(price_dummy)[i] = sprintf("price_%s", tmp)
}

prdata = cbind(price_dummy, prdata)
prdata = prdata[,!(names(prdata) %in% c("Color","Model"))]

trn_ratio = 0.7
trn_idx = sample(1:nrow(prdata), round(trn_ratio*nrow(prdata)))
tst_idx = setdiff(1:nrow(prdata), trn_idx)

trn_data = prdata[trn_idx,]
tst_data = prdata[tst_idx,]

fit_lr = lm(formula = Price ~., data = trn_data)
fit_lr
summary(fit_lr)
pred_lr = predict(fit_lr, tst_data)
mse_lr = mean((tst_data$Price-pred_lr)^2)

step_lr = step(fit_lr, direction = "both")
summary(step_lr)

pred_step = predict(step_lr, tst_data)

mse_step = mean((tst_data$Price-pred_step)^2)

par(mfrow=c(1,2))
plot(tst_data$Price, pred_lr)
plot(tst_data$Price, pred_step)

