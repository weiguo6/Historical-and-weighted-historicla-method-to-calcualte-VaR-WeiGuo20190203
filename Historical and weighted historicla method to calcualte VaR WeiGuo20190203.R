rm(list = ls())

library(xts)
library(PerformanceAnalytics)

path = 'C:/Users/allac/Desktop/'

SP500 = read.csv(paste(path,'^GSPC.csv',sep = "",collapse = "")) 
DJI = read.csv(paste(path,'^DJI.csv',sep = "",collapse = ""))
Nasdaq = read.csv(paste(path,'^IXIC.csv',sep = "",collapse = ""))
RUT = read.csv(paste(path,'^RUT.csv',sep = "",collapse = ""))

Indexes = data.frame(date = SP500[1:nrow(SP500),1],
                     SP500 = SP500[1:nrow(SP500),5],
                     DJI = DJI[1:nrow(DJI),5],
                     Nasdaq = Nasdaq[1:nrow(Nasdaq),5],
                     RUT = RUT[1:nrow(RUT),5]
)
#compute index returns and put them into a dataframe---we will use this dataframe
# Returns = data.frame(date = SP500[2:nrow(SP500),1],
#                      SP500 = log(SP500[2:nrow(SP500),5]/SP500[1:(nrow(SP500)-1),5]),
#                      DJI = log(DJI[2:nrow(DJI),5]/DJI[1:(nrow(DJI)-1),5]),
#                      Nasdaq = log(Nasdaq[2:nrow(Nasdaq),5]/Nasdaq[1:(nrow(Nasdaq)-1),5]),
#                      RUT = log(RUT[2:nrow(RUT),5]/RUT[1:(nrow(RUT)-1),5])
# )


Returns = data.frame(date = SP500[2:nrow(SP500),1],
                     SP500 = (SP500[2:nrow(SP500),5]/SP500[1:(nrow(SP500)-1),5]-1),
                     DJI = (DJI[2:nrow(DJI),5]/DJI[1:(nrow(DJI)-1),5]-1),
                     Nasdaq = (Nasdaq[2:nrow(Nasdaq),5]/Nasdaq[1:(nrow(Nasdaq)-1),5]-1),
                     RUT = (RUT[2:nrow(RUT),5]/RUT[1:(nrow(RUT)-1),5]-1)
)


total.return = data.frame(date = Returns[1:nrow(Returns),1],
                          rowSums(Returns[2:5]))

names(total.return) <- c("Date", "returns")

total.return_xts <-xts(total.return$returns,order.by=as.Date(total.return$Date,"%m/%d/%Y"))
names(total.return_xts) = "returns"



#Q1(historical method)------------------------------------------------------------------
first_date = as.Date('01/01/2008',"%m/%d/%Y")
last_date = as.Date('12/31/2009',"%m/%d/%Y")

Var_Q1 = c()
Date_Q1 = c()

while (first_date <= last_date) {
  check = total.return_xts[first_date]
  if (nrow(check) == 1) {
    sample_data = head(tail(total.return_xts[paste('/',first_date,sep = "",collapse = "")],1001),1000)
    Var_Q1 = c(Var_Q1, VaR(sample_data, p = 0.99))
    
    #quantile(sample_data$returns,prob = 0.01)
    # 
    Date_Q1 = c(Date_Q1,first_date)
  }
  first_date = first_date + 1
}

VaR_1 <- as.xts(Var_Q1, order.by=as.Date(Date_Q1, origin = "1970-01-01"))
plot(VaR_1)

#Q4(weighted historical method)---------------------------------------------------------
weights = c()
nita = 0.995
for (i in 1:1000) {
  temp_weights =nita ^ (i - 1) * (1 - nita) / (1 - nita ^ 1000)
  weights = c(weights, temp_weights)
}

VaR_Q4 = c()
Date_Q4 = c()
first_date = as.Date('01/02/2008',"%m/%d/%Y")
last_date = as.Date('12/31/2009',"%m/%d/%Y")
while (first_date <= last_date) {
  check = total.return_xts[first_date]
  if (nrow(check) == 1) {
    sample_data = head(tail(total.return_xts[paste('/',first_date,sep = "",collapse = "")],1001),1000)
    sample_data_weighted = cbind(sample_data,weights)
    sample_data_weighted_sort = data.frame(sample_data_weighted)
    sample_data_weighted_sort = sample_data_weighted_sort[order(sample_data_weighted_sort$returns),]
    
    weight = 0
    i = 1
    while(weight <= 0.01) {
      weight = weight + sample_data_weighted_sort$weights[i]
      i = i + 1
    }

    i = i - 1
    Var = (sample_data_weighted_sort$returns[i] - sample_data_weighted_sort$returns[i - 1]) * (0.01 - weight + sample_data_weighted_sort$weights[i]) / sample_data_weighted_sort$weights[i] + sample_data_weighted_sort$returns[i - 1] 
    VaR_Q4 = c(VaR_Q4,Var)
    Date_Q4 = c(Date_Q4,first_date)
   
  }
  first_date = first_date + 1
}

VaR_4 <- as.xts(VaR_Q4, order.by=as.Date(Date_Q4, origin = "1970-01-01"))
plot(VaR_4)


