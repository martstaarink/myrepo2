OKEEEEEEEEEEE


OKEEEE2


for (i in 0:9){
  vals = head(seq(i,i+0.99,by=0.99/365),-1)
  emp = append(emp,vals)
}



for (i in 1:length(stock_prices[1,])){
  for (j in 2:length(stock_prices[,1])){
    
    stock_prices[j,i] = stock_prices[j-1,i] * exp((mu-0.5*vol^2)*delta_t + vol*sqrt(delta_t) * rnorm(1,0,1))
    
  }
}


df = as.data.frame(stock_prices)

colors <- c("Index" = "blue", "Current index = 33" = "navyblue", "Barrier level = 26" = "red", "Base rate = 40" = "deepskyblue")
year = c(rep(1,365),rep(2,365),rep(3,365),rep(4,365),rep(5,365),rep(6,365),rep(7,365),rep(8,365),rep(9,365),rep(10,365))
days = 1:3650

#1 good one mid
#2 super high
#3 low
#4 low
#5 mid
#6 mid
#7low
#8 good one high
#9 super low
#10 good one low

baser = 40 #s_0*1.2              #*1.2
barrier = 0.8*s_0      

plot1 = ggplot(df, aes(x=emp))+
  geom_line(aes(y=V16, color="Index"),linewidth=0)+
  geom_hline(aes(yintercept=s_0, color="Current index = 33"),linewidth=1)+
  geom_hline(aes(yintercept=baser, color="Base rate = 40"),linewidth=1)+
  geom_hline(aes(yintercept=barrier, color="Barrier level = 26"),linewidth=1)+
  labs(x="Time (years)",
       y = "Index",
       color = "Legend")+
  scale_color_manual(values = colors)+
  scale_x_continuous(breaks = (seq(0, 10, by = 1)))+
  scale_y_continuous(breaks = (seq(0,100,by = 10)))+
  ggtitle("1: Return = 61%*")+
  annotate("text", x = 7.5, y = 60, label = round(df$V16[length(df$V16)],2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_light()

plot1

plot2 = ggplot(df, aes(x=emp))+
  geom_line(aes(y=V37, color="Index"),linewidth=0)+
  geom_hline(aes(yintercept=s_0, color="Current index = 33"),linewidth=1)+
  geom_hline(aes(yintercept=baser, color="Base rate = 40"),linewidth=1)+
  geom_hline(aes(yintercept=barrier, color="Barrier level = 26"),linewidth=1)+
  labs(x="Time (years)",
       y = "Index",
       color = "Legend")+
  scale_color_manual(values = colors)+
  scale_x_continuous(breaks = (seq(0, 10, by = 1)))+
  scale_y_continuous(breaks = (seq(0,100,by = 10)))+
  ggtitle("2: Return = -5%*")+
  annotate("text", x = 9.2, y = 43, label = round(df$V37[length(df$V37)],2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_light()

plot2

plot3 = ggplot(df, aes(x=emp))+
  geom_line(aes(y=V11, color="Index"),linewidth=0)+
  geom_hline(aes(yintercept=s_0, color="Current index = 33"),linewidth=1)+
  geom_hline(aes(yintercept=baser, color="Base rate = 40"),linewidth=1)+
  geom_hline(aes(yintercept=barrier, color="Barrier level = 26"),linewidth=1)+
  labs(x="Time (years)",
       y = "Index",
       color = "Legend")+
  scale_color_manual(values = colors)+
  scale_x_continuous(breaks = (seq(0, 10, by = 1)))+
  scale_y_continuous(breaks = (seq(0,100,by = 10)))+
  ggtitle("3: Return = -15%")+
  annotate("text", x = 9.2, y = 11, label = round(df$V11[length(df$V11)],2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_gray()

plot3

ggarrange(plot1, plot2, plot3, ncol=3, nrow=1, common.legend = TRUE, legend="right")

plot(df$V11)

last_row = stock_prices[3650,]

for (i in 1:3650){
  print(i)
  value = last_row[i]
  print(value)
  if (value < 20 & value > 10){
    print(i)
    break
  }
}

last_row[16]

#----------- calculate expected return van the product
#0.83
baser = s_0*1.2              #*1.2
barrier = 0*s_0            #0.9*
floor = -0.15                 #-0.2
floor2 = (1+floor)*s_0

returns = rep(NA,length(df[1,]))
payoffs = rep(NA,length(df[1,]))

for (i in 1:length(df[1,])){
  prices = df[,i]
    
  
  index_return = 1 + (prices[length(prices)] - baser) / baser
  if (min(prices)< barrier){
    return = max(1+floor,index_return)
    payoff = return*notional_invested
    print('hi')
  }
  else {
    return = index_return
    payoff = return*notional_invested
  }
  returns[i] = return
  payoffs[i] = payoff
  
}

contract_cost = (mean(returns)*s_0 - s_0)*exp(-0.05*10)

notional_invested = 10000
m = notional_invested/s_0

#mean return
mean_return = mean(returns)

#mean payoff
pv_coupons = 4000
mean_payoff = mean_return*notional_invested

#calculate participation rate:
alpha = pv_coupons / (m*contract_cost)

alpha
