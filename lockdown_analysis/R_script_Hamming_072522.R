## Default repo
local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})


setwd("E:\\Ghosh Dropbox\\SUJOY GHOSH\\covid_analysis\\lockdown_related\\lockdown_June30")
dir()

df = read.delim("input_hamming_diffcutoffs_lockdown_070722.txt", sep='\t', header=T)
head(df)

#calculate Hamming distance between comparable vectors

sum(df$a_10 != df$a_20)
sum(df$a_10 != df$a_30)
sum(df$a_20 != df$a_30)
sum(df$b_5 != df$b_10)
sum(df$b_5 != df$b_15)
sum(df$b_10 != df$b_15)
sum(df$c_5 != df$c_10)
sum(df$c_5 != df$c_15)
sum(df$c_10 != df$c_15)
sum(df$e_5 != df$e_10)
sum(df$e_5 != df$e_15)
sum(df$e_10 != df$e_15)
