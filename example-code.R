library("readxl")

df1 <- read_excel("df1.xlsx") ## This is big table #1
df2 <- read_excel("df2.xlsx") ## Thsi is big table #2, same column names but different order

df.merged <- rbind(df1,df2) ## Create new data frame of both tables but keeping the order of columns as in df1
