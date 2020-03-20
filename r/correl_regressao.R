library(odbc)
sort(unique(odbcListDrivers()[[1]]))

con <- dbConnect(odbc(), Driver = "ODBC Driver 17 for SQL Server", 
                 Server = "172.27.2.4", Database = "db_foco", 
                 UID = "desenv", PWD = "desenv", Port = 1433)


data <- dbGetQuery(con,'
SELECT *
FROM DB_FOCO..TB_SAEB_SARESP
WHERE DESEMPENHO_SARESP IS NOT NULL
ORDER BY DESEMPENHO_SAEB DESC
')


summary(data)

res <- cor.test(data$DESEMPENHO_SAEB, data$DESEMPENHO_SARESP, 
                method = "pearson")
res

plot(data, col="red", main="Plotting Pairs Against Each Other", lwd = 1000)

Model <- lm(DESEMPENHO_SAEB ~ DESEMPENHO_SARESP, data = data)
Model
summary(Model)


plot(x = data$DESEMPENHO_SAEB, y = data$DESEMPENHO_SARESP)

library(ggplot2)

ggplot(data = data, aes(x = DESEMPENHO_SAEB, y = DESEMPENHO_SARESP)) +
  geom_point(color = 'red') +
  stat_smooth(method = 'lm')


summary(data)