library("tidyverse")
library("leaps")
library("AmesHousing")
ames        <- AmesHousing::make_ames()
numericVars <- ames %>% summarise_all(is.numeric) %>% unlist()
ames        <- ames[, numericVars]
head(ames)

NumCols <- ncol(ames)

res <- regsubsets(Sale_Price ~ .,data=ames, method = "backward",  nvmax=NumCols)
smm <- summary(res)
smm$rss

plot(smm$rss,xlab="Number of Variables", ylab="RSS", type='l')
min_rss <- which.min(smm$rss)
points(min_rss, smm$rss[min_rss], col="red", cex=2, pch=20)
