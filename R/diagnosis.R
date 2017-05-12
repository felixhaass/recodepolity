#
# library(plyr)
#
# politymin <- ddply(polity, .(ccode), function(x) data.frame(recode_polity2(x)))
#
# test <- merge(politymin, plumper_pol, by = c("ccode", "year"), all.x = TRUE)
#
# # diagnosis
# plot(test$polity2min.y, test$polity2min.x)
#
# test$diff <- test$polity2min.x - test$polity2min.y
# cor(test$polity2min.x, test$polity2min.y, use ="complete.obs")
#
# View(test %>% filter(abs(diff) >= 2))
