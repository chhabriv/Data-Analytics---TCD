qplot(BOD$Time, BOD$demand) + geom_bar(stat="identity")
ggplot(BOD$Time, BOD$demand) + geom_bar(stat="identity")
ggplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(Time, demand, data=BOD, geom="bar", stat="identity")
dev.off()
ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat="identity")
