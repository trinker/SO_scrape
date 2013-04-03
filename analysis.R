htruncdf(dat3)

dat3 <- dat2[-which.min(dat2$min.ellapse), ]
dat3$sec.ellapse <- dat3$min.ellaps*60
tags2 <- strsplit(as.character(dat3$tags), "\\|")
dat3$ntags <- sapply(tags2, length)
dat3$tags2 <- sapply(tags2, paste, collapse = " ")
dat3$time <- factor(round(dat3$hour), levels=0:24)
dat3$day.o.wk <- factor(dat3$day.o.wk, 
	levels = c("Sunday", "Monday","Tuesday", "Wednesday", 
	"Thursday", "Friday", "Saturday"))

ggplot(dat3, aes(min.ellapse, post.wc)) + geom_point(alpha=1/5, size=3) +
	stat_smooth()

ggplot(dat3, aes(min.ellapse, code.len))  + geom_point(alpha=1/5, size=3) +
	stat_smooth()


ggplot(dat3, aes(time, fill = accept))  + geom_bar() +
	facet_wrap(~day.o.wk, ncol = 4) + coord_flip()



ggplot(dat3, aes(day.o.wk))  + geom_bar() 

ggplot(dat3, aes(day.o.wk, fill = as.factor(n.ans)))  + 
	geom_bar(position='dodge') + coord_flip()

ggplot(dat3, aes(n.votes, post.wc)) + geom_point(aes(colour=code.len), alpha=1/8) +
	 scale_color_gradient(high="blue", low="yellow")

ggplot(dat3, aes(n.code.chunks, n.votes))  + 
	geom_point() + facet_grid(~day.o.wk)

ggplot(dat3, aes(sec.ellapse, hour)) + geom_point(alpha=1/5, size=3) +
	stat_smooth() + facet_grid(~day.o.wk)

ggplot(dat3, aes(ntags, min.ellapse))  + geom_point(alpha=1/5, size=3) +
	stat_smooth()


ggplot(dat3, aes(time, min.ellapse/60))  + geom_point(size=2, alpha=1/10) +
	coord_flip() + facet_grid(day.o.wk~.)

dat4 <- ddply(dat3, .(day.o.wk, time), summarise, 
	tot=length(sec.ellapse), ave.time =mean(sec.ellapse, na.rm=TRUE))

ggplot(dat4, aes(time, ave.time))  + geom_bar() +
	 facet_grid(day.o.wk~.)

ggplot(dat4, aes(y=ave.time, x=time))  + geom_point(aes(size=tot)) + 
	facet_wrap(~day.o.wk, ncol = 4) + coord_flip()

dat3$half.day <- factor(lookup(dat3$hour, 11:22, "10-22EST", "23-9EST"))
mod1 <- lm(sec.ellapse ~ n.votes + day.o.wk, data=dat3)
mod2 <- lm(sec.ellapse ~ n.votes + day.o.wk, data=dat3)
mod3 <- lm(sec.ellapse ~ n.votes + day.o.wk + half.day, data=dat3)
mod4 <- lm(sec.ellapse ~ n.votes + day.o.wk + half.day + post.wc, data=dat3)
rmod5 <- lm(sec.ellapse ~ day.o.wk + half.day + , data=dat3)

summary(mod4)

anova(mod1, mod2, mod3)