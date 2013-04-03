URLS <- do.call(rbind.data.frame, lapply(5:54, FUN))
save(URLS, file=paste0(WD, "/data/urls.RData"))

len <- seq_len(nrow(URLS))

pb <- txtProgressBar(min = 0, max = max(len), style = 3)
dat <- lapply(len, function(i) {
        x <- tryCatch(FUN2(i), error=function(err) NULL)
        setTxtProgressBar(pb, i)
        Sys.sleep(.75)
        return(x)
    }			  
)
close(pb)

#NA for n.ans means it was closed



dat2 <- do.call(rbind, dat)
rownames(dat2) <- NULL
save(dat2, file=paste0(getwd(), "/data/info.RData"))
sink(file=paste0(getwd(), "/data/info.txt"))
dat2
sink()

library(gmailR)
gmail(to=c("tyler.rinker@gmail.com", cell2email(7164722642, "sprint")), 
	  password = "rinktw16", attachment=paste0(getwd(), "/data/info.txt"))

delete(paste0(getwd(), "/data/info.txt"))


