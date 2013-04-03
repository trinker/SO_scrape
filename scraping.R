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

dat2 <- do.call(rbind, dat)
rownames(dat2) <- NULL
save(dat2, file=paste0(getwd(), "/data/info.RData"))
sink(file=paste0(getwd(), "/data/info.txt"))
dat2