
# Plots only using repeated measures data 
rm_s <- bwplot(scrape_avg ~ treatment, data = complete,
               xlab = "Treatment", ylab = "Scrape Duration (s)", 
               fill = rmcols,
               main = "Differences in Scrape Duration For Three Temperature Treatments",
               par.settings = list(
                 plot.symbol=cols,
                 box.rectangle = cols,
                 box.dot = cols,
                 box.umbrella=cols 
                 
               )
)


plot(rm_s)

rm_t <- bwplot(thump_avg ~ treatment, data = complete,
               xlab = "Treatment", ylab = "Thump Duration (s)", 
               fill = rmcols,
               main = "Differences in Thump Duration For Three Temperature Treatments",
               par.settings = list(
                 plot.symbol=cols,
                 box.rectangle = cols,
                 box.dot = cols,
                 box.umbrella=cols 
                 
               )
)
plot(rm_t)

rm_b <- bwplot(buzz_avg ~ treatment, data = complete,
               xlab = "Treatment", ylab = "Buzz Duration (s)", 
               fill = rmcols,
               main = "Differences in Buzz Duration For Three Temperature Treatments (repeated measures)",
               par.settings = list(
                 plot.symbol=cols,
                 box.rectangle = cols,
                 box.dot = cols,
                 box.umbrella=cols 
                 
               )
)
plot(rm_b)

rm_srate <- bwplot(srates_avg ~ treatment, data = complete,
                   xlab = "Treatment", ylab = "Scrapes/Second", 
                   fill = rmcols,
                   main = "Differences in Scrape Rate For Three Temperature Treatments (repeated measures)",
                   par.settings = list(
                     plot.symbol=cols,
                     box.rectangle = cols,
                     box.dot = cols,
                     box.umbrella=cols 
                     
                   )
)
plot(rm_srate)

rm_bfreq <- bwplot(bfreq_avg ~ treatment, data = complete,
                   xlab = "Treatment", ylab = "Fundamental Frequency (Hz)", 
                   fill = rmcols,
                   main = "Differences in Fundamental Frequency For Three Temperature Treatments (repeated measures)",
                   par.settings = list(
                     plot.symbol=cols,
                     box.rectangle = cols,
                     box.dot = cols,
                     box.umbrella=cols 
                     
                   )
)
