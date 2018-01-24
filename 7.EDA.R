# categorical variable
ggplot(data = diamonds) +
        geom_bar(mapping = aes(x = cut))
diamonds %>% 
        count(cut)

# continuous variable
ggplot(data = diamonds) +
        geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
diamonds %>% 
        count(carat)
diamonds %>% 
        count(cut_width(carat, 0.5))

diamonds %>% 
        filter(carat < 3) %>% 
        ggplot() +
        geom_histogram(mapping = aes(x = carat), binwidth = 0.1)

smaller <- diamonds %>% 
        filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
        geom_freqpoly(binwidth = 0.1) # if you wish to overlay multiple histograms, use geom_freqpoly.
# it does the same thing with geom_histogram, but using lines

# Typical values
ggplot(data = smaller, mapping = aes(x = carat)) +
        geom_histogram(binwidth = 0.01)
ggplot(data = faithful, mapping = aes(x = eruptions)) +
        geom_histogram(binwidth = 0.25)

# Unusual values
ggplot(data = diamonds) +
        geom_histogram(mapping = aes(x = y), binwidth = 0.5) # To make it easy to see the unusual
# values, we need to zoom to small values of the y-axis with coord_cartesian()
ggplot(data = diamonds) +
        geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
        coord_cartesian(ylim = c(0,50))
unusual <- diamonds %>% 
        filter(y < 3 | y > 20) %>% 
        select(price, x, y, z) %>% 
        arrange(y)
unusual
# repeat your analysis with and without outliers. If the outliers don't have large effect on the results,
# and you can't figure out why there are there, you should replace them with missing values and move on.
# If they have substantial effect on the results, you shouldn't drop them without justification.


# Exercises
# 1. explore the distribution of x,y,z in diamonds dataset
ggplot(data = diamonds, mapping = aes(x = x, y = y, color = z)) +
        geom_point() 
# 2. explore the distribution of price.
ggplot(data = diamonds, mapping = aes(x = price)) +
        geom_histogram(binwidth = 25) +
        coord_cartesian(xlim = c(1200,1600))
# 3. how many diamonds are 0.99 carats? how many are 1 carat?
diamonds %>% 
        filter(carat == 0.99) %>% 
        count(carat)
diamonds %>% 
        filter(carat == 1) %>% 
        count(carat)
sml <- diamonds %>% 
        filter(carat == 0.99 | carat == 1)
ggplot(data = sml, mapping = aes(x = price, color = as.factor(carat))) +
        geom_freqpoly(binwidth = 20)
# 4. compare coord_cartesian vs xlim(), ylim() when zooming in on a histogram
ggplot(data = diamonds, mapping = aes(x = price)) +
        geom_histogram(binwidth = 25) +
        coord_cartesian(xlim = c(1200,1600))
ggplot(data = diamonds, mapping = aes(x = price)) +
        geom_histogram(binwidth = 25) +
        xlim(c(1200, 1600)) 
# coord_cartesian() simply zooms in on the area specified by the limits. The calculation of the histogram is unaffected.
# xlim and ylim functions first drop any values outside the limits, then calculates the histogram, and draws the graph with the given limits


# replace the unusual values with missing values, you can use ifelse() function
diamonds2 <- diamonds %>% 
        mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
        geom_point()
## ggplot will warn how many missing values were removed. To suppress this warning, use na.rm=T.
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
        geom_point(na.rm = TRUE)

# compare the scheduled departure time for cancelled and non-cancelled flights
nycflights13::flights %>% 
        mutate(cancelled = is.na(dep_time),
               sched_hour = sched_dep_time %/% 100,
               sched_min = sched_dep_time %% 100,
               sched_dep_time = sched_hour + sched_min / 60) %>% 
        ggplot(mapping = aes(x = sched_dep_time)) +
        geom_freqpoly(mapping = aes(color = cancelled), binwidth = 0.25)