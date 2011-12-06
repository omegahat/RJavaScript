switch(type,
       mean = mean(x),
       median = median(x),
       trimmed = mean(x, trim = .1),
       foo = { x = x^2; sum(x)/(length(x) - 1)})
