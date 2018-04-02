# check of zone.test:

# WE rule 1:
# first check for series with length <=1:
zone.test(c(),
          3, 1, 1)
zone.test(c(0),
          3,1,1)
zone.test(c(3.1),
          3,1,1)
# check for longer series:
zone.test(c(-3.1),
          3, 1, 1)
zone.test(c(0, -1, -2, 2, 1, 2, 0, 3, 1),
          3, 1, 1)
zone.test(c(0, -1, -2, 2, 1, 2, 0, 3.1),
          3, 1, 1)
zone.test(c(0, -1, -2, 2, 1, 2, 0, 3.1),
          3, 1, 1)
zone.test(c(0, -1, -2, 2, 1, 2, 0, 3.1, -2),
          3, 1, 1)

# WE rule 2:
# first too short series:
zone.test(c(),
          2, 2, 3)
zone.test(c(0),
          2, 2, 3)
zone.test(c(2.1, 2.1, 2.1),
          2, 2, 3)

zone.test2(c(2.1, 2.1),
           2, 2, 3)

# then longer series:
zone.test(c(0, -1, -2, 2, 1, 2, 0, 3.1),
          2, 2, 3)
zone.test(c(0, -1, -2, 1, -2.1, 2.1, 1, 2, 0, 3.1),
          2, 2, 3)
zone.test2(c(0, -1, -2, 1, -2.1, 2.1, 0, 2.1, 1, 2, 0, 3.1),
           2, 2, 3)
zone.test(c(0, -1, -2, 1, 2.1, 2.1, 1, 2, 0, 3.1),
          2, 2, 3)
zone.test(c(0, -1, -2, 1, 1.1, 1.1, 1, 2, 0, 2.1, 2.1),
          2, 2, 3)

# WE rule 3:
# short series:
zone.test(c(),
          1, 4, 5)
zone.test(c(0),
          1, 4, 5)
zone.test(c(-1.1, -1.1, -1.1),
          1, 4, 5)
zone.test(c(-1.1, -1.1, 1.1, -1.1),
          1, 4, 5)
zone.test(c(-1.1, -1.1, -1.1, -1.1),
          1, 4, 5)
# long series:
zone.test(c(0, -1, -2, 2, 1, 2, 0, 3.1),
          1, 4, 5)
zone.test(c(0, -1, -2, -1.1, 1.1, 1.1, 0, 3.1),
          1, 4, 5)
zone.test(c(0, -1, -2, -1.1, 1.1, 0, 1.1, 0, 3.1),
          1, 4, 5)
zone.test(c(0, -1, -2, 1.1, 1.1, 1.1, 0, 3.1),
          1, 4, 5)
zone.test(c(0, -1, -2, .1, .1, .1, 0, 3.1, -1, 1.1, 1.1, 1.1),
          1, 4, 5)

# WE rule 4:
# short series:
zone.test(c(),
          1, 8, 8)
zone.test(c(1),
          1, 8, 8)
zone.test(c(1, 1, 2, 1, 1, 1, 2),
          1, 8, 8)
# long series:
zone.test(c(0, -1, -2, 2, 1, 2, 0, 3.1),
          1, 8, 8)
zone.test(c(0, -1, -2, 2, 1, 2, 0, 3.1, -1,0, -1.1,
            -1.1, -1.1, -1.1, -1.1, -1.1, -1.1),
          1,8,8)
zone.test(c(0, -1, -2, 2, 1, 2, 0, 3.1, -1,0, -1.1, 
            -1.1, -1.1, -1.1, -1.1, -1.1, -1.1, -1.1),
          1,8,8)

