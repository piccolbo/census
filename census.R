files = list.files("splits/")

read.one =
  function(fname)
    read.csv(file.path("splits", fname), nrows = 3198, fill = FALSE)

clean.one =
  function (df)
    df[,sapply(df, function(x) length(unique(x))) >50]

all = lapply(files, function(x) clean.one(read.one(x)))

census = do.call(cbind, all)
census = census[, unique(names(census))]
census = census[, -1993]
census = census [grep(census$Areaname, pattern = ","), ]

prcensus = prcomp(census[, 3:6428], scale. = FALSE)
hist(log(prcensus$sdev))

signlog =
  function(x)
    ifelse(x > 0, log(x + 1), -log(-x + 1))

ggplot(data = data.frame(signlog(prcensus$x[,1:4])), aes(x = PC2, y = PC3, , size = PC1, color = PC4, label = census$Areaname)) + geom_point() + geom_text()

mastdata = read.csv("csv/Mastdata.csv")
row.names(mastdata) = mastdata$Item_Id

extreme.load =
  function(i, top) {
     few = head(sort(prcensus$rotation[,i], decreasing = top), 20)
    data.frame(desc = mastdata[names(few), "Item_Description"], load = few)}
