library(rmongodb)

# your connection details will likely differ from these defaults
host <- "127.0.0.1"
db <- "testdatabase"

#connect to mongo
#the create function has the following signature
#mongo.create(host="127.0.0.1", name="", username="", password="", db="admin", timeout=0L)

mongo <- mongo.create(host=host , db=db)

# lets create a string that points to our namespace
# the database we're working with is "testdatabase", and the collection is "example"

#get a list of collections within our namespace
mongo.get.database.collections(mongo, db)

collection <- "functions"
namespace <- paste(db, collection, sep=".")

# find the number of documents in 'exampleCollection' collection in the the 'testdatabase' database
mongo.count(mongo, namespace, mongo.bson.empty())


fun1 <- serialize(mean, NULL, 1)

fun1doc <- list(name = "fun1", FunctionInternalName = "mean", funObj = fun1)

fun2 <- serialize(sd, NULL, 1)

fun2doc <- list(name = "fun2", FunctionInternalName = "sd", funObj = fun2)

mongo.insert(mongo, namespace, mongo.bson.from.list(fun1doc))
mongo.insert(mongo, namespace, mongo.bson.from.list(fun2doc))
cursor <- mongo.find(mongo, namespace)
objects <- list()
while (mongo.cursor.next(cursor)) {
  val <- mongo.cursor.value(cursor)
  objects[[length(objects)+1]] <- val
}
objects


id1 <- mongo.bson.to.list(objects[[1]])[[1]]


json <- '{"a":{"$gte":1}}'
> bson <- mongo.bson.from.JSON(json)
> cursor <- mongo.find(m, ns, bson)
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "funid", id1)
query <- mongo.bson.from.buffer(buf)



collection <- "settings"
namespace <- paste(db, collection, sep=".")
cur <- mongo.find(mongo, namespace, query)
objects <- list()
while (mongo.cursor.next(cur)) {
  val <- mongo.cursor.value(cur)
  objects[[length(objects)+1]] <- val
}
objects



set1 <- list(na.rm = T, testset = "test1")
set2 <- list(na.rm = F, testset2 = "test2")

set1doc <- list(funid = mongo.bson.to.list(objects[[1]])[[1]], name = "SettingForTheMeanFunc", settings = set1)
set2doc <- list(funid = mongo.bson.to.list(objects[[2]])[[1]], name = "SettingForTheSdFunc", settings = set2)




collection <- "functions"
namespace <- paste(db, collection, sep=".")

mongo.insert(mongo, namespace, mongo.bson.from.list(fun2doc))



readFiles <- function(folderPath)
{
  d <- dir(folderPath, pattern = "*.txt", full.names = T)
  return(d)
}
rfiles <- serialize(readFiles, NULL, 1)



mongo.bs


collectionElement <- list(fun = rfiles, id = 1, yo = "char")

#lets create a document to insert
b <- mongo.bson.from.list(collectionElement)

#insert the document into our namespace
ok <- mongo.insert(mongo, namespace, b)



# lets insert a few documents
for (i in 2:50 ) {
  collectionElement$id <- i
  b <- mongo.bson.from.list(collectionElement)
  mongo.insert(mongo, namespace, b)
}

# build a query to find all "language: R"
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "id", 1)
query <- mongo.bson.from.buffer(buf)

# get the count
count <- mongo.count(mongo, namespace, query)


#and bring them back into a list
objects <- list()
cursor <- mongo.find(mongo, namespace, query)
while (mongo.cursor.next(cursor)) {
  val <- mongo.cursor.value(cursor)
  objects[[length(objects)+1]] <- mongo.bson.value(val, name = "fun")
}
str(unserialize(objects[[1]]))



fun <- get("test")
