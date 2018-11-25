source("rating_generator.r")
source("output_generator.r")

library(data.table)
library(tm) 
library(png)
options(scipen = 999)

options(warn = -1) #Ignore Warnings
#--------------- Global Variables ------------------------
assign("product_name","",.GlobalEnv)
assign("stopwords",as.array(readLines("data_cleaning/stopwords.txt")),.GlobalEnv)
assign("stemCompletionDict",as.array(readLines("data_cleaning/lemmatization.txt")),.GlobalEnv)
assign("actualRating",0.0,.GlobalEnv)
assign("posprereview",c(),.GlobalEnv)
assign("negprereview",c(),.GlobalEnv)
assign("sumOfPositiveVocab",new.env(),.GlobalEnv)
assign("sumOfNegativeVocab",new.env(),.GlobalEnv)
assign("uniquePositiveVocab",new.env(),.GlobalEnv)
assign("uniqueNegativeVocab",new.env(),.GlobalEnv)
assign("actualPositiveVocab",c(),.GlobalEnv)
assign("actualNegativeVocab",c(),.GlobalEnv)
assign("positiveVocabProbability",new.env(),.GlobalEnv)
assign("negativeVocabProbability",new.env(),.GlobalEnv)
assign("uniqueCombineBagOfWords",list(),.GlobalEnv)
assign("positiveClassProbability",new.env(),.GlobalEnv)
assign("negativeClassProbability",new.env(),.GlobalEnv)
assign("decisionPositivePredicted",c(),.GlobalEnv)
assign("decisionNegativePredicted",c(),.GlobalEnv)
assign("training_set",new.env(),.GlobalEnv)
assign("testing_set",new.env(),.GlobalEnv)
assign("process_local",0,.GlobalEnv)
#--------------- Global Variables ------------------------

calculateRealProductRating= function(trainingData){
  print("Calculating Actual Product Rating By Taking Mean of Rating Column From Dataset..")
  actualValue = round(mean(trainingData$rating, na.rm=TRUE),digits = 1)
  assign("actualRating",actualValue,.GlobalEnv)
}

formatUptoToTwoDecimal = function(value){
  return((floor(value*1000000000)/1000000000))
}

formulaToCalculateProbability = function(wordCount,total,sumOfEachWordCount){
  data = (wordCount + 1)/(total + sumOfEachWordCount)
  return(formatUptoToTwoDecimal(data))
}

predictionTestSet = function(testing_set){
  print("Dividing Into Chunks For Testing DataSet.....")
  divideIntoChunks(testing_set,"testing")
}

calculateProabilityofEachWord = function(){
  print("Calculating Probability Of Each Word In Both Bag Of Words.....")
  totalLengthOfVocab = length(uniqueCombineBagOfWords)
  sumPositiveVocabEachWordCount = sumOfPositiveVocab
  sumNegativeVocabEachWordCount = sumOfNegativeVocab
  tempPositiveVocabProbability = list()
  tempNegativeVocabProbability = list()
  for(item in as.list(names(uniquePositiveVocab))){
    answer = formulaToCalculateProbability(uniquePositiveVocab[[item]],totalLengthOfVocab,sumPositiveVocabEachWordCount)
    tempPositiveVocabProbability[[ item ]] = answer
  }
  for(item in as.list(names(uniqueNegativeVocab))){
    answer = formulaToCalculateProbability(uniqueNegativeVocab[[item]],totalLengthOfVocab,sumNegativeVocabEachWordCount)
    tempNegativeVocabProbability[[ item ]] = answer
  }
  assign("positiveVocabProbability",tempPositiveVocabProbability,.GlobalEnv)
  assign("negativeVocabProbability",tempNegativeVocabProbability,.GlobalEnv)
  print("Probability Of Each Word In Both Bag Of Words Calculated")
  print("---------------------------------------------------")
  print("  Training Phase Of Multinomial Naive Bayes Ended  ")
  print("---------------------------------------------------")
  print("---------------------------------------------------")
  print("  Testing Phase Of Multinomial Naive Bayes Started ")
  print("---------------------------------------------------")
  predictionTestSet(testing_set)
}

removeStopWords = function(review,englishStopWords) {
  review = tolower(review)
  review = gsub('[[:punct:] ]+',' ',review)
  review = gsub('[[:digit:] ]+',' ',review)
  review = gsub('(?<=\\s)(\\w{1,2}\\s)',' ',review,perl=T)
  review = iconv(review, to = "ASCII//TRANSLIT")
  convertedToTokens = unlist(strsplit(review, " "))
  selectedTokens = convertedToTokens[!convertedToTokens %in% englishStopWords]
  selectedTokens = stemDocument(selectedTokens,language = "english")
  selectedTokens = stemCompletion(selectedTokens,stemCompletionDict)
  selectedTokens = selectedTokens[selectedTokens != ""]
  for(i in seq(length(selectedTokens)-1)){
    selectedTokens = c(selectedTokens,paste(selectedTokens[i],selectedTokens[i+1],sep = " "))
  }
  return(paste(selectedTokens, collapse = " "))
}

divideIntoChunks = function(reviews,phase){
  chunkreviews = split(reviews, (seq(nrow(reviews))-1) %/% 400)
  if(phase == "training"){
    print("PreProcessing of Training Set......")
    preProcessingSteps(chunkreviews)
  }else{
    print("PreProcessing of Testing Set......")
    preProcessingStepsForTesting(chunkreviews)
  }
}

checkForCompleteList = function(){
  print("Forming Unique Vocab For Both Positive and Negative....")
  if(length(as.list(actualPositiveVocab)) > 0 && length(as.list(actualNegativeVocab)) > 0){
    combineVocab = do.call(c,list(as.list(actualPositiveVocab),as.list(actualNegativeVocab)))
    uniqueBagOfWords = unique(names(combineVocab))
    assign("uniqueCombineBagOfWords",uniqueBagOfWords,.GlobalEnv)
    for(item in uniqueCombineBagOfWords){
      if(!item %in% as.list(names(actualPositiveVocab))){
        uniquePositiveVocab[[ item ]] = 0
      }else{
        uniquePositiveVocab[[ item ]] = actualPositiveVocab[[ item ]]
      }
      if(!item %in% as.list(names(actualNegativeVocab))){
        uniqueNegativeVocab[[ item ]] = 0
      }else{
        uniqueNegativeVocab[[ item ]] = actualNegativeVocab[[ item ]]
      }
    }
    assign("uniquePositiveVocab",uniquePositiveVocab,.GlobalEnv)
    assign("uniqueNegativeVocab",uniqueNegativeVocab,.GlobalEnv)
    calculateProabilityofEachWord()
  }
}

generateBagOfWords = function(reviews,label){
  print("Generating Bag Of Words")
  reviewsCollection =   Corpus(VectorSource(reviews))
  tdm = TermDocumentMatrix(reviewsCollection,
                            control = list(minWordLength=3))
  tdm = removeSparseTerms(tdm,0.95)
  m = as.matrix(tdm)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(word = names(v),freq=v,stringsAsFactors = FALSE)
  if(label == "positive"){
    positivesum = 0
    for(i in rownames(d)){
      count = d[i,'freq']
      positivesum = positivesum + count
    }
    assign("sumOfPositiveVocab",positivesum,.GlobalEnv)
  }else{
    negativesum = 0
    for(i in rownames(d)){
      count = d[i,'freq']
      negativesum = negativesum + count
    }
    assign("sumOfNegativeVocab",negativesum,.GlobalEnv)
  }
  map = new.env()
  for(i in rownames(d)){
    count = d[i,'freq']
    word = d[i,'word']
    map[[ word ]] = count  
  }
  return(map)
}

callerFunctionToGeneratePositiveBagWords= function(reviews){
  print("Generating Positive Bag Of Words Vocab....")
  map = generateBagOfWords(reviews,"positive")
  assign("actualPositiveVocab",map,.GlobalEnv)
  checkForCompleteList()
}

callerFunctionToGenerateNegativeBagWords = function(reviews){
  print("Generating Negative Bag Of Words Vocab....")
  map = generateBagOfWords(reviews,"negative")  
  assign("actualNegativeVocab",map,.GlobalEnv)
  checkForCompleteList()
}

positiveClassChances = function(tmp){
  posClass = positiveClassProbability
  product = 1 
  for(i in as.list(names(tmp))){
     product= product * (positiveVocabProbability[[i]] ** tmp[[i]]) 
  }
  return(product*posClass)
}

negativeClassChances = function(tmp){
  negClass = negativeClassProbability
  product = 1 
  for(i in as.list(names(tmp))){
    product= product * (negativeVocabProbability[[i]] ** tmp[[i]]) 
  }
  return(product*negClass)
}

goForPrediction = function(review,wordsFreq){
  tmp = list()
  for(item in review){
    tmp[[ item ]] = wordsFreq[[ item ]]     
  }
  posop = positiveClassChances(tmp)
  negop = negativeClassChances(tmp)
  if(posop > negop){
    flag = 1
  }else{
    flag = 0
  }
  return(flag)
}

predictClassPostiveOrNegative = function(review){
  decisionready = c()
  words = strsplit(review," ")
  words.freq = table(words)
  for(i in words){
    for(j in i){
      if(j %in% uniqueCombineBagOfWords){
        decisionready = c(decisionready,j)
      }
    }
  }
  decisionready = unique(decisionready)
  op = goForPrediction(decisionready,words.freq)
  if(op == 1){
    assign("decisionPositivePredicted",decisionPositivePredicted <- c(decisionPositivePredicted,review),.GlobalEnv)
  }else{
    assign("decisionNegativePredicted",decisionNegativePredicted <- c(decisionNegativePredicted,review),.GlobalEnv)    
  }
  if(length(decisionNegativePredicted) + length(decisionPositivePredicted) == nrow(testing_set)){
    print("Trying To Predict Rating.....")
    ratingGiven = predictRating(length(decisionNegativePredicted),
                  length(decisionPositivePredicted),
                  nrow(testing_set))
    accuracy = ratingGiven/actualRating
    error = abs(1 - accuracy)
    print(paste("Actual Rating : ",actualRating," | Predicted Rating : ",ratingGiven,sep = " "))
    paste(paste("-ve sign means Above Actual Rating","+ve sign means Below Actual Rating",sep = " "))
    print(paste("Rating Difference : ",actualRating - ratingGiven,sep = ""))
    df1 = data.frame(Factors = "Actual Rating",Model = "Multinomial NB",Values = actualRating,Product = product_name)
    df2 = data.frame(Factors = "Predicted Rating",Model = "Multinomial NB",Values = ratingGiven,Product = product_name)
    df_bind = rbind(df1,df2)
    print("CSV File Operation Started(Wrtiting Results)")
    write.table(df_bind, file = "output/results.csv",sep = ",",row.names = F,col.names = T,append = T)
    print("CSV File Operation Completed")
    # grid::grid.raster(img) -- To Show In Rstudio Plot Section
    print("Saving Star Image Predicted By Multinomial Naive Bayes")
    generate_star_image("nultinomial_nb",ratingGiven)
    print("Results are available in output folder")
    
    print("---------------------------------------------------")
    print("Testing Phase Of Multinomial Naive Bayes Ended")
    print("---------------------------------------------------")
  }
}

preProcessingStepsForTesting = function(reviewsInChunk){
  print("Calculating Probability of Each Test Review Words In Both Classes....i.e P(W(t)|C(i))")
  for(single in reviewsInChunk){
    for(item in single$review){
        data = removeStopWords(item,stopwords)
        predictClassPostiveOrNegative(data)
    }
  }
}


preProcessingSteps = function(reviewsInChunk){
  for(single in reviewsInChunk){
    for(data in single$review){
      data = removeStopWords(data,stopwords)
      process_local <<- process_local + 1
      len = nrow(training_set) + nrow(testing_set)
      print(paste("Review PreProcessed",paste(process_local,len,sep = "/"),sep = " : "))
      ifelse(single$label == "positive",posprereview <- c(posprereview,data),negprereview <- c(negprereview,data))
    }
  }
  assign("posprereview",posprereview,.GlobalEnv)
  assign("negprereview",negprereview,.GlobalEnv)
  if(length(posprereview) > 0 && length(negprereview) > 0){
    print("PreProcessing of Training Test Finished....")
    callerFunctionToGeneratePositiveBagWords(posprereview)
    callerFunctionToGenerateNegativeBagWords(negprereview)
  }
}

splitTrainingsetIntoPositiveNegative = function(trainingset){
  print("Splitting Training(75%) Into Positive and Negative.......")
  totalTrainingData = nrow(trainingset)
  positive_reviews = subset(trainingset,label == "positive")
  negative_reviews = subset(trainingset,label == "negative")
  priorposprob = formatUptoToTwoDecimal(nrow(positive_reviews)/totalTrainingData)
  priornegprob = formatUptoToTwoDecimal(nrow(negative_reviews)/totalTrainingData)
  assign("positiveClassProbability",priorposprob,.GlobalEnv)
  assign("negativeClassProbability",priornegprob,.GlobalEnv)
  print("Dividing Huge Data Into Chunks......")
  divideIntoChunks(positive_reviews,"training")
  divideIntoChunks(negative_reviews,"training")
}

startTrainingPhase = function(file_name){
  print("Reading File.....")
  dataset = fread(paste("dataset",file_name,sep = "/"),header = TRUE,encoding = "unknown")
  dataset = dataset[,5:7]
  trainingsize = floor(0.75 * nrow(dataset))
  set.seed(123)
  print("Splitting Training(75%) and Testing(25%) From DataSet.......")
  prepare_dataset = sample(seq_len(nrow(dataset)),size = trainingsize)
  training_set <<- dataset[prepare_dataset,]
  testing_set <<- dataset[-prepare_dataset,]
  calculateRealProductRating(dataset)
  splitTrainingsetIntoPositiveNegative(dataset)
}
print("---------------------------------------------------")
print(" Training Phase Of Multinomial Naive Bayes Started ")
print("---------------------------------------------------")

args = commandArgs(T)
if(length(args) == 0){
  print("Error : No Arguments of File Name Found")
}else{
  file_name = args[1]
  product_name <<- args[2]
  startTrainingPhase(file_name)
}