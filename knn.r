library(data.table)
library(tm)
library(png)

source("rating_generator.r")
source("output_generator.r")

options(warn = -1) #Ignore Warnings
#--------------- Global Variables ------------------------
assign("product_name","",.GlobalEnv)
assign("stopwords",as.array(readLines("data_cleaning/stopwords.txt")),.GlobalEnv)
assign("stemCompletionDict",as.array(readLines("data_cleaning/lemmatization.txt")),.GlobalEnv)
assign("actualRating",0.0,.GlobalEnv)
assign("frequencyOfWords",list(),.GlobalEnv)
assign("store_reviews",c(),.GlobalEnv)
assign("store_labels",c(),.GlobalEnv)
assign("review_with_polarity",c(),.GlobalEnv)
assign("predictedPositive",c(),.GlobalEnv)
assign("predictedNegative",c(),.GlobalEnv)
assign("training_features",c(),.GlobalEnv)
assign("chunkProcessed",0,.GlobalEnv)
assign("chunkReleased",0,.GlobalEnv)
assign("training_set",new.env(),.GlobalEnv)
assign("testing_set",new.env(),.GlobalEnv)

#--------------- Global Variables ------------------------

calculateRealProductRating= function(trainingData){
  print("Calculating Actual Product Rating By Taking Mean of Rating Column From Dataset..")
  actualValue = round(mean(trainingData$rating, na.rm=TRUE),digits = 1)
  assign("actualRating",actualValue,.GlobalEnv)
}

cleanReviews = function(review,englishStopWords){
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
  return(selectedTokens)
} 

storeToStack = function(review,label){
  store_labels <- c(store_labels,label)
  store_reviews <- c(store_reviews,review)
  assign("store_reviews",store_reviews,.GlobalEnv)
  assign("store_labels",store_labels,.GlobalEnv)
}

predictionStarted = function(){
  print("Prediction Started...")
  error = 0
  form_common_dictionary = c()
  testchunkreviews = split(testing_set,(seq(nrow(testing_set))-1) %/% 200)
  for(eachTestChunk in testchunkreviews){
    for(test_review in eachTestChunk$review){
        print("New Test Review In Process....")
        clean_review = cleanReviews(test_review,stopwords)
        clean_review = unlist(strsplit(as.character(clean_review)," "))
        test_word_score = c()
        for(featuresTrain in training_features){
          review = strsplit(featuresTrain,",(?=[^,]+$)", perl=TRUE)
          extract_review = review[[1]][1]
          polarity = as.numeric(review[[1]][2])
          for(word in extract_review){
            if(word %in% clean_review){
              form_common_dictionary <- c(form_common_dictionary,word)
            }
          }
          form_common_dictionary = unique(form_common_dictionary)
          prediction_chance = 0.0
          for(dict_word in form_common_dictionary){
            # Ignore Those Words Whose Frequency is not available
            if(!is.null(frequencyOfWords[[dict_word]])){
              prediction_chance = prediction_chance + log10(nrow(training_set)/frequencyOfWords[[dict_word]])
            }
          }
          test_word_score <- c(test_word_score,paste(prediction_chance,polarity,sep = ","))
        }
        bestTrainWord = sort(test_word_score,decreasing = T)
        top_ten_words = bestTrainWord[1:10]
        count_positive = 0
        count_negative = 0
        for(word in top_ten_words){
          word = strsplit(word,split = ",")
          ifelse(word[[1]][2] == 1,
                 count_positive <- count_positive + 1,
                 count_negative <- count_negative + 1)
        }
        ifelse(count_positive > count_negative,
               predictedPositive <<- c(predictedPositive,test_review),
               predictedNegative <<- c(predictedNegative,test_review))
        print(paste("Positive : ",length(predictedPositive),
                    "Negative :",length(predictedNegative),
                    "Total Test Reviews :",nrow(testing_set),
                    sep = " "))
    }
  }
  if(length(predictedPositive) + length(predictedNegative) == nrow(testing_set)){
      print("Trying To Predict Rating.....")
      ratingGiven = predictRating(length(predictedNegative),
                                  length(predictedPositive),
                                  nrow(testing_set))
      accuracy = ratingGiven/actualRating
      error = abs(1 - accuracy)
      print(paste("Actual Rating : ",actualRating," | Predicted Rating : ",ratingGiven,sep = " "))
      paste(paste("-ve sign means Above Actual Rating","+ve sign means Below Actual Rating",sep = " "))
      print(paste("Rating Difference : ",actualRating - ratingGiven,sep = ""))
      df1 = data.frame(Factors = "Actual Rating",Model = "KNN",Values = actualRating,Product = product_name)
      df2 = data.frame(Factors = "Predicted Rating",Model = "KNN",Values = ratingGiven,Product = product_name)
      df_bind = rbind(df1,df2)
      print("CSV File Operation Started(Wrtiting Results)")
      write.table(df_bind, file = "output/results.csv",sep = ",",row.names = F,col.names = F,append = T)
      print("CSV File Operation Completed")
      # grid::grid.raster(img) -- To Show In Rstudio Plot Section
      print("Saving Star Image Predicted By KNN")
      generate_star_image("knn",ratingGiven)
      print("Results are available in output folder")
      print("---------------------------------------------------")
      print("        Testing Phase Of KNN Ended                 ")
      print("---------------------------------------------------")
    }
}

formFrequencyOfWords = function(eachChunk){
  for(single_review in eachChunk){
    review = strsplit(single_review,",(?=[^,]+$)", perl=TRUE)
    extract_review = review[[1]][1]
    polarity = as.numeric(review[[1]][2])
    if(!is.null(extract_review)){
      tmp_review = cleanReviews(extract_review,stopwords)
      for(unit in as.character(tmp_review)){
         if(unit %in% names(frequencyOfWords)){
           frequencyOfWords[[unit]] <<- frequencyOfWords[[unit]] + 1
         }else{
           frequencyOfWords[[unit]] <<- 0
         }
      }
      training_features <<- c(training_features,
                             paste(tmp_review,rep(polarity,each = length(tmp_review)),sep = ","))
      }
  }
  chunkProcessed <<- chunkProcessed + 1
  print(paste("Chunk of Reviews Processed",chunkProcessed,sep = " : "))
  if(chunkProcessed == chunkReleased){
      print("Generating Frequency Of Words...")
      # Considering Non-Zero Frequency Words Only
      frequencyOfWords <<- frequencyOfWords[frequencyOfWords != 0]
      predictionStarted()
  }
}

divideIntoChunks = function(reviews){
  chunkreviews = split(reviews,(seq(nrow(reviews))-1) %/% 400)
  # Adding Polarity With Each Review In Polarity Form  (-1 or 1)
  for(singleChunk in chunkreviews){
    for(eachReview in singleChunk$review){
      ifelse(singleChunk$label == "positive",
             review_with_polarity <<- c(review_with_polarity,paste(eachReview,1,sep = ",")),
             review_with_polarity <<- c(review_with_polarity,paste(eachReview,-1,sep = ",")))
    }
  }
  if(length(review_with_polarity)  == nrow(training_set)){
    chunksHugeArrayIntoSmallParts = split(review_with_polarity,ceiling(seq_along(review_with_polarity)/200))
    chunkReleased <<- length(lengths(chunksHugeArrayIntoSmallParts))
    print(paste("Total Chunks Created",chunkReleased,sep = " : "))
    for(eachChunk in chunksHugeArrayIntoSmallParts){
       formFrequencyOfWords(eachChunk)
    }
  }
}

splitTrainingsetIntoPositiveNegative = function(trainingset){
  print("Splitting Training(75%) Into Positive and Negative.......")
  positive_reviews = subset(trainingset,label == "positive")
  negative_reviews = subset(trainingset,label == "negative")
  print("Dividing Huge Data Into Chunks......")
  divideIntoChunks(positive_reviews)
  divideIntoChunks(negative_reviews)
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
  splitTrainingsetIntoPositiveNegative(training_set)
}
args = commandArgs(T)
if(length(args) == 0){
  print("Error : No Arguments of File Name Found")
}else{
  file_name = args[1]
  product_name <<- args[2]
  startTrainingPhase(file_name)
}