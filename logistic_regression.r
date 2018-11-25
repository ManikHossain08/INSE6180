setwd("M:/Concordia/summer/term2/INSE_6180_DM/Projects/newproject")

library(data.table)
library(tm)
library(freqdist)
library(SOAR)

source("rating_generator.r")
source("output_generator.r")

options(warn = -1) #Ignore Warnings
#--------------- Global Variables ------------------------
assign("product_name","",.GlobalEnv)
assign("stopwords",as.array(readLines("data_cleaning/stopwords.txt")),.GlobalEnv)
assign("stemCompletionDict",as.array(readLines("data_cleaning/lemmatization.txt")),.GlobalEnv)
assign("doc_train",new.env(),.GlobalEnv)
assign("doc_test",new.env(),.GlobalEnv)
assign("polarity_train",new.env(),.GlobalEnv)
assign("polarity_test",new.env(),.GlobalEnv)
assign("documents_list",c(),.GlobalEnv)
assign("polarity_list",c(),.GlobalEnv)
assign("results",c(),.GlobalEnv)
assign("wei_",matrix(),.GlobalEnv)
assign("synchronizer",10.0,.GlobalEnv)
assign("gradle",matrix(),.GlobalEnv)
assign("positivePredicted",c(),.GlobalEnv)
assign("negativePredicted",c(),.GlobalEnv)
assign("actualRating",0.0,.GlobalEnv)
#--------------- Global Variables ------------------------

calculateRealProductRating= function(dataset){
  print("Calculating Actual Product Rating By Taking Mean of Rating Column From Dataset..")
  actualValue = round(mean(dataset$rating, na.rm=TRUE),digits = 1)
  assign("actualRating",actualValue,.GlobalEnv)
}

sigmoid = function(x){
  return(1.0/(1.0 + exp(-1.0*x)))
}
frequentWordsFunc = function(features = 10){
    if(exists("listofFeatures")){
      listofFeatures = find("listofFeatures")
    }else{
      frequent_words_array = c()
      process_local = 0
      print(paste("Total Review",length(doc_train),sep = " : "))
      print("Computing Words With Frequency Distribution")
      for(item in doc_train){
       process_local = process_local + 1
       print(paste(process_local,length(doc_train),sep = "/"))  
       tokens = cleanReviews(item,stopwords,2)
       for(token in tokens){
         frequent_words_array <- c(frequent_words_array,token)
       }
      }
      frequent_words_df = as.data.frame(frequent_words_array)
      freq = freqdist(frequent_words_df)
      freq = freq[-nrow(freq),]
      topfeatures = head(freq[order(freq$frequencies, decreasing= T),],n=features)
      listofFeatures = as.list(row.names(topfeatures))
      Store(listofFeatures)
      print("Returning List of Features")
    }
    return(listofFeatures)
}

cleanReviews = function(review,englishStopWords,no){
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
  if(no == 1){
    return(selectedTokens)
  }else{
    for(i in seq(length(selectedTokens))){
      selectedTokens = c(selectedTokens,paste(selectedTokens[i],selectedTokens[i+1],sep = " "))
    }
    return(selectedTokens)
  }
} 

uniBiGram = function(review){
  selectedFeatures = c()
  ngram = 2 # Till 2 Gram
  features = 2000/ngram #Selecting only 1000 Features 
  word_found = c()
  common_dict = frequentWordsFunc()
  for(item in seq(1,ngram)){
     resulted_grams = cleanReviews(review,stopwords,item)
     for(word in common_dict){
       if(word %in% resulted_grams){
         word_found <- c(word_found,1)
       }else{
         word_found <- c(word_found,0)
       }
     }
     for(item in word_found){
       selectedFeatures <- c(selectedFeatures,item)
     }
  }
  return(as.array(selectedFeatures))
}

generateFeatureBox = function(doc){
  process = 0
  print("Generating Feature Words Matrix !! Please Wait...")
  for(review in doc){
    process = process + 1
    arr_obt = uniBiGram(review)
    results[[process]] <<- arr_obt
  }
  print("Feature Words Matrix Generated !! Thanks For Your Patience")
  matrixForm = do.call(rbind,results)
  return(matrixForm)
}

stoch_des = function(features,polarity,wei_,speed){
  dimension =  dim(features)
  rowdim = dimension[1]
  coldim = dimension[2]
  for(outer in seq(rowdim)){
    for(inner in seq(coldim)){
      reg =  (synchronizer * wei_[inner]) / rowdim
      tim = sigmoid(t(features[outer]) %*% wei_)
      wei_[inner] <- wei_[inner] - speed * ((tim - polarity[outer]) * features[outer,inner] - synchronizer)
    }
  }
  #Assign To Global  
  return(wei_)
}

score = function(features,polarity,wei_){
  dimension =  dim(features)
  rowdim = dimension[1]
  coldim = dimension[2]
  init = 0
  for(item in seq(rowdim)){
    tmp  = (polarity[item]*log10(sigmoid(t(features[item]) %*% wei_))) + (1.0 - polarity[item]) * log10(1.0 - sigmoid(t(features[item]) %*% wei_))
    init <- init + tmp
  }
  # 30 x 30 Matrix
  sync = synchronizer * ((t(wei_) %*% wei_) / (2.0 * rowdim))
  # 1 x 30 Matrix
  x = -1.0 * (init/rowdim)
  resu = mapply("+",sync,x[1,])
  return(resu)
}

grad_method  = function(features,polarity,zero_matrix,speed,convergence,max = 15){
    dimension =  dim(features)
    rowdim = dimension[1]
    coldim = dimension[2]
    wei_ = zero_matrix
    loop = 0
    change = convergence + 1
    previous_cost = 0
    while(loop < max && change > convergence){
      loop <- loop + 1
      wei_ = stoch_des(features,polarity,wei_,speed)
      score_calculated = score(features,polarity,wei_)
      if(loop > 1){
         change <- abs(100 - (previous_cost/score_calculated)*100)
      }
      previous_cost <- score_calculated
      leg = 0
      for(item in seq(rowdim)){
        ifelse(sigmoid(t(wei_) %*% features[item]) > 0.5,p <- 1,p <- 0)
        ifelse(polarity[item] == p,leg <- leg + 1,leg <- leg + 0)
      }
      prob = 100.0 * (leg/rowdim)
    }
    return(wei_)
} 

trainLRModel = function(){
  print("---------------------------------------------------")
  print("    Training Phase Of Logistic Regression Started  ")
  print("---------------------------------------------------")
  matrixForm = generateFeatureBox(doc_train)
  y = as.matrix(polarity_train)
  no_of_features = ncol(matrixForm)
  no_of_documents = nrow(matrixForm)
  features = matrixForm[sample.int(no_of_documents),]
  polarity = y[sample.int(no_of_documents),]
  data = c()
  print("Creating Zero Matrix of Size of M Features Selected")
  zero_matrix = matrix(c(data,rep(0,each= no_of_features)),ncol = no_of_features)
  print("Computing Other Parameters...wait for Training Phase to end!!")
  grad_result = grad_method(features, polarity,zero_matrix,0.001,1)
  print("---------------------------------------------------")
  print("    Training Phase Of Logistic Regression Ended    ")
  print("---------------------------------------------------")
  return(grad_result)
}

generateDocumentsAndPolarityList = function(dataset){
   for(item in dataset$review){
     documents_list <<- c(documents_list,item)
   }
   for(item in dataset$label){
     ifelse(item == "positive",
            polarity_list <<- c(polarity_list,1),
            polarity_list <<- c(polarity_list,0))
   }
  if(length(documents_list) == nrow(dataset) && length(polarity_list) == nrow(dataset)){
    return(1)
  }else{
    return(0)
  }
}


predictClasses = function(review){
  if(!is.null(gradle)){
    arr = as.array(uniBiGram(review))
    ifelse(sigmoid(t(gradle) %*% arr) >= 0.50,return(1),return(0))
  }else{
    print("Please Train Model First")
  }
}

startTrainingPhase = function(file_name){
  print("Reading File.....")
  dataset = fread(paste("dataset",file_name,sep = "/"),header = TRUE,encoding = "unknown")
  dataset = dataset[,5:7]
  calculateRealProductRating(dataset)
  flag = generateDocumentsAndPolarityList(dataset)
  if(flag == 1){
    trainingsize = floor(0.75 * nrow(dataset))
    set.seed(123)
    print("Splitting Training(75%) and Testing(25%) From DataSet.......")
    prepare_dataset = sample(seq_len(nrow(dataset)),size = trainingsize)
    index = length(prepare_dataset) + 1
    doc_train <<- c(documents_list[prepare_dataset])
    doc_test <<- c(documents_list[index:nrow(dataset)])
    polarity_train <<- c(polarity_list[prepare_dataset])
    polarity_test <<- c(polarity_list[index:nrow(dataset)])
    output = trainLRModel()
    gradle <<- output
    print("---------------------------------------------------")
    print("    Testing Phase Of Logistic Regression Started   ")
    print("---------------------------------------------------")
    print("Let's Start Predicting !!")
    doctest = generateFeatureBox(doc_test)
    legal = 0
    ran = seq(nrow(as.matrix(polarity_test)))
    for(item in ran){
      prediction = predictClasses(doc_test[item])
      if(prediction == polarity_test[item]){
        positivePredicted <<- c(positivePredicted,doc_test[item])
      }else{
        negativePredicted <<- c(negativePredicted,doc_test[item])
      }
    }
    print("Trying To Predict Rating.....")
    ratingGiven = predictRating(length(negativePredicted),
                                length(positivePredicted),
                                nrow(as.matrix(polarity_test)))
    accuracy = ratingGiven/actualRating
    error = abs(1 - accuracy)
    print(paste("Actual Rating : ",actualRating," | Predicted Rating : ",ratingGiven,sep = " "))
    paste(paste("-ve sign means Above Actual Rating","+ve sign means Below Actual Rating",sep = " "))
    print(paste("Rating Difference : ",actualRating - ratingGiven,sep = ""))
    df1 = data.frame(Factors = "Actual Rating",Model = "Logistic Regression",Values = actualRating,Product = product_name)
    df2 = data.frame(Factors = "Predicted Rating",Model = "Logistic Regression",Values = ratingGiven,Product = product_name)
    df_bind = rbind(df1,df2)
    print("CSV File Operation Started(Wrtiting Results)")
    write.table(df_bind, file = "output/results.csv",sep = ",",row.names = F,col.names = F,append = T)
    print("CSV File Operation Completed")
    # # grid::grid.raster(img) -- To Show In Rstudio Plot Section
    print("Saving Star Image Predicted By Logistic Regression")
    generate_star_image("logistic_regression",ratingGiven)
    print("Results are available in output folder")
    print("---------------------------------------------------")
    print("    Testing Phase Of Logistic Regression  Started  ")
    print("---------------------------------------------------")
  }else{
    print("Too Less Data Or Some Issues !! Can't Run Ahead Or Out of Total No. of Data Are Accessed")
  }
}
args = commandArgs(T)
if(length(args) == 0){
  print("Error : No Arguments of File Name Found")
}else{
  if(exists("listofFeatures")){
    Remove("listofFeatures")
  }
  file_name = args[1]
  product_name <<- args[2]
  startTrainingPhase(file_name)
}