# install.packages("data.table")  #For Reading Huge Data File
# install.packages("tm") #Using Just For StemDocument and StemCompletion Function
# install.packages("freqdist") #For Logistic Regression --- Selecting Best Features
# install.packages("png") #To Show Star Rating Images From images Folder
# install.packages("ggplot2") #For Plotting Graph
# install.packages("SOAR") #For Cache Management
source("output_generator.r")

print(" --------------------------------------------------------------------------------")
print(" Project Title : Product Rating System Based On Customer Reviews [Group : 7]")
print("---------------------------------------------------------------------------------")

print(" --------------------------------------------------------------------------------")
print(" Algorithms Chosen : KNN , Multinomial Naive Bayes , Logistic Regression         ")
print(" --------------------------------------------------------------------------------")


print(" --------------------------------------------------------------------------------")
print("     Before Starting Please Check You Have Install Required Packages             ")
print(" --------------------------------------------------------------------------------")


getProductNameFromFile = function(filename){
  productName = ""
  if(filename == "echo_dot.csv"){
    productName = "EchoDot"
  }else if(filename == "bedsheet.csv"){
    productName = "BedSheet"
  }else{
    productName = "FireStick"
  }
  return(productName)
}

askFromUser = function(){
  print(" --------------------------------------------------------------------------------")
  print("Enter 1 ,2 or 3 For Futher Process")
  print("Products Available For Product Rating")
  print("1. Echo Dot [Total : 17731 | Positive Reviews : 10644 | Negative Reviews : 7087 | Current Rating : 4.3]")
  print("2. Bed Sheet[Total : 12522 | Positive Reviews : 6582 | Negative Reviews : 5940 | Current Rating : 4.5]")
  print("3. Fire Stick(**Quick Process Demo**) [Total : 4110 | Positive Reviews : 2104 | Negative Reviews : 2006 | Current Rating : 4]")
  print(" --------------------------------------------------------------------------------")
  choice = as.integer(readLines("stdin",n = 1))
  if(1 <= choice && choice <= 3){
    file_name = switch(choice,"echo_dot.csv","bedsheet.csv","fire_stick.csv")
    print(paste("Predicting Rating For",getProductNameFromFile(file_name),sep = " : "))
    print("Let's Train & Test 3 Models")
    product_name = getProductNameFromFile(file_name)
    print("----------------------------------------------------------------------------")
    print("           Training and  Testing For Multinomial Naive Bayes Started        ")
    print("----------------------------------------------------------------------------")
    system(paste("Rscript multinomial_naive_bayes.r",file_name,product_name,sep = " "),intern = FALSE)
    print("----------------------------------------------------------------------------")
    print("           Training and  Testing For Multinomial Naive Bayes Ended          ")
    print("----------------------------------------------------------------------------")
    
    print("----------------------------------------------------------------------------")
    print("         Training and  Testing For Logistic Regression Started               ")
    print("----------------------------------------------------------------------------")
    system(paste("Rscript logistic_regression.r",file_name,product_name,sep = " "),intern = FALSE)
    print("----------------------------------------------------------------------------")
    print("       Training and  Testing For Logistic Regression Started Ended          ")
    print("----------------------------------------------------------------------------")

    print("----------------------------------------------------------------------------")
    print("           Training and  Testing For KNN Started                            ")
    print("----------------------------------------------------------------------------")
    system(paste("Rscript knn.r",file_name,product_name,sep = " "),intern = FALSE)
    print("----------------------------------------------------------------------------")
    print("           Training and  Testing For KNN Ended                              ")
    print("----------------------------------------------------------------------------")
    
    plotComparsion(product_name)
  }else{
    print("Please Select Option From Above Menu [1,2,3]")
    print(" --------------------------------------------------------------------------------")
    print("           Enter Choice From Above Option                                        ")
    print(" --------------------------------------------------------------------------------")
    askFromUser()
  }
}

askFromUser()