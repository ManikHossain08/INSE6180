mapRatingWithValue = function(value){
  rating = 0
  if(between(value,0,0.124)){
    rating = 1 
  }
  if(between(value,0.125,0.24)){
    rating = 1.5
  }
  if(between(value,0.25,0.374)){
    rating = 2
  }
  if(between(value,0.375,0.5)){
    rating = 2.5
  }
  if(between(value,0.51,0.55)){
    rating = 3
  }
  if(between(value,0.56,0.60)){
    rating = 3.5
  }
  if(between(value,0.61,0.733)){
    rating = 4
  }
  if(between(value,0.734,0.866)){
    rating = 4.5
  }
  if(between(value,0.867,1)){
    rating = 5
  }
  return(rating)
}

predictRating = function(negativeReviews,positiveReviews,testData_length){
  value = 0

  if(positiveReviews > negativeReviews){
    value = positiveReviews/testData_length
  }else{
    value = 1 - (negativeReviews/testData_length)
  }
  print("Mapping With Rating Table To Get Rating Between 1-5....")
  rating = mapRatingWithValue(value)
  return(rating)
}

