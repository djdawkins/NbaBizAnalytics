



function(pred_data){
  # Load saved random forest model
  random_forest_model <- get(load("nbaRandomForrestModel.RData"))
  
  # Predict on current college players
  y_pred = predict(random_forest_model, newdata = pred_data)
  
  message("running code...")
  return(y_pred)
}


