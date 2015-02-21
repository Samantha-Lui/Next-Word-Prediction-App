# Next-Word-Prediction-App

These are the R scripts used in creating this [Next Word Prediction App](https://samantheluidatascience.shinyapps.io/Capstone/) which was the capstone project (Oct 27, 2014-Dec 13, 2014) for a program in Data Science Specialization. Please visit [this page](http://rpubs.com/Samantha_Lui/57441) for the details about this project. 

The implementation was divided among the scripts as following:
 
1. *preprocess.R*: Extracts the training data, cleans the corpus, and tokenizes the corpus to prepare the data for analysis.

2. *Helpers.R*: Contains functions for the model building and analysis; this is sourced in *NGramModel.R*.

3. *NGramModel.R*: Builds 1,2,3, and 4-gram models with modified Kneser-Ney interpolation based on the training data.

4. *NGramTree.R*: Converts the results from the implementation of *NGramModel.R* into tree-like structures to reduce the size of the output data.

5. *predict.R*: Returns the predictions for the next word using the specified input from the user. When there is no match in a higher-order N-gram model, *predict()* backs off to the next lower-order model until a match is found.

6. *global.R*: Provides data that are to be shared across the servers.

7. *UI.R*: Provides a web-based interface for the interactions between the user and the server.

8. *server.R*: Processes the inputs obtained from the user interface and coordinates the operations in prediction for the result.  
