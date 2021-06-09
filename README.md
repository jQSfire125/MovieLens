# MovieLens

This project is part of the "PH125.9x Data Science: Capstone" course on HarvardX. 
The goal of this project is to create a recommendation system using the [MovieLens dataset](https://grouplens.org/datasets/movielens/latest/). For this project, to make computation simpler, we will be using the [10M version of the MovieLens dataset](http://grouplens.org/datasets/movielens/10m/). This dataset contains 10 million ratings of more than 10,000 movies given by about 70,000 users.

We will build an algorithm that will predict the rating a particular user would give movie. To make the prediction, our model uses the movie, the user, and the year the movie was released. 

To score how accurate the model predicts a given rating, we use root mean square error (RMSE) as a metric. Our final model has a **RMSE (_Root Mean Square Error_) of 0.8645.**