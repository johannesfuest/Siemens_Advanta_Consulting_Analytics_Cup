# Siemens_Advanta_Consulting_Analytics_Cup

This project was a team submission to a 48 hour competition held jointly by the Technical University of Munich's Chair for Decision Sciences and Systems (DSS) and Siemens Advanta Consulting. 

Teams were given medium-sized real-world datasets on one of Siemens' past clients (anonymized). This data included information on past offers which the client had sent out to their customers and whether or not these had been accepted. The data also included plenty of supplementary information on offer details, customer properties and geography. 

The challenge was to build and train a binary classifier that would predict whether offers in a test data set would be accepted or not. The sole evaluation metric used for grading of the models was the balanced accruacy achieved on a test data set. The main obstacles to overcome were poor data quality, which resulted from the real-world nature of the data, as well as deliberate impairment by the competition hosts. Our team's solution used a random forest with optimization based on a grid search for ideal hyperparameters. We also gained a significant advantage through intense data-wrangling (including spotting a UN industry classifier (ISIN number) in the data, as well as computing several additional useful columns that ended up contributing significantly to gini impurity reduction in our model, such as margin and time of offer creation. We ended up receiving the top grade. 

Unfortunately the data used is confidential and I am only allowed to share the R script, as well as a presentation I used to present our team's solution during the final interview round for the Master of Business Analytics program at the Massachusetts Institute of Technology. 
