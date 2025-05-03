#!/usr/bin/env python
# coding: utf-8

# # Importing Relevant Libraries.

# In[1]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns


# ## Load The Data.

# In[2]:


train_data = pd.read_excel('Data_Train.xlsx')


# In[3]:


train_data.head(30)


# In[4]:


train_data.tail(30)


# ## Data Cleaning.

# In[5]:


train_data.info()


# In[6]:


train_data.isnull().sum()


# In[7]:


train_data[train_data['Total_Stops'].isnull()]


# In[8]:


train_data.dropna(inplace = True)


# In[9]:


train_data.isnull().sum()


#  ## Data Preparation.

# In[10]:


data = train_data.copy()


# In[11]:


data.columns


# ##### NB: Data_of_Journey, Arrival_Time & Dep_Time should not be a string but a Timestamp nature.

# In[12]:


def change_into_Datetime(col):
    data[col] = pd.to_datetime(data[col])


# In[13]:


import warnings
from warnings import filterwarnings
filterwarnings("ignore")


# In[14]:


for feature in ['Dep_Time', 'Arrival_Time', 'Date_of_Journey']:
    change_into_Datetime(feature)


# In[15]:


data.dtypes


# In[16]:


data['Journey_day'] = data['Date_of_Journey'].dt.day


# In[17]:


data['Journey_month'] = data['Date_of_Journey'].dt.month


# In[18]:


data['Journey_year'] = data['Date_of_Journey'].dt.year


# In[19]:


data.head(10)


# In[ ]:





# In[20]:


def extract_hour_min(df, col):
    df[col+"_hour"] = df[col].dt.hour
    df[col+"_minute"] = df[col].dt.minute
    return df.head(3)


# In[21]:


extract_hour_min(data, "Dep_Time")


# In[22]:


extract_hour_min(data, "Arrival_Time")


# In[ ]:





# In[23]:


cols_to_drop = ['Arrival_Time' , 'Dep_Time']

data.drop(cols_to_drop, axis = 1, inplace = True)


# In[24]:


data.head(3)


# In[ ]:





# ## Data Analysis.

# #### First Analysis:
# #### When will most of the flights take off?

# In[25]:


data.columns


# In[26]:


# Using The Dep_Time_Hour Feature:
def flight_dep_time(x):

    if (x>4) and (x<=8):
        return "Early Morning"

    elif (x>8) and (x<=12):
        return "Morning"

    elif (x>12) and (x<=16):
        return "Afternoon"

    elif (x>16) and (x<=20):
        return "Evening"

    elif (x>20) and (x<=24):
        return "Late Evening"

    else:
        return "Midnight"


# In[27]:


data['Dep_Time_hour'].apply(flight_dep_time).value_counts().plot(kind = 'bar', color = 'g')


# In[ ]:





# In[28]:


get_ipython().system('pip install plotly')
get_ipython().system('pip install chart_studio')


# In[29]:


get_ipython().system('pip install cufflinks')


# In[30]:


import plotly
import cufflinks as cf
from cufflinks.offline import go_offline
from plotly.offline import plot , iplot , init_notebook_mode , download_plotlyjs
init_notebook_mode(connected = True)
cf.go_offline()


# In[31]:


data['Dep_Time_hour'].apply(flight_dep_time).value_counts().iplot(kind = "bar")


# In[ ]:





# ## Preprocessing On The Duration Feature.

# In[32]:


data.head(2)


# In[33]:


### Converting Egs. 19h -> 19h 0min and 50min -> 0h 50min
def preprocess_duration(x):
    if 'h' not in x:
        x = '0h' + ' ' + x
    elif 'm' not in x:
        x = x + ' ' + '0m'

    return x


# In[34]:


data['Duration'] = data['Duration'].apply(preprocess_duration)


# In[35]:


data['Duration']


# In[ ]:





# In[36]:


data['Duration'][0]


# In[37]:


'2h 50m'.split(' ')


# In[38]:


'2h 50m'.split(' ')[0]


# In[39]:


'2h 50m'.split(' ')[0][0:-1]


# In[40]:


int('2h 50m'.split(' ')[0][0:-1])


# In[41]:


int('2h 50m'.split(' ')[1][0:-1])


# In[42]:


data['Duration_hours'] = data['Duration'].apply(lambda x : int(x.split(' ')[0][0:-1]))


# In[43]:


data['Duration_mins'] = data['Duration'].apply(lambda x : int(x.split(' ')[1][0:-1]))


# In[44]:


data.head(2)


# In[ ]:





# ### Analyzing  Whether Duration Has An Impact On Price Or Not?

# In[45]:


##### Replacing 'h' -> '*60' , ' ' -> '+' and  'm' -> '*1' and using the 'eval()' function to perform the arithmetric operation over the string  column 
##### 'Duration'.
data['Duration_total_hour_mins'] = data['Duration'].str.replace('h' , '*60').str.replace(' ' , '+').str.replace('m' , '*1').apply(eval)


# In[46]:


data['Duration_total_hour_mins']


# In[ ]:





# In[47]:


data.columns


# In[48]:


sns.scatterplot(x = 'Duration_total_hour_mins', y = 'Price' , data = data)


# In[ ]:





# In[49]:


##### Creating A Scatterplot for flights which has 1 stop, 2 stops and the rest.


# In[50]:


sns.scatterplot(x = 'Duration_total_hour_mins', y = 'Price' , hue = 'Total_Stops', data = data)


# In[ ]:





# In[51]:


##### Using A Regression Plot.
sns.lmplot(x = 'Duration_total_hour_mins', y = 'Price' , data = data)


# In[ ]:





# ## Bivariate Analysis.

# In[52]:


#### 1. On which route Jet Airways is extremely used??


# In[53]:


##### Extracting data with 'Jet Airways' as column.


# In[54]:


data['Airline'] == 'Jet Airways'


# In[55]:


data[data['Airline'] == 'Jet Airways'].groupby('Route').size().sort_values(ascending = False)


# In[ ]:





# In[56]:


#### 2. Airline Vs Price Analysis..


# In[57]:


data.columns


# In[58]:


sns.boxplot(y = 'Price', x = 'Airline', hue = 'Airline', data = data.sort_values('Price', ascending = False))
plt.xticks(rotation = 'vertical')
plt.show()


# In[ ]:





# ## Feature Engineering.

# #### Feature Encoding.

# In[59]:


##### Applying one-hot on the data..


# In[60]:


data.head(2)


# In[ ]:





# In[61]:


cat_col = [col for col in data.columns if data[col].dtype == "object"]


# In[62]:


num_col = [col for col in data.columns if data[col].dtype != "object"]


# In[63]:


cat_col


# In[ ]:





# In[64]:


data['Source'].unique()


# In[65]:


data['Source'].apply(lambda x : 1 if x == 'Banglore' else 0)


# In[ ]:





# In[66]:


for sub_category in data['Source'].unique():
    data['Source_'+sub_category] = data['Source'].apply(lambda x : 1 if x == sub_category else 0)


# In[67]:


data.head(3)


# In[ ]:





# In[68]:


##### Applying Target Guided Encoding.


# In[69]:


cat_col


# In[70]:


data['Airline'].unique()


# In[71]:


data['Airline'].nunique()


# In[ ]:





# In[72]:


data.groupby(['Airline'])['Price'].mean().sort_values()


# In[73]:


airlines = data.groupby(['Airline'])['Price'].mean().sort_values().index


# In[74]:


airlines


# In[ ]:





# In[75]:


dict_airlines = {key:index for index , key in enumerate(airlines , 0)}


# In[76]:


dict_airlines


# In[77]:


data['Airline'] = data['Airline'].map(dict_airlines)


# In[78]:


data['Airline']


# In[ ]:





# In[79]:


data.head(3)


# In[80]:


data['Destination'].unique()


# In[81]:


data['Destination'].replace('New Delhi' , 'Delhi' , inplace = True)


# In[82]:


data['Destination'].unique()


# In[ ]:





# In[83]:


dest = data.groupby(['Destination'])['Price'].mean().sort_values().index


# In[84]:


dest


# In[85]:


dict_dest = {key:index for index , key in enumerate(dest , 0)}


# In[86]:


dict_dest


# In[87]:


data['Destination'] = data['Destination'].map(dict_dest)


# In[88]:


data['Destination']


# In[89]:


data.head(3)


# In[ ]:





# In[ ]:





# In[90]:


#### Applying Manual Encoding.


# In[91]:


data.head(3)


# In[92]:


data['Total_Stops']


# In[93]:


data['Total_Stops'].unique()


# In[94]:


###### We can note that the Total_Stops Column is an ordinal data, that is, it has an order. Therefore we use Label Encoding.


# In[95]:


stop = {'non-stop':0, '2 stops':2, '1 stop':1, '3 stops':3, '4 stops':4}


# In[96]:


data['Total_Stops'] = data['Total_Stops'].map(stop)


# In[97]:


data['Total_Stops']


# In[ ]:





# In[98]:


##### Removing Unnecessary  Features.


# In[99]:


data.head(2)


# In[100]:


data.columns


# In[101]:


data['Additional_Info'].value_counts()


# In[102]:


data['Additional_Info'].value_counts()/len(data)*100


# In[103]:


#### 78% of the datasets under the Additional_Info has 'No info', so we drop it.


# In[ ]:





# In[104]:


data.head(2)


# In[105]:


data['Journey_year'].unique()


# In[106]:


##### We can also see that 'Route' Column is closely related to the 'Total_Stops' Column , so we drop that too.
##### Also, for those we extracted data from like 'Date_of_Journey, Duration_total_hour_mins, Duration, etc.'


# In[107]:


data.columns


# In[108]:


data.drop(columns = ['Date_of_Journey', 'Additional_Info', 'Duration_total_hour_mins', 'Journey_year'],
         axis = 1, inplace = True)


# In[109]:


data.head(2)


# In[110]:


data.drop(columns = ['Route'], axis = 1, inplace = True)


# In[111]:


data.drop(columns = ['Duration'], axis = 1, inplace = True)


# In[112]:


data.head(2)


# In[113]:


data.drop(columns = ['Source'], axis = 1, inplace = True)


# In[114]:


data.head(2)


# In[ ]:





# ## Outlier Detection.

# In[115]:


def plot(df, col):
    fig , (ax1, ax2, ax3) = plt.subplots(3, 1)

    sns.distplot(df[col], ax = ax1)
    sns.boxplot(x = df[col], ax = ax2)
    sns.distplot(df[col], ax = ax3 , kde = False)


# In[116]:


plot(data, 'Price')


# In[ ]:





# In[117]:


q1 = data['Price'].quantile(0.25)
q3 = data['Price'].quantile(0.75)

iqr = q3 - q1

maximum = q3 + 1.5*iqr
minimum = q1 - 1.5*iqr


# In[118]:


print(maximum)


# In[119]:


print(minimum)


# In[120]:


print([price for price in data['Price'] if price > maximum or price <  minimum])


# In[121]:


len([price for price in data['Price'] if price > maximum or price <  minimum])


# In[ ]:





# In[122]:


##### Replacing the outliers with the median value.
##### Treshold value = 35,000


# In[123]:


data['Price'] = np.where(data['Price'] >= 35000 , data['Price'].median(), data['Price'])


# In[124]:


plot(data, 'Price')


# In[ ]:





# ## Selecting Best Features Using Feature Selection.

# In[125]:


##### The 'Price' feature is a Dependent Feature or Target Variable for all other features.
##### The rest of all other features are Independent Features.


# In[126]:


X = data.drop(['Price'], axis = 1)


# In[127]:


y = data['Price']


# In[128]:


from sklearn.feature_selection import mutual_info_regression


# In[129]:


imp = mutual_info_regression(X , y)


# In[130]:


imp


# In[131]:


imp_df = pd.DataFrame(imp , index = X.columns)


# In[132]:


imp_df.columns = ['Importance']


# In[133]:


imp_df


# In[134]:


imp_df.sort_values(by = 'Importance' , ascending = False)


# In[ ]:





# ## Applying Machine Learning Algorithm On The Data.

# In[135]:


##### 1. Building Ml Model.


# In[136]:


from sklearn.model_selection import train_test_split


# In[137]:


X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.25, random_state=42)


# In[138]:


###### Since our data is a regression used case, we use the 'RandomForestRegressor()'.


# In[139]:


from sklearn.ensemble import RandomForestRegressor


# In[140]:


ml_model = RandomForestRegressor()


# In[141]:


ml_model.fit(X_train , y_train)


# In[ ]:





# In[142]:


y_pred = ml_model.predict(X_test)


# In[143]:


y_pred


# In[ ]:





# In[144]:


###### Evaluating How Well The Model Is Performing.


# In[145]:


from sklearn import metrics


# In[146]:


metrics.r2_score(y_test , y_pred)


# In[ ]:





# In[147]:


##### 2. Model Dumping / Saving The Model.


# In[148]:


###!pip install pickle


# In[149]:


import pickle


# In[150]:


file = open('rf_random.pkl' , 'wb')


# In[151]:


pickle.dump(ml_model , file)


# In[ ]:





# In[152]:


#### Defining your model.
model = open('rf_random.pkl' , 'rb') ### 'rb' stands for read mode. That is read the model.


# In[153]:


### Loading The Model.
forest = pickle.load(model)


# In[154]:


y_pred2 = forest.predict(X_test)


# In[155]:


metrics.r2_score(y_test , y_pred2)


# In[ ]:





# In[156]:


#### How To Define Your Evaluation Metric.
#### Evaluation Metric -> Evaluating how well the Machine Learning Algorithm is performing.


# In[157]:


#### MAPE -> Mean Absolute Percentage Error.
#### Error = Actual - Predicted
def mape(y_true , y_pred):
    y_true , y_pred = np.array(y_true) , np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100


# In[158]:


mape(y_test , y_pred)


# In[ ]:





# In[159]:


#### How To Automate The Machine Learning Pipeline.


# In[160]:


from sklearn import metrics


# In[161]:


def predict(ml_model):
    model = ml_model.fit(X_train , y_train)
    print('Training score : {}'.format(model.score(X_train , y_train)))
    y_prediction = model.predict(X_test)
    print('predictions are : {}'.format(y_prediction))
    print('\n')
    r2_score = metrics.r2_score(y_test , y_prediction)
    print('r2 score : {}'.format(r2_score))
    print('MAE : {}'.format(metrics.mean_absolute_error(y_test , y_prediction)))
    print('MSE : {}'.format(metrics.mean_squared_error(y_test , y_prediction)))
    print('RMSE : {}'.format(np.sqrt(metrics.mean_squared_error(y_test , y_prediction))))
    print('MAPE : {}'.format(mape(y_test , y_prediction)))
    
    sns.distplot(y_test - y_prediction)


# In[162]:


predict(RandomForestRegressor())


# In[ ]:





# In[163]:


from sklearn.tree import DecisionTreeRegressor


# In[164]:


predict(DecisionTreeRegressor())


# In[ ]:





# In[ ]:





# In[165]:


##### Hypertuning Machine Learning Model.


# In[166]:


from sklearn.model_selection import RandomizedSearchCV


# In[167]:


reg_rf = RandomForestRegressor()


# In[168]:


np.linspace(start =100 , stop = 1200 , num = 6)


# In[ ]:





# In[181]:


n_estimators = [int(x) for x in np.linspace(start = 100 , stop = 1200 , num = 6)]

max_features = ["auto" , "sqrt"]

max_depth = [int(x) for x in np.linspace(start = 5 , stop = 30 , num = 4)]
min_samples_split = [5, 10, 15, 100]


# In[182]:


random_grid = {
    'n_estimators' : n_estimators ,
    'max_features' : max_features ,
    'max_depth' : max_depth ,
    'min_samples_split' : min_samples_split
}


# In[183]:


random_grid


# In[ ]:





# In[184]:


rf_random = RandomizedSearchCV(estimator=reg_rf , param_distributions=random_grid , cv=3 ,n_jobs=-1 ,verbose=2)


# In[185]:


rf_random.fit(X_train , y_train)


# In[186]:


rf_random.best_params_


# In[187]:


rf_random.best_estimator_


# In[188]:


rf_random.best_score_


# In[ ]:




