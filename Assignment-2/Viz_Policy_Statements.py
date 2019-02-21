#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import sys
import os
import scattertext as st
import spacy


# In[2]:


os.getcwd()
os.chdir(os.getcwd()+'\\Data')


# In[21]:


#st.SampleCorpora.ConventionData2012.get_data()


# In[6]:


df = pd.read_csv('full_text.csv', encoding = "ISO-8859-1")


# In[10]:


nlp = spacy.load('C:/Users/austi/Anaconda/Lib/site-packages/en_core_web_sm/en_core_web_sm-2.0.0')


# In[18]:


corpus = st.CorpusFromPandas(df, text_col='text', category_col='date', nlp=nlp).build()


# In[19]:


html = st.produce_scattertext_explorer(corpus, category='2008-01-22', category_name='Policy Date')


# In[20]:


open("FedPloicy.html", 'wb').write(html.encode('utf-8'))

