# -*- coding: utf-8 -*-
"""
Created on Tue Aug 01 16:32:22 2017

@author: ag16225
"""

from collections import Counter
import numpy as np
#from wordcloud import WordCloud
import pandas as pd
import matplotlib.pyplot as plt
import operator
from nltk.stem.wordnet import WordNetLemmatizer

#For logging to console purposes
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

class dmmModel(object):
    
    """Object for Dirichlet Multinomial Mixture (DMM) Model
    
    Parameters
    ----------
    corpus : list (default=None)
        Documents having IDs of words.
    
    data : Pandas DataFrame (default=None)
        It consists of all the features for the given text.
        
    col : string (default=None)
        The name of the column to be analysed for DMM model.
    
    numberOfTopics : int, optional (default=5)
        The number of topics for which model has to be implemented.
        
    iterations : int, optional (default=10)
        The number of iterations for which model is run.
        
    alpha : float, optional (default=0.1)
        Dirichlet parameter for document-topic allocation. Usually between 0.0 and 1.0.
        
    beta : float, optional (default=0.01)
        Dirichlet parameter for word-topic allocation. Usually between 0.0 and 1.0.
        
    lambdaVal : float, optional (default=0.5)
        Proportion of words distribution to be considered.
        0.0 - 0.5 -> More weightage to distribution across the other topics as well
        0.5 - 1.0 -> More weightage to frequency in the current topic
        
    numberOfTopWords : int, optional (default=10)
        Top words to be considered for understanding the topic.
        
    id2Word : dict (default=None)
        Mapping from ID to word for the given corpus.
        
    freqOfWordsInCorpus : string, optional (default='word_proportion')
        Technique used for quantifying frequency of words. 
        {'word_count': Uses just the frequency, 'word_proportion': Finds proportion in the corpus}
        
    docsStringForEntities : list (default=None)
        List of documents having words in the str format, without entity removal.
        
    stemmers : list, optional (default=[])
        Words that have been stemmed as per user definition.
        
    random_state : int, optional (default=1234)
        Any random number for replication of results.
        
    Attributes
    ----------
    numDocuments : int
        Number of documents in the entire corpus.
            
    wordsInCorpus : int
        Number of distinct words in the vocabulary.
           
    freqInCorpus : Numpy Array
        Frequency of actual words.
        
    theta : Numpy 2D Array
        Document - topic mapping.
        
    docTopicCount : Numpy Array
        Number of documents in each topic.
            
    topicAssignments : Numpy 2D Array
        Topics assigned to each document.
            
    sumTopicWordCount : Numpy Array
        Sum of words in each topic.
            
    topicWordCount : Numpy 2D Array
        Frequency of a particular word in a given topic.
        
    relevanceScores : Numpy 2D Array
        Relevance of a particular word in a given topic
        
    topicToWordsFrequency : dict
        Stores the top words in the topic based on their phi-values
        
    topicToWordsRelevance : dict
        Stores the top words in the topic based on their relevance scores
            
    topicImportance : list
        Stores the order of topic importance based on the sum of relevance scores normalised by #documents in the topic
        
    coherenceMatrix : Numpy 2D Array
        Stores the coherence and entropy for each topic.
            
    totalCoherence : float
        Total coherence averaged out by the number of topics.
            
    total Entropy : float
        Total entropy averaged out by the number of topics.
        
    """
    
    def __init__(self, corpus=None, data=None, col=None, numberOfTopics=5, iterations=10, 
                 alpha=0.1, beta=0.01, lambdaVal=0.5, numberOfTopWords=10, 
                 id2Word=None, freqOfWordsInCorpus='word_proportion', docsStringForEntities=None, stemmers=[],
                 random_state=1234):
        
        self.corpus = corpus
        if self.corpus is None:
            raise ValueError('Specify the corpus to be modelled.')
            
        self.data = data
        if self.data is None:
            raise ValueError('Specify the dataframe for the corpus.')
        
        self.col = col
        if self.col is None:
            raise ValueError('Specify the column of the dataframe for running the model.')
        
        self.numberOfTopics = numberOfTopics
        self.iterations = iterations
        self.alpha = alpha
        self.beta = beta
        self.lambdaVal = lambdaVal
        self.numberOfTopWords = numberOfTopWords
        
        self.id2Word = id2Word
        if self.id2Word is None:
            raise ValueError('Specify the mapping from ID to Word.')
            
        self.freqOfWordsInCorpus = freqOfWordsInCorpus
        
        self.docsStringForEntities = docsStringForEntities
        if self.docsStringForEntities is None:
            raise ValueError('Specify the documents in word format for entity recognition.')
        
        self.listOfStemmers = stemmers
        
        self.random_state = random_state
        np.random.seed(random_state)
    
    def parametersDefinition(self):
        
        """Used to define the parameters for running DMM model.
        
        Attributes
        ----------
        numDocuments : int
            Number of documents in the entire corpus.
            
        wordsInCorpus : int
            Number of distinct words in the vocabulary.
           
        freqInCorpus : Numpy Array
            Frequency of actual words.
            
        theta : Numpy 2D Array
            Document - topic mapping.
            
        docTopicCount : Numpy Array
            Number of documents in each topic.
            
        topicAssignments : Numpy 2D Array
            Topics assigned to each document.
            
        sumTopicWordCount : Numpy Array
            Sum of words in each topic.
            
        topicWordCount : Numpy 2D Array
            Frequency of a particular word in a given topic.
        """

        self.numDocuments = len(self.corpus)
        self.wordsInCorpus = len(self.id2Word)
        
        self.freqInCorpus = np.zeros(self.wordsInCorpus)
        for doc in self.corpus:
            for word in doc:
                self.freqInCorpus[word] += 1
        if(self.freqOfWordsInCorpus=='word_proportion'):
            totalWords = float(sum(self.freqInCorpus))
            for word,count in enumerate(self.freqInCorpus):
                self.freqInCorpus[word] = float(count/totalWords)
    
        self.theta = np.zeros((self.numDocuments,self.numberOfTopics))
        self.docTopicCount = np.zeros(self.numberOfTopics)
        self.topicAssignments = np.zeros((self.numDocuments,self.numberOfTopics))
        self.sumTopicWordCount = np.zeros(self.numberOfTopics)
        self.topicWordCount = np.zeros((self.numberOfTopics,self.wordsInCorpus))
        
        #Initial multinomial sampling of topics
        for docNumber,doc in enumerate(self.corpus):
            #Topic is sampled from multinomial distirbution
            topicSampledArray = np.random.multinomial(1,(1/float(self.numberOfTopics))*np.ones(self.numberOfTopics))
            topicSampled = topicSampledArray.argmax()
            
            #Equal probability of a document belonging to one of the topics
            self.theta[docNumber,:] = (1/float(self.numberOfTopics))*np.ones(self.numberOfTopics)
            self.docTopicCount[topicSampled] += 1
            self.topicAssignments[docNumber,:] = topicSampledArray
            self.sumTopicWordCount[topicSampled] += len(doc)
            for word in doc:
                self.topicWordCount[topicSampled,word] +=1
        
    def inference(self):            
        
        """Used for inference of DMM for every iteration."""
        
        for docNumber,doc in enumerate(self.corpus):
            currentTopic = self.topicAssignments[docNumber,:].argmax()
            
            #Recalculate parameters without this doc-topic pairing
            self.docTopicCount[currentTopic] -= 1
            self.sumTopicWordCount[currentTopic] -= len(doc)
            for word in doc:
                self.topicWordCount[currentTopic,word] -= 1
                
            #Sample new topic
            topicProb = self.sampleTopicNo(docNumber,doc)
            topicProbDist = np.random.multinomial(1,topicProb/topicProb.sum())
            newTopic = topicProbDist.argmax()
            
            #Calculate parameters after new assignment
            self.theta[docNumber,:] = topicProb/topicProb.sum()
            self.topicAssignments[docNumber,:] = topicProbDist
            self.docTopicCount[newTopic] += 1
            self.sumTopicWordCount[newTopic] += len(doc)
            for word in doc:
                self.topicWordCount[newTopic,word] += 1
                
    def sampleTopicNo(self,docNumber,doc):
        
        """This method samples a topic number for the given document."""
        
        topicProb = np.zeros(self.numberOfTopics)
        overFlowCount = np.zeros(self.numberOfTopics)
        
        for topicNo in range(self.numberOfTopics):
            topicProb[topicNo] = (self.docTopicCount[topicNo] + self.alpha)/(self.numDocuments - 1 + self.numberOfTopics*self.alpha)
            valueOfRule2 = 1.0
            counter = Counter(doc)
            i = 0
            for word in doc:
                wordFreq = counter[word]
                for j in range(wordFreq):
                    tmp = (self.topicWordCount[topicNo,word] + self.beta + j)/(self.sumTopicWordCount[topicNo] + self.wordsInCorpus*self.beta + i)
                    i += 1
                    valueOfRule2 *= tmp
                    valueOfRule2, overFlowCount[topicNo] = self.isOverFlow(valueOfRule2, overFlowCount[topicNo])
                    
            topicProb[topicNo] *= valueOfRule2
            
        return self.reComputeProbs(topicProb, overFlowCount)
    
    def isOverFlow(self, valueOfRule2, overFlowCount):
    
        """This method checks for value overflow."""
        
        if valueOfRule2 < 1.0e-150:
            overFlowCount -= 1
            return valueOfRule2 * 1.0e150, overFlowCount
        else:
            return valueOfRule2, overFlowCount
        
    def reComputeProbs(self, topicProb, overFlowCount):
        
        """It is used to recompute probability distribution before deciding on the best one."""
        
        max = overFlowCount[0]
        for topicNo in range(self.numberOfTopics):
            if overFlowCount[topicNo] > max and topicProb[topicNo] > 0 :
                max = overFlowCount[topicNo]
                
        for topicNo in range(self.numberOfTopics):
            if topicProb[topicNo] > 0:
                topicProb[topicNo] = topicProb[topicNo] * np.power(1.0e150, overFlowCount[topicNo] - max)
                
        return topicProb
    
    def worddist(self):
        
        """It gives the topic to word distribution."""
        
        return (self.topicWordCount + self.beta)/(self.sumTopicWordCount[:,np.newaxis] + self.wordsInCorpus*self.beta)
    
    def docdist(self):
        
        """It gives the document distribution across all topics."""
        
        return (self.docTopicCount + self.alpha)/(self.docTopicCount + self.numberOfTopics*self.alpha)
    
    def perplexity(self):
        
        """It is used to calculate the perplexity measure for comparison of models.
        
        Returns
        -------
        Perplexity of the given topic assignments (float)
        """
        
        phi = self.worddist()
        log_per = 0
        N = 0
        topics = (self.sumTopicWordCount + self.alpha)/(self.sumTopicWordCount.sum() + self.numberOfTopics*self.alpha)
        for docNumber, doc in enumerate(self.corpus):
            log_multinomial = np.zeros(self.numberOfTopics)
            for word in doc:
                log_multinomial += np.log(phi[:,word])
            log_per += (self.theta[docNumber,:] * (np.log(topics) + log_multinomial)).sum()
            N += len(doc)
        return np.exp(-log_per/N)
    
    def relevance(self):
        
        """Used to find the relevance scores to take into account the distribution of words across the entire corpus.
        
        Attributes
        ----------
        relevanceScores : Numpy 2D Array
            Relevance of a particular word in a given topic
        """
        
        self.relevanceScores = np.zeros((self.numberOfTopics,self.wordsInCorpus))
        for topicNo in range(self.numberOfTopics):
            for word in range(self.wordsInCorpus):
                if(self.topicWordCount[topicNo,word]==0):
                    rel = 0
                else:
                    rel = (self.lambdaVal)*np.log(self.topicWordCount[topicNo,word]) + (1-self.lambdaVal)*np.log(self.topicWordCount[topicNo,word]/self.freqInCorpus[word])
                self.relevanceScores[topicNo,word] = rel
    
    def topic_to_words(self):
        
        """It assigns the topics to top words based on the word frequency only.
        
        Attributes
        ----------
        topicToWordsFrequency : dict
            Stores the top words in the topic based on their phi-values
        """
        
        self.topicToWordsFrequency = {}
        phi = self.worddist()
        for topicNo in xrange(self.numberOfTopics):
            self.topicToWordsFrequency[topicNo] = []
            for word in np.argsort(-phi[topicNo])[:self.numberOfTopWords]:
                self.topicToWordsFrequency[topicNo].append((self.id2Word[word], phi[topicNo,word]))
    
    def topic_to_words_rel(self):
        
        """It assigns the topics to top words based on relevance metric as explained in pyLDAvis documentation.
        
        Attributes
        ----------
        topicToWordsRelevance : dict
            Stores the top words in the topic based on their relevance scores
            
        topicImportance : list
            Stores the order of topic importance based on the sum of relevance scores normalised by #documents in the topic
        """
        
        tempTopicImp = {}
        self.topicToWordsRelevance = {}
        for topicNo in xrange(self.numberOfTopics):
            self.topicToWordsRelevance[topicNo] = []
            topicImp = 0.0
            for word in np.flip((np.argsort(self.relevanceScores[topicNo])[-1*self.numberOfTopWords:]),axis=0):
                self.topicToWordsRelevance[topicNo].append((self.id2Word[word],self.relevanceScores[topicNo,word]))
                topicImp += self.relevanceScores[topicNo,word]
            tempTopicImp[topicNo] = topicImp*np.sqrt(self.docTopicCount[topicNo])
        self.topicImportance = sorted(tempTopicImp.items(), key = lambda x:x[1],reverse=True)
    
    def topic_to_data(self):
        
        """Appends the 'topic' column to dataframe of corpus."""

        self.data['topic'] = -1
        for index,row in self.data.iterrows():
            topicOfDoc = self.topicAssignments[index,:].argmax()
            self.data.set_value(index,'topic',topicOfDoc)
    
    def documentsPercentage(self, words, top):
        
        """Calculates the %age of documents for a given word.
        
        Returns
        -------
        Proportion of documents in the given topic having the particular entity
        """
        
        dicForEntities = {}
        for word in words:
            c = 0.0
            t = 0.0
            for index,row in self.data.iterrows():
                if(row['topic']==top):
                    t = t + 1.0
                    if word in self.docsStringForEntities[index]:
                        c = c + 1.0
            dicForEntities[word] = (c/t)
        return sorted(dicForEntities.items(),key=operator.itemgetter(1),reverse=True)
    
    def display(self, noDisplayWordcloud, saveWordcloud, saveCSV, pathOfFile, entitiesOfDocs, noOfEntities, wordsToHighlight, sentencesToView):
        
        """Display and save wordclouds, save data CSV, print entities and sample documents per topic.
        
        Parameters
        ----------
        noDisplayWordcloud : int, optional (default=5)
            Number of wordclouds or topic-word distribution to be printed on screen.
            
        saveWordcloud : boolean, optional (default=False)
            Whether the wordclouds should be saved or not.
            
        saveCSV : boolean, optional (default=False)
            Whether the CSV of dataframe should be saved or not.
            
        pathOfFile : string (default=None)
            The path of the folder where the files must be saved.
            
        entitiesOfDocs : list (default=None)
            The entities of each document which have been separated out.
            
        noOfEntities : int, optional (default=5)
            Number of entities to be printed on screen.
            
        wordsToHighlight : int, optional (default=numberOfTopWords)
            Number of top words to be highlighted with majority color. Put '-1'  if no word has to be highlighted.
            
        sentencesToView : int, optional (default=5)
            The top documents to be printed for each topic.
        """
        
        entityTopicWise = {} #Topic-to-entity mapping
        for index, row in self.data.iterrows():
            topicOfThisDoc = row['topic']
            entityOfThisDoc = entitiesOfDocs[index]
            if topicOfThisDoc not in entityTopicWise:
                entityTopicWise[topicOfThisDoc] = entityOfThisDoc
            else:
                for entity in entityOfThisDoc:
                    entityTopicWise[topicOfThisDoc].append(entity)

        i = 1
        if(wordsToHighlight!=-1):
            print '\nTop %d words are marked \033[94m\033[1mBlue\033[0m, and the next %d words are marked \033[91m\033[1mRed\033[0m.'%(wordsToHighlight,self.numberOfTopWords-wordsToHighlight)
        sentenceDf = pd.DataFrame(columns=['Topic','Sentences'])
        indexSen = 0
        for top,score in self.topicImportance[:noDisplayWordcloud]:
            if(self.docTopicCount[top]==0): #Skip if this document has zero documents
                continue
            docText = "#Docs (topic %d) = %d"%(top, self.docTopicCount[top])
            wcDic = {}
            wcDic10 = {}
            wcDic20 = {}
            c = 0
            for x in self.topicToWordsRelevance[top]:
                wcDic[x[0].encode('ascii','ignore')] = float(x[1])
                if(c<wordsToHighlight):
                    wcDic10[x[0].encode('ascii','ignore')] = float(x[1])
                else:
                    wcDic20[x[0].encode('ascii','ignore')] = float(x[1])
                c+=1
            wordcloud = WordCloud(min_font_size=10,max_font_size=60,background_color="white",width=400,height=200).generate_from_frequencies(wcDic)
            plt.figure(figsize=(8,4))
            plt.text(130,-10,docText)
            if(noOfEntities>0):
                h = 50
                plt.text(420,h,"Distribution of Entities:")
                h += 20
                entitiesTop = []
                for name,count in sorted(dict(Counter(entityTopicWise[top])).items(),key=operator.itemgetter(1),reverse=True)[:noOfEntities]:
                    entitiesTop.append(name)
                entitiesOrder = self.documentsPercentage(entitiesTop, top)
                for entity, freq in entitiesOrder:
                    plt.text(420,h,entity)
                    entityText = "%.0f%% (%d docs)"%(round(freq*100.0),int(freq*self.docTopicCount[top]))
                    plt.text(500,h,entityText)
                    h = h + 20
            plt.imshow(wordcloud, interpolation="bilinear")
            plt.axis("off")
            if(saveWordcloud==True):
                plt.savefig('%s/WC_%d'%(pathOfFile,i))
            plt.show()
            sent = self.printDocuments(wcDic10, wcDic20, top, sentencesToView, wordsToHighlight)
            for s in sent:
                sentenceDf.loc[indexSen] = [top,s]
                indexSen+=1
            i+=1
        
        if(saveCSV==True):
            self.data.to_csv('%s/Documents with Topics.csv'%(pathOfFile),index=False)
            sentenceDf.to_csv('%s/Topic-Sentence Mapping.csv'%(pathOfFile),index=False)
            
    
    def isStemmed(self,word):
        
        """Checks if the given word belongs to the list of stemmers.
        
        Returns
        -------
        The word or the stemmed word, which can be checked in the dictionary of top words.
        """
        
        if word in self.listOfStemmers:
            return self.listOfStemmers[word]
        else:
            return word
        
    def printDocuments(self, dicOfWords10, dicOfWords20, top, sentencesToView, wordsToHighlight):
        
        """Prints the most relevant documents for a given topic with properly highlighted words."""
        
        relevantSentences = {}
        for index,row in self.data.iterrows():
            if(row['topic']==top):
                c = 0.0
                document = self.docsStringForEntities[index]
                for word in document:
                    if word in dicOfWords10:
                        c += dicOfWords10[word]
                    if word in dicOfWords20:
                        c += dicOfWords20[word]
                relevantSentences[index] = c

        count = 1
        s = []
        for index,rel in sorted(relevantSentences.items(),key=operator.itemgetter(1),reverse=True)[:sentencesToView]:
            sentence = self.data.iloc[index][self.col]
            word = ''
            final_indices = []
            final_indices2 = []
            indices = []
            print "%d."%(count),
            for index,letter in enumerate(sentence):
                letter = letter.lower()
                if((letter>='a' and letter<='z') or (letter>='A' and letter<='Z')):
                    if word in dicOfWords10 or WordNetLemmatizer().lemmatize(word) in dicOfWords10 or self.isStemmed(word) in dicOfWords10:
                        for i in indices:
                            if i not in final_indices:
                                final_indices.append(i)
                    if word in dicOfWords20 or WordNetLemmatizer().lemmatize(word) in dicOfWords20 or self.isStemmed(word) in dicOfWords20:
                        for i in indices:
                            if i not in final_indices2:
                                final_indices2.append(i)
                    word+=letter
                    indices.append(index)
                else:
                    if word in dicOfWords10 or WordNetLemmatizer().lemmatize(word) in dicOfWords10 or self.isStemmed(word) in dicOfWords10:
                        for i in indices:
                            if i not in final_indices:
                                final_indices.append(i)
                    if word in dicOfWords20 or WordNetLemmatizer().lemmatize(word) in dicOfWords20 or self.isStemmed(word) in dicOfWords20:
                        for i in indices:
                            if i not in final_indices2:
                                final_indices2.append(i)
                        word = ''
                        indices = []
                    else:
                        word = ''
                        indices = []
            sen = ''
            for index,letter in enumerate(sentence):
                if(wordsToHighlight==-1):
                    sen+=letter
                else:
                    if index in final_indices:
                        sen+= '\033[1m'+'\033[94m'+letter+'\033[0m'
                    elif index in final_indices2:
                        sen+= '\033[1m'+'\033[91m'+letter+'\033[0m'
                    else:
                        sen+= letter
            
            print sen
            s.append(sentence)
            count+=1
        return s
        
    def coherence(self):
        
        """Calculate average coherence across all topics.
        
        Attributes
        ----------
        coherenceMatrix : Numpy 2D Array
            Stores the coherence and entropy for each topic.
            
        totalCoherence : float
            Total coherence averaged out by the number of topics.
            
        total Entropy : float
            Total entropy averaged out by the number of topics.
        """
        
        self.coherenceMatrix = np.zeros((self.numberOfTopics,2))
        self.totalCoherence = 0.0
        self.totalEntropy = 0.0
        total_coherence = 0.0
        total_entropy = 0.0
        for top in range(self.numberOfTopics):
            topic_coherence = 0.0
            topic_entropy = 0.0
            documentsInThisTopic = []
            for index, row in self.data.iterrows():
                if(row['topic']==top):
                    documentsInThisTopic.append(self.corpus[index])
            wordsInThisTopic = {}
            for index,wordCount in enumerate(self.topicWordCount[top,:]):
                if(wordCount>0.0):
                    wordsInThisTopic[index] = 0.0
            wordsInThisTopicList = []
            for word, count in wordsInThisTopic.iteritems():
                wordsInThisTopicList.append(word)
                currentCount = 0.0
                for doc in documentsInThisTopic:
                    if word in doc:
                        currentCount+=1.0
                wordsInThisTopic[word] = currentCount
            masterDoubleDic = {}    
            for doc in documentsInThisTopic:
                i = 0
                docUnique = dict.fromkeys(doc).keys()
                while(i<len(docUnique)):
                    j = i + 1
                    while(j<len(docUnique)):
                        if(min(docUnique[i],docUnique[j]),max(docUnique[i],docUnique[j])) in masterDoubleDic:
                            masterDoubleDic[(min(docUnique[i],docUnique[j]),max(docUnique[i],docUnique[j]))]+=1.0
                        else:
                            masterDoubleDic[(min(docUnique[i],docUnique[j]),max(docUnique[i],docUnique[j]))] = 1.0
                        j+=1
                    i+=1
            for wordPair, count in masterDoubleDic.iteritems():
                topic_coherence = topic_coherence + np.log((count+1.0)/wordsInThisTopic[wordPair[0]]) + np.log((count+1.0)/wordsInThisTopic[wordPair[1]])
                topic_entropy = topic_entropy + count/wordsInThisTopic[wordPair[0]] + count/wordsInThisTopic[wordPair[1]]                
            self.coherenceMatrix[top][0] = topic_coherence
            self.coherenceMatrix[top][1] = topic_entropy
            total_coherence+=topic_coherence
            total_entropy+=topic_entropy
        self.totalCoherence = total_coherence/self.numberOfTopics
        self.totalEntropy = total_entropy/self.numberOfTopics
    
    def learning(self, verbose=True):
        
        """Caller function for running DMM."""
        
        if(verbose==False):
            logger.disabled = True #Toggle for logging to console or not
        else:
            logger.disabled = False
            
        self.parametersDefinition()
        logger.info('Parameters have been defined.')
        
        logger.info('DMM Model is running now.')
        pre_perp = self.perplexity()
        logger.info("Initial perplexity: p = %f" %pre_perp)
        for i in range(self.iterations):
            self.inference()
            perp = self.perplexity()
            logger.info("Iteration %d:\t p = %f" %(i+1, perp))
        
        self.relevance()
        logger.info('Relevance scores have been calculated.')
        
        self.topic_to_data()
        logger.info('Topics colummn has been added to the dataframe.')
        
        self.topic_to_words()
        self.topic_to_words_rel()
        logger.info('Top words have been identified for each topic.')
        
        self.coherence()
        logger.info('Coherence for the corpus has been computed.')

#End of DMM Class