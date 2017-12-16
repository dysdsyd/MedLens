# -*- coding: utf-8 -*-
"""
Created on Tue Aug 01 19:48:08 2017

@author: ag16225
"""
#Ignoring the warnings
import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)

#For in-house lemmatization
from nltk.stem.wordnet import WordNetLemmatizer

#For accessing DataFrame objects
import pandas as pd

#For logging to console purposes
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

class textPreprocessing(object):
    
    """Preprocesses the given corpus.
    
    Parameters
    ----------
    data : Pandas DataFrame (default=None)
        Contains the data file to be acted upon.
       
    col : string (default=None)
        Name of the column of 'data' for processing the text.
        
    entities : list, optional (default=[])
        Key words that have to be removed from 'col' as special entities.
        
    stemmers : list, optional (default=[])
        Words that need to be stemmed as per user definition.
        
    tokenize : string, optional (default='word')
        Specify the type of tokenization technique. 
        {'word': breaks document into words, 'sentence': breaks into sentences}
        
    contractions : dict, optional (default={})
        Dictionary from contraction to expansion.
        
    punctuations : list, optional (default=[])
        Contains all the punctuations that have to be removed.
        
    stopword : list, optional (default=[])
        Contains all the words to be removed as stopwords.
        
    lemmatization : boolean, optional (default=False)
        Whether WordNetLemmatizer has to be used on the text or not.
        
    blank_spaces : boolean, optional (default=True)
        Whether the blank spaces have to be removed from the text after preprocessing.
        
    min_word_count : integer, optional (default=1)
        Threshold for inclusion of words in the vocabulary.
        
    remove_values : list of strings, optional (default=[])
        The text values that have to be removed from the corpus indefinitely.
        
    replace_values : dict, optional (default={})
        Contains special characters that should be changed before any type of preprocessing.
        
    remove_duplicates : boolean, optional (default=False)
        Whether the duplicates have to be removed or not.
        
    duplicacy_order : list, optional (default=col)
        Sorting order of duplicated data visualisation.
    
    Attributes
    ----------
    data : Pandas Dataframe
        Updated dataframe after removing specific rows and replacing values.
        
    docsID : list
        Tokenized documents having IDs instead of the words.
        
    docsString : list
        Tokenized documents having words in the str format.
        
    emptyIndexes : list
        Indexes of documents which become empty after preprocessing.
        
    entitiesOfDocs : list
        Entities for each document are listed here.
        
    freqOfWordsInCorpus : dict
        Count of each word in the entire corpus.
        
    id2WordVocabulary : dict
        Mapping from ID to actual word for the entire vocabulary.
        
    word2IdVocabulary : dict
        Mapping from word to ID for the entire vocabulary.
        
    duplicatedData : Pandas Dataframe
        The separated out duplicated data from the given data.
    """
    
    def __init__(self, data=None, col=None, entities=[], stemmers=[], tokenize='word', contractions={}, 
                 punctuations=[], stopword=[], lemmatization=False, blank_spaces=True, min_word_count = 1,
                 remove_values=[], replace_values={}, remove_duplicates=False, duplicacy_order=None):
        
        self.data = data
        if(self.data.empty==True):
            raise ValueError('Dataframe is empty!')
            
        self.col = col
        if(self.col==None):
            raise ValueError('Specify the name of the column to be analyzed.')
    
        self.entities = entities
        self.stemmers = stemmers
        self.tokenize = tokenize
        self.contractions = contractions
        self.punctuations = punctuations
        self.stopword = stopword
        self.lemmatization = lemmatization
        self.blank_spaces = blank_spaces
        self.min_word_count = min_word_count
        self.remove_values = remove_values
        self.replace_values = replace_values
        self.remove_duplicates = remove_duplicates
        
        if(duplicacy_order==None):
            self.duplicacy_order = self.col
        else:
            self.duplicacy_order = duplicacy_order
        
        #List of recover words for lemmatizer
        self.recoveryList = {"wa":"was","ha":"has"}
        
    def tokenizeText(self):

        """Used to tokenize the documents into a Bag-of-Words representation, after removing specific
        values, replacing the given ones and removing duplicates from the original data.
        
        Attributes generated
        --------------------
        duplicatedData : Pandas Dataframe
            The separated out duplicated data from the given data.
        """
        
        shape_initial = self.data.shape
        if(len(self.remove_values)>0): #Not empty
            for value in self.remove_values:
                self.data = self.data[self.data[self.col]!=value]
            self.data.reset_index(drop=True,inplace=True)
            logger.info("Shape changed from (%d,%d) to (%d,%d)."%(shape_initial[0],shape_initial[1],self.data.shape[0],self.data.shape[1]))
        if(len(self.replace_values)>0): #Not empty
            for value in self.replace_values.iterkeys():
                flag = 0
                for index,row in self.data.iterrows():
                    if value in row[self.col]:
                        if(flag==0):
                            logger.info("Found %s. Replacing with %s."%(value,self.replace_values[value]))
                            flag = 1
                        row[self.col] = row[self.col].replace(value,self.replace_values[value])
        if(self.remove_duplicates==True):
            superSen = {}
            duplicSen = []
            removeIndices = []
            self.duplicatedData = pd.DataFrame(columns=self.data.columns)
            for index,row in self.data.iterrows():
                sentence = row[self.col]
                if sentence not in superSen:
                    superSen[sentence] = index
                else:
                    if sentence not in duplicSen:
                        duplicSen.append(sentence)
                        self.duplicatedData = self.duplicatedData.append(self.data.iloc[superSen[sentence]])
                        self.duplicatedData = self.duplicatedData.append(self.data.iloc[index])
                    else:
                        self.duplicatedData = self.duplicatedData.append(self.data.iloc[index])
                    nullInOld = self.data.isnull().sum(axis=1)[superSen[sentence]]
                    nullInNew = self.data.isnull().sum(axis=1)[index]
                    if(nullInOld<=nullInNew): #Throw this one
                        removeIndices.append(index)
                    else:
                        removeIndices.append(superSen[sentence])
                        superSen[sentence] = index
            self.data.drop(self.data.index[removeIndices],inplace=True)
            self.data = self.data.reset_index(drop=True)
            self.duplicatedData.sort_values(self.duplicacy_order,inplace=True)
            self.duplicatedData.reset_index(drop=True,inplace=True)
            logger.info("Duplicate data removed.")
        if(self.tokenize=='word'):
            docs = [doc.split() for doc in self.data[self.col].tolist()]
            self.corpus = docs
            logger.info("Word tokenization done.")
        elif(self.tokenize=='sentence'):
            docs = [doc.split() for doc in self.data[self.col].tolist()]
            self.corpus = docs
            logger.info("Sentence tokenization done.")
        else:
            raise ValueError('Specify a valid tokenization technique.')
        
    def removeContractions(self):
        
        """Used to remove contractions by expanding them."""
        
        docs = []
        for doc in self.corpus:
            tempDoc = []
            for word in doc:
                word = word.lower()
                word = word.decode('ascii','ignore')
                if word in self.contractions:
                    merged = self.contractions[word].split()
                    for single in merged:
                        tempDoc.append(unicode(single,"utf-8"))
                else:
                    tempDoc.append(word)
            docs.append(tempDoc)
        self.corpus = docs
        logger.info("Contractions replaced.")
        
        """
        Detecting cases like 'Andy is a boy.Sarah is a girl'
        It is required to separate boy and Sarah.
        """
        
        docs = []
        for doc in self.corpus:
            tempDoc = []
            for word in doc:
                flag = 0
                for punc in self.punctuations:
                    if punc in word:
                        flag = 1
                        tempWords = word.split(punc)
                        for w in tempWords:
                            tempDoc.append(w)
                        break
                if(flag==0):
                    tempDoc.append(word)
            docs.append(tempDoc)
        self.corpus = docs
        
    def removePunctuations(self):
        
        """Used to remove punctuations from the text."""
        
        docs = []
        for doc in self.corpus:
            tempDoc = []
            for word in doc:
                characters = ''
                for char in word:
                    if char not in self.punctuations:
                          characters+=char
                tempDoc.append(characters)
            docs.append(tempDoc)
        self.corpus = docs
        logger.info("Punctuations removed.")
        
    def removeStopwords(self):
        
        """Used to remove stopwords."""
        
        docs = []
        for doc in self.corpus:
            tempDoc = []
            for word in doc:
                if word not in self.stopword:
                    tempDoc.append(word)
            docs.append(tempDoc)
        self.corpus = docs
        logger.info("Stopwords removed.")
        
    def lemmatizeText(self):
        
        """Used to bring text to it's root form."""
        
        docs = []
        for doc in self.corpus:
            tempDoc = []
            for word in doc:
                lemma = WordNetLemmatizer().lemmatize(word)
                if lemma in self.recoveryList:
                    tempDoc.append(self.recoveryList[lemma])
                else:
                    tempDoc.append(lemma)
            docs.append(tempDoc)
        self.corpus = docs
        logger.info("Text lemmatized.")
        
        """Replace words which seem to have been left unstemmed."""
        
        if(len(self.stemmers)==0):
            logger.info("No stemmers have been passed.")
        else:
            docs = []
            for doc in self.corpus:
                tempDoc = []
                for word in doc:
                    if word in self.stemmers:
                        tempDoc.append(self.stemmers[word])
                    else:
                        tempDoc.append(word)
                docs.append(tempDoc)
            self.corpus = docs
            logger.info("Stemmers have been replaced.")
        
    def removeBlankSpaces(self):
        
        """Used to remove blank words from the corpus."""
        
        docs = []
        for doc in self.corpus:
            tempDoc = []
            for word in doc:
                if word!='':
                    tempDoc.append(word)
            docs.append(tempDoc)
        self.corpus = docs
        logger.info("Blank spaces removed.")
        
    def frequencyCalculation(self):
        
        """Calculates the frequency of words in corpus.
        
        Attributes generated
        --------------------
        freqOfWordsInCorpus : dict
            Stores frequency of each word in the entire corpus.
        """
        
        self.freqOfWordsInCorpus = {}
        for doc in self.corpus:
            for word in doc:
                if word in self.freqOfWordsInCorpus:
                    self.freqOfWordsInCorpus[word] += 1
                else:
                    self.freqOfWordsInCorpus[word] = 1
        
    def doc2String(self):
        
        """Encodes the unicode into string format."""
        
        self.docsString = [[word.encode('ascii','ignore') for word in doc] for doc in self.corpus]
        
    def shortenText(self):
        
        """Used to shorten the corpus by eliminating words which occur lesser than min_word_count times."""
        
        filterTable = {}
        for word,count in self.freqOfWordsInCorpus.iteritems():
            if(count >= self.min_word_count):
                filterTable[word] = count
        
        tempDocsString = []
        for doc in self.docsString:
            tempDoc = []
            for word in doc:
                if word in filterTable:
                    tempDoc.append(word)
            tempDocsString.append(tempDoc)
        self.docsString = tempDocsString
        
    def entityFilteration(self):
        
        """Used to separate out the entities from the main corpus.
        
        Attributes generated
        --------------------
        entitiesOfDocs : list 
            Entities for every document in the corpus
            
        docsString : list of documents without entity words
        """
        
        self.entitiesOfDocs = []
        docsWithoutEntities = []
        for doc in self.docsString:
            currentDoc = []
            currentEntities = []
            for word in doc:
                if word in self.entities:
                    currentEntities.append(word)
                else:
                    currentDoc.append(word)
            self.entitiesOfDocs.append(currentEntities)
            docsWithoutEntities.append(currentDoc)
        self.docsString = docsWithoutEntities
        logger.info("Entities separated out.")
        
    def word2Id(self):
        
        """Creates word2Id and id2Word Vocabulary. Also, updates the freqOfWordsInCorpus.
        
        Attributes generated
        --------------------
        word2IdVocabulary : dict
            Contains mapping of words to ID for the given corpus after preprocessing.
            
        id2WordVocabulary : dict
            Contains mapping of ID to words for the given corpus after preprocessing.
        """
        
        self.word2IdVocabulary = {}
        self.id2WordVocabulary = {}
        self.freqOfWordsInCorpus = {}
    
        index = 0
        for doc in self.docsString:
            for word in doc:
                if word in self.freqOfWordsInCorpus:
                    self.freqOfWordsInCorpus[word] += 1
                else:
                    self.freqOfWordsInCorpus[word] = 1
                if word not in self.word2IdVocabulary:
                    self.word2IdVocabulary[word] = index
                    self.id2WordVocabulary[index] = word
                    index += 1
    
    def doc2Id(self):
        
        """Creates a document mapping on IDs based on word2Id function.
        
        Attributes generated
        --------------------
        docsID : list
            Similar to docsString, but, it contains IDs instead of the actual words.
        """
        
        self.docsID = []
        for doc in self.docsString:
            tempDoc = []
            for word in doc:
                tempDoc.append(self.word2IdVocabulary[word])
            self.docsID.append(tempDoc)
            
    def checkEmptyDocs(self):
        
        """Returns indexes of docs which are empty after the preprocessing.
        
        Attributes generated
        --------------------
        emptyIndexes : list
            Document indexes which become empty after all the preprocessing.
        """
        
        self.emptyIndexes = []
        for i, doc in enumerate(self.docsString):
            if len(doc) == 0:
                self.emptyIndexes.append(i)
    
    def preprocess(self, verbose=True):
        
        """Function for doing all the tasks pertaining to preprocessing."""
        
        if(verbose==False):
            logger.disabled = True #Toggle for logging to console or not
        else:
            logger.disabled = False

        self.tokenizeText()
        
        if len(self.contractions)>0:
            self.removeContractions()
            
        if len(self.punctuations)>0:
            self.removePunctuations()
            
        if len(self.stopword)>0:
            self.removeStopwords()
            
        if self.lemmatization==True:
            self.lemmatizeText()
            if len(self.stopword)>0:
                self.removeStopwords()
            
        if self.blank_spaces==True:
            self.removeBlankSpaces()
            
        #Calculate frequency of words which shall assist in shortening text
        self.frequencyCalculation()
        
        #Convert unicode to string to take care of encoding issues
        self.doc2String()
        
        #Shorten the text as per user-specified min_word_count
        self.shortenText()
        
        if(len(self.entities)==0):
            logger.info("No entities have been passed.")
        else:
            self.entityFilteration()

        self.word2Id()
        self.doc2Id()
        self.checkEmptyDocs()

#End of textPreprocessing class