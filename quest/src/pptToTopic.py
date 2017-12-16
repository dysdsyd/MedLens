import sys
import os 
sys.path.append('quest/src')

def topicModeling(textFormat):  
    import pandas as pd
    import preprocessing, dmm
    import string
    from nltk.corpus import stopwords
    from collections import Counter
    import operator
    t = pd.DataFrame(data={'text':textFormat.split('.')[:-1]})
    ranges = [1,8,20,40,80,200,400]
    
    pathToContractionsFile = 'quest/src/contractions.csv'
    pathToStopwordsFile = 'quest/src/English_stopwords.csv'
    col = 'text'
    topicData = pd.DataFrame(columns=[col])
    u = 0
    for index,row in t.iterrows():
        for col in t.columns:
            if(type(row[col])!=float):
                topicData.loc[u] = [row[col]]
                u+=1
    contractionList = {}
    conFile = pd.read_csv(pathToContractionsFile)
    for index,row in conFile.iterrows():
        contractionList[row['Contraction']] = row['Expansion']

    punctuationList = list(string.punctuation)

    stopwordsList = list(stopwords.words('english'))
    more_stopwords = pd.read_csv(pathToStopwordsFile)
    for word in more_stopwords['stopwords'].tolist():
        if word not in stopwordsList:
            stopwordsList.append(unicode(word,'utf-8'))
    for character in range(ord('a'), ord('z')+1):
        stopwordsList.append(unicode(chr(character),'utf-8'))
    for no in range(0,10,1):
        stopwordsList.append(str(no))
    listOfStopwords = ['additional Stop Words']
    for word in listOfStopwords:
        stopwordsList.append(word)

    listOfStemmers = {'working':'work','trying':'try','affecting':'affect','changing':'change'}
    listOfEntities = ['patient','hospital','clinic','physician']
    duplicacyOrder = [col]

    # dmm Model Parameters
    #col = 'text'
    numberOfTopics = min(range(len(ranges)),key=lambda x:abs(ranges[x]-t.shape[0]))
    iterations = 5
    alpha = 0.1
    beta = 0.01
    lambdaVal = 0.5
    numberOfTopWords = 10
    freqOfWordsInCorpus = 'word_proportion'
    verbose = False

    # Display Parameters
    noDisplayWordcloud = 10
    saveWordcloud = True
    saveCSV = True
    newPath = r'Results'
    wordsToHighlight = 10
    sentencesToView = 5


    preprocessed = preprocessing.textPreprocessing(topicData,col, listOfEntities, listOfStemmers, 'word', contractionList, punctuationList, stopwordsList, True, True, 1, [], [], True, duplicacyOrder)
    preprocessed.preprocess()
    model = dmm.dmmModel(preprocessed.docsID,preprocessed.data,col,numberOfTopics,iterations,alpha,beta,lambdaVal,numberOfTopWords,preprocessed.id2WordVocabulary,freqOfWordsInCorpus,preprocessed.docsString, listOfStemmers)
    model.learning(verbose)
    freq = {}
    for topicNo,details in model.topicToWordsFrequency.iteritems():
        rank = 1
        for pair in details:
            if pair[0] not in freq:
                freq[pair[0]] = rank
                rank+=1
    rele = {}
    for topicNo,details in model.topicToWordsRelevance.iteritems():
        rank = 1
        for pair in details:
            if pair[0] not in rele:
                rele[pair[0]] = rank
                rank+=1
    final = {}
    for word in rele:
        
        if word not in final:
            if word in freq:
                final[word] = float((rele[word]+freq[word])/2.0)
            else:
                final[word] = float((rele[word]+10)/2.0)
    for word in freq:
        if word not in final:
            final[word] = float((freq[word]+10)/2.0)
    final = [(x,1.0/y) for x,y in final.iteritems()]
    return sorted(final,key=lambda x:x[1],reverse=True)

def pptToTopic(nameOfFile):
    import textract
    text = textract.process(nameOfFile)
    text = text.replace('\n','')
    text = text.replace('\t','')
    return topicModeling(text)

# print pptToTopic(str(sys.argv[1]))