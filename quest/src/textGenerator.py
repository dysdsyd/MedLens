def file2Text(nameOfTheFile):
    import pandas as pd
    a = pd.read_csv(nameOfTheFile)
    text = str(a['Text'][0])
    print("*******************************")
    print(text)
    print(type(text))
    print("**********************************")
    # text = textract.process(nameOfTheFile).decode('utf-8')
    text = text.replace('\n',' ')
    text = text.replace('\t',' ')
    text = text.encode('ascii','ignore')
    return text