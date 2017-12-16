def file2Text(nameOfTheFile):
    import textract
    text = textract.process(nameOfTheFile).decode('utf-8')
    text = text.replace('\n',' ')
    text = text.replace('\t',' ')
    text = text.encode('ascii','ignore')
    return text