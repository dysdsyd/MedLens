import xml.etree.ElementTree as ET
import pandas as pd
import urllib
import requests

def itertext(self):
    tag = self.tag
    if not isinstance(tag, str) and tag is not None:
        return
    if self.text:
        yield self.text
    for e in self:
        for s in e.itertext():
            yield s
        if e.tail:
            yield e.tail
def crawl(search_query):
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pmc&term="+disease+"+AND+free+fulltext[filter]&retmax=10"
    response_page = requests.get(url)
    tree = ET.fromstring(response_page.content)
    
    for i in tree.iter():
        if i.tag == "Id":
            print ("PMC"+i.text)
            url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&id="+i.text
            response_page = requests.get(url)
            tr = ET.fromstring(response_page.content)        
            #print itertext(tr.findall(".//abstract"))
            text = ''.join(itertext(tr.findall(".//abstract")[0]))
            break
    return text

def pmcid_to_display(pmc_id):
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&id="+pmc_id
    response_page = requests.get(url)
    tree = ET.fromstring(response_page.content)
    article_title = ''.join(itertext(tree.findall(".//article-title")[0]))
    return article_title




def pmcid_to_crawl(pmc_id):
    #url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&id="+pmc_id
    urllib.request.urlretrieve("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&id="+pmc_id, "quest/data/"+pmc_id+".xml")
    tree = ET.parse("quest/data/"+pmc_id+".xml")
    root = tree.getroot()
    abstract = ''.join(itertext(tree.findall(".//abstract")[0])).replace("\n"," ")
    full_text = ''.join(itertext(tree.findall(".//article")[0])).replace("\n"," ")
    article_title = ''.join(itertext(tree.findall(".//article-title")[0])).replace("\n"," ")
    return abstract, full_text, article_title	


def entityToKeywords(searchTerm):
    search_query = searchTerm
    to_search = ""
    to_search = search_query.split()[0]
    for entity in search_query.split()[1:]:
        to_search = to_search + "+AND+" + entity.lower().strip()
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pmc&term="+to_search+"+AND+free+fulltext[filter]&retmax=5"
    response_page = requests.get(url)
    tree = ET.fromstring(response_page.content)
    df_kwd = pd.DataFrame(columns = ['file_name','kwd'])
    pmc_ids = []
    for i in tree.iter():
        if i.tag == "Id":
            pmc_ids.append(i.text)
            print (i.text)
            url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&id="+i.text
            #pmcid_to_crawl(i.text)
            response_page = requests.get(url)
            tree = ET.fromstring(response_page.content)
            for j in tree.iter():
                if j.tag == "kwd":
                    dic = {}
                    dic['kwd'] = ''.join(itertext(j))
                    dic['file_name'] = i.text
                    df_kwd = df_kwd.append(dic, ignore_index = True)
                if j.tag == "body":
                    break
    
    df_kwd['kwd'] = df_kwd['kwd'].apply(lambda x: x.lower().strip().replace("\n"," "))
    to_display = df_kwd['kwd'].value_counts().reset_index()
    to_display.columns = ['kwd','count']
    df_kwd = pd.merge(df_kwd, to_display)
    
    to_merge = df_kwd.groupby('file_name')['count'].sum().reset_index()
    to_merge.columns = ['file_name','score']
    df_kwd = pd.merge(df_kwd, to_merge)
    df_kwd['kwd'] = df_kwd['kwd'].str.encode('ascii','ignore')
    to_display['kwd'] = to_display['kwd'].str.encode('ascii','ignore')
    return to_display, df_kwd

def helper(searchTerm):
    keywordsToDisplay = 20
    articlesForEachKeyword = 5
    
    a,b = entityToKeywords(searchTerm)
    a1 = list(a[:keywordsToDisplay]['kwd'])
    a2 = list(a[:keywordsToDisplay]['count'])
    pmcid = list(b[:keywordsToDisplay]['file_name'].unique())
    a = zip(a1,a2)
    c = {}
    for i,j in a:
        c[i] = list(b[b['kwd']==i].sort_values('score',ascending=False)['file_name'][:articlesForEachKeyword])
    return a1, c
#keywords,keywordsToArticles = helper('lupus')