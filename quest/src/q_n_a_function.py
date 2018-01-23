
#libraries
import os
import sys
import json
from subprocess import (Popen,PIPE)

def get_answer(passage, question):

	cq_dict = {"passage" : passage,
			"question" : question}

	m = json.dumps(cq_dict)
	m = json.loads(m)

	with open('quest/src/examples.jsonl', 'w') as outfile:
		json.dump(m, outfile)

	a = Popen("python -m allennlp.run predict \quest/src/qna_model.gz \quest/src/examples.jsonl", stdout = PIPE, shell = True).stdout.read()
	a = a.decode('utf-8')
	print("---------------------------------")
	print(a)
	print("---------------------------------")
	return  a[a.index('"best_span_str"')+18:-3]


