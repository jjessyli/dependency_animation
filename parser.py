# Parse an article using the Stanford Dependency Parser.

from nltk import sent_tokenize
import string
import argparse
from stanford_parser.parser import Parser

def load_file_sentences(path):
    file = open(path, 'r')
    raw = file.read()
    sents = sent_tokenize(raw)
    sents = [string.replace(string.lower(s), "\n", "") for s in sents]
    return sents

def dependency_parse_sentence(sentence, p):
    deps = p.parseToStanfordDependencies(sentence)
    return [(r, gov.text, dep.text) for r, gov, dep in deps.dependencies]

def dependency_parse_file(path):
    p = Parser()
    deps = []
    sents = load_file_sentences(path)
    for sent in sents:
        deps.extend(dependency_parse_sentence(sent, p))
    return deps

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-f', default='data2',
                        help='file name to parse (default: data2)')
    args = parser.parse_args()
    path = args.f
    file = open(path, 'r')
    dl = dependency_parse_file(path)
    s = ""
    for (a,b,c) in dl:
        s += (b+" "+c+"\n")
    fout = open('parsed2', "w")
    fout.write(s)
    
    