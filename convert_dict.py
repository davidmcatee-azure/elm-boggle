"""
Convert dictionary of words into a json file
"""
import json

WORDS_FILE = open("src/static/ospd4.txt", "r")
WORDS = WORDS_FILE.readlines()
WORDS_FILE.close()

DICTIONARY = {}

for i, word in enumerate(WORDS):
    trimmedWord = word.strip()
    wordLength = len(trimmedWord)
    if wordLength > 2:
        DICTIONARY[wordLength] = DICTIONARY.get(wordLength, '') + trimmedWord

OUTPUT_FILE = open("src/static/ospd4.json", "w")
OUTPUT_FILE.write(json.dumps(DICTIONARY))
OUTPUT_FILE.close()
