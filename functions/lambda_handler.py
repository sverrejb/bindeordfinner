# -*- coding: utf-8 -*

import json


def lambda_handler(event, context):
    body = json.loads(event["body"])
    first_word = body['first']
    second_word = body['second']


    solutions = find_glueword(first_word, second_word)
    solution_strings = []
    for solution in solutions:
        solution_strings.append(f'{solution} - {first_word}{solution} - {solution}{second_word}')

    return {
        'statusCode': 200,
        'headers': {
                "content-type":"application/json; charset=utf-8"},
        'body': json.dumps(solution_strings)
    }


def find_glueword(first_word, second_word):
    wordlists = ['wordlist.txt']
    starts_with_first = set()
    ends_with_second = set()

    for wordlist in wordlists:

        with open(wordlist, 'r', encoding='utf-8') as file:
            for line in file.readlines():
                word = line.lower().split(" ")[0].strip()
                if word.startswith(first_word):
                    starts_with_first.add(word.replace(first_word, ''))
                    continue
                if word.endswith(second_word):
                    ends_with_second.add(word.replace(second_word, ''))



    solutions = [x for x in list(starts_with_first.intersection(ends_with_second)) if x != ""]
    return solutions


if __name__ == "__main__":

    for pair in [("smør","maler"),("knall","kjede"),("ratt","stang"),("flaske","horn"),("bade","hund"),("følge","post"),("tann","kake"),("glass","lus"),("stjerne", "sikte"),("stolpe", "salve"),("hval", "kake"), ("pusse","rye")]:
        a,b = pair
        print(find_glueword(a, b))
