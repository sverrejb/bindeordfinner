# -*- coding: utf-8 -*

import json


def lambda_handler(event, context):
    body = json.loads(event["body"])
    first_word = body['first']
    second_word = body['second']

    solutions = find_glueword(first_word, second_word)

    return {
        'statusCode': 200,
        'headers': {
                "content-type":"application/json; charset=utf-8",
                "Access-Control-Allow-Headers" : "Content-Type",
                "Access-Control-Allow-Origin": "*",
                "Access-Control-Allow-Methods": "OPTIONS,POST,GET"},
        'body': json.dumps(solutions)
    }


def find_glueword(first_word, second_word):
    wordlists = ['wordlist.txt']
    starts_with_first = set()
    ends_with_second = set()

    for wordlist in wordlists:

        with open(wordlist, 'r', encoding='utf-8') as file:
            for line in file.readlines():
                word = line.lower().split(" ")[0].strip()
                if word.startswith(first_word) and not word == first_word:
                    starts_with_first.add(word.replace(first_word, ''))
                    continue
                if word.endswith(second_word) and not word == second_word:
                    ends_with_second.add(word.replace(second_word, ''))



    solutions = [x for x in list(starts_with_first.intersection(ends_with_second)) if x != ""]

    solutions.sort()
    startsWith = sorted(list(starts_with_first))
    endsWith = sorted(list(ends_with_second))

    solutions_response = {
        "firstWord": first_word,
        "secondWord": second_word,
        "solutions": solutions,
        "startsWith": startsWith,
        "endsWith": endsWith
    }
    return solutions_response


if __name__ == "__main__":

    for pair in [("gir","skudd")]:
        a,b = pair
        print(find_glueword(a, b))
