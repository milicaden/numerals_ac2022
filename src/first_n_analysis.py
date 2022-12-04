# from __future__ import division
import math
import pandas as pd
import random
import pygmo
import re
from itertools import combinations


def rSubset(arr, r):
    # return list of all subsets of length r
    # to deal with duplicate subsets use
    # set(list(combinations(arr, r)))
    initial_list=list(combinations(arr, r))
    output_list=list(map(list, initial_list))
    return output_list

#get unique elements in a list
def get_unique_elements(numbers):
    unique = []
    for number in numbers:
        if number in unique:
            continue
        else:
            unique.append(number)
    return unique


# Functions
def format_number(primitives):
    # format [morphosyntax, denotation, complexity]
    primitives_formatted = []
    for prim in primitives:
        i_formatted = [str(prim), prim, 1]
        primitives_formatted.append(i_formatted)
    return primitives_formatted


def common_member(a, b):
    a_set = set(a)
    b_set = set(b)
    if (a_set & b_set):
        return True
    else:
        return False


def delete_multiple_element(list_object, indices):
    for ind in sorted(indices, reverse=True):
        del list_object[ind]
    return list_object


def filter_within(constructions):
    if len(constructions) == 0:
        return constructions
    elif len(constructions) > 0:
        unwanted = []
        for i in range(len(constructions)):
            for j in range(len(constructions)):
                if i == j:
                    continue
                else:
                    if constructions[i][1] == constructions[j][1]:
                        if (constructions[i][2] == constructions[j][2] and i < j) or (
                                constructions[j][2] > constructions[i][
                            2]):  # throw out the one with larger complexity; if they are of equal complexity, throw out the one with larger index
                            unwanted.append(j)
        unwanted = list(set(unwanted))
        if len(unwanted) > 0:
            constructions = delete_multiple_element(constructions, unwanted)
        return constructions


# Filter_across
def filter_across(constructions, lower_constructions):
    if len(constructions) == 0:
        return constructions
    elif len(constructions) > 0:
        unwanted = []
        for i in range(len(constructions)):
            for j in range(len(lower_constructions)):
                if constructions[i][1] == lower_constructions[j][1]:
                    unwanted.append(i)
        unwanted = list(set(unwanted))
        if 0 < len(unwanted) < len(constructions):
            constructions = delete_multiple_element(constructions, unwanted)
        return constructions


# Filter 100

def filter_100(constructions):
    unwanted = []
    for i in range(len(constructions)):
        if constructions[i][1] > 99:
            unwanted.append(i)
        unwanted = list(set(unwanted))
    if 0 < len(unwanted) < len(constructions):
        constructions = delete_multiple_element(constructions, unwanted)
    return constructions


# TEST

# Expected morphosyntactic complexity
# Prior
prior_sum = 0  # for normalization of prior probas
for i in range(1, 100, 1):
    prior_sum += i ** (-2)


def probaf(number):
    return (number ** (-2)) / prior_sum


def expected_complexity(language_id, df):
    temp = df[df['language'] == language_id]
    return sum(temp['weighted_complexity'])



# Generate a single language from its morphemes, arguments a list of digits and a list of multiplicatives
def generate(digits, multiplicatives):
    np = {}
    np['phrase1'] = format_number(multiplicatives)
    np['number1'] = format_number(digits) + format_number(multiplicatives)
    np['number1'] = filter_within(np['number1'])
    lowest_complexity = np['number1']
    start = 2
    while start < 8 and len(lowest_complexity) != 200:
        # first we build phrases with * and append them to NumP
        np['phrase' + str(start)] = []
        np['number' + str(start)] = []
        for first_element in np['number' + str(start - 1)]:
            for second_element in np['phrase1']:
                current_multiplication = [first_element[0] + '*' + second_element[0],
                                          first_element[1] * second_element[1],
                                          first_element[2] + second_element[2] + 1]
                if current_multiplication[1] < 201:  # 101 so that things like 5*20-1 can be generated
                    np["phrase" + str(start)].append(current_multiplication)
        np['number' + str(start)].extend(np["phrase" + str(start)])
        # second we build NumP with + and -
        for x in range(1, start, 1):
            for y in range(1, start, 1):
                if x + y == start:
                    for first_element in np['phrase' + str(x)]:
                        for second_element in np['number' + str(y)]:
                            current_plus = [first_element[0] + '+' + second_element[0],
                                            first_element[1] + second_element[1],
                                            first_element[2] + second_element[2] + 1]
                            current_minus = [first_element[0] + '-' + second_element[0],
                                             first_element[1] - second_element[1],
                                             first_element[2] + second_element[2] + 1]
                            if current_plus[1] < 201:
                                np["number" + str(start)].append(current_plus)
                            if 0 < current_minus[1] < 201:
                                np["number" + str(start)].append(current_minus)

        np["number" + str(start)] = filter_within(np["number" + str(start)])

        np["number" + str(start)] = filter_across(np["number" + str(start)], lowest_complexity)
        lowest_complexity.extend(np["number" + str(start)])
        lowest_complexity = filter_within(lowest_complexity)

        start = start + 1
    lowest_complexity = filter_100(lowest_complexity)
    if len(lowest_complexity) == 99:
        return lowest_complexity
    else:
        return []



# Generate languages from a list of languages
def generate_list(language_list, generation):
    all_languages_df = pd.DataFrame(
        columns=['language', 'morphology', 'extension', 'digs', 'multis', 'complexity', 'word_n', 'weighted_complexity',
                 'expected_complexity', "coordinates"])
    for language in language_list:
        language_name = 'language' + 'gen' + str(generation) + 'index' + str(
            language_list.index(language)) + '_' + '.'.join([str(elem) for elem in language[0]]) + '_' + '.'.join(
            [str(elem) for elem in language[1]])
        language_expressions = generate(language[0], language[1])
        if len(language_expressions) > 0:
            language_expressions_df = pd.DataFrame(language_expressions,
                                                   columns=["morphology", "extension", "complexity"])
            language_expressions_df['language'] = language_name
            language_expressions_df['digs'] = '.'.join([str(elem) for elem in language[0]])
            language_expressions_df['multis'] = '.'.join([str(elem) for elem in language[1]])
            language_expressions_df['word_n'] = len(language[0]) + len(language[1])
            language_expressions_df['weighted_complexity'] = language_expressions_df['extension'].map(probaf) * \
                                                             language_expressions_df['complexity']
            language_expressions_df['expected_complexity'] = expected_complexity(language_name, language_expressions_df)
            all_languages_df = pd.concat([all_languages_df, language_expressions_df])  # maybe problem here??
            all_languages_df['coordinates'] = list(zip(all_languages_df.word_n, all_languages_df.expected_complexity))
            all_languages_df.reset_index(inplace=True, drop=True)
    all_languages_df.to_csv(Folder + "evo_full_generation_" + str(generation) + ".csv", index=False)
    # prepare output where 1 row = 1 language
    all_languages_output = all_languages_df[
        ['language', 'digs', 'multis', 'word_n', 'expected_complexity', 'coordinates']]
    all_languages_output = all_languages_output.drop_duplicates()
    all_languages_output.reset_index(inplace=True, drop=True)
    all_languages_output.to_csv(Folder + "evo_coordinates_generation_" + str(generation) + ".csv", index=False)
    return all_languages_output


######################################
# Evolutionary algorithm
######################################
if __name__ == "__main__":
    # Read relevant files
    Folder = "../data/"
    bases = [[*range(1, n)] for n in [*range(2, 12)]]
    one_extra = [x + [y] for x in bases for y in [*range(x[-1]+1, 100)] ]
    two_extra = [x + y for x in bases for y in rSubset([*range(x[-1]+1, 100)],2)]

    lexicalized_concepts_list_prefiltered = bases + one_extra + two_extra
    lexicalized_concepts_list = get_unique_elements(lexicalized_concepts_list_prefiltered)
    languages_digs_and_multis = []
    for lex_list in lexicalized_concepts_list:
        for multi in lex_list:
            digs = [x for x in lex_list if x not in [multi]]
            language = [digs, [multi]]
            languages_digs_and_multis.append(language)
    for lex_list in lexicalized_concepts_list:
        for multi in rSubset(lex_list, 2):
            digs = [x for x in lex_list if x not in multi]
            language = [digs, multi]
            languages_digs_and_multis.append(language)

    the_sample = random.sample(languages_digs_and_multis, 50000)
    generate_list(the_sample, "the_sample")
