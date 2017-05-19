#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Rosalind.info problem 9N

@author: michaellinderman
"""

import sys
from operator import itemgetter

def read9N(filename):
    """ Read 9N input """
    with open(filename, "r") as file:
        text = file.readline().strip()
    return text
#%%
def build_first_occur(first_column):
    """ Build first occurence array from first_column

    Args:
        first_column: First column as string

    Return:
        Tuple of (letters as string, list of first occurences)
    """
    letters = "$"
    first_occur = [0]
    for i in range(1, len(first_column)):
        letter = first_column[i]
        if letter != letters[-1]:
            letters += letter
            first_occur.append(i)
    first_occur.append(len(first_column))

    return (letters, first_occur)
#%%
def build_count_matrix(letters, last_column):
    """ Build count matrix from last_column

    Args:
        letters: String of letters in index
        last_column: Last column as string

    Return:
        Dictionary of count array indexed by symbol
    """
    count_matrix = {}
    for letter in letters:
        count_matrix[letter] = [0]
    for current_letter in last_column:
        for letter in letters:
            letter_list = count_matrix[letter]
            if letter == current_letter:
                letter_list.append(letter_list[-1] + 1)
            else:
                letter_list.append(letter_list[-1])
    return count_matrix
 #%%
def build_indices(text):
    """Build BWT FM-index data structures

    Args:
        text: String to index

    Returns:
        Tuple of
        (letters, suffix array, first occurence, last col, count matrix)
    """

    assert(sorted(text + "$")[0] == "$")

    # Append "$"
    text = text + "$"

    # Create all cyclic rotations with starting position
    cycles = []
    for i in range(len(text)):
        cycles.append((i, text[i:]+text[:i]))

    # '$' is less than "acgtACGT" in Python so specialized
    # comparison is not needed
    cycles.sort(key=itemgetter(1))

    suffix_array = [cycle[0] for cycle in cycles]
    last_col = "".join([cycle[1][-1] for cycle in cycles])

    # Determine 1st occurence
    first_col = "".join([cycle[1][0] for cycle in cycles])
    letters, first_occur = build_first_occur(first_col)

    count_matrix = build_count_matrix(letters, last_col)

    return (letters, suffix_array, first_occur, last_col, count_matrix)

#%%
def query(letters, suffix_array, first_occur, last_col, count_matrix, pattern):
    """ Query BWT FM-index

    Args:
        letters: String of letters in index
        suffix_array: SuffixArray as list
        first_occur: List of first occurences
        last_col: Last column of BW matrix
        count_matrix: Counts as a dictionary
        pattern: Query as a string

    Returns:
        List of match positions
    """
    which_letter = letters.find(pattern[-1])
    top = first_occur[which_letter]
    bot = first_occur[which_letter+1]

    i = len(pattern)-2
    while i >= 0 and top != bot:
        first = first_occur[letters.find(pattern[i])]
        count_array = count_matrix[pattern[i]]
        top = first + count_array[top]
        bot = first + count_array[bot]


        i -= 1

    return suffix_array[top:bot] ##


if __name__ == "__main__":
    text = read9N("BRACA1.txt")
    # text = "AACGATAGCGGTAGA"
    letters, suffix_array, first_occur, last_col, count_matrix = \
        build_indices(text)
    print(last_col,first_occur)
    # print(suffix_array)
    # matches = []
    # for pattern in patterns:
    #     matches += query(letters, suffix_array, first_occur, last_col, count_matrix, pattern)
    # print(" ".join([str(match) for match in sorted(matches)]))
    #
