#!/usr/bin/env python3

import argparse
from collections import Counter
import random
import time


def bit_question():
    rand_int = random.choice(range(16))
    rand_bin = bin(rand_int)
    print("{0:0>4}".format(rand_bin[2:]))
    start_time = time.time()
    user_int = input()
    while not user_int:
        user_int = input()
    end_time = time.time()
    return rand_int, int(rand_bin, 2) == int(user_int), end_time - start_time


def main(args):
    results = Counter()
    threshold = args.threshold

    try:
        while True:
            rand_int, is_correct, time_taken = bit_question()
            print(rand_int, is_correct, time_taken)
            if not is_correct:
                results[rand_int] += 3
            if time_taken >= threshold:
                results[rand_int] += 1

    except ValueError:
        print(results)

    except KeyboardInterrupt:
        print(results)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('threshold', nargs='?', type=float, default=5.0)
    args = parser.parse_args()
    main(args)
