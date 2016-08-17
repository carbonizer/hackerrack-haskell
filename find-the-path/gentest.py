""" gentest.py

Generate input to test find-the-path

Usage: gentest.py <num_of_rows> <num_of_cols> <num_of_queries>
"""
import random
import sys


def main(args=sys.argv[1:]):
    rows, cols, querys = map(int, args)
    nums = [[random.randint(0,9) for _ in range(cols)] for _ in range(rows)]
    queries = []
    while not queries or any(a == b for a, b in zip(*queries)):
        queries = [
            [
                [random.randint(0, rows-1), random.randint(0, cols-1)]
                for _ in range(querys)
            ]
            for _ in range(2)
        ]
    queries = [a + b for a, b in zip(*queries)]
    print(rows, cols)
    print('\n'.join(' '.join(map(str, row)) for row in nums))
    print(querys)
    print('\n'.join(' '.join(map(str, query)) for query in queries))

if __name__ == '__main__':
    main()

