import os
import sys
import argparse

def get_num(line):
    start = line.find('=')
    if start == -1:
        return 0
    else:
        try:
            return int(line[start+1:].strip())
        except:
            print(line)
            exit(1)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Stats for preproc results')
    parser.add_argument( '--input', dest='input_dir', type=str
                       , help='input directory'
                       )
    parser.add_argument( '--format', dest='output_format', type=str
                       , help='output format, csv'
                       )
    parser.add_argument('--filter', dest='filter_non_zero', action='store_true')
    parser.add_argument('--no-filter', dest='filter_non_zero', action='store_false')
    parser.add_argument('--discard-full', dest='discard_full', action='store_true')
    parser.set_defaults(input_dir = './results/', output_format = 'csv', filter_non_zero = False, discard_full=False)
    args = parser.parse_args()

    # path, resol, raf, far
    stats = []
    for root, _, files in os.walk(args.input_dir, topdown=False):
        if args.discard_full:
            if 'mochi_full' in root:
                continue
        for name in files:
            path = os.path.join(root, name)
            print(path)
            resol = 0
            raf = 0
            far = 0
            with open(path, 'r') as f:
                for line in f:
                    if 'resol' in line:
                        resol += get_num(line)
                    elif 'raf' in line:
                        raf += get_num(line)
                    elif 'far' in line:
                        far += get_num(line)
            if args.filter_non_zero:
                if resol != 0 or raf != 0 or far != 0:
                    stats.append((path, resol, raf, far))
            else:
                stats.append((path, resol, raf, far))
    output_name = 'stats'
    if args.filter_non_zero:
        output_name += '-filter'
    if args.discard_full:
        output_name += '-no-full'
    if args.output_format == 'csv':
        with open('{}.csv'.format(output_name), 'w') as f:
            f.write('name, resol, raf, far\n')
            for stat in stats:
                f.write(', '.join(str(i) for i in stat) + '\n')

