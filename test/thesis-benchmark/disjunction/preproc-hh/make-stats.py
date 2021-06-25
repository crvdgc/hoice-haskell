import os
import sys

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
    if len(sys.argv) >= 2:
        input_dir = sys.argv[1]
    else:
        input_dir = './results/'
    if len(sys.argv) >= 3:
        out_format = sys.argv[2]
    else:
        out_format = 'csv'
    # path, resol, raf, far
    stats = []
    for root, _, files in os.walk(input_dir, topdown=False):
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
            stats.append((path, resol, raf, far))
    if out_format == 'csv':
        with open('stats.csv', 'w') as f:
            f.write('name, resol, raf, far\n')
            for stat in stats:
                f.write(', '.join(str(i) for i in stat) + '\n')

