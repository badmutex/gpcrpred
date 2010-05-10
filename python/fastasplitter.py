
"""
Splits a fasta file into a bunch of individual fasta files
"""

import re
import sys
import os

def go(bigfasta, outdir='fastasplits'):
    repat = r'(>[.$]*)'
    reflags = re.IGNORECASE + re.MULTILINE
    regex = re.compile(repat, reflags)

    os.path.exists(outdir) or os.mkdir(outdir)

    fd = open(bigfasta)
    data = fd.read()
    fd.close()

    splits = regex.split(data)
    newfastas = []
    j = 0
    for i in range(2, len(splits), 2) :
        fasta = '>' + splits[i]
        with open('%s/%d.fasta' % (outdir, j), 'w') as fd:
            fd.write(fasta)

        j += 1



if __name__ == '__main__':
    if len(sys.argv) > 1:
        bigfasta=sys.argv[1]
        try: outdir = sys.argv[2]
        except: outdir=None

        outdir and go(sys.argv[1], outdir=outdir) or go(sys.argv[1])
