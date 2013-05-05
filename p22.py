from __future__ import with_statement

def name2num (name, idx, charmap):
  return sum(map(lambda c: charmap[c], name))*idx

if __name__ == '__main__':
  charmap = {c: i for i,c in enumerate("ABCDEFGHIJKLMNOPQRSTUVWXYZ", start=1)}
  with open("p22_names.txt") as f:
    names = map(lambda x: filter(lambda y: y != '"', x).upper(), f.read().split(','))
    names.sort()
    names = [(i, name) for i,name in enumerate(names, start=1)]

    total = 0
    for i,w in names:
      total += name2num(w, i, charmap)
    print total