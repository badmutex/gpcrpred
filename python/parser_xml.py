from BeautifulSoup import BeautifulSoup
import re
import sys

if len(sys.argv) != 2:
  print "Insuficient Arguments"
  sys.exit(42)

html = open(sys.argv[1], 'r')
soup = BeautifulSoup(html)
paraText = soup.findAll(text=re.compile("CAUTION"))
if len(paraText) != 0: 
  print 'non GPCR'
else:
  texto = soup.findAll(['i','font'])

  for i,v in enumerate(texto[1::]):
    if i > 8: break
    if (i+2)%2 == 0:
      print '%s\t' % v.string,
    else:
      print '%s' % v.string


# i = iter(texto[1::])
# l1=i.next().string
# Gi=i.next().string
# l2=i.next().string
# Gq=i.next().string
# l3=i.next().string
# G12=i.next().string
# l4=i.next().string
# Gs=i.next().string
# print soup.b.string, l1, Gi, l2, Gq, l3, G12, l4, Gs
