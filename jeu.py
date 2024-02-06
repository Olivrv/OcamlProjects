def noyau(d: dict):
    pass

def sansSuccesseur(d):
    for sommet in d.keys():
        if d[sommet] == []:
            return sommet

def predecesseur(d, i):
    for sommet in d.keys():
        if i in d[sommet]:
            return sommet

def supprimer(d, i):
    a_supprimer = pred(j)
    d.pop(j)
    for cle in d:
        for k in range(len(d[cle])):
            if d[cle][k] in a_supprimer:
                d[cle] = d[cle][k] + d[cle][k+1]
        if cle in a_supprimer:
            d.pop(cle)
    return d

def noyau(d):
    noyau = []
    while d != {}:
        s = sansSuccesseur(d)
        noyeau.append(s)
        supprimer(d,s)
    return noyau