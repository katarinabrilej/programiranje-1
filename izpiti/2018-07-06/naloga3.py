# naloga 3 
def simetricen(niz):
    return niz[::-1] == niz

def stevilo_delov(niz):
    if simetricen(niz):
        return 1
    if niz == []:
        return 0
    
    return min([stevilo_delov(niz[:i]) + stevilo_delov(niz[i:]) for i in range(1,len(niz))])

def razdeli(niz):
    if simetricen(niz):
        return niz
    if niz == []:
        return niz 
    else:
        