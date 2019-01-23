##############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 1                                             1, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7) # pivotiramo samo tabelo med indeksoma 1 in 7 in ne cele
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##############################################################################
def pivot(a,start,end):
    pivot = a[start]
    for i in range(start,end+1):
        if a[i] > pivot:
            for j in range(i+1,end+1):
                if a[j] < pivot:
                    desni = a[j]
                    levi = a[i]
                    a[i], a[j] =  desni, levi
              
    for i in range(start, end+1):
        if a[i] > pivot:
            a[start], a[i-1] =  a[i-1], a[start]
            return(i-1)
    else:
        a[start], a[end] =  a[end], a[start]
        return end
    return a 

a = [10, 4, 5, 15, 11, 2, 17, 0, 18]

# uradna rešitev
def pivot(a, start, end):
    # save pivot
    pivot = a[start]
    # save pointers
    front_i = start
    back_i = end
    # move pointers and change elements if needed
    while front_i != back_i:
        if a[front_i + 1] <= pivot:
            front_i += 1
        elif a[back_i] > pivot:
            back_i -= 1
        else:
            temp = a[front_i + 1]
            a[front_i + 1] = a[back_i]
            a[back_i] = temp
    # move pivot
    a[start] = a[front_i]
    a[front_i] = pivot
    # return the final index of pivot
    return front_i


##############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##############################################################################
def quicksort_part(a, start, end):
    if start >= end:
        return a
    else:
        p = pivot(a, start, end)
        quicksort_part(a, start, p-1)
        quicksort_part(a, p+1, end)
        return a

def quicksort(a):
    return quicksort_part(a, 0, len(a)-1)

# uradna rešitev
def quicksort_part(a, start, end):
    if start >= end:
        return
    else:
        pivot_i = pivot(a, start, end)
        quicksort_part(a, start, pivot_i - 1)
        quicksort_part(a, pivot_i + 1, end)
        return


def quicksort(a):
    quicksort_part(a, 0, len(a) - 1)
    return

##############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
##############################################################################

def kth_element(a,k):
    p = pivot(a,0,len(a)-1)
    if k == p:
        return a[p]     
    elif k > p:
        return kth_element(a[p+1:],k-p-1)
    else:
        return kth_element(a[:p],k)

# uradna rešitev
def kth_el_part(a, k, start, end):
    if start > end:
        return None
    else:
        pivot_i = pivot(a, start, end)
        if pivot_i == k:
            return a[pivot_i]
        elif pivot_i > k:
            return kth_el_part(a, k, start, pivot_i - 1)
        else:
            return kth_el_part(a, k, pivot_i + 1, end)


def kth_element(a, k):
    if k > len(a):
        return None
    else:
        return kth_el_part(a, k, 0, len(a)-1)