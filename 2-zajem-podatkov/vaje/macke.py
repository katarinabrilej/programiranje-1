import requests
import re
import os
import csv

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'cat_data'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'frontpage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'cat_data.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        # del kode, ki morda sproži napako
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        # koda, ki se izvede pri napaki
        print("Could not access page " + url)
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        return ""
    # nadaljujemo s kodo če ni prišlo do napake
    return r.text


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(url, ime_datoteke):
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''

    vsebina =  download_url_to_string(url)

    with open(ime_datoteke, 'w', encoding='utf-8') as datoteka:
            datoteka.write(vsebina)
    print('shranjeno!')
    

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, ime_datoteke):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    with open(ime_datoteke, encoding='utf-8') as datoteka:
        return datoteka.read()

# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.

def page_to_ads(directory, ime_datoteke):
    '''Split "page" to a list of advertisement blocks.'''
    vsebina = read_file_to_string(directory, ime_datoteke)
    seznam_oglasov = []
    vzorec = r'<div class="ad.*?">' + r'.*?' + r'<div class="clear"></div>'
    for ujemanje in re.finditer(vzorec, vsebina, re.DOTALL):
        nas_oglas = ujemanje.group(0)
        seznam_oglasov.append(nas_oglas)
    return seznam_oglasov
    
# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.

#r'<table><tr><td><a title="(?P<ime>\w+)'
vzorec = re.compile(
    r'</a></h3>(?P<opis>.*?) <div class="additionalInfo">.*?'
    r'<div class="price">(?P<cena>.*?)</div>.*?',
    re.DOTALL
)

#?????
def izloci_podatke_oglasa(ujemanje_oglasa):
    podatki_oglasa = ujemanje_oglasa.groupdict()
    podatki_oglasa['opis'] = podatki_oglasa['opis'].strip()
    return podatki_oglasa

podatki_oglasov = []
#to bo seznam slovarjev

#?????
def get_dict_from_ad_block(directory, filename):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    seznam_oglasov = page_to_ads(directory, filename)
    for oglas in seznam_oglasov:
        for ujemanje in vzorec.finditer(oglas):
            podatki_oglasov.append(izloci_podatke_oglasa(ujemanje))
    return podatki_oglasov

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.

#?????
def ads_from_file(directory, filename):
    '''Parse the ads in filename/directory into a dictionary list.'''
    slovar_oglasov = get_dict_from_ad_block(directory, filename)
    return slovar_oglasov

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(directory, filename,csv):
    rows =  ads_from_file(directory, filename)
    fieldnames = ["opis","cena"]
    write_csv(fieldnames, rows, directory, csv_filename)
    #return TODO
