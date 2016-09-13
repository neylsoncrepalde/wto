# -*- coding: utf-8 -*-
# Data Mining atributo "Simple average final bound"
import xlrd
import csv
import os

saida = open('bound.csv', 'w')
export = csv.writer(saida, quoting=csv.QUOTE_NONNUMERIC)

path = '/home/neylson/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/WTO Tarifas'
for root,dirs,files in os.walk(path):
    xlsfiles=[ _ for _ in files if _.endswith('.xls') ]

xlsfiles.sort()

for xlsfile in xlsfiles:
    sheet = xlrd.open_workbook(os.path.join(root,xlsfile)).sheet_by_index(1)
    pais = sheet.cell_value(0,0)
    pais = pais.replace('(28)','')
    pais = pais.replace(' ','')
    pais = pais.replace(',','')
    pais = pais.replace('.','')
    pais = pais.replace('\'','')
    
    #fazendo a lista de paises para 
        
    tarifa_bound = sheet.cell_value(3,3)
    
    
    if xlsfile == xlsfiles[0]:
        export.writerow(["pais","simple.average.final.bound"])
        export.writerow([pais, tarifa_bound])                
    else:
        export.writerow([pais, tarifa_bound])

saida.close()
print('Acabou!')
