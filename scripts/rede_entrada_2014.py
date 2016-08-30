# -*- coding: utf-8 -*-
# Data Mining
import xlrd
import csv
import os

saida = open('rede_entrada.csv', 'w')
export = csv.writer(saida, quoting=csv.QUOTE_NONNUMERIC)

path = '/home/neylson/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/2014'
for root,dirs,files in os.walk(path):
    xlsfiles=[ _ for _ in files if _.endswith('.xls') ]

for xlsfile in xlsfiles:
    sheet = xlrd.open_workbook(os.path.join(root,xlsfile)).sheet_by_index(0)
    pais = sheet.cell_value(1,0)
    pais = pais.replace('(28)','')
    pais = pais.replace(' ','')
    pais = pais.replace(',','')
    pais = pais.replace('.','')
    pais = pais.replace('\'','')
    print(pais)
    
    #fazendo a lista de paises para 
        
    pais1 = (sheet.cell_value(52,0))
    pais2 = (sheet.cell_value(53,0))
    pais3 = (sheet.cell_value(54,0))
    pais4 = (sheet.cell_value(55,0))
    pais5 = (sheet.cell_value(56,0))
    
    paises = [pais1, pais2, pais3, pais4, pais5]
    
    #
    for x in range(len(paises)):
        paises[x] = paises[x][3:]
        paises[x] = paises[x].replace('(28)','')
        paises[x] = paises[x].replace(' ','')
        paises[x] = paises[x].replace(',','')
        paises[x] = paises[x].replace('.','')
        paises[x] = paises[x].replace('\'','')
    
    
    value1 = (sheet.cell_value(52,5))
    value2 = (sheet.cell_value(53,5))
    value3 = (sheet.cell_value(54,5))
    value4 = (sheet.cell_value(55,5))
    value5 = (sheet.cell_value(56,5))
    
    values = [value1, value2, value3, value4, value5]
    
    rede_entrada = {paises[0]: value1, paises[1]: value2,
                    paises[2]: value3, paises[3]: value4,
                    paises[4]: value5}
    rede_entrada
    
    
    for row in range(len(paises)):
        export.writerow([pais, paises[row], values[row]])
    

print('Acabou!')
