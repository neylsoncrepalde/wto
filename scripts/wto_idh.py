# -*- coding: utf-8 -*-
# Data Mining atributo "IDH"
import xlrd
import csv
import os

os.chdir('/home/giars/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/idh')
saida = open('idh.csv', 'w')
export = csv.writer(saida, quoting=csv.QUOTE_NONNUMERIC)


sheet = xlrd.open_workbook('/home/giars/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/idh/PNUD IDH com UNIAO E.xls').sheet_by_index(0)

for linha in range(162):
    pais = sheet.cell_value(linha,1)
    pais = pais.replace('(28)','')
    pais = pais.replace(' ','')
    pais = pais.replace(',','')
    pais = pais.replace('.','')
    pais = pais.replace('\'','')
    pais = pais.replace('(','')
    pais = pais.replace(')','')
    pais = pais.replace('-','')
    
    idh = sheet.cell_value(linha,13)
    
    #Escrevendo no .csv

    export.writerow([pais, idh])

saida.close()
print('Acabou!')
