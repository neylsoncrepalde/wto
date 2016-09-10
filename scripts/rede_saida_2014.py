# -*- coding: utf-8 -*-
# Data Mining
import xlrd
import csv
import os

saida = open('rede_entrada.csv', 'w')
saida2 = open('atributos.csv', 'w')
export = csv.writer(saida, quoting=csv.QUOTE_NONNUMERIC)
atributos = csv.writer(saida2, quoting=csv.QUOTE_NONNUMERIC)

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
    
    #fazendo a lista de paises para 
        
    pais1 = sheet.cell_value(52,0)
    pais2 = sheet.cell_value(53,0)
    pais3 = sheet.cell_value(54,0)
    pais4 = sheet.cell_value(55,0)
    pais5 = sheet.cell_value(56,0)
    
    paises = [pais1, pais2, pais3, pais4, pais5]
    
    #
    for x in range(len(paises)):
        paises[x] = paises[x][3:]
        paises[x] = paises[x].replace('(28)','')
        paises[x] = paises[x].replace(' ','')
        paises[x] = paises[x].replace(',','')
        paises[x] = paises[x].replace('.','')
        paises[x] = paises[x].replace('\'','')
    
    
    value1 = sheet.cell_value(52,5)
    value2 = sheet.cell_value(53,5)
    value3 = sheet.cell_value(54,5)
    value4 = sheet.cell_value(55,5)
    value5 = sheet.cell_value(56,5)
    
    values = [value1, value2, value3, value4, value5]
    
    pop = sheet.cell_value(5,0)
    GDP = sheet.cell_value(6,0)
    GDP_ppp = sheet.cell_value(7,0)
    balance = sheet.cell_value(8,0)
    trade_per_capita = sheet.cell_value(9,0)
    trade_GDP = sheet.cell_value(10,0)
    total_exp = sheet.cell_value(39,0)
    total_imp = sheet.cell_value(40,0)
    share = sheet.cell_value(43,0)
    
    atrib_names = ['pais',pop, GDP, GDP_ppp, balance, trade_per_capita, 
                   trade_GDP, total_exp, total_imp, share]
    
    val1 = sheet.cell_value(5,5)
    val2 = sheet.cell_value(6,5)
    val3 = sheet.cell_value(7,5)
    val4 = sheet.cell_value(8,5)
    val5 = sheet.cell_value(9,5)
    val6 = sheet.cell_value(10,5)
    val7 = sheet.cell_value(39,5)
    val8 = sheet.cell_value(40,5)
    val9 = sheet.cell_value(43,5)
    
    atrib_values = [pais, val1, val2, val3, val4, val5, val6, val7, val8,
                    val9]
    
    
    for row in range(len(paises)):
        export.writerow([pais, paises[row], values[row]])


# os nomes das variáveis ainda não aparecem
    if xlsfile == xlsfiles[0]:
        atributos.writerows(atrib_names, atrib_values)
    else:
        atributos.writerow(atrib_values)
        
    

print('Acabou!')
