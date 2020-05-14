# -*- coding: utf-8 -*-
"""
Created on Sat May  9 08:30:05 2020

@author: Jason
"""
import arcpy
from arcpy import env
import numpy as np
import dirfind
from datetime import datetime
from collections import Counter

# Set workspace environment
dropbox_dir = dirfind.guess_dropbox_dir()
data_dir = dropbox_dir + "KH/Data/Indonesia/klhk/"
output_wksp = dropbox_dir + "KH/Data/Indonesia/klhk/"
env.workspace = dropbox_dir + "KH/Analysis/"
env.overwriteOutput=True

##############################
## PREP KAWASAN HUTAN LAYER
##############################
# Make layer
arcpy.MakeFeatureLayer_management(data_dir + "kh_2018/kawasan_hutan.shp", "kh_lyr")
# Copy to new feature layer
arcpy.CopyFeatures_management("kh_lyr", "kh_reclass_2018.shp")
# Make layer
arcpy.MakeFeatureLayer_management("kh_reclass_2018.shp", "kh_rc_lyr")
# Reclass
fields = [f.name for f in arcpy.ListFields('kh_rc_lyr')]
arcpy.AddField_management('kh_rc_lyr', "KH", "DOUBLE")
fields = [f.name for f in arcpy.ListFields('kh_rc_lyr')]
keep_fields = ['OBJECTID','FID','Shape','FUNGSIKWS','KODEPROV','KH']
result = list((Counter(fields)-Counter(keep_fields)).elements())
arcpy.DeleteField_management ('kh_rc_lyr',result)

fieldname = "KH"
expression = "reclass(!FUNGSIKWS!)"
codeblock = """
def reclass(FUNGSIKWS):
    if (FUNGSIKWS == 1007):
        return 0
    elif (FUNGSIKWS == 5001):
        return 0
    elif (FUNGSIKWS == 5003):
        return 0
    else:
        return 1"""

arcpy.CalculateField_management("kh_rc_lyr",fieldname, expression, "PYTHON_9.3",codeblock)

#############################
# PREP FOREST RELEASE LAYER
#############################
# Make layer
arcpy.MakeFeatureLayer_management(data_dir + "PKH/Pelepasan_Kawasan_Hutan.shp", "pkh_lyr")

# Copy to new feature layer
arcpy.CopyFeatures_management("pkh_lyr", "pkh_reclass.shp")

# Make layer
arcpy.MakeFeatureLayer_management("pkh_reclass.shp", "pkh_rc_lyr")

####################
# CREATE KH LAYERS
####################

# Get list of years
rows = arcpy.SearchCursor('pkh_rc_lyr')
listYears = []
for row in rows:
    datetimeVal = row.getValue("TGLSKPLS")
    year = datetime.strftime(datetimeVal, "%m/%d/%Y")[-4:]
    listYears.append(year)
    years=list(set(listYears))
    years.sort(reverse=False)

# Get list of years
#with arcpy.da.SearchCursor("pkh_rc_lyr", "YEAR") as cursor:
#    years = set([row[0] for row in cursor])
#    years = list(years)

# remove years 2018 and 2019
I = [32,33]
years = np.delete(years, I).tolist()

# Generate yearly KH layers using back propagation
start_year = 1984
end_year = 1985

for d in range(start_year,end_year):
    print("Process starting...")
    arcpy.AddField_management('pkh_rc_lyr', "YEAR", "LONG")
    field = "YEAR"
    expression = "get_year(!TGLSKPLS!)"
    codeblock = """def get_year(date):
                    try:
                        return date.split("/")[-1]
                    except:
                        return date"""
    arcpy.CalculateField_management('pkh_rc_lyr', field, expression, "PYTHON_9.3", codeblock)
    where='"YEAR"' + " " + '>='+ " " + str(d) + " AND " '"YEAR"' + " " + '<='+ " " + str(2017)
    pkh_selected = arcpy.SelectLayerByAttribute_management("pkh_rc_lyr", "NEW_SELECTION",where)
    arcpy.CopyFeatures_management(pkh_selected,"pkh_"+ str(d) + ".shp")
    pkh = "pkh_" + str(d) + ".shp"
    arcpy.AddField_management(pkh, "KH", "DOUBLE")
    expression = "1"
    arcpy.CalculateField_management(pkh,"KH", expression, "PYTHON_9.3")
    fields = [f.name for f in arcpy.ListFields(pkh)]
    keep_fields = ['OBJECTID','FID','Shape','FUNGSIKWS','KODEPROV','KH']
    result = list((Counter(fields)-Counter(keep_fields)).elements())
    arcpy.DeleteField_management (pkh,result)
    arcpy.MakeFeatureLayer_management("kh_reclass_2018.shp", "kh_rc_lyr")
    print("Combining Kawasan Hutan and Forest Releases...")
    kh_union = arcpy.Union_analysis(['kh_rc_lyr',pkh],'kh_'+ str(d) + '.shp', "ALL", "", "")
    print("Cleaning up...")
    fieldname = "KH"
    expression = "reclass(!KH_1!,!KH!)"
    codeblock = """def reclass(KH,KH_1):
        if (KH_1 > 0):
           return 1
        else:
           return KH"""
    arcpy.CalculateField_management(kh_union,fieldname, expression, "PYTHON_9.3",codeblock)
    fields = [f.name for f in arcpy.ListFields(kh_union)]
    keep_fields = ['OBJECTID','FID','Shape','KODEPROV','KH']
    result = list((Counter(fields)-Counter(keep_fields)).elements())
    arcpy.DeleteField_management (kh_union,result)
    print("Process complete...")


