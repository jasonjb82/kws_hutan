# -*- coding: utf-8 -*-
"""
Created on Tue May 12 09:29:08 2020

@author: Jason
"""

import arcpy
from arcpy import env
import numpy as np
import dirfind

# Set workspace environment
dropbox_dir = dirfind.guess_dropbox_dir()
data_dir = dropbox_dir + "KH/Data/Indonesia/klhk/"
output_wksp = dropbox_dir + "KH/Data/Indonesia/klhk/"
env.workspace = dropbox_dir + "KH/Data/Indonesia/klhk/"
env.overwriteOutput=True

# Make feature layer
arcpy.MakeFeatureLayer_management(data_dir + "PKH/Pelepasan_Kawasan_Hutan.shp", "pkh_lyr")
arcpy.MakeFeatureLayer_management(data_dir + "kh_2019/kawasan_hutan.shp", "kh_lyr")


# Intersect
inFeatures = ['kh_lyr','pkh_lyr']
unionOutput = dropbox_dir + "KH/Analysis/pkh_kh_union.shp"
arcpy.Union_analysis(inFeatures, unionOutput, "ALL", "", "")
