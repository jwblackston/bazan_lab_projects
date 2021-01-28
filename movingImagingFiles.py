#This function will sort through IMARIS imaging result data and return the files you want
#modify source (src), the identifier of the files you want (txt_ID), and where you want them to go (dest) on your computer

import shutil
import os

dest = 'C:/Users/jbla12/Desktop/R Analyses/p65_project/sum_files/'
src = 'C:/Users/jbla12/Desktop/R Analyses/p65_project/'
txt_ID = 'statistics_Intensity_Sum_Ch=3_Img=1.csv'
def moveSpecFiles(txt_ID, src, dest):
    #src is the original file(s) destination
    #dest is the destination for the files to end up in
    #spec_txt is what the files end with that you want to ID
    for foldername, subfolders, filenames in os.walk(src):
        for file in filenames:
            if file.endswith(txt_ID):
                shutil.copy(os.path.join(foldername, file), dest)
    print('Your files are ready sir/madam!')


moveSpecFiles(txt_ID, src, dest)
