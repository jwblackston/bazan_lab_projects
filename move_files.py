#This function will sort through *** imaging result data and return the files you want
#modify source (src_path), the identifier of the files you want (txt_id), and where you want them to go (dest_path) on your computer

import shutil
import os

dest = 'C:/Users/jbla12/Desktop/R Analyses/p65_project/sum_files/'
src = 'C:/Users/jbla12/Desktop/R Analyses/p65_project/'
txt_id = 'statistics_Intensity_Sum_Ch=3_Img=1.csv'
def move_files(txt_id, src, dest):
    #src_path is the original file(s) destination
    #dest_path is the destination for the files to end up in
    #txt_id is what the files end with that you want to ID
    for foldername, subfolders, filenames in os.walk(src):
        for file in filenames:
            if file.endswith(txt_id):
                shutil.copy(os.path.join(foldername, file), dest)
    print('Your files are ready sir/madam!')

if __name__ == "__main__":
    move_files(txt_ID, src, dest)
