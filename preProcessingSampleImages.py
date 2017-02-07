# PreProcessing sample images for patient. 
# I have not invented anything, used this link: https://www.kaggle.com/gzuidhof/data-science-bowl-2017/full-preprocessing-tutorial

# 1. Loads Scans
# 2. Convert Pixels to Hounsfield units (HU)
# 3. Plots 3D figure of Lungs

import numpy as np
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import dicom
import os
import scipy.ndimage
import matplotlib.pyplot as plt
import sys
from skimage import measure, morphology
from mpl_toolkits.mplot3d.art3d import Poly3DCollection

# Inital Setup
sys.path.insert(1, 'C:/Python27/Lib/site-packages')
INPUT_FOLDER = './input/sample_images/'

# Patients ID number
patients = os.listdir(INPUT_FOLDER)
patients.sort()
# print patients


"""
 + Dicom is the standard in medical imaging.
 + File has metadata (pixel size, How long one pixel is in every dimension in the real world)
 + Distance between slices may differ (Non uniform slices) CNN (Convolution Neural Network) will not work.
"""



def load_scan(path):
    slices = [dicom.read_file(path+'/'+s) for s in os.listdir(path)]
    slices.sort(key = lambda x: int(x.InstanceNumber))
    try:
        slice_thickness = np.abs(slices[0].ImagePositionPatient[2] - slices[1].ImagePositionPatient[2])
    except:
        slice_thickness = np.abs(slices[0].SliceLocation - slices[1].SliceLocation)
    for s in slices:
        s.SliceThickness = slice_thickness
    return slices



def get_pixels_hu(scans):
    image = np.stack([s.pixel_array for s in scans])
    # Convert to int16
    image = image.astype(np.int16)

    # Set outside of scan pixels to 0
    # the intercept is usually -1024, so air is approx. 0
    image[image == -2000] = 0

    # Convert to Hounsfield units (HU)
    intercept = scans[0].RescaleIntercept
    slope = scans[0].RescaleSlope

    if slope != 1:
        image = slope * image.astype(np.float64)
        image = image.astype(np.int16)

    image += np.int16(intercept)
    return np.array(image, dtype=np.int16)



def resample(image, scan, new_spacing=[1,1,1]):
    # Determine current pixel spacing
    spacing = map(float, ([scan[0].SliceThickness] + scan[0].PixelSpacing))
    spacing = np.array(list(spacing))

    resize_factor = spacing / new_spacing
    new_real_shape = image.shape * resize_factor
    new_shape = np.round(new_real_shape)
    real_resize_factor = new_shape / image.shape
    new_spacing = spacing / real_resize_factor

    image = scipy.ndimage.interpolation.zoom(image, real_resize_factor, mode='nearest')
    return image, new_spacing


def plot_3d(image, threshold=-300):
    # Position the scan upright,
    # so the head of the patient would be at the top facing the camera
    p = image.transpose(2, 1, 0)

    verts, faces = measure.marching_cubes(p, threshold)

    fig = plt.figure(figsize=(10, 10))
    ax = fig.add_subplot(111, projection='3d')

    # Fancy indexing: `verts[faces]` to generate a collection of triangles
    mesh = Poly3DCollection(verts[faces], alpha=0.1)
    face_color = [0.5, 0.5, 1]
    mesh.set_facecolor(face_color)
    ax.add_collection3d(mesh)

    ax.set_xlim(0, p.shape[0])
    ax.set_ylim(0, p.shape[1])
    ax.set_zlim(0, p.shape[2])

    plt.show()


first_patient = load_scan(INPUT_FOLDER + patients[0])
first_patient_pixels = get_pixels_hu(first_patient)
plt.hist(first_patient_pixels.flatten(), bins=80, color='c')
plt.xlabel("Housfield Units (HU)")
plt.ylabel("Frequency")
plt.show()

plt.imshow(first_patient_pixels[80], cmap=plt.cm.gray)
plt.show()


pix_resampled, spacing = resample(first_patient_pixels, first_patient, [1,1,1])
print("Shape before resampling\t", first_patient_pixels.shape)
print("Shape after resampling\t", pix_resampled.shape)

plot_3d(pix_resampled, 400)
