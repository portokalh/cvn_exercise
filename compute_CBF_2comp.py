import nibabel as nib
import numpy as np
import os
import matplotlib.pyplot as plt
from skimage.restoration import denoise_bilateral
import nibabel as nib
import numpy as np
import os
import subprocess
from skimage.restoration import denoise_bilateral


# === Parameters for manganese-enhanced imaging ===
FIELD_STRENGTH = '7T'

if FIELD_STRENGTH == '7T':
    T1b = 1.2   # blood (s) - estimated
    T1t = 0.6   # tissue (s) - measured
    alpha = 0.7
else:
    raise ValueError("Update parameters for other field strengths.")

lambda_blood = 0.9  # mL/g
tau = 3.0           # label duration (s)
PLD = 0.3           # post-labeling delay (s)

# === Set paths ===
paros = os.environ.get("PAROS", "/mnt/newStor/paros")
asl_path = f"{paros}/paros_WORK/aashika/perfusion_032425/input/co_reg_A24080507_perfusion_stack.nii.gz"
filtered_asl_path= f"{paros}/paros_WORK/aashika/perfusion_032425/input/co_reg_A24080507_perfusion_stack_filtered.nii.gz"
stc_asl_path = f"{paros}/paros_WORK/aashika/perfusion_032425/stc_A24080507.nii.gz"
deltaM_path = f"{paros}/paros_WORK/aashika/perfusion_032425/deltaM_A24080507.nii.gz"
estimated_M0_path = f"{paros}/paros_WORK/aashika/perfusion_032425/co_reg_A24080507_M0.nii.gz"
cbf_output_path = f"{paros}/paros_WORK/aashika/perfusion_032425/CBF_A24080507_2comp.nii.gz"
PLD_fraction_file = f"{paros}/paros_WORK/aashika/perfusion_032425/PLD_fraction_4p5.txt"
cbf_clip_path=f"{paros}/paros_WORK/aashika/perfusion_032425/CBF_A24080507_clip.nii.gz"


# === Step 1: Load ASL data ===
asl_img = nib.load(asl_path)
asl_data = asl_img.get_fdata()
affine = asl_img.affine
header = asl_img.header

'''
# === Step 2: Apply bilateral filter slice-wise per timepoint ===
filtered_asl = np.zeros_like(asl_data)
for t in range(asl_data.shape[3]):
    for z in range(asl_data.shape[2]):
        slice_img = asl_data[:, :, z, t]
        filtered_slice = denoise_bilateral(slice_img, sigma_color=3, sigma_spatial=1.5, channel_axis=None)
        filtered_asl[:, :, z, t] = filtered_slice

# === Step 3: Save filtered ASL ===
filtered_img = nib.Nifti1Image(filtered_asl, affine, header)
nib.save(filtered_img, filtered_asl_path)
print(f"Filtered ASL saved to: {filtered_asl_path}")

'''

'''
# === Step 1: Slice Timing Correction with FSL ===

print("🌀 Running FSL slicetimer...")

cmd = f'slicetimer -r 4.5 --tcustom={PLD_fraction_file} -i {filtered_asl_path} -o {stc_asl_path}'

print(cmd)
subprocess.run(cmd, shell=True, executable='/bin/bash', check=True)
print("✅ Slice timing correction complete.")
'''

# === Step 5: Load ΔM and M0 ===
#deltaM_data = nib.load(deltaM_path).get_fdata()
#M0_estimated = nib.load(estimated_M0_path).get_fdata()

# === Step 4: Estimate M0 from control images ===
# Assuming odd timepoints are control (index 1, 3, 5...)
stc_asl=nib.load(stc_asl_path).get_fdata()

control_volumes = stc_asl[:, :, :, 1::2]
label_volumes = stc_asl[:, :, :, 0::2]

M0_estimated = np.mean(control_volumes, axis=3)
M0_img = nib.Nifti1Image(M0_estimated, affine, header)
nib.save(M0_img, estimated_M0_path)
print(f"M0 estimated and saved to: {estimated_M0_path}")

# === Step 5: Estimate deltaM (control - label) ===
# Assuming even index = label, odd index = control

n_pairs = min(control_volumes.shape[3], label_volumes.shape[3])
deltaM = np.mean(control_volumes[:, :, :, :n_pairs] - label_volumes[:, :, :, :n_pairs], axis=3)
deltaM_img = nib.Nifti1Image(deltaM, affine, header)
nib.save(deltaM_img, deltaM_path)
print(f"deltaM estimated and saved to: {deltaM_path}")




# === Step 6: Create brain mask ===
brain_mask = M0_estimated > 6
print(f"Brain voxels in mask: {np.sum(brain_mask)}")

# === Step 7: Two-compartment CBF calculation ===
denom_blood = T1b * (1 - np.exp(-tau / T1b)) * np.exp(-PLD / T1b)
denom_tissue = T1t * (1 - np.exp(-tau / T1t)) * np.exp(-PLD / T1t)
denominator = 2 * alpha * M0_estimated * (denom_blood + denom_tissue)

numerator = 6000 * lambda_blood * deltaM
cbf_2comp = numerator / (denominator + 1e-8)
cbf_2comp[~brain_mask] = 0


cbf_clip=cbf_2comp
cbf_clip[~brain_mask] = 0
cbf_clip = np.clip(cbf_2comp, 0, 250)

# === Step 8: Save CBF 2-compartment map ===
cbf_img = nib.Nifti1Image(cbf_2comp, asl_img.affine, asl_img.header)
nib.save(cbf_img, cbf_output_path)
print(f"Two-compartment CBF map saved to: {cbf_output_path}")

cbf_img_clip = nib.Nifti1Image(cbf_clip, asl_img.affine, asl_img.header)
nib.save(cbf_img_clip, cbf_clip_path)
print(f"Clipped CBF map saved to: {cbf_clip_path}")

