import os
import shutil
import random
from pathlib import Path


IMAGE_DIR = "dataset/images"
LABEL_DIR = "dataset/labels"
OUTPUT_DIR = "dataset_yolo"

TRAIN_RATIO = 0.8  # 80% train and 20% validation

# create folder
for split in ["images/train", "images/val", "labels/train", "labels/val"]:
    os.makedirs(os.path.join(OUTPUT_DIR, split), exist_ok=True)

# get all images
image_files = [f for f in os.listdir(IMAGE_DIR) if f.endswith(('.jpg', '.png'))]
random.shuffle(image_files)

split_idx = int(len(image_files) * TRAIN_RATIO)
train_files = image_files[:split_idx]
val_files = image_files[split_idx:]

def copy_files(file_list, split):
    for img_file in file_list:
        src_img = os.path.join(IMAGE_DIR, img_file)
        dst_img = os.path.join(OUTPUT_DIR, "images", split, img_file)
        shutil.copy(src_img, dst_img)

        label_file = os.path.splitext(img_file)[0] + ".txt"
        src_lbl = os.path.join(LABEL_DIR, label_file)
        dst_lbl = os.path.join(OUTPUT_DIR, "labels", split, label_file)
        if os.path.exists(src_lbl):
            shutil.copy(src_lbl, dst_lbl)
        else:
            open(dst_lbl, 'w').close()

copy_files(train_files, "train")
copy_files(val_files, "val")

print(f"All finished! Train: {len(train_files)} val: {len(val_files)}")