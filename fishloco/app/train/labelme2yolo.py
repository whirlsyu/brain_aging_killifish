import os
import json
import cv2
from glob import glob

IMAGE_DIR = "test_images"      # image path
JSON_DIR = "test_labelme"        # labelme input path (.json)
OUTPUT_LABELS_DIR = "test_YOLO"  # YOLO output path (.txt)

class_name_to_id = {
    "fish": 0,
}

os.makedirs(OUTPUT_LABELS_DIR, exist_ok=True)

def convert_polygon_to_bbox(points):
    """(x_min, y_min, x_max, y_max)"""
    xs = [p[0] for p in points]
    ys = [p[1] for p in points]
    return min(xs), min(ys), max(xs), max(ys)

def main():
    json_files = glob(os.path.join(JSON_DIR, "*.json"))
    print(f"Found {len(json_files)} JSON files")

    for json_path in json_files:
        with open(json_path, 'r', encoding='utf-8') as f:
            data = json.load(f)

        # get image path
        image_filename = data.get("imagePath")
        if not image_filename:
            continue

        image_path = os.path.join(IMAGE_DIR, os.path.basename(image_filename))
        if not os.path.exists(image_path):
            print(f"⚠️ 图片不存在: {image_path}")
            continue

        # get image size
        img = cv2.imread(image_path)
        if img is None:
            print(f"ERROR: {image_path}")
            continue
        h, w = img.shape[:2]

        yolo_lines = []

        for shape in data["shapes"]:
            label = shape["label"]
            shape_type = shape["shape_type"]
            points = shape["points"]

            if label not in class_name_to_id:
                print(f"WRONG CLASS NAME: '{label}'. skip")
                continue

            class_id = class_name_to_id[label]

            if shape_type == "rectangle":
                x1, y1 = points[0]
                x2, y2 = points[1]
                xmin = min(x1, x2)
                ymin = min(y1, y2)
                xmax = max(x1, x2)
                ymax = max(y1, y2)
            elif shape_type == "polygon":
                xmin, ymin, xmax, ymax = convert_polygon_to_bbox(points)
            else:
                print(f"WRONG shape_type: {shape_type}. skip")
                continue

            # to YOLO format
            x_center = (xmin + xmax) / 2 / w
            y_center = (ymin + ymax) / 2 / h
            box_w = (xmax - xmin) / w
            box_h = (ymax - ymin) / h

            # range to [0, 1]
            x_center = max(0, min(1, x_center))
            y_center = max(0, min(1, y_center))
            box_w = max(0, min(1, box_w))
            box_h = max(0, min(1, box_h))

            yolo_lines.append(f"{class_id} {x_center:.6f} {y_center:.6f} {box_w:.6f} {box_h:.6f}")

        # write file
        txt_filename = os.path.splitext(os.path.basename(json_path))[0] + ".txt"
        txt_path = os.path.join(OUTPUT_LABELS_DIR, txt_filename)
        with open(txt_path, 'w') as f:
            f.write("\n".join(yolo_lines))

        print(f"Success: {txt_path}")

    print("All finished!")

if __name__ == "__main__":
    main()