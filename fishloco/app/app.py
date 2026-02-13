"""
    Main track app
"""

import cv2
import os
import csv
import queue
import time
import numpy as np
from ultralytics import YOLO
import threading
from tqdm import tqdm
import logging
from accelerate import instantaneous_accelerate

class FishTrackerYolo:
    def __init__(self, device_preference="auto"):
        """ 
            :param device_preference: "auto", "cuda", "cpu"
        """
        logging.getLogger("ultralytics").setLevel(logging.WARNING)

        self.video = None   # video file
        self.video_fps = 0   # video fps
        self.video_labeled = None   # output video file
        self.frame_count = 1   # fps count
        self.csv_file = None   # output csv file

        # ROI
        self.roi_points = []    # (x, y)
        self.roi_defined = False
        self.roi_transform_matrix = None

        # use another thread to write csv file
        self.csv_queue = queue.Queue()
        self.csv_thread = None
        self.csv_file = None
        self.csv_writer = None
        self.csv_path = None
        
        # CSV writing control
        self.csv_writing_active = False
        self.csv_file_open = False
        
        # CSV batch control
        self.csv_batch_size = 100
        self.csv_flush_interval = 1.0
        self.csv_last_flush = time.time()

        # init model
        self.device = self._select_device(device_preference)
        self.model = YOLO("best.pt", verbose=False)
        self.model.to(self.device)
        print(f"use device: {self.device}")
        
        self.threshold = 0.7

    def _select_device(self, preference):
        """ select device """
        if preference == "cpu":
            return "cpu"
        
        if preference == "cuda":
            try:
                import torch
                if torch.cuda.is_available():
                    return "cuda:0"
                else:
                    print("CUDA is unavilable, use CPU instead")
                    return "cpu"
            except ImportError:
                print("PyTorch or CUDA is unavilable, use CPU instead")
                return "cpu"
        
        if preference == "auto":
            try:
                import torch
                if torch.cuda.is_available():
                    print("use CUDA")
                    return "cuda:0"
            except ImportError:
                pass
            
            print("use CPU")
            return "cpu"
        
        return "cpu"

    def on_mouse_click(self, event, x, y, flags, param):
        """ mouse click to select ROI """
        if event == cv2.EVENT_LBUTTONDOWN:
            if len(self.roi_points) < 4:
                self.roi_points.append((x, y))
                print(f"{len(self.roi_points)}: ({x}, {y})")
                
                if len(self.roi_points) == 4:
                    self.roi_defined = True
                    src_pts = np.float32(self.roi_points)
                    dst_pts = np.float32([[0, 0], [1, 0], [1, 1], [0, 1]])
                    self.roi_transform_matrix = cv2.getPerspectiveTransform(src_pts, dst_pts)
            else:
                # 5 on click clear ROI
                self.roi_points.clear()
                self.roi_defined = False
                self.roi_transform_matrix = None
                print("ROI Cleared")

    def draw_roi(self, frame):
        """ draw ROI """
        for i, point in enumerate(self.roi_points):
            cv2.circle(frame, point, 5, (0, 255, 0), -1)
            cv2.putText(frame, str(i+1), (point[0]+5, point[1]-5), 
                       cv2.FONT_HERSHEY_SIMPLEX, 0.7, (0, 255, 0), 2)
        
        if len(self.roi_points) >= 2:
            for i in range(len(self.roi_points)):
                next_idx = (i + 1) % len(self.roi_points)
                if len(self.roi_points) == 4 or i < len(self.roi_points) - 1:
                    cv2.line(frame, self.roi_points[i], self.roi_points[next_idx], (0, 0, 255), 2)
        
        if self.roi_defined and len(self.roi_points) == 4:
            pts = np.array(self.roi_points, np.int32)
            pts = pts.reshape((-1, 1, 2))
            cv2.polylines(frame, [pts], isClosed=True, color=(0, 0, 255), thickness=2)
        
        return frame

    def annotate_roi(self, frame):
        """ ROI annotate """
        cv2.namedWindow("ROI Annotation", cv2.WINDOW_NORMAL)
        cv2.setMouseCallback("ROI Annotation", self.on_mouse_click)
        
        display_frame = frame.copy()
        
        while True:
            display_frame = self.draw_roi(frame.copy())
            cv2.imshow("ROI Annotation", display_frame)
            
            key = cv2.waitKey(1) & 0xFF
            if key == 27:  # ESC button
                if not self.roi_defined:
                    print("use image size to ROI size")
                    height, width = frame.shape[:2]
                    self.roi_points = [(0, 0), (width, 0), (width, height), (0, height)]
                    self.roi_defined = True
                    src_pts = np.float32(self.roi_points)
                    dst_pts = np.float32([[0, 0], [1, 0], [1, 1], [0, 1]])
                    self.roi_transform_matrix = cv2.getPerspectiveTransform(src_pts, dst_pts)
                break
            elif key != 255:
                if self.roi_defined:
                    break
        
        # close window
        cv2.destroyWindow("ROI Annotation")
        
        return self.roi_defined

    def open_video(self, video_path: str):
        """ open video file and init output files """
        try:
            self.video = cv2.VideoCapture(video_path)
        except:
            raise Exception(f"ERROR on open file: {video_path}")
        
        # get fps
        self.video_fps = self.video.get(cv2.CAP_PROP_FPS)
        print(self.video_fps)
        print(f"""{video_path}
            FPS: {self.video_fps},
            Width: {int(self.video.get(cv2.CAP_PROP_FRAME_WIDTH))},
            Height: {int(self.video.get(cv2.CAP_PROP_FRAME_HEIGHT))},
            Total frames: {int(self.video.get(cv2.CAP_PROP_FRAME_COUNT))},
            Length: {int(self.video.get(cv2.CAP_PROP_FRAME_COUNT)) / self.video_fps:.2f} s
        """)
        
        # init output video file
        if not os.path.exists("labeled_videos"):
            os.makedirs("labeled_videos")
        
        fourcc = cv2.VideoWriter_fourcc(*'XVID')
        self.video_labeled = cv2.VideoWriter(os.path.join("labeled_videos", f"{os.path.basename(video_path).split('.')[0]}_labeled.avi"), fourcc, self.video_fps, (int(self.video.get(cv2.CAP_PROP_FRAME_WIDTH)), int(self.video.get(cv2.CAP_PROP_FRAME_HEIGHT))))

        # init output csv file
        if not os.path.exists("trajectories"):
            os.makedirs("trajectories")
        
        self.csv_path = os.path.join("trajectories", f"{os.path.basename(video_path).split('.')[0]}.csv")
        self.csv_file = open(self.csv_path, "w", newline="")
        self.csv_writer = csv.writer(self.csv_file)
        self.csv_writer.writerow([
            "frame", "class_id", "confidence", 
            "x1", "y1", "w", "h", 
            "cx", "cy", 
            "roi_x", "roi_y"
        ])
        self.csv_file_open = True
        
        # start csv writing thread
        self.csv_writing_active = True
        self.csv_last_flush = time.time()
        self.csv_thread = threading.Thread(target=self.write_csv_worker)
        self.csv_thread.daemon = True
        self.csv_thread.start()


    def detect_frame(self, frame):
        results = self.model(frame, device=self.device)
        
        # draw ROI
        if self.roi_defined and len(self.roi_points) == 4:
            pts = np.array(self.roi_points, np.int32)
            pts = pts.reshape((-1, 1, 2))
            cv2.polylines(frame, [pts], isClosed=True, color=(0, 0, 255), thickness=2)
        
        # draw result
        for result in results:
            boxes = result.boxes
            if len(boxes) >= 1:  # over a result
                max_conf = 0
                for box in boxes:   # get most prob result
                    if float(box.conf) > max_conf:
                        max_conf = float(box.conf)
            for box in boxes:
                if (float(box.conf) >= self.threshold) and (float(box.conf) == max_conf):
                # if (float(box.conf) >= self.threshold):
                    x1, y1, x2, y2 = map(int, box.xyxy[0])
                    w, h = x2 - x1, y2 - y1
                    cx, cy = x1 + w//2, y1 + h//2
                    
                    rel_x, rel_y = None, None
                    if self.roi_defined and self.roi_transform_matrix is not None:
                        pt = np.array([[cx, cy]], dtype=np.float32)
                        pt = np.array([pt])
                        dst_pt = cv2.perspectiveTransform(pt, self.roi_transform_matrix)
                        rel_x, rel_y = dst_pt[0][0]
                        
                        rel_x = max(0, min(1, rel_x))
                        rel_y = max(0, min(1, rel_y))
                    
                    csv_data = [
                        self.frame_count,
                        int(box.cls),
                        float(box.conf),
                        x1, y1, w, h,
                        cx, cy,
                        rel_x, rel_y
                    ]
                    self.add_csv_data(csv_data)
                    
                    cv2.rectangle(frame, (x1, y1), (x2, y2), (0, 255, 0), 2)
                    cv2.putText(frame, f"{result.names[int(box.cls)]} {float(box.conf):.2f}", 
                                (x1, y1 - 5), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 255, 0), 2)
                    
                    if rel_x is not None and rel_y is not None:
                        cv2.putText(frame, f"ROI: ({rel_x:.3f}, {rel_y:.3f})", 
                                    (x1, y1 + h + 15), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 255, 0), 2)
        
        status_text = f"Frame: {self.frame_count}"
        if self.roi_defined:
            status_text += " | ROI: Active"
        else:
            status_text += " | ROI: Not Defined"
        cv2.putText(frame, status_text, (10, 30), 
                    cv2.FONT_HERSHEY_SIMPLEX, 0.7, (255, 255, 255), 2)
        
        return frame
    
    def detect_in_video(self):
        """ track fish """

        # get total frames
        total_frames = int(self.video.get(cv2.CAP_PROP_FRAME_COUNT))
        if total_frames <= 0:
            print("WRONG: get total frames error, no progress will be shown")
            use_tqdm = False
        else:
            use_tqdm = True

        ret, frame = self.video.read()
        if not ret:
            raise Exception("ERROR")

        roi_defined = self.annotate_roi(frame)
        if not roi_defined:
            height, width = frame.shape[:2]
            self.roi_points = [(0, 0), (width, 0), (width, height), (0, height)]
            self.roi_defined = True
            src_pts = np.float32(self.roi_points)
            dst_pts = np.float32([[0, 0], [1, 0], [1, 1], [0, 1]])
            self.roi_transform_matrix = cv2.getPerspectiveTransform(src_pts, dst_pts)

        self.frame_count = 0

        # create tqdm progress bar
        if use_tqdm:
            pbar = tqdm(total=total_frames, desc="Processing frames", unit="frame")
        else:
            pbar = None

        self.frame_count += 1
        frame = self.detect_frame(frame)
        self.video_labeled.write(frame)
        if pbar:
            pbar.update(1)

        while self.video.isOpened():
            ret, frame = self.video.read()
            if not ret:
                break
            self.frame_count += 1
            frame = self.detect_frame(frame)
            self.video_labeled.write(frame)
            if pbar:
                pbar.update(1)

        if pbar:
            pbar.close()

        # stop csv writer and video writer
        self.stop_csv_writer()
        self.video.release()
        if self.video_labeled is not None:
            self.video_labeled.release()
    
    def write_csv_worker(self):
        """ csv writing """
        batch = []
        
        while self.csv_writing_active:
            try:
                data = self.csv_queue.get(timeout=0.5)
                
                batch.append(data)
                
                current_time = time.time()
                if (len(batch) >= self.csv_batch_size or 
                    (len(batch) > 0 and current_time - self.csv_last_flush >= self.csv_flush_interval)):
                    
                    try:
                        self.csv_writer.writerows(batch)
                        self.csv_file.flush()
                        batch = []
                        self.csv_last_flush = current_time
                    except Exception as e:
                        print(f"CSV writing ERROR: {e}")
                        
            except queue.Empty:
                current_time = time.time()
                if batch and current_time - self.csv_last_flush >= self.csv_flush_interval:
                    try:
                        self.csv_writer.writerows(batch)
                        self.csv_file.flush()
                        batch = []
                        self.csv_last_flush = current_time
                    except Exception as e:
                        print(f"CSV writing ERROR: {e}")
                continue
        
        if batch:
            try:
                self.csv_writer.writerows(batch)
                self.csv_file.flush()
            except Exception as e:
                print(f"CSV writing ERROR: {e}")
    
    def add_csv_data(self, data):
        """ add data to csv writer queue """
        if self.csv_file_open and self.csv_writing_active:
            self.csv_queue.put(data)
    
    def stop_csv_writer(self):
        """ stop csv writing and save file """
        self.csv_writing_active = False
        
        if self.csv_thread and self.csv_thread.is_alive():
            self.csv_thread.join(timeout=2.0)
            if self.csv_thread.is_alive():
                print("ERROR: csv writer over time")
        
        # close csv file
        if self.csv_file and self.csv_file_open:
            try:
                self.csv_file.close()
                self.csv_file_open = False
                print("CSV file closed.")
            except Exception as e:
                print(f"CSV file close ERROR: {e}")
    
    def __del__(self):
        """ release all resources """
        self.stop_csv_writer()
        if hasattr(self, 'video_labeled') and self.video_labeled is not None:
            self.video_labeled.release()
        if hasattr(self, 'video') and self.video is not None:
            self.video.release()

    def run_tracker(self, video_path):
        """ fish tracker
            :return: csv file path
        """
        # 1. open file
        self.open_video(video_path)
        # 2. start detect
        self.detect_in_video()
        return self.csv_path

def main(input_file):
    """ Main function """
    tracker = FishTrackerYolo()
    csv_path = tracker.run_tracker(input_file)


if __name__ == "__main__":
    input_files = [
        "fish_video_20251229_070006.avi",
    ]

    for file in input_files:
        main(file)
        print(f"##### success: {file}  #####")
    