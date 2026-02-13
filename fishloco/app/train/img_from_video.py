import cv2
import os

def extract_frames(video_path, output_folder, interval_seconds):
    """
    extract frame images from video

    Params:
        video_path (str): video path
        output_folder (str): images sorge path
        interval_seconds (float): interval time (second)
    """
    # create folder
    os.makedirs(output_folder, exist_ok=True)

    video_name = os.path.basename(video_path).split(".")[0]

    # open video file
    cap = cv2.VideoCapture(video_path)
    if not cap.isOpened():
        print(f"ERROR: {video_path}")
        return

    # get video fps
    fps = cap.get(cv2.CAP_PROP_FPS)
    if fps <= 0:
        print("FPS ERROR")
        fps = 30.0

    frame_interval = int(fps * interval_seconds)

    frame_count = 0
    saved_count = 0

    while True:
        ret, frame = cap.read()
        if not ret:
            break

        if frame_count % frame_interval == 0:
            filename = os.path.join(output_folder, f"{video_name}_{saved_count:06d}.jpg")
            cv2.imwrite(filename, frame)
            print(f"Saved: {filename}")
            saved_count += 1

        frame_count += 1

    cap.release()
    print(f"Finish! Saved {saved_count} images to {output_folder}")


if __name__ == "__main__":

    output_dir = "test_images" # output folder
    interval = 30   # 30 seconds extract a frame

    video_files = [ # put your video files here
        
    ]

    for video_file in video_files:
        extract_frames(video_file, output_dir, interval)
