#!/bin/env python3

import cv2
import numpy as np
import argparse
import os
from skimage.metrics import structural_similarity as ssim
import time

def open_video(video_path):
    cap = cv2.VideoCapture(video_path)

    if not cap.isOpened():
        print(f"Error opening video file {video_path}")
        return None

    return cap

def get_video_info(cap):
    frame_rate = cap.get(cv2.CAP_PROP_FPS)
    frame_width = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    frame_height = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    total_frames = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))

    print(f"Video information: Frame rate: {frame_rate}, Frame width: {frame_width}, Frame height: {frame_height}, Total frames: {total_frames}")

    return frame_rate, frame_width, frame_height, total_frames

def compare_frames(frame1, frame2):
    grayA = cv2.cvtColor(frame1, cv2.COLOR_BGR2GRAY)
    grayB = cv2.cvtColor(frame2, cv2.COLOR_BGR2GRAY)
    score = ssim(grayA, grayB)

    return score

def extract_slides(video_path, output_prefix, ssim_threshold, consecutive_frames):
    cap = open_video(video_path)
    if cap is None:
        return

    frame_rate, frame_width, frame_height, total_frames = get_video_info(cap)

    ret, comparison_frame = cap.read()
    if not ret:
        print("Error reading the first frame")
        return

    frame_count = 1
    similar_frames = 0
    slide_count = 0
    start_time = time.time()

    while cap.isOpened():
        ret, frame = cap.read()

        if not ret:
            break

        frame_count += 1

        if compare_frames(comparison_frame, frame) >= ssim_threshold:
            similar_frames += 1
            if similar_frames == consecutive_frames:
                timestamp = cap.get(cv2.CAP_PROP_POS_MSEC) / 1000
                cv2.imwrite(f"{output_prefix}-{timestamp}.png", comparison_frame)
                slide_count += 1
                print(f"Extracted slide {slide_count} at timestamp {timestamp}s")
        else:
            comparison_frame = frame
            similar_frames = 0

        # Progress indicator
        if time.time() - start_time >= 1:  # Update progress every second of real time
            progress = (frame_count / total_frames) * 100
            print(f"Progress: {progress:.2f}%, Frame: {frame_count}")
            start_time = time.time()

    cap.release()
    cv2.destroyAllWindows()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Extract slides from video.')
    parser.add_argument('video_path', type=str, help='Path to the video file')
    parser.add_argument('ssim_threshold', type=float, help='SSIM threshold for frames to be considered similar')
    parser.add_argument('consecutive_frames', type=int, help='Number of consecutive frames that need to be similar for a frame to be considered a slide')

    args = parser.parse_args()

    video_path = args.video_path
    ssim_threshold = args.ssim_threshold
    consecutive_frames = args.consecutive_frames
    output_prefix = os.path.splitext(video_path)[0]  # Remove file extension

    extract_slides(video_path, output_prefix, ssim_threshold, consecutive_frames)
