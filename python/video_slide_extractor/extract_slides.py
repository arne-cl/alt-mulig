#!/bin/env python3

import cv2
import numpy as np
import argparse
import os
from skimage.metrics import structural_similarity as ssim
import time


def open_video(video_path):
    cap = cv2.VideoCapture(video_path)

    if cap is None or not cap.isOpened():
        raise Exception(f"Error opening video file {video_path}")
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
    return ssim(grayA, grayB)


class SlideExtractor(object):
    def __init__(self, video_path, output_prefix, ssim_threshold, consecutive_frames):
        self.video_path = video_path
        self.output_prefix = output_prefix
        self.ssim_threshold = ssim_threshold
        self.consecutive_frames = consecutive_frames

    def extract_slides(self):
        cap = open_video(self.video_path)
        frame_rate, frame_width, frame_height, total_frames = get_video_info(cap)

        ret, comparison_frame = cap.read()
        if not ret:
            raise Exception("Error reading the first frame")

        self.frame_count = 1
        self.similar_frames = 0
        self.slide_count = 0
        self.start_time = time.time()

        while cap.isOpened():
            ret, frame = cap.read()
            if not ret:
                break

            self.frame_count += 1

            if compare_frames(comparison_frame, frame) >= self.ssim_threshold:
                self.similar_frames += 1
            else: # frame is not similar to comparison_frame
                if self.similar_frames >= self.consecutive_frames:
                    timestamp = cap.get(cv2.CAP_PROP_POS_MSEC) / 1000
                    cv2.imwrite(f"{self.output_prefix}-{timestamp}.png", comparison_frame)
                    self.slide_count += 1
                    print(f"Extracted slide {self.slide_count} at timestamp {timestamp}s")

                comparison_frame = frame
                self.similar_frames = 0

            # Progress indicator
            if time.time() - self.start_time >= 1:  # Update progress every second of real time
                progress = (self.frame_count / total_frames) * 100
                print(f"Progress: {progress:.2f}%, Frame: {self.frame_count}")
                start_time = time.time()

        cap.release()
        cv2.destroyAllWindows()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Extract slides from video.')
    parser.add_argument('video_path', type=str, help='Path to the video file')
    parser.add_argument('ssim_threshold', type=float, help='SSIM threshold for frames to be considered similar')
    parser.add_argument('consecutive_frames', type=int, help='Number of consecutive frames that need to be similar for a frame to be considered a slide')

    args = parser.parse_args()

    output_prefix = os.path.splitext(args.video_path)[0]  # Remove file extension
    extractor = SlideExtractor(args.video_path, output_prefix, args.ssim_threshold, args.consecutive_frames)
    extractor.extract_slides()
