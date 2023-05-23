#!/bin/env python3

import argparse
from datetime import timedelta
import os
import time

import cv2
import numpy as np
from skimage.metrics import structural_similarity as ssim


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

        # load video and metadata
        cap = cv2.VideoCapture(self.video_path)
        if cap is None or not cap.isOpened():
            raise Exception(f"Error opening video file {self.video_path}")
        self.cap = cap
        self.get_video_info()

    def get_video_info(self):
        self.frame_rate = self.cap.get(cv2.CAP_PROP_FPS)
        self.frame_width = int(self.cap.get(cv2.CAP_PROP_FRAME_WIDTH))
        self.frame_height = int(self.cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
        self.total_frames = int(self.cap.get(cv2.CAP_PROP_FRAME_COUNT))
        print(f"Video information: Frame rate: {self.frame_rate}, Frame width: {self.frame_width}, Frame height: {self.frame_height}, Total frames: {self.total_frames}")

    def save_slide(self, comparison_frame):
        # extract and format timestamp of the slide
        timestamp_seconds = self.cap.get(cv2.CAP_PROP_POS_MSEC) / 1000
        timestamp = str(timedelta(seconds=timestamp_seconds))
        # Keep only the first two digits of the milliseconds
        timestamp = timestamp[:timestamp.rfind('.')+3]

        cv2.imwrite(f"{self.output_prefix}-{timestamp}-{self.similar_frames}-similar-frames.png", comparison_frame)
        print(f"Extracted slide {self.slide_count} (after {self.similar_frames} similar frames) at timestamp {timestamp}s.")

    def print_progress(self):
        progress = (self.frame_count / self.total_frames) * 100
        print(f"Progress: {progress:.2f}%, Frame: {self.frame_count}")

    def extract_slides(self):
        ret, comparison_frame = self.cap.read()
        if not ret:
            raise Exception("Error reading the first frame")

        self.frame_count = 1
        self.similar_frames = 0
        self.slide_count = 0
        self.start_time = time.time()

        while self.cap.isOpened():
            ret, current_frame = self.cap.read()
            if not ret:
                break

            self.frame_count += 1

            if compare_frames(comparison_frame, current_frame) >= self.ssim_threshold:
                self.similar_frames += 1
            else: # current_frame is not similar to comparison_frame
                if self.similar_frames >= self.consecutive_frames:
                    self.save_slide(comparison_frame)
                    self.slide_count += 1

                comparison_frame = current_frame
                self.similar_frames = 0

            # print progress every 5 seconds of real time
            if time.time() - self.start_time >= 5:
                self.print_progress()
                self.start_time = time.time()

        self.cap.release()
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
