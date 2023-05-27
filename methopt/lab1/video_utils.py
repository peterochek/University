import glob
import os
import shutil

import cv2
from PIL import Image
import numpy as np
from matplotlib import animation
import matplotlib.pyplot as plt

vidcap = cv2.VideoCapture('big_buck_bunny_720p_5mb.mp4')
success,image = vidcap.read()
count = 0
while success:
  cv2.imwrite("frame%d.jpg" % count, image)     # save frame as JPEG file      
  success,image = vidcap.read()
  print('Read a new frame: ', success)
  count += 1


def convert_mp4_to_jpgs(path, output='output'):
    vid_cap = cv2.VideoCapture(path)
    success, image = vid_cap.read()
    count = 0
    if os.path.exists(output):
        shutil.rmtree(output)
    try:
        os.mkdir(output)
    except IOError:
        return

    while success:
        cv2.imwrite(f"{output}/{count:05d}.jpg", image)

        success, image = vid_cap.read()
        count += 1


def make_gif(gif_path, frame_folder="output"):
    imgs = glob.glob(f"{frame_folder}/*.jpg")
    imgs.sort()
    pics = [Image.open(image) for image in imgs]
    ff = pics[0]
    ff.save(gif_path, format="GIF", append_images=pics, save_all=True, duration=50, loop=0)
    
    
def animate(
        x_range: np.ndarray,
        f,
        trace,
        output: str,
        fps=10
):
    fig, ax = plt.subplots(figsize=(10, 10))
    ax.plot(x_range, f(x_range))

    (x0, y0) = trace[0]
    xs, ys = zip(*trace[1:])

    line, = ax.plot(x0, y0, 'red')  # initial data
    dot = ax.scatter(x0, y0, c='g', marker="o")  # initial point

    ax.set_xlabel('X Position')
    ax.set_ylabel('Y Position')
    
    def update(iteration):
        ax.relim()  # resizing plot area
        ax.autoscale_view(True, True, True)  # resizing plot area
        dot.set_offsets([xs[iteration], ys[iteration]])
        ax.set_title(r'Gradient Descent, iteration: ' + str(iteration))
        return line, ax
    
    ani = animation.FuncAnimation(fig, update, frames=len(trace) - 1, interval=200, blit=False, repeat_delay=200)

    file = f'{output}.mp4'
    ani.save(file, fps=fps, extra_args=['-vcodec', 'libx264'])