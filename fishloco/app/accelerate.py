import csv
import numpy as np
import matplotlib.pyplot as plt

def instantaneous_accelerate(p0, p1, p2, fps):
    """ caculate p1 acceleration """
    x0, y0 = p0
    x1, y1 = p1
    x2, y2 = p2
    if fps <= 0:
        raise ValueError("fps must >0")

    vx0 = (x1 - x0) * fps
    vy0 = (y1 - y0) * fps

    vx1 = (x2 - x1) * fps
    vy1 = (y2 - y1) * fps

    ax = (vx1 - vx0)*  fps
    ay = (vy1 - vy0)*  fps
    
    a_magnitude = np.sqrt(ax**2 + ay**2)

    return a_magnitude

# def acce(data):
#     """ 
#     Params:
#         data(list): [x, y]
#     """
#     accs = []
#     for i in range(len(data[0])-2):
#         p0 = (data[0][i], data[1][i])
#         p1 = (data[0][i+1], data[1][i+1])
#         p2 = (data[0][i+2], data[1][i+2])
#         accs.append(instantaneous_accelerate(p0, p1, p2, 1/30)) # 30帧的视频
    
#     # 创建直方图
#     plt.figure(figsize=(10, 6), dpi=300)
#     plt.hist(accs, color='blue', bins=1000)

#     plt.title('Distribution of Data')
#     plt.xlabel('Value')
#     plt.ylabel('Frequency')

#     # 保存图表
#     save_id = input("Enter save id:")
#     plt.savefig(f"acc/{save_id}.png")
    
    

# def main(file_path):
#     with open(file_path, "r") as f:
#         reader = csv.DictReader(f)

#         data = [[], []]
#         for line in reader:
#             data[0].append(int(line["cx"]))
#             data[1].append(int(line["cy"]))
    
#     acce(data)
