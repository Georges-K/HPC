import numpy as np
import matplotlib.pyplot as plt

filename = "means_n=1000_nth=8_m=4.txt"
data = np.loadtxt(filename)
mean = data[:,1]
time = data[:,0]
coef2 = np.polyfit(time,mean,4)
poly2d_fn = np.poly1d(coef2) 

plt.scatter(time, mean, label="Data", color='red',s=0.5)
plt.plot(time, poly2d_fn(time), label="Fitted Line")
plt.xlabel("Time (s)")
plt.ylabel("Mean Square Displacement (m)")
plt.legend()
plt.show()


