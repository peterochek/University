import numpy as np
from matplotlib import pyplot as plt
from matplotlib import cm
from matplotlib.colors import ListedColormap

def plot1d_descent(idx, minimum_x, fn, path, ax, freq=30, x_range=(-4, 4)):
    xs_range = np.linspace(*x_range, 500)
    fn = np.vectorize(fn)
    if isinstance(freq, int):
        trace_len = len(path) // freq + 1
    else:
        trace_len = 1
    x_trace, y_trace = zip(*path[::trace_len])

    ax.plot(xs_range, fn(xs_range))
    ax.plot(minimum_x, fn(minimum_x), marker='o', c='blue', markersize=10)
    ax.set_xlim(x_range)
    ax.scatter(x_trace, y_trace, c='red')
    

def plot3d_descent(fn, path, info="", color=cm.coolwarm, angle=(0, 0)):
    fig = plt.figure(figsize=(6, 9))
    ax3d = fig.add_subplot(211, projection='3d')
    axheat = fig.add_subplot(212)
    
    ax3d.set_title(info[0])
    axheat.set_title(info[1])
    
    plot3d_descent_default(ax3d, axheat, fn, path, color, angle)

def plot3d_descent_default(ax3d, axheat, fn, path, color=cm.coolwarm, angle=(0, 0)):
    levels = [x ** 3 for x in range(8)]
    r = np.linspace(240, 166, 256) / 255.
    g = np.linspace(244, 188, 256) / 255.
    b = np.linspace(246, 203, 256) / 255.
    cmap = ListedColormap(np.vstack((r, g, b)).T)

    X = np.linspace(-5, 5, 200)
    Y = np.linspace(-5, 5, 200)

    X, Y = np.meshgrid(X, Y)
    Z = fn(X, Y)

    ax3d.plot_surface(X, Y, Z, cmap=color, rstride=2, cstride=2, alpha=0.6)
    trace_xs, z_trace = zip(*path)
    trace_xs = np.array(trace_xs)
    x_trace, y_trace = trace_xs[:, 0], trace_xs[:, 1]
    z_trace = np.array(z_trace)

    ax3d.scatter(x_trace, y_trace, z_trace, c='green', s=10)
    ax3d.set_xlabel('x')
    ax3d.set_ylabel('y')
    ax3d.set_zlabel('z')

    ax3d.view_init(*angle)

    axheat.contourf(X, Y, Z, levels=levels, cmap=cmap)
    cs = axheat.contour(X, Y, Z, levels=levels, colors="#ABBECC")
    axheat.clabel(cs, colors="#2618B1")
    axheat.plot(x_trace, y_trace, '-og', linewidth=.5, ms=1.5)
    

def plot3d_descent_double(fn, paths, info, color=cm.coolwarm, angle=(0, 0), title = ""):
    fig = plt.figure(figsize=(16, 10))
    
    fig.suptitle(title, fontsize=16)
    
    ax3d_1 = fig.add_subplot(221, projection='3d')
    axheat_1 = fig.add_subplot(223)
    
    ax3d_1.set_title('Default Method')
    axheat_1.set_title(info[0])
    
    ax3d_2 = fig.add_subplot(222, projection='3d')
    axheat_2 = fig.add_subplot(224)
    
    ax3d_2.set_title('Dichotomy Method')
    axheat_2.set_title(info[1])
    
    plot3d_descent_default(ax3d_1, axheat_1, fn, paths[0], color, angle)
    plot3d_descent_default(ax3d_2, axheat_2, fn, paths[1], color, angle)