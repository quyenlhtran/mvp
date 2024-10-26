# 🎉 MVP: Multiple Visualizations of Percentiles 🎉

Welcome to **MVP** (Multiple Visualizations of Percentiles), an R package designed to bring together various methods of percentile calculation, ensuring consistency and flexibility. MVP is an essential tool for **educators, students, data analysts,** and **researchers** seeking to calculate and visualize percentiles across different statistical software conventions.

> For more information, check out our paper:  
> **[*Multiple Visualizations of Percentiles (MVP): An R Package Unifying Different Methods of Calculating Percentiles*]()**.

---

## 🌟 Overview

Percentiles are essential statistical measures that indicate the value below which a given percentage of data falls. However, different software packages, such as R, Python, SAS, and SPSS, use distinct conventions and methods to compute percentiles. The **MVP** package unifies these methods, offering a comprehensive solution to calculate percentiles according to a range of commonly used conventions.

### ✨ Key Features
- **11 Different Methods for Calculation**: Choose from a comprehensive selection, including inverse ECDF, piecewise linear interpolation, weighted ECDF, and more.
- **Interactive Method Selection**: An intuitive decision tree helps guide users to the best method based on dataset properties (e.g., distribution, weighting).
- **Visual Comparative Analysis**: Plot different methods to visually compare and better understand each approach’s impact on results.
- **Automatic Setup**: Install the required packages to ensure a smooth experience for users of any skill level.

---

## 🚀 Installation

You can install the **MVP** package directly from this respiratory.

---

## 🔍 Usage

Here’s a quick guide to using the `mvp` function for percentile calculation and visualization:

1. **Prepare Your Data**: Ensure it’s in a numeric vector format.
2. **Define Percentiles**: Specify the desired percentiles in a numeric vector.
3. **Select Methods**: Use `selected_methods = NULL` to let the function suggest the best methods.
4. **Enable Plotting**: Set `plot = TRUE` to visualize results.

### 📝 Example

```r
data <- c(37, 67, 36, 67, 47, 43, 60, 82, 49, 86, 95, 92, 54, 94, 63, 99, 17, 32, 85, 30)
percentiles <- c(20, 50)
methods <- c("inv.ecdf", "pli.UM")

mvp(data, percentiles, selected_methods = methods, plot = TRUE)
```
---

## 🙏 Acknowledgments
I would like to thank **Dr. Mamunur Rashid** for facilitating this research and for this constant encouragement.

Special thanks to **Dr. Jyotirmoy Sarkar** for his guidance and helpful feedback, especially on the graphs and project direction. 

Thank you both for your unwavering support and encouragement.


