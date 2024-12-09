import spreg
import libpysal
import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
from pyarrow import parquet
import pandas as pd

shape = gpd.read_parquet("shape_final.parquet")
shape['log_populacao'] = np.log(shape['populacao'])

# OLS

y = 'ice'
x = [
    'PLP',
    'PLB',
    'diversidade_credito',
    'diversidade_bancaria',
    'log_populacao',
    'op_cred'
]

shape = shape.dropna()

Y = shape[y]
X = shape[x]

shape["centroid"] = shape.geometry.centroid

# 3. Separar coordenadas dos centróides
shape["centroid_x"] = shape["centroid"].x
shape["centroid_y"] = shape["centroid"].y

coords = shape[["centroid_x", "centroid_y"]].values
knn = libpysal.weights.KNN.from_array(coords, k=5)

w_sparse = knn.sparse
knn.transform = "R"

ols1 = spreg.OLS(Y, X)

ols1b = spreg.OLS(Y, X, w=knn, spat_diag=True, moran=True)

mod, finreg = spreg.spsearch.stge_pre(Y,X,w=knn,name_ds="shape",name_w="KNN 5")