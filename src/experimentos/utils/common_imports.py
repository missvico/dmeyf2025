#import pandas as pd 
import gzip
import polars as pl
import numpy as np

def show_versions():
    print("polars:", pl.__version__)
    print("numpy:", np.__version__)

print("✅ Common imports loaded (gzip, polars, numpy, pandas)")

__all__ = ["gzip", "pl", "np", "show_versions"]
