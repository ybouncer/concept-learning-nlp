"""
Download Coffee Quality Dataset
Source: Coffee Quality Database from CQI (Coffee Quality Institute)
"""

import urllib.request
import pandas as pd
import os

print("Downloading Coffee Quality Dataset...")

# URL for the coffee quality dataset (arabica)
url = "https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/arabica_data_cleaned.csv"

# Download the file
output_file = "raw/coffee_quality_arabica.csv"
os.makedirs("raw", exist_ok=True)

try:
    urllib.request.urlretrieve(url, output_file)
    print(f"[OK] Downloaded coffee dataset to {output_file}")

    # Verify the download
    df = pd.read_csv(output_file)
    print(f"[OK] Dataset loaded: {len(df)} coffee samples")
    print(f"[OK] Columns: {len(df.columns)} attributes")
    print(f"\nPreview:")
    print(df.head())

except Exception as e:
    print(f"Error downloading: {e}")
    print("\nAlternative: Download manually from:")
    print("https://github.com/jldbc/coffee-quality-database")
