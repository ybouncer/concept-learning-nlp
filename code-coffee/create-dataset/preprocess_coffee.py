"""
Coffee Quality Dataset Preprocessing
Prepares coffee sensory and quality data for the concept learning experiment
"""

import pandas as pd
import numpy as np

print("=" * 60)
print("COFFEE QUALITY DATASET PREPROCESSING")
print("=" * 60)

# Dataset name
DATASET_NAME = "coffee"

# Load the raw data
print("\n[1/6] Loading raw coffee data...")
data = pd.read_csv('raw/coffee_quality_arabica.csv')
print(f"   Loaded {len(data)} coffee samples with {len(data.columns)} columns")

# Select the most relevant sensory and quality attributes
# These are the continuous attributes rated by professional coffee tasters
print("\n[2/6] Selecting sensory and quality attributes...")
selected_attributes = [
    'Aroma',           # Smell/fragrance intensity (0-10)
    'Flavor',          # Taste intensity (0-10)
    'Aftertaste',      # Lingering taste (0-10)
    'Acidity',         # Acidity level (0-10)
    'Body',            # Mouthfeel/texture (0-10)
    'Balance',         # Overall harmony (0-10)
    'Uniformity',      # Consistency across cups (0-10)
    'Clean.Cup',       # Lack of off-flavors (0-10)
    'Sweetness',       # Natural sweetness (0-10)
    'Moisture',        # Bean moisture content (0-1)
    'altitude_mean_meters'  # Growing altitude
]

# Filter to selected columns
data = data[selected_attributes].copy()
print(f"   Selected {len(selected_attributes)} attributes")

# Clean column names: replace dots and spaces with dashes
print("\n[3/6] Cleaning column names...")
data.columns = data.columns.str.replace('.', '-', regex=False)
data.columns = data.columns.str.replace(' ', '-')
data.columns = data.columns.str.replace('_', '-')
print(f"   Cleaned column names: {list(data.columns)}")

# Remove rows with missing values
print("\n[4/6] Handling missing values...")
print(f"   Before: {len(data)} samples")
data = data.dropna()
print(f"   After removing NaN: {len(data)} samples")

# Normalize all columns to [0, 1] range
print("\n[5/6] Normalizing data to [0, 1] range...")
def normalise_data(df):
    """Normalize each column to 0-1 range"""
    for col in df.columns:
        min_val = df[col].min()
        max_val = df[col].max()
        if max_val > min_val:  # Avoid division by zero
            df[col] = (df[col] - min_val) / (max_val - min_val)
        else:
            df[col] = 0.0
    return df

data = normalise_data(data)

# Show summary statistics
print("\n   Normalization complete. Sample statistics:")
print(data.describe().loc[['mean', 'min', 'max']].round(3))

# Create metadata file
print("\n[6/6] Creating metadata file...")
def create_metadata(df):
    """Create metadata describing each attribute"""
    metadata = []
    for column in df.columns:
        # All coffee attributes are continuous (normalized sensory scores)
        metadata.append({
            'channel': column,
            'type': 'continuous',
            'symbolic-attribute': 'nil'
        })
    return pd.DataFrame(metadata)

meta = create_metadata(data)

# Save files
print("\n" + "=" * 60)
print("SAVING OUTPUT FILES")
print("=" * 60)

# Save main data file
data.to_csv(f"{DATASET_NAME}.csv", index=False)
print(f"\n[OK] Saved {DATASET_NAME}.csv")
print(f"     - {len(data)} coffee samples")
print(f"     - {len(data.columns)} attributes")

# Save metadata file
meta.to_csv(f"{DATASET_NAME}_meta.csv", index=False)
print(f"\n[OK] Saved {DATASET_NAME}_meta.csv")

# Display metadata
print("\nMetadata preview:")
print(meta.to_string(index=False))

# Display sample data
print("\n" + "=" * 60)
print("SAMPLE DATA (first 3 rows)")
print("=" * 60)
print(data.head(3).to_string(index=False))

print("\n" + "=" * 60)
print("PREPROCESSING COMPLETE!")
print("=" * 60)
print("\nNext steps:")
print("1. Run: python create-scenes.py --dataset coffee --scenes 20000")
print("2. Move coffee/ folder to datasets/")
print("3. Run the FCG-Editor experiment")
