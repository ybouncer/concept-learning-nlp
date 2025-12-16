import argparse
import json
import random
import os

import pandas as pd

parser = argparse.ArgumentParser()

parser.add_argument("--dataset",required=True)
parser.add_argument("--context_min",type=int, default=3)
parser.add_argument("--context_max",type=int, default=10)
parser.add_argument("--scenes",type=int, default=20000)

args = parser.parse_args()

def get_index_string(index: int, leading_zeros: int = 6):
    """Converts an index to a string with leading zeros."""
    index_string: str = str(index)
    while len(index_string) < leading_zeros:
        index_string = '0' + index_string
    return index_string

def create_filename(split: str, index: int):
    """Creates a filename for a scene."""
    return f"{args.dataset}_{split}_{get_index_string(index)}.json"

def select_random_rows_using_id(df: pd.DataFrame, n: int):
    """Given a dataframe selects randomly n rows."""
    # get the number of rows in the dataframe
    n_rows = df.shape[0]
    # generate a list of n random numbers between 0 and n_rows
    random_ids = random.sample(range(n_rows), n)
    # select the rows with the ids in random_ids
    df_random = df.iloc[random_ids]
    return df_random

def train_test_split(df: pd.DataFrame, split_size: float=0.9):
    """Splits a dataframe into train and test sets."""
    dataset_size = len(df)
    train_size = round(len(df) * split_size)

    train_ids = random.sample(range(dataset_size), train_size)
    train_ids = sorted(train_ids)
    test_ids = sorted(list(set([i for i in range(dataset_size)]).difference(set(train_ids))))

    train_df = df.iloc[train_ids]
    test_df = df.iloc[test_ids]
    return train_df, test_df

def create_dataset(data: pd.DataFrame, split: str, scenes: int):
    """Creates a dataset with scenes."""
    dataset: list = []
    for i in range(scenes):
        scene_size: int = random.randint(args.context_min, args.context_max)
        new_scene: dict = {"image_index": i, "image_filename": "nil", "objects": []}
        for _, row in select_random_rows_using_id(data, scene_size).iterrows():
            new_object: dict = {"attributes": row.to_dict(), "description": {}}
            for key, val in new_object["attributes"].items():
                new_object["attributes"][key] = round(val, 5) # round to 5 decimal places
            new_scene["objects"].append(new_object)
        dataset.append(new_scene)

    # save every scene in a separate file
    out_dir: str = os.path.join(args.dataset, "scenes", split)
    os.makedirs(out_dir, exist_ok=True)
    for scene in dataset:
        with open(os.path.join(out_dir, create_filename(split, scene['image_index'])), "w") as f:
            json.dump(scene, f)

if __name__ == "__main__":
    # read csv -> expects csv to be in same folder
    data = pd.read_csv(f"{args.dataset}.csv")
    # split train test by 90/10
    train, test = train_test_split(data, split_size=0.9)
    # create n train scenes
    create_dataset(train, split="train", scenes=args.scenes)
    # create 0.1n test scenes (easy to calculate)
    create_dataset(test, split="test", scenes=round(args.scenes*0.1))
    # move the "<dataset_name>_meta.csv" to the "<dataset_name>" folder
    os.rename(f"{args.dataset}_meta.csv", os.path.join(args.dataset, f"{args.dataset}_meta.csv"))
