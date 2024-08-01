import pandas as pd

from parsing import get_parsed_movies

df = pd.DataFrame(
    columns=[
        "url",
        "title",
        "year",
        "rating",
        "runtime",
        "certificate",
        "genre",
        "metascore",
        "description",
        "cast",
        "votes",
        "gross",
    ]
)


def create_imdb_df():
    for column in df.columns:
        df[column] = df[column].astype(str)

    movies = get_parsed_movies()

    for column in df.columns:
        cur_field = []
        for movie in movies:
            cur_field.append(getattr(movie, column))
        df[column] = cur_field

    return df
