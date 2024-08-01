from bs4 import BeautifulSoup
from tqdm import trange
from constants import PAGES

from dataclasses import dataclass


@dataclass
class Movie:
    url: str = ""
    title: str = ""
    year: str = ""
    rating: str = ""
    runtime: str = ""
    certificate: str = ""
    genre: str = ""
    metascore: str = ""
    description: str = ""
    cast: str = ""
    votes: str = ""
    gross: str = ""


def parse_movie(movie):
    title_url = movie.find("h3").find("a")
    title = title_url.text
    url = title_url["href"].strip("title/")

    year = movie.find("span", class_="lister-item-year").text
    rating = (
        movie.find("div", class_="inline-block ratings-imdb-rating").find("strong").text
        if movie.find("div", class_="inline-block ratings-imdb-rating")
        else ""
    )
    runtime = (
        movie.find("span", class_="runtime").text
        if movie.find("span", class_="runtime")
        else ""
    )
    certificate = (
        movie.find("span", class_="certificate").text
        if movie.find("span", class_="certificate")
        else ""
    )
    genre = (
        movie.find("span", class_="genre").text.strip()
        if movie.find("span", class_="genre")
        else ""
    )
    metascore = (
        movie.find("div", class_="inline-block ratings-metascore").find("span").text
        if movie.find("div", class_="inline-block ratings-metascore")
        else ""
    )
    description = movie.find_all("p", class_="text-muted")[1].text.strip()
    cast = movie.find_all("p", class_="")[0].text.strip()

    footer = movie.find("p", class_="sort-num_votes-visible")
    votes = ""
    gross = ""
    if footer:
        value_gross = footer.find_all("span", attrs={"name": "nv"})
        votes = value_gross[0].text if len(value_gross) > 0 else ""
        gross = value_gross[1].text if len(value_gross) > 1 else ""

    fields = [
        url,
        title,
        year,
        rating,
        runtime,
        certificate,
        genre,
        metascore,
        description,
        cast,
        votes,
        gross,
    ]

    return Movie(*fields)


def get_parsed_movies():
    movies = []

    for i in trange(1, PAGES + 1):
        page = open(f"./scraped/imdb/page_{i}.html", "r").read()

        soup = BeautifulSoup(page, "lxml")
        movie_block = soup.find("div", class_="lister-list")

        for movie in filter(lambda x: x != "\n", movie_block.contents):
            movies.append(parse_movie(movie))

    return movies
