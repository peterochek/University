from concurrent.futures import ThreadPoolExecutor, as_completed
from multiprocessing import Pool
from tqdm import tqdm
from bs4 import BeautifulSoup
from lxml import etree


def parse_movie(movie_id):
    page = open(f"./scraped/{movie_id}.html", "r").read()

    soup = BeautifulSoup(page, "lxml")

    review_containers = soup.find_all("div", class_="review-container")

    reviews = []

    for container in review_containers:
        rating_tag = container.find("span", class_="rating-other-user-rating")
        rating = rating_tag.span.text if rating_tag else "No Rating"
        review_text = container.find("div", class_="text show-more__control").text
        reviews.append({"movie_id": movie_id, "rating": rating, "review": review_text})

    return reviews


def parse_movies(movie_ids, num_processes=8):
    all_reviews = []
    with Pool(num_processes) as pool:
        for reviews in tqdm(pool.imap(parse_movie, movie_ids), total=len(movie_ids)):
            all_reviews.extend(reviews)

    return all_reviews