import concurrent.futures
import multiprocessing

import requests
from tqdm import tqdm
import json

headers = {"Accept-Language": "en-US,en;q=0.5"}


def imdb_url(movie_id: int) -> str:
    return f"https://www.imdb.com/title/{movie_id}/reviews?sort=totalVotes&dir=desc&ratingFilter=0"


def download_review_page(movie_id):
    response = requests.get(imdb_url(movie_id), headers=headers)
    with open(f"./scraped/{movie_id}.html", "w") as f:
        f.write(response.text)


def run(f, my_iter):
    l = len(my_iter)
    # num_threads = multiprocessing.cpu_count() - 1
    num_threads = 1
    with tqdm(total=l) as pbar:
        with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
            futures = {executor.submit(f, arg): arg for arg in my_iter}
            results = {}
            for future in concurrent.futures.as_completed(futures):
                arg = futures[future]
                results[arg] = future.result()
                pbar.update(1)


def download_all_imdb_pages(urls):
    run(download_review_page, urls)
