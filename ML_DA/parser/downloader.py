import concurrent.futures
import multiprocessing

import requests
from tqdm import tqdm

from constants import PAGES

headers = {"Accept-Language": "en-US,en;q=0.5"}


def imdb_url(page: int) -> str:
    base_path = "https://www.imdb.com/search/title/?sort=num_votes,desc&adult=include&count=250"
    if page == 1:
        return (
            f"{base_path}&ref_=adv_prv"
        )
    else:
        return f"{base_path}&start={250 * (page - 1) + 1}&ref_=adv_nxt"


def download_imdb_page(i):
    response = requests.get(imdb_url(i), headers=headers)
    with open(f"./scraped/imdb/page_{i}.html", "w") as f:
        f.write(response.text)


def download_boxoffice_page(id):
    url = f"https://www.boxofficemojo.com/title/tt{id}/"
    response = requests.get(url, headers=headers)
    with open(f"./scraped/boxoffice/{id}.html", "w") as f:
        f.write(response.text)


def run(f, my_iter):
    l = len(my_iter)
    num_threads = multiprocessing.cpu_count() - 1
    with tqdm(total=l) as pbar:
        with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
            futures = {executor.submit(f, arg): arg for arg in my_iter}
            results = {}
            for future in concurrent.futures.as_completed(futures):
                arg = futures[future]
                results[arg] = future.result()
                pbar.update(1)


def download_all_imdb_pages():
    run(download_imdb_page, range(1, PAGES + 1))


def download_all_boxoffice_pages(ids):
    run(download_boxoffice_page, ids)
