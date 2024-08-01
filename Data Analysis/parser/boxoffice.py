from bs4 import BeautifulSoup
import pandas as pd
from tqdm.contrib.concurrent import process_map
import multiprocessing


from dataclasses import dataclass


@dataclass
class BoxOffice:
    id: str = ""
    domestic: str = ""
    domestic_percent: str = ""
    international: str = ""
    international_percent: str = ""
    worldwide: str = ""
    domestic_opening: str = ""
    budget: str = ""
    release_date: str = ""
    distributor: str = ""


def parse_table(id, table):
    releases = table.find("div", class_="mojo-performance-summary-table")
    moneys = releases.find_all("span", class_="money")
    percents = releases.find_all("span", class_="percent")

    domestic = moneys[0].text.strip() if len(moneys) > 0 else ""
    domestic_percent = percents[0].text.strip() if len(percents) > 0 else ""
    international = moneys[1].text.strip() if len(moneys) > 1 else ""
    international_percent = percents[1].text.strip() if len(percents) > 1 else ""
    worldwide = moneys[2].text.strip() if len(moneys) > 2 else ""

    distributor = ""
    domestic_opening = ""
    budget = ""
    release_date = ""

    right_table = table.find("div", class_="mojo-summary-values")
    if right_table:
        divs = right_table.find_all("div")
        for div in divs:
            if div.find_all("span")[0].text.strip() == "Domestic Distributor":
                # print(div.find_all('span'))
                distributor = div.find_all("span")[1].text.strip()

            if div.find_all("span")[0].text.strip() == "Domestic Opening":
                domestic_opening = div.find_all("span")[1].text.strip()

            if div.find_all("span")[0].text.strip() == "Budget":
                budget = div.find_all("span")[1].text.strip()

            if div.find_all("span")[0].text.strip() == "Earliest Release Date":
                release_date = div.find_all("span")[1].text.strip()

    fields = [
        id,
        domestic,
        domestic_percent,
        international,
        international_percent,
        worldwide,
        domestic_opening,
        budget,
        release_date,
        distributor,
    ]

    return BoxOffice(*fields)


def get_boxoffice(id):
    page = open(f"./scraped/boxoffice/{id}.html", "r").read()

    soup = BeautifulSoup(page, "lxml")
    table = soup.find("div", class_="mojo-summary-table")

    return parse_table(id, table)


def get_parsed_boxoffices(ids):
    cpus = multiprocessing.cpu_count() // 2

    box_offices = process_map(get_boxoffice, ids, max_workers=cpus)

    return box_offices


df = pd.DataFrame(
    columns=[
        "id",
        "domestic",
        "domestic_percent",
        "international",
        "international_percent",
        "worldwide",
        "domestic_opening",
        "budget",
        "release_date",
        "distributor",
    ]
)


def create_boxoffice_df(ids):
    for column in df.columns:
        df[column] = df[column].astype(str)

    boxoffices = get_parsed_boxoffices(ids)

    for column in df.columns:
        cur_field = []
        for boxoffice in boxoffices:
            cur_field.append(getattr(boxoffice, column))
        df[column] = cur_field

    return df
