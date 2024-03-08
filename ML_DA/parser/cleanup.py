import re


def clean_spaces(text: str) -> str:
    if not text:
        return ""
    text = text.strip()
    text = text.replace("\n", "")
    text = text.replace("\t", "")
    return text


COMMA_SPACE = ", "
BRACKETS_SPACE = "() "


def parse_cast(cell: str):
    if not cell:
        return "", ""
    dirs_stars = cell.split("|")
    if len(dirs_stars) == 1:
        if dirs_stars[0].startswith("Director:"):
            dirs = dirs_stars[0].strip(COMMA_SPACE).removeprefix("Director:")
            stars = ""
        elif dirs_stars[0].startswith("Stars:"):
            dirs = ""
            stars = dirs_stars[0].strip(COMMA_SPACE).removeprefix("Stars:")
        else:
            dirs = ""
            stars = ""
    else:
        dirs = dirs_stars[0].strip(COMMA_SPACE).removeprefix("Director:")
        stars = dirs_stars[1].strip(COMMA_SPACE).removeprefix("Stars:")

    return dirs, stars


def parse_year(cell: str):
    if not cell:
        return ""
    cleaned_str = re.sub("[^\d–]", "", cell)
    
    return cleaned_str


def parse_votes(cell: str):
    votes = cell.replace(',', '')
    
    return int(votes) if votes else 0
