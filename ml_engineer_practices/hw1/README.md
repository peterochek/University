# Шаги для выполнения домашнего задания

## Установка `poetry`

```bash
curl -sSL https://install.python-poetry.org | python3 -
```

## Инициализация проекта с помощью `poetry`

```bash
poetry new your-project-name
cd your-project-name
```

## Добавление `dev`-зависимостей

```bash
poetry add --dev black ruff pre-commit isort
```

## Добавление правил и исключений в `pyproject.toml`

```bash
[tool.black]
line-length = 88
target-version = ['py38']
include = '\.pyi?$'
exclude = '''
/(
    \.git
  | \.venv
  | _build
  | build
  | dist
)/
'''

[tool.isort]
profile = "black"

[tool.ruff]
```

## Установка `pre-commit`

```bash
poetry run pre-commit install
```

```bash
pre-commit install
```

## Запуск `pre-commit`

```bash
pre-commit run --all-files
```

## Добавляем правила в `pre-commit`

```yaml
repos:
  - repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v4.5.0
    hooks:
      - id: trailing-whitespace
      - id: end-of-file-fixer

  - repo: https://github.com/psf/black
    rev: 23.11.0
    hooks:
      - id: black
        language_version: python3.11

  - repo: https://github.com/pycqa/isort
    rev: 5.12.0
    hooks:
      - id: isort
        name: isort (python)

  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.1.6
    hooks:
      - id: ruff
      - id: ruff-format
```

## Запуск форматтера и линтера

Форматтеры:

```bash
poetry run isort .
poetry run black .
```

Линтеры:

```bash
poetry run ruff .
```
