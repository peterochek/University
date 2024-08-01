# Шаги

## Выбрал `git-lfs` как систему версионирования

## Устанавливаем `git-lfs` (Arch Linux btw)

```bash
yay -S git-lfs
```

## Инициализируем `git-lfs` внутри проекта

```bash
git lfs install
```

## Добавляем файлы `*.csv` для наблюдения

```bash
git lfs track "*.csv"
```

Автоматически добавился файл `.gitattributes`:

```txt
*.csv filter=lfs diff=lfs merge=lfs -text
```

## Если хотим добавить файлы - стандартный подход

```bash
git add hw2/imdb.csv
git commit -m "added imdb dataset"
```

## Копирование удаленного проекта локально

```bash
git pull `url`
git lfs clone `url` - подтянет все lfs файлы
```

```bash
git pull
git lfs pull - при "рядовом" обновлении
```

## Добавлять и обновлять файлы также - если добавится новый формат / отдельный файл, сначала `track` с помощью `git-lfs`, затем `push` в репозиторий

## P.S. преимущества `git-lfs`

Предположим, что создаём файл объемом 100 МБ, изменяем его 100 раз. В итоге получим `.git` объемом 10 ГБ. `git-lfs` умным образом обработает изменения и будет удерживать размер репозитория в адекватных пределах!