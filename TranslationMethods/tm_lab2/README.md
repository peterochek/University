# Вариант 6. Описание переменных в Kotlin.

Каждое описание начинается ключевым словом ```var``` или ```val```, далее идет описание переменной.  <br/>
Описание содержит имя переменной, затем двоеточие, затем имя типа. <br/>
Затем может идти начальное значение, предусмотреть инициализацию только для типа Int числом, выражения рассматривать не
требуется. <br/>
Используйте один терминал для всех имен переменных и имен типов. <br/>
Используйте один терминал для ключевого слова ```var``` (не три ‘v’, ‘a’, ‘r’). <br/>
```Пример: var a: Int; val c: Int = 2;```

## Грамматика

```
All -> Desc Cont - вся программа
Cont -> Desc Cont - продолжение описания блоков
Cont -> "" - блоков может и не быть (если предыдущий последний)
Desc -> Decl Value Semi - описание одного блока
Value -> Eq Num - описание присваивания после идентификатора (= 5)
Value -> "" - описания может не быть, если только объявление
Decl -> V N Col T - объявление (val x: Int)

Eq -> "=" - терминал
Num -> num - терминал (число)
Semi -> ";" - терминал
V -> "var" | "val" - терминал
N -> id - терминал (идентификатор)
Col -> ":" - терминал
T -> "Int" - терминал
```

## Модификация
```
All -> Desc Cont
Cont -> Desc Cont
Cont -> ""
Desc -> Decl Value ";"
Value -> "=" Exp
Value -> ""
Decl -> V id ":" "Int"
V -> "var" | "val"
Exp -> Term Exp'
Exp' -> "+" Term Exp' | "-" Term Exp' | ""
Term -> Factor Term'
Term' -> "*" Factor Term' | "/" Factor Term' | ""
Factor -> ("+" | "-" | "") Factor | DigitSeq | "(" Exp ")" 
DigitSeq -> Digit DigitSeq | Digit
Digit -> "0" | ... | "9"
```

## Получилась готовая LL(1) грамматика (нет левой рекурсии и правого ветвления)

### Упростим её подстановкой терминалов (надеюсь интерпретация понятна)

```
All -> Desc Cont
Cont -> Desc Cont
Cont -> ""
Desc -> Decl Value ";"
Value -> "=" num
Value -> ""
Decl -> V id ":" "Int"
V -> "var" | "val"
```

## Терминалы

| TERM  | TOKEN     |
|-------|-----------|
| `var` | VAR       |
| `val` | VAL       |
| `id`  | NAME      |
| `:`   | COLON     |
| `Int` | TYPE      |
| `=`   | EQ        |
| `num` | NUMBER    |
| `;`   | SEMICOLON | 
| `$`   | END       | 

### [Lexer](./src/LexicalAnalyzer.java)

## Строим first / follow для Lexer'а  (```''``` = `EPSILON`)

| NONTERM | FIRST          | FOLLOW        |
|---------|----------------|---------------|
| `All`   | `{var,val}`    | `{$}`         |
| `Desc`  | `{var,val}`    | `{$,var,val}` |
| `Cont`  | `{'',var,val}` | `{$}`         |
| `Decl`  | `{var,val}`    | `{=,;}`       |
| `Value` | `{=,''}`       | `{;}`         |
| `V`     | `{var,val}`    | `{id}`        |

### [Parser](./src/Parser.java)

## Тесты

### Для тестирования использовался JUnit

### [Тесты Lexer](./test/LexicalAnalyzerTest.java)

### [Тесты Parser](./test/ParserTest.java)
