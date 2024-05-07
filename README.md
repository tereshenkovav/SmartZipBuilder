# SmartZipBuilder - умный скриптовый архиватор

Проект утилиты, которая позволяет на основе скрипта построить zip-архив сложной структуры.
Разработан на Delphi, поддерживается сборка компилятором FreePascalCompiler.
Использует код репозитория [d7zip](https://github.com/zedalaye/d7zip) и файл 7z.dll из архиватора
[7-Zip](https://www.7-zip.org)

## Возможности

* Автоматическое удаление целевого файла при запуске построения архива
* Установка имени целевого файла
* Установка степени сжатия и алгоритма сжатия
* Установка рабочего каталога в архиве (по умолчанию, используется корень архива)
* Добавление конкретного файла в архив с возможным изменением имени
* Добавление файлов по маске в архив
* Добавление каталога в архив и всех вложенных подкаталогов
* Добавление строки в указанный файл в архиве
* Поддержка переменных среды как строк в скрипте
* Поддержка аргументов архиватора как строк в скрипте

## Команды

`SetCompressor [имя алгоритма]`

Устанавливает алгоритм сжатия (Deflate, Deflate64, BZip2, LZMA, PPMD). По умолчанию используется Deflate.

`CompressionLevel [уровень сжатия]`

Задает уровень сжатия, от 0 до 9, где 9 - максимальное сжатие. По умолчанию уровень сжатия 5.

`ZipFile [имя файла]`

Задает имя выходного файла относительно текущего каталога.

`SetOutPath [каталог]`

Задает каталог в архиве, куда будут записываться данные, выводимые командами File, Files, Dir и Text.

`File [имя файла] [имя файла в архиве]`

Записывает файл в архив. Имя файла указывается относительно текущего каталога локальной файловой системы. Имя файла в архиве может быть пропущено, тогда используется исходное имя файла.

`Files [маска файлов]`

Записывает все файлы, соответствующие маске, в архив. Маска файла указывается относительно текущего каталога локальной файловой системы.

`Dir [имя каталога] [имя каталога в архиве]`

Записывает каталог в архив со всеми вложенными подкаталогами. Имя каталога указывается относительно текущего каталога локальной файловой системы. Имя каталога в архиве может быть пропущено, тогда используется исходное имя каталога.

`Text [строка] [имя файла в архиве]`

Записывает в архив строку в файл с заданным именем. Если файл существует, то строка к нему добавляется. Строка может содержать символы переноса строки /r и /n

## Запуск построения, использование переменных среды и аргументов программы

В коде скрипта можно использовать переменные среды через знак %

`%VARNAME%`

и аргументы программы через знак $

`$ARGNAME$`

которые передаются при запуске по формату

`/ARGNAME=ARGVALUE`

Запуск построения выполняется через указание имени файла скрипта как первого аргумента исполнимого файла

`SmartZipBuilder.exe [имя файла скрипта] [дополнительные необязательные аргументы]`

## Пример скрипта

Текст файла скрипта
```
# Это комментарий
SetCompressor Deflate
CompressionLevel 9
ZipFile %TEMP%\myarc.zip
SetOutPath bin
File builds\myprog.exe prog1.exe
Files builds\*.dll
SetOutPath data
Dir data\images
Text "$LANG$" defautlang
```

и его выполнение

`SmartZipBuilder.exe script.zsb /LANG=ru`

## Сборка из исходных текстов

Для сборки проекта нужен либо установленный
[Delphi](https://delphi.embarcadero.com/)
(проверено с версиями 10 и 11),
либо [FreePascalCompiler](https://www.freepascal.org)
(проверено с версией 3.0.4)

При использовании Delphi нужно открыть проект
`src\SmartZipBuilder.dproj` и выполнить его сборку в конфигурации Release.
В каталоге bin появится исполняемый файл `SmartZipBuilder.exe`

При использовании FreePascal нужно открыть каталог
`build` и запустить файл `make_win32.bat`
В каталоге bin появится исполняемый файл `SmartZipBuilder.exe`

Файл 7z.dll необходим для работы исполняемого файла. 
