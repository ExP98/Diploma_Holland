[![License: Apache 2.0](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
![Under Review](https://img.shields.io/badge/status-under_review-orange)
![Main language: R](https://img.shields.io/badge/main_language-R-276DC3?logo=r)
![Also uses: Python](https://img.shields.io/badge/also%20uses-Python-3776AB?logo=python)
![Research](https://img.shields.io/badge/type-academic%20research-blueviolet)
![Commits](https://img.shields.io/github/commit-activity/y/ExP98/Diploma_Holland)
[![](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=black&logo=RStudio&logoColor=blue)](https://exp98.shinyapps.io/diploma_holland/)

# Предсказание кода Голланда (RIASEC) по результатам психометрических тестов на основе ML

## Краткое описание

Выполнено в рамках магистерской ВКР по теме **"Определение кода Голланда по результатам психометрических тестов личности на основе методов машинного обучения в условиях неполноты информации"**.  

Проект посвящён анализу данных психометрических тестов и построению моделей машинного обучения для предсказания индивидуальных характеристик пользователей. Разработано Shiny-приложение для интерактивного взаимодействия с результатами.

Выполнил **Глушков Егор Александрович**, группа 23.М04-мм, программа "Математическое обеспечение и администрирование информационных систем"

## Описание структуры проекта

- `0. Data/` — исходные данные
- `1. R/` — скрипты на R -- основные расчетные модули и эксперименты
- `2. Python/` — модели на Python: нейросетевые модели, ранжирование
- `3. Shiny_app/`, app.R — код и вспомогательные файлы интерактивного веб-приложения
- `4. Output/` — графики и результаты
- `5. Presentation_TeX/` — исходники-материалы презентации
- `rsconnect/` — конфигурация публикации приложения

## Структура проекта

```
Diploma_Holland/
├── 0. Data/
├── 1. R/
│   ├── 00. Source_all.R
│   ├── 01. EDA/
│   ├── 02. Data_preparation.R
│   ├── 03. Regression_functions.R
│   ├── 04. Regressor_R6classes.R
│   ├── 05. Rcpp_functions.cpp
│   ├── 06. Regression_models.Rmd
│   ├── 07. Classification_models.Rmd
│   ├── 08. Learning_to_rank_models.Rmd
│   ├── 09. Non_standard_models.Rmd
│   ├── 10. Classification_functions.R
│   └── 11. Weights_selection.R
├── 2. Python/
│   ├── 01.multitarget_regression.qmd
│   ├── 02.learn_to_rank.qmd
│   ├── 03.classification_to_cluster_relations.qmd
│   ├── MLP_classifier.py
│   └── regr_metrics_func.py
├── 3. Shiny_app/
│   ├── model/
│   ├── psytest_constraints
│   └── result_output.Rmd
├── 4. Output/
├── 5. Presentation_TeX/
├── rsconnect/shinyapps.io/exp98/
├── .gitignore
├── Diploma_Holland.Rproj
├── LICENSE
├── README.md
└── app.R
```

## Как запустить проект

1. Положите данные в папку `0. Data`
2. Откройте среду разработки для R (RStudio, VS Code), установите R (4.4.2) и необходимые пакеты
3. Запустите `00. Source_all.R`
4. Для проверки регрессионных моделей проведите тесты из файла `06. Regression_models.Rmd`
5. Для проверки классификационных моделей проведите тесты из файла `07. Classification_models.Rmd`
6. Для проверки моделей ранжирования и регрессионных моделей на основе нейронных сетей:
  - Установите Python 3.12.3, PyTorch 12.8
  - регрессия: `01.multitarget_regression.qmd`
  - ранжирование: `02.learn_to_rank.qmd`
7. Для проверки моделей и подходов, не вошедших в итоговый анализ:
  - `09. Non_standard_models.Rmd`
  - `03.classification_to_cluster_relations.qmd`
8. Для запуска приложения app.R: ```r shiny::runApp()```

## Дисклеймер

Данные для настоящего научного исследования были предоставлены сторонней организацией, обладающей заинтересованностью в его результатах, и являются её собственностью. В рамках достигнутых соглашений о неразглашении конфиденциальной информации исходные материалы были оставлены в закрытом доступе.

В случае заинтересованности во внесении вклада в исследования или ознакомлении с используемыми данными просьба направить соответствующий запрос автору работы. Автор, в свою очередь, организует взаимодействие со сторонней организацией — владельцем данных.

## Обратная связь

Автор открыт к предложениям, идеям и сотрудничеству по теме научной работы: egorglushkov2014 [at] yandex [dot] ru
