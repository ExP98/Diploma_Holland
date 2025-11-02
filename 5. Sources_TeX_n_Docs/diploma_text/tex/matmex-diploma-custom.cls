%!TEX root = vkr.tex
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{tex/matmex-diploma-custom}[2020/12/09 MatMex LaTeX diploma class]
\def\my@fontsize{12pt}\def\my@baseclass{article}\def\my@title12pt{}\def\my@body14pt{}

% Если используется опция 14pt то нужно несколько хаков
\DeclareOption{14pt}{
    \def\ignore#1{}
    % во-первых, класс наследуется от extarticle
    \def\my@fontsize{14pt}
    \def\my@baseclass{extarticle}

    % во-вторых, титульный лист должен набираться так, как будто
    % используется шрифт 12pt - иначе он выглядит совсем конским
    % Для этого мы явно загружаем size12.clo, но так как он на такое не рассчитан,
    % и использует \newcommand для определения всяких \small и \Large
    % то у нас \newcommand временно превращается в \renewcommand
    \let\orignewcommand\newcommand
    \let\origNeedsTeXFormat\NeedsTeXFormat
    \def\my@title12pt{
        \let\newcommand\renewcommand
        \makeatletter
        \input{size12.clo}%
        \makeatother
        \let\newcommand\orignewcommand
    }
    % в-третьих, после того как титульный лист сгенерирован, нужно снова вернуться
    % к базовому шрифту 14pt. Применяется такой же хак: загрузи size14.clo
    % временно переопределив \newcommand => \renewcommand
    \def\my@body14pt{
        % отдельное внимание подавлению команды \NeedsTeXFormat, которая требует
        % чтобы код size14.clo выполнялся в преамбуле (а тут он уже внутри документа)
        \let\NeedsTeXFormat\ignore
        \let\newcommand\renewcommand
        \makeatletter
        \input{size14.clo}%
        \makeatother
        \let\newcommand\orignewcommand
        \let\NeedsTeXFormat\origNeedsTeXFormat
    }
}
\ProcessOptions\relax
\LoadClass[a4paper,\my@fontsize]{\my@baseclass}

% Поля настраиваются командами \newgeometry во время и после генерации титульных
% страниц и
\usepackage[a4paper]{geometry}

\usepackage{fancyhdr}
\usepackage{ifthen}

%% Мы используем компилятор XeLaTex или LuaLaTeX, который дружит с Unicode и
%% True Type/Open Type шрифтами. Попытка собрать чем-то другим приведет к
%% страшным и непонятным ошибкам, поэтому упадём с понятным сообщением.
\usepackage{iftex}
\ifXeTeX
\else
    \ifLuaTeX
    \else
        %% https://github.com/latex3/iftex/blob/a36f09ed85c47dfd8ab59ae2cc0a8c9fb99c73e1/iftex.sty#L44
        \newlinechar 64\relax
        \errorcontextlines -1\relax
        \immediate\write20{@
            ***********************************************************@
            * XeLaTex or LuaLaTex is required to compile this document.@
            * Sorry!@
            ***********************************************************}%
        \batchmode\read -1 to \@tempa
    \fi
\fi

%% Для русификации достаточно подключить пакет fontspec и
%% выбрать Unicode шрифт в котором есть кириллические глифы. Ниже
%% основным шрифтом выбирается Unicode версия шрифта Computer Modern с заcечками
\usepackage{fontspec}
\setmainfont{CMU Serif}
\setsansfont{CMU Sans Serif}
\setmonofont{CMU Typewriter Text}
%\setmonofont{Fira Code}[Contextuals=Alternate,Scale=0.9]
%\setmonofont{Inconsolata}
% \newfontfamily\cyrillicfont{CMU Serif}

%% В XeLaTex или LuaLaTeX альтернативой известного пакета babel является пакет polyglossia.
%% Теперь у нас будут переносы слов
\usepackage{polyglossia}
\setdefaultlanguage{russian}
\setotherlanguage{english}

% \addto\captionsrussian{%
%   \renewcommand{\figurename}{Рис.}%
%   \renewcommand{\tablename}{Табл.}%
% }

% Основные необходимые пакеты и команды.

%%% Пакеты
%% Графика
\usepackage[table]{xcolor} % Работа с цветами
\usepackage{graphicx} % Вставка графики в текст (изображения, в т.ч. pdf)
\graphicspath{ {figures/} } % Задает папку по-умолчанию для графики

\usepackage[labelsep=period,            % вместо ':' ставить '.'
            labelfont=bf,               % «Таблица 1» жирным (опционально)
            justification=centering,
            singlelinecheck=false
           ]{caption} % Настройка подписей "не текста"
\usepackage{subcaption} % Подписи для разделенного "не текста"

\addto\captionsrussian{\renewcommand{\figurename}{Рисунок}}

%% Математика
\usepackage{amsmath, amsfonts, amssymb, amsthm, mathtools} % "Адекватная" работа с математикой в LaTeX
\usepackage{thmtools} % Мощный пакет для работы с математическими окружениями типа "теорема"
\usepackage{thm-restate} % Дополнение к предыдущему пакету, позволяющее повторять теоремы

%% Таблицы
\usepackage{tabularx} % Добавляет столбец типа "X", который автоматически занимает максимально возможное место
\usepackage{makecell} % Позволяет кастомизировать ячейки таблицы

\usepackage{booktabs} % Пакет для верстки "более книжных" таблиц, вполне годится для оформления результатов
% В шаблоне есть команда \multirowcell, которой нужен этот пакет.
\usepackage{multirow}
\usepackage{ltablex}
\usepackage{longtable}
\usepackage{multicol}
\usepackage{array}

\setlength{\tabcolsep}{3pt}

%% Код
\usepackage{listings} % Позволяет вставлять код в документ
% Перевод заголовков в листингах
\renewcommand\lstlistingname{Листинг}
\renewcommand\lstlistlistingname{Листинги}
% Отступ перед подписью листинга
\DeclareCaptionFormat{listing}{
    \hspace{15pt}#1#2#3
}
\captionsetup[lstlisting]{
    format=listing,
    margin=0pt,
    font={bf}
}
\lstset{
    mathescape=true,
    identifierstyle=\ttfamily,
    keywordstyle=\bfseries,
    commentstyle=\scriptsize\rmfamily,
    basewidth={0.5em,0.5em},
    fontadjust=true,
}

% Требует Python с пакетом pygmentize, зато позволяет верстать очень годные листинги с синтаксической подсветкой
\usepackage[kpsewhich, newfloat]{minted}

\setminted[r]{
    breaklines=true,
    linenos,              % нумерация строк
    frame=lines,          % рамка
    numbersep=5pt,        % отступ между номерами и кодом
    stepnumber=1          % шаг нумерации
}

% Запретим minted подсвечивать по его мнению лексические ошибки
\AtBeginEnvironment{minted}{\dontdofcolorbox}
\BeforeBeginEnvironment{minted}{\fontsize{10pt}{12pt}\selectfont}
\def\dontdofcolorbox{\renewcommand\fcolorbox[4][]{##4}}

\SetupFloatingEnvironment{listing}{name=Листинг}
\SetupFloatingEnvironment{listing}{listname=Листинги}
\captionsetup[listing]{
    format=listing,
    margin=0pt,
    labelfont=bf,       % «Листинг 1» не жирным и не большим
    textfont=normal,    % текст подписи тоже нормальным размером
    skip=0pt   
}

%% Текст
\usepackage[inline]{enumitem} % Настройка списков, а так же "строчные" списки
% \setitemize{noitemsep,topsep=0pt,parsep=0pt,partopsep=0pt}
% \setenumerate{noitemsep,topsep=0pt,parsep=0pt,partopsep=0pt}

\usepackage[useregional]{datetime2} % Форматирование дат
\usepackage[normalem]{ulem} % Дополнительные варианты форматирования текста, например подчеркивание или зачеркивание
\usepackage[nopatch=footnote]{microtype} % Полезные типографические ништячки, по-хорошему требует LuaLaTeX
\usepackage[autostyle]{csquotes} % Правильные кавычки в зависимости от языка

%% Разное
\usepackage{afterpage} % Выполнение команд после разрыва страниц
\usepackage{pdflscape} % Правильное отображение альбомной страницы в pdf файле

%%% Команды
%https://tex.stackexchange.com/questions/30720/footnote-without-a-marker
\newcommand\blfootnote[1]{%
    \begingroup
    \renewcommand\thefootnote{}\footnote{#1}%
    \addtocounter{footnote}{-1}%
    \endgroup
}

%% Гиперссылки
\usepackage[
    colorlinks=true,
    urlcolor=blue,
    linkcolor=black,
    filecolor=black,
    citecolor=black]{hyperref}

%% Включение графических файлов
%% we don't need to load graphicx since it will be loaded by xunicode (which is loaded by fontspec)
\usepackage[titletoc]{appendix}
\usepackage{titlesec}

\input{tex/pretitle.tex}

%% Каждая глава начинается с новой страницы, введение и заключение не
%% нумеруются, но в оглавление входят. А само оглавление в оглавление не входит
\let\old@section\section
\def\section{\@ifstar\@section\@@section}
\def\@section#1{\newpage\old@section*{#1}%
    \ifthenelse{\equal{#1}{\contentsname}}%
    {}%
    {\addcontentsline{toc}{section}{#1}}%
}
\def\@@section#1{\newpage\old@section{#1}}

%% Полуторный интервал
\usepackage[nodisplayskipstretch]{setspace}
% \onehalfspacing
\linespread{1.2} % соответствует полуторному интервалу

%% Переименование "содержания" в "оглавление"
\gappto\captionsrussian{\renewcommand{\contentsname}{Оглавление}}

\let\savenumberline\numberline
\def\numberline#1{\savenumberline{#1.}}

%% Запретим тексту выезжать на поля вообще и совсем
\sloppy

%% Раскраска ячеек таблиц розово-зеленым градиентом
\usepackage{tex/highlight}
\definecolor{low}{HTML}{ef3b2c}
\definecolor{mid}{HTML}{fff7f7}
\definecolor{high}{HTML}{66FF66}
\newcommand{\g}[1]{\gradientcelld{#1}{6}{10.5}{11.5}{low}{mid}{high}{70}}

% Для названий стоит использовать \textsc{}
\newcommand{\BibTeX}{\textsc{BibTeX}}
\newcommand{\GitHub}{\textsc{GitHub}}

\usepackage{silence}
\WarningsOff[fancyhdr]

\usepackage{cite} % подряд идущие ссылки на источники
\bibliographystyle{tex/ugost2008l.bst}
% \bibliographystyle{plain} % plain, ieeetr

%% Сделать оглавление чуть плотнее
\usepackage{tocloft}
\setlength{\cftbeforesecskip}{0pt}
\setlength{\cftbeforesubsecskip}{0pt}
\renewcommand{\cftsecaftersnum}{.}
